"""
Main function for the T-Rex selector
"""

import numpy as np
from scipy import stats
from scipy.cluster.hierarchy import linkage, fcluster
from scipy.spatial.distance import squareform
import warnings

from .random_experiments import random_experiments
from .phi_prime_fun import Phi_prime_fun
from .fdp_hat import fdp_hat
from .select_var_fun import select_var_fun
from .select_var_fun_DA_BT import select_var_fun_DA_BT

def trex(X, y, tFDR=0.2, K=20, max_num_dummies=10, max_T_stop=True,
         method="trex", GVS_type="IEN", cor_coef=None, type="lar",
         corr_max=0.5, lambda_2_lars=None, rho_thr_DA=0.02,
         hc_dist="single", hc_grid_length=None, parallel_process=False,
         parallel_max_cores=None, seed=None, eps=np.finfo(float).eps,
         verbose=True):
    """
    Run the T-Rex selector for high-dimensional variable selection with FDR control.
    
    Parameters
    ----------
    X : ndarray, shape (n, p)
        Predictor matrix.
    y : ndarray, shape (n,)
        Response vector.
    tFDR : float, default=0.2
        Target FDR level (between 0 and 1, i.e., 0% and 100%).
    K : int, default=20
        Number of random experiments.
    max_num_dummies : int, default=10
        Integer factor determining the maximum number of dummies as a multiple of the number of original variables p.
    max_T_stop : bool or int, default=True
        If True, the maximum number of dummies that can be included before stopping is set to ceiling(n / 2).
        If an integer is provided, it is used as the maximum value for T_stop.
        If False, the maximum is set to the number of dummies.
    method : {'trex', 'trex+GVS', 'trex+DA+AR1', 'trex+DA+equi', 'trex+DA+BT', 'trex+DA+NN'}, default='trex'
        Method to use.
    GVS_type : {'IEN', 'EN'}, default='IEN'
        Type of group variable selection.
    cor_coef : float, default=None
        AR(1) autocorrelation coefficient for the T-Rex+DA+AR1 selector 
        or equicorrelation coefficient for the T-Rex+DA+equi selector.
    type : {'lar', 'lasso'}, default='lar'
        Type of algorithm to use.
    corr_max : float, default=0.5
        Maximum allowed correlation between predictors from different clusters.
    lambda_2_lars : float, default=None
        Lambda_2 value for LARS-based Elastic Net.
    rho_thr_DA : float, default=0.02
        Correlation threshold for the T-Rex+DA+AR1 selector and the T-Rex+DA+equi selector.
    hc_dist : str, default='single'
        Distance measure of the hierarchical clustering/dendrogram (only for trex+DA+BT).
    hc_grid_length : int, default=None
        Length of the height-cutoff-grid for the dendrogram.
        If None, it is set to min(20, p).
    parallel_process : bool, default=False
        If True, random experiments are executed in parallel.
    parallel_max_cores : int, default=None
        Maximum number of cores to be used for parallel processing.
        If None, it is set to min(K, available_cores).
    seed : int, default=None
        Seed for random number generator.
    eps : float, default=machine epsilon
        Numerical zero.
    verbose : bool, default=True
        If True, progress in computations is shown.
    
    Returns
    -------
    dict
        A dictionary containing the estimated support vector and additional information,
        including the number of used dummies and the number of included dummies before stopping.
    """
    # Error control
    if not isinstance(X, np.ndarray) or X.ndim != 2:
        raise ValueError("'X' must be a 2D numpy array.")
    
    if not isinstance(y, np.ndarray) or y.ndim != 1:
        raise ValueError("'y' must be a 1D numpy array.")
    
    if X.shape[0] != y.shape[0]:
        raise ValueError("Number of rows in X must match length of y.")
    
    if not isinstance(tFDR, (int, float)) or tFDR < 0 or tFDR > 1:
        raise ValueError("'tFDR' must be a number between 0 and 1 (including 0 and 1).")
    
    if not isinstance(K, int) or K < 2:
        raise ValueError("The number of random experiments 'K' must be an integer larger or equal to 2.")
    
    if not isinstance(max_num_dummies, int) or max_num_dummies < 1:
        raise ValueError("'max_num_dummies' must be an integer larger or equal to 1.")
    
    if method not in ["trex", "trex+GVS", "trex+DA+AR1", "trex+DA+equi", "trex+DA+BT", "trex+DA+NN"]:
        raise ValueError("'method' must be one of 'trex', 'trex+GVS', 'trex+DA+AR1', 'trex+DA+equi', 'trex+DA+BT', 'trex+DA+NN'.")
    
    if GVS_type not in ["IEN", "EN"]:
        raise ValueError("'GVS_type' must be one of 'IEN', 'EN'.")
    
    if type not in ["lar", "lasso"]:
        raise ValueError("'type' must be one of 'lar', 'lasso'.")
    
    # Scale X and center y
    X = (X - X.mean(axis=0)) / X.std(axis=0, ddof=1)
    y = y - y.mean()
    
    # Number of rows n and columns p of X
    n, p = X.shape
    
    # Set default for hc_grid_length if None
    if hc_grid_length is None:
        hc_grid_length = min(20, p)
    
    # Set up binary tree for T-Rex+DA+BT
    if method == "trex+DA+BT":
        # Compute correlation matrix
        cor_mat = np.corrcoef(X, rowvar=False)
        
        # Compute distance matrix: 1 - |corr_mat|
        cor_mat_distance = 1 - np.abs(cor_mat)
        
        # Hierarchical clustering - use squareform to convert to condensed format
        dendrogram = linkage(squareform(cor_mat_distance, checks=False), method=hc_dist)
        
        # Create rho grid - match R's seq() behavior more closely
        rho_grid_subsample = np.round(np.linspace(1, p, hc_grid_length)).astype(int) - 1  # Convert to 0-based
        rho_grid_len = hc_grid_length
        # In R: c(1 - rev(dendrogram$height), 1)[rho_grid_subsample]
        heights_reversed = 1 - dendrogram[::-1, 2]  # Reverse and convert heights
        rho_grid = np.concatenate([heights_reversed, [1]])[rho_grid_subsample]
        
        # Generate clusters
        clusters = np.zeros((p, rho_grid_len), dtype=int)
        for x in range(rho_grid_len):
            clusters[:, x] = fcluster(dendrogram, 1 - rho_grid[x], criterion='distance') - 1  # 0-based indexing
        
        # Generate group lists
        gr_j_list = []
        for j in range(p):
            gr_j_sub_list = []
            for x in range(rho_grid_len):
                gr_num_j = clusters[j, x]
                gr_j = np.where(clusters[:, x] == gr_num_j)[0]
                gr_j = gr_j[gr_j != j]
                gr_j_sub_list.append(gr_j)
            gr_j_list.append(gr_j_sub_list)
        
        # Closest correlation point to reference point (for determining number of dummies)
        opt_point_BT = int(round(0.75 * rho_grid_len)) - 1
    
    # Set up nearest neighbors groups for T-Rex+DA+NN
    if method == "trex+DA+NN":
        # Compute correlation matrix
        cor_mat = np.corrcoef(X, rowvar=False)
        
        # Create rho grid
        rho_grid_len = hc_grid_length
        rho_grid = np.linspace(0, 1, rho_grid_len)
        
        # Generate group lists
        gr_j_list = []
        for j in range(p):
            gr_j_sub_list = []
            for x in range(rho_grid_len):
                gr_j = np.where(np.abs(cor_mat[:, j]) >= rho_grid[x])[0]
                gr_j = gr_j[gr_j != j]
                gr_j_sub_list.append(gr_j)
            gr_j_list.append(gr_j_sub_list)
        
        opt_point_BT = int(round(0.75 * rho_grid_len)) - 1
    
    # Calculate AR(1) coefficient for T-Rex+DA+AR1 if not provided
    if method == "trex+DA+AR1" and cor_coef is None:
        # This is a simplified version - R code is more complex
        # For a proper implementation, you'd need to fit ARIMA models
        ar_coeffs = []
        for j in range(p):
            # Simple approximation: correlation between consecutive elements
            if len(X) > 1:
                ar_coeffs.append(abs(np.corrcoef(X[:-1, j], X[1:, j])[0, 1]))
            else:
                ar_coeffs.append(0)
        cor_coef = np.mean(ar_coeffs)
    
    # Calculate equicorrelation coefficient for T-Rex+DA+equi if not provided
    if method == "trex+DA+equi" and cor_coef is None:
        # Compute average off-diagonal correlation
        cor_mat = np.corrcoef(X, rowvar=False)
        cor_coef = np.mean(cor_mat[np.triu_indices(p, k=1)])
    
    # Voting level grid
    V = np.arange(0.5, 1.0, 1/20)
    V = np.append(V, 1.0 - eps)
    V_len = len(V)
    
    # Initialize L-loop
    LL = 1
    T_stop = 1
    
    # Initialize FDP_hat with NaN (equivalent to R's NA) - THIS IS THE KEY FIX
    if method in ["trex+DA+BT", "trex+DA+NN"]:
        FDP_hat = np.full((V_len, rho_grid_len), np.nan)
    else:
        FDP_hat = np.full(V_len, np.nan)
    
    # 75% voting reference point for determining number of dummies
    opt_point = np.argmin(np.abs(V - 0.75))
    if len(V[V < 0.75]) > 0 and np.abs(V[opt_point] - 0.75) >= eps:
        # If 75% optimization point does not exist, choose closest optimization point lower than 75%
        opt_point = len(V[V < 0.75]) - 1  # -1 for 0-based indexing
    
    # FDP larger than tFDR - check with NaN handling
    if method in ["trex+DA+BT", "trex+DA+NN"]:
        fdp_larger_tFDR = not np.isnan(FDP_hat[opt_point, opt_point_BT]) and FDP_hat[opt_point, opt_point_BT] > tFDR
    else:
        fdp_larger_tFDR = not np.isnan(FDP_hat[opt_point]) and FDP_hat[opt_point] > tFDR
    
    # First phase: determine number of dummies
    while (LL <= max_num_dummies and fdp_larger_tFDR) or np.all(np.isnan(FDP_hat)):
        num_dummies = LL * p
        LL += 1
        
        # Run K random experiments
        rand_exp = random_experiments(
            X=X,
            y=y,
            K=K,
            T_stop=T_stop,
            num_dummies=num_dummies,
            method=method,
            GVS_type=GVS_type,
            type=type,
            corr_max=corr_max,
            lambda_2_lars=lambda_2_lars,
            early_stop=True,
            verbose=verbose,
            intercept=False,
            standardize=True,
            parallel_process=parallel_process,
            parallel_max_cores=parallel_max_cores,
            eps=eps,
            seed=seed
        )
        
        phi_T_mat = rand_exp["phi_T_mat"]
        Phi = rand_exp["Phi"]
        
        # Dependency aware relative occurrences for T-Rex+DA+AR1 selector
        if method == "trex+DA+AR1":
            kap = int(np.ceil(np.log(rho_thr_DA) / np.log(cor_coef)))
            DA_delta_mat = np.zeros((p, T_stop))
            
            for t in range(T_stop):
                for j in range(p):
                    # Build sliding window - match R logic exactly
                    sliding_window = np.concatenate([
                        np.arange(max(0, j-kap), j),  # max(1, j-kap) to max(1, j-1) in R (1-based)
                        np.arange(j+1, min(p, j+kap+1))  # min(p, j+1) to min(p, j+kap) in R
                    ])
                    
                    # Remove j if it's at boundaries (R logic)
                    if j in [0, p-1]:
                        sliding_window = sliding_window[sliding_window != j]
                    
                    if len(sliding_window) > 0:
                        DA_delta_mat[j, t] = 2 - np.min(np.abs(phi_T_mat[j, t] - phi_T_mat[sliding_window, t]))
                    else:
                        DA_delta_mat[j, t] = 2
            
            phi_T_mat = phi_T_mat / DA_delta_mat
            Phi = Phi / DA_delta_mat[:, T_stop-1]
        
        # Dependency aware relative occurrences for T-Rex+DA+equi selector
        if method == "trex+DA+equi":
            if abs(cor_coef) > rho_thr_DA:
                DA_delta_mat = np.zeros((p, T_stop))
                
                for t in range(T_stop):
                    for j in range(p):
                        sliding_window = np.delete(np.arange(p), j)
                        DA_delta_mat[j, t] = 2 - np.min(np.abs(phi_T_mat[j, t] - phi_T_mat[sliding_window, t]))
                
                phi_T_mat = phi_T_mat / DA_delta_mat
                Phi = Phi / DA_delta_mat[:, T_stop-1]
        
        # Dependency aware relative occurrences for T-Rex+DA+BT or T-Rex+DA+NN selector
        if method in ["trex+DA+BT", "trex+DA+NN"]:
            DA_delta_mat_BT = np.zeros((p, rho_grid_len))
            
            for j in range(p):
                for rho_idx in range(rho_grid_len):
                    gr_j = gr_j_list[j][rho_idx]
                    if len(gr_j) == 0:
                        DA_delta_mat_BT[j, rho_idx] = 2
                    else:
                        DA_delta_mat_BT[j, rho_idx] = 2 - np.min(np.abs(phi_T_mat[j, T_stop-1] - phi_T_mat[gr_j, T_stop-1]))
            
            # Create 3D array for phi_T_mat with rho dimension
            phi_T_array_BT = np.zeros((p, T_stop, rho_grid_len))
            for rho_idx in range(rho_grid_len):
                phi_T_array_BT[:, :, rho_idx] = phi_T_mat / DA_delta_mat_BT[:, rho_idx].reshape(-1, 1)
            
            Phi_BT = Phi / DA_delta_mat_BT
        
        # Compute Phi_prime and FDP_hat
        if method in ["trex+DA+BT", "trex+DA+NN"]:
            # Phi_prime for each rho value
            Phi_prime = np.zeros((p, rho_grid_len))
            for rho_idx in range(rho_grid_len):
                Phi_prime[:, rho_idx] = Phi_prime_fun(
                    p=p,
                    T_stop=T_stop,
                    num_dummies=num_dummies,
                    phi_T_mat=phi_T_array_BT[:, :, rho_idx],
                    Phi=Phi_BT[:, rho_idx],
                    eps=eps
                )
            
            # FDP_hat for each rho value
            for rho_idx in range(rho_grid_len):
                FDP_hat[:, rho_idx] = fdp_hat(
                    V=V,
                    Phi=Phi_BT[:, rho_idx],
                    Phi_prime=Phi_prime[:, rho_idx]
                )
        else:
            # Compute Phi_prime
            Phi_prime = Phi_prime_fun(
                p=p,
                T_stop=T_stop,
                num_dummies=num_dummies,
                phi_T_mat=phi_T_mat,
                Phi=Phi,
                eps=eps
            )
            
            # Compute FDP_hat
            FDP_hat = fdp_hat(
                V=V,
                Phi=Phi,
                Phi_prime=Phi_prime
            )
        
        # Check if FDP > tFDR
        if method in ["trex+DA+BT", "trex+DA+NN"]:
            fdp_larger_tFDR = FDP_hat[opt_point, opt_point_BT] > tFDR
        else:
            fdp_larger_tFDR = FDP_hat[opt_point] > tFDR
        
        # Display progress
        if verbose:
            print(f"Appended dummies: {num_dummies}")
    
    # Initialize arrays for tracking FDP_hat across T_stop values
    if method in ["trex+DA+BT", "trex+DA+NN"]:
        FDP_hat_array_BT = np.expand_dims(FDP_hat, axis=0)
        Phi_array_BT = np.expand_dims(Phi_BT, axis=0)
    else:
        FDP_hat_mat = np.expand_dims(FDP_hat, axis=0)
        Phi_mat = np.expand_dims(Phi, axis=0)
    
    # Determine maximum T_stop
    if isinstance(max_T_stop, int):
        max_T = max_T_stop
    elif max_T_stop:
        max_T = min(num_dummies, int(np.ceil(n / 2)))
    else:
        max_T = num_dummies
    
    # Reset seed (match R behavior)
    if seed is not None:
        seed += 12345
    
    # Check if FDP < tFDR at highest voting threshold
    if method in ["trex+DA+BT", "trex+DA+NN"]:
        fdp_lower_tFDR = FDP_hat[V_len-1, opt_point_BT] <= tFDR
    else:
        fdp_lower_tFDR = FDP_hat[V_len-1] <= tFDR
    
    # Second phase: increase T_stop until FDP exceeds tFDR
    while fdp_lower_tFDR and (T_stop < max_T):
        T_stop += 1
        
        # Run K random experiments with increased T_stop
        rand_exp = random_experiments(
            X=X,
            y=y,
            K=K,
            T_stop=T_stop,
            num_dummies=num_dummies,
            method=method,
            GVS_type=GVS_type,
            type=type,
            corr_max=corr_max,
            lambda_2_lars=lambda_2_lars,
            early_stop=True,
            lars_state_list=rand_exp.get("lars_state_list"),
            verbose=verbose,
            intercept=False,
            standardize=True,
            parallel_process=parallel_process,
            parallel_max_cores=parallel_max_cores,
            eps=eps,
            seed=seed
        )
        
        phi_T_mat = rand_exp["phi_T_mat"]
        Phi = rand_exp["Phi"]
        
        # Apply dependency-aware adjustments (same as above)
        if method == "trex+DA+AR1":
            DA_delta_mat = np.zeros((p, T_stop))
            
            for t in range(T_stop):
                for j in range(p):
                    sliding_window = np.concatenate([
                        np.arange(max(0, j-kap), j),
                        np.arange(j+1, min(p, j+kap+1))
                    ])
                    
                    if j in [0, p-1]:
                        sliding_window = sliding_window[sliding_window != j]
                    
                    if len(sliding_window) > 0:
                        DA_delta_mat[j, t] = 2 - np.min(np.abs(phi_T_mat[j, t] - phi_T_mat[sliding_window, t]))
                    else:
                        DA_delta_mat[j, t] = 2
            
            phi_T_mat = phi_T_mat / DA_delta_mat
            Phi = Phi / DA_delta_mat[:, T_stop-1]
        
        if method == "trex+DA+equi":
            if abs(cor_coef) > rho_thr_DA:
                DA_delta_mat = np.zeros((p, T_stop))
                
                for t in range(T_stop):
                    for j in range(p):
                        sliding_window = np.delete(np.arange(p), j)
                        DA_delta_mat[j, t] = 2 - np.min(np.abs(phi_T_mat[j, t] - phi_T_mat[sliding_window, t]))
                
                phi_T_mat = phi_T_mat / DA_delta_mat
                Phi = Phi / DA_delta_mat[:, T_stop-1]
        
        if method in ["trex+DA+BT", "trex+DA+NN"]:
            DA_delta_mat_BT = np.zeros((p, rho_grid_len))
            
            for j in range(p):
                for rho_idx in range(rho_grid_len):
                    gr_j = gr_j_list[j][rho_idx]
                    if len(gr_j) == 0:
                        DA_delta_mat_BT[j, rho_idx] = 2
                    else:
                        DA_delta_mat_BT[j, rho_idx] = 2 - np.min(np.abs(phi_T_mat[j, T_stop-1] - phi_T_mat[gr_j, T_stop-1]))
            
            phi_T_array_BT = np.zeros((p, T_stop, rho_grid_len))
            for rho_idx in range(rho_grid_len):
                phi_T_array_BT[:, :, rho_idx] = phi_T_mat / DA_delta_mat_BT[:, rho_idx].reshape(-1, 1)
            
            Phi_BT = Phi / DA_delta_mat_BT
            
            # Expand the arrays
            Phi_array_BT = np.concatenate([Phi_array_BT, np.expand_dims(Phi_BT, axis=0)], axis=0)
        else:
            # Expand Phi_mat
            Phi_mat = np.vstack([Phi_mat, Phi])
        
        # Compute Phi_prime and FDP_hat
        if method in ["trex+DA+BT", "trex+DA+NN"]:
            # Phi_prime for each rho value
            Phi_prime = np.zeros((p, rho_grid_len))
            for rho_idx in range(rho_grid_len):
                Phi_prime[:, rho_idx] = Phi_prime_fun(
                    p=p,
                    T_stop=T_stop,
                    num_dummies=num_dummies,
                    phi_T_mat=phi_T_array_BT[:, :, rho_idx],
                    Phi=Phi_BT[:, rho_idx],
                    eps=eps
                )
            
            # FDP_hat for each rho value
            FDP_hat_current = np.zeros((V_len, rho_grid_len))
            for rho_idx in range(rho_grid_len):
                FDP_hat_current[:, rho_idx] = fdp_hat(
                    V=V,
                    Phi=Phi_BT[:, rho_idx],
                    Phi_prime=Phi_prime[:, rho_idx]
                )
            
            # Expand FDP_hat_array_BT
            FDP_hat_array_BT = np.concatenate([FDP_hat_array_BT, np.expand_dims(FDP_hat_current, axis=0)], axis=0)
            FDP_hat = FDP_hat_current  # Update for next iteration check
        else:
            # Compute Phi_prime
            Phi_prime = Phi_prime_fun(
                p=p,
                T_stop=T_stop,
                num_dummies=num_dummies,
                phi_T_mat=phi_T_mat,
                Phi=Phi,
                eps=eps
            )
            
            # Compute FDP_hat
            FDP_hat = fdp_hat(
                V=V,
                Phi=Phi,
                Phi_prime=Phi_prime
            )
            
            # Expand FDP_hat_mat
            FDP_hat_mat = np.vstack([FDP_hat_mat, FDP_hat])
        
        # Check if FDP < tFDR at highest voting threshold
        if method in ["trex+DA+BT", "trex+DA+NN"]:
            fdp_lower_tFDR = FDP_hat[V_len-1, opt_point_BT] <= tFDR
        else:
            fdp_lower_tFDR = FDP_hat[V_len-1] <= tFDR
        
        if verbose:
            print(f"Included dummies before stopping: {T_stop}")
    
    # Select variables based on estimated FDP and voting thresholds
    if method in ["trex+DA+BT", "trex+DA+NN"]:
        res_T_dummy = select_var_fun_DA_BT(
            p=p,
            tFDR=tFDR,
            T_stop=T_stop,
            FDP_hat_array_BT=FDP_hat_array_BT,
            Phi_array_BT=Phi_array_BT,
            V=V,
            rho_grid=rho_grid
        )
        selected_var = res_T_dummy["selected_var"]
        v_thresh = res_T_dummy["v_thresh"]
        rho_thresh = res_T_dummy["rho_thresh"]
        R_array = res_T_dummy["R_array"]
        
        # Create results dictionary
        result = {
            "selected_var": selected_var,
            "tFDR": tFDR,
            "T_stop": T_stop,
            "num_dummies": num_dummies,
            "V": V,
            "rho_grid": rho_grid,
            "v_thresh": v_thresh,
            "rho_thresh": rho_thresh,
            #
            "FDP_hat_array_BT": FDP_hat_array_BT,
            "Phi_array_BT": Phi_array_BT,
            "R_array": R_array,
            "phi_T_array_BT": phi_T_array_BT,
            #
            "Phi_prime": Phi_prime,
            "method": method,
            "GVS_type": GVS_type,
            "cor_coef": cor_coef,
            "type": type,
            "corr_max": corr_max,
            "lambda_2_lars": lambda_2_lars,
            "rho_thr_DA": rho_thr_DA,
            "hc_dist": hc_dist
        }
    else:
        res_T_dummy = select_var_fun(
            p=p,
            tFDR=tFDR,
            T_stop=T_stop,
            FDP_hat_mat=FDP_hat_mat,
            Phi_mat=Phi_mat,
            V=V
        )
        selected_var = res_T_dummy["selected_var"]
        v_thresh = res_T_dummy["v_thresh"]
        rho_thresh = None
        R_mat = res_T_dummy["R_mat"]
        
        # Create results dictionary
        result = {
            "selected_var": selected_var,
            "tFDR": tFDR,
            "T_stop": T_stop,
            "num_dummies": num_dummies,
            "V": V,
            "v_thresh": v_thresh,
            "rho_thresh": rho_thresh,
            #
            "FDP_hat_mat": FDP_hat_mat,
            "Phi_mat": Phi_mat,
            "R_mat": R_mat,
            "phi_T_mat": phi_T_mat,
            #
            "Phi_prime": Phi_prime,
            "method": method,
            "GVS_type": GVS_type,
            "cor_coef": cor_coef,
            "type": type,
            "corr_max": corr_max,
            "lambda_2_lars": lambda_2_lars,
            "rho_thr_DA": rho_thr_DA,
            "hc_dist": hc_dist
        }
    
    return result