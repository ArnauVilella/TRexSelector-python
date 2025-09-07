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
    max_T_stop : bool, default=True
        If True, the maximum number of dummies that can be included before stopping is set to ceiling(n / 2),
        where n is the number of data points/observations.
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
    
    if not isinstance(tFDR, float) or tFDR < 0 or tFDR > 1:
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
    
    # Set up binary tree for T-Rex+DA+BT or T-Rex+DA+NN
    if method == "trex+DA+BT":
        # Compute correlation matrix
        cor_mat = np.corrcoef(X, rowvar=False)
        
        # Compute distance matrix: 1 - |corr_mat|
        cor_mat_distance = 1 - np.abs(cor_mat)
        
        # Hierarchical clustering
        from scipy.spatial.distance import pdist
        dendrogram = linkage(pdist(X.T), method=hc_dist)
        # dendrogram = linkage(squareform(cor_mat_distance, checks=False), method=hc_dist)
        
        # Create rho grid
        rho_grid_subsample = np.round(np.linspace(0, p-1, hc_grid_length)).astype(int)
        rho_grid_len = hc_grid_length
        rho_grid = np.concatenate([[1 - h for h in dendrogram[:, 2]], [1]])[rho_grid_subsample]
        
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
        opt_point_BT = round(0.75 * rho_grid_len)
    
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
        
        opt_point_BT = round(0.75 * rho_grid_len)
    
    # Calculate AR(1) coefficient for T-Rex+DA+AR1 if not provided
    if method == "trex+DA+AR1" and cor_coef is None:
        # Compute average AR(1) coefficient across all variables
        ar_coeffs = []
        for j in range(p):
            try:
                # Fit AR(1) model
                with warnings.catch_warnings():
                    warnings.simplefilter("ignore")
                    ar_model = stats.arima_model.ARIMA(X[:, j], order=(1, 0, 0))
                    ar_results = ar_model.fit(disp=0)
                    ar_coeffs.append(abs(ar_results.arparams[0]))
            except:
                # If fit fails, use zero
                ar_coeffs.append(0)
        
        cor_coef = np.mean(ar_coeffs)
    
    # Calculate equicorrelation coefficient for T-Rex+DA+equi if not provided
    if method == "trex+DA+equi" and cor_coef is None:
        # Compute average off-diagonal correlation
        cor_mat = np.corrcoef(X, rowvar=False)
        cor_coef = np.mean(cor_mat[np.triu_indices(p, k=1)])
    
    # Voting level grid
    V = np.arange(0.5, 1.0 + eps, 1/K)
    V_len = len(V)
    
    # Find 75% voting reference point
    opt_point = np.abs(V - 0.75).argmin()
    
    # Initialize arrays for dependency-aware variants
    if method in ["trex+DA+BT", "trex+DA+NN"]:
        FDP_hat_values = np.zeros((V_len, rho_grid_len))
    else:
        FDP_hat_values = np.zeros(V_len)
    
    # Initialize L-loop (for determining number of dummies)
    LL = 1
    T_stop = 1
    
    # Initialize for checking if FDP > tFDR
    if method in ["trex+DA+BT", "trex+DA+NN"]:
        fdp_larger_tFDR = FDP_hat_values[opt_point, opt_point_BT] > tFDR
    else:
        fdp_larger_tFDR = FDP_hat_values[opt_point] > tFDR
    
    # Determine number of dummies
    while (LL <= max_num_dummies and fdp_larger_tFDR) or np.all(np.isnan(FDP_hat_values)):
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
            seed=seed,
            eps=eps
        )
        
        phi_T_mat = rand_exp["phi_T_mat"]
        Phi = rand_exp["Phi"]
        
        # Dependency aware relative occurrences for T-Rex+DA+AR1 selector
        if method == "trex+DA+AR1":
            kap = int(np.ceil(np.log(rho_thr_DA) / np.log(cor_coef)))
            DA_delta_mat = np.zeros((p, T_stop))
            
            for t in range(T_stop):
                for j in range(p):
                    sliding_window = np.concatenate([
                        np.arange(max(0, j-kap), max(0, j)),
                        np.arange(min(p, j+1), min(p, j+kap+1))
                    ])
                    
                    if j in [0, p-1]:
                        sliding_window = sliding_window[sliding_window != j]
                    
                    DA_delta_mat[j, t] = 2 - min(np.abs(phi_T_mat[j, t] - phi_T_mat[sliding_window, t]))
            
            phi_T_mat = phi_T_mat / DA_delta_mat
            Phi = Phi / DA_delta_mat[:, T_stop-1]
        
        # Dependency aware relative occurrences for T-Rex+DA+equi selector
        if method == "trex+DA+equi":
            if abs(cor_coef) > rho_thr_DA:
                DA_delta_mat = np.zeros((p, T_stop))
                
                for t in range(T_stop):
                    for j in range(p):
                        sliding_window = np.delete(np.arange(p), j)
                        DA_delta_mat[j, t] = 2 - min(np.abs(phi_T_mat[j, t] - phi_T_mat[sliding_window, t]))
                
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
                        DA_delta_mat_BT[j, rho_idx] = 2 - min(np.abs(phi_T_mat[j, T_stop-1] - phi_T_mat[gr_j, T_stop-1]))
            
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
            FDP_hat_values = np.zeros((V_len, rho_grid_len))
            for rho_idx in range(rho_grid_len):
                FDP_hat_values[:, rho_idx] = fdp_hat(
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
            FDP_hat_values = fdp_hat(
                V=V,
                Phi=Phi,
                Phi_prime=Phi_prime
            )
        
        # Check if FDP > tFDR
        if method in ["trex+DA+BT", "trex+DA+NN"]:
            fdp_larger_tFDR = FDP_hat_values[opt_point, opt_point_BT] > tFDR
        else:
            fdp_larger_tFDR = FDP_hat_values[opt_point] > tFDR
        
        # Display progress
        if verbose:
            print(f"Appended dummies: {num_dummies}")
    
    # Initialize arrays for tracking FDP_hat across T_stop values
    if method in ["trex+DA+BT", "trex+DA+NN"]:
        FDP_hat_array_BT = np.expand_dims(FDP_hat_values, axis=0)
        Phi_array_BT = np.expand_dims(Phi_BT, axis=0)
    else:
        # If we haven't initialized Phi yet, do so with zeros
        if 'Phi' not in locals():
            Phi = np.zeros(p)
            # Initialize num_dummies as well
            num_dummies = p
        FDP_hat_mat = np.expand_dims(FDP_hat_values, axis=0)
        Phi_mat = np.expand_dims(Phi, axis=0)
    
    # Determine maximum T_stop
    if max_T_stop:
        max_T = min(num_dummies, int(np.ceil(n / 2)))
    else:
        max_T = num_dummies
    
    # Reset seed for second phase
    if seed is not None:
        seed += 12345
    
    # Check if FDP < tFDR at highest voting threshold
    if method in ["trex+DA+BT", "trex+DA+NN"]:
        fdp_lower_tFDR = FDP_hat_values[V_len-1, opt_point_BT] <= tFDR
    else:
        fdp_lower_tFDR = FDP_hat_values[V_len-1] <= tFDR
    
    # Initialize rand_exp if not defined yet
    if 'rand_exp' not in locals():
        rand_exp = {'lars_state_list': None}
    
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
            lars_state_list=rand_exp["lars_state_list"],
            verbose=verbose,
            intercept=False,
            standardize=True,
            parallel_process=parallel_process,
            parallel_max_cores=parallel_max_cores,
            seed=seed,
            eps=eps
        )
        
        phi_T_mat = rand_exp["phi_T_mat"]
        Phi = rand_exp["Phi"]
        
        # Dependency aware relative occurrences for the T-Rex+DA+AR1 selector
        if method == "trex+DA+AR1":
            DA_delta_mat = np.zeros((p, T_stop))
            
            for t in range(T_stop):
                for j in range(p):
                    sliding_window = np.concatenate([
                        np.arange(max(0, j-kap), max(0, j)),
                        np.arange(min(p, j+1), min(p, j+kap+1))
                    ])
                    
                    if j in [0, p-1]:
                        sliding_window = sliding_window[sliding_window != j]
                    
                    DA_delta_mat[j, t] = 2 - min(np.abs(phi_T_mat[j, t] - phi_T_mat[sliding_window, t]))
            
            phi_T_mat = phi_T_mat / DA_delta_mat
            Phi = Phi / DA_delta_mat[:, T_stop-1]
        
        # Dependency aware relative occurrences for T-Rex+DA+equi selector
        if method == "trex+DA+equi":
            if abs(cor_coef) > rho_thr_DA:
                DA_delta_mat = np.zeros((p, T_stop))
                
                for t in range(T_stop):
                    for j in range(p):
                        sliding_window = np.delete(np.arange(p), j)
                        DA_delta_mat[j, t] = 2 - min(np.abs(phi_T_mat[j, t] - phi_T_mat[sliding_window, t]))
                
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
                        DA_delta_mat_BT[j, rho_idx] = 2 - min(np.abs(phi_T_mat[j, T_stop-1] - phi_T_mat[gr_j, T_stop-1]))
            
            # Create 3D array for phi_T_mat with rho dimension
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
            FDP_hat_values = np.zeros((V_len, rho_grid_len))
            for rho_idx in range(rho_grid_len):
                FDP_hat_values[:, rho_idx] = fdp_hat(
                    V=V,
                    Phi=Phi_BT[:, rho_idx],
                    Phi_prime=Phi_prime[:, rho_idx]
                )
            
            # Expand FDP_hat_array_BT
            FDP_hat_array_BT = np.concatenate([FDP_hat_array_BT, np.expand_dims(FDP_hat_values, axis=0)], axis=0)
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
            FDP_hat_values = fdp_hat(
                V=V,
                Phi=Phi,
                Phi_prime=Phi_prime
            )
            
            # Expand FDP_hat_mat
            FDP_hat_mat = np.vstack([FDP_hat_mat, FDP_hat_values])
        
        # Check if FDP < tFDR at highest voting threshold
        if method in ["trex+DA+BT", "trex+DA+NN"]:
            fdp_lower_tFDR = FDP_hat_values[V_len-1, opt_point_BT] <= tFDR
        else:
            fdp_lower_tFDR = FDP_hat_values[V_len-1] <= tFDR
        
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