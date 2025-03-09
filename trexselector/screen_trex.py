"""
Screening variant of the T-Rex selector for ultra-high dimensional datasets
"""

import numpy as np
from .trex import trex

def screen_trex(X, y, tFDR=0.2, K=20, max_num_dummies=10, max_T_stop=True,
                method="trex", GVS_type="IEN", cor_coef=None, type="lar",
                corr_max=0.5, lambda_2_lars=None, rho_thr_DA=0.02,
                hc_dist="single", hc_grid_length=None, parallel_process=False,
                parallel_max_cores=None, seed=None, eps=np.finfo(float).eps,
                verbose=True, q=None, num_splits=1):
    """
    Screening variant of the T-Rex selector for ultra-high dimensional datasets.
    
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
    q : int, default=None
        Number of variables to be selected in each of the split sub-problems.
        If None, it is determined automatically.
    num_splits : int, default=1
        Number of splits of the original problem.
    
    Returns
    -------
    dict
        A dictionary containing the screening results.
    """
    # Error control
    if not isinstance(X, np.ndarray) or X.ndim != 2:
        raise ValueError("'X' must be a 2D numpy array.")
    
    if not isinstance(y, np.ndarray) or y.ndim != 1:
        raise ValueError("'y' must be a 1D numpy array.")
    
    if X.shape[0] != y.shape[0]:
        raise ValueError("Number of rows in X must match length of y.")
    
    if method not in ["trex", "trex+GVS", "trex+DA+AR1", "trex+DA+equi", "trex+DA+BT", "trex+DA+NN"]:
        raise ValueError("'method' must be one of 'trex', 'trex+GVS', 'trex+DA+AR1', 'trex+DA+equi', 'trex+DA+BT', 'trex+DA+NN'.")
    
    # Number of rows n and columns p of X
    n, p = X.shape
    
    # Set default for hc_grid_length if None
    if hc_grid_length is None:
        hc_grid_length = min(20, p)
    
    # Set default for q if None (auto-adjustment)
    if q is None:
        q = max(10, min(p // 5, n))
    
    # Error check for q
    if not isinstance(q, int) or q < 1 or q > p:
        raise ValueError(f"'q' must be an integer between 1 and {p}.")
    
    # Error check for num_splits
    if not isinstance(num_splits, int) or num_splits < 1:
        raise ValueError("'num_splits' must be an integer larger or equal to 1.")
    
    # Use original T-Rex if p is small or num_splits is 1 and q equals p
    if p <= n or (num_splits == 1 and q == p):
        if verbose:
            print("Using original T-Rex selector (no screening needed)...")
        
        return trex(
            X=X,
            y=y,
            tFDR=tFDR,
            K=K,
            max_num_dummies=max_num_dummies,
            max_T_stop=max_T_stop,
            method=method,
            GVS_type=GVS_type,
            cor_coef=cor_coef,
            type=type,
            corr_max=corr_max,
            lambda_2_lars=lambda_2_lars,
            rho_thr_DA=rho_thr_DA,
            hc_dist=hc_dist,
            hc_grid_length=hc_grid_length,
            parallel_process=parallel_process,
            parallel_max_cores=parallel_max_cores,
            seed=seed,
            eps=eps,
            verbose=verbose
        )
    
    # Set random seed if provided
    if seed is not None:
        np.random.seed(seed)
    
    # Partition the variable indices
    if num_splits > 1:
        # Randomly permute indices
        indices_perm = np.random.permutation(p)
        
        # Split indices into num_splits parts
        splits = np.array_split(indices_perm, num_splits)
    else:
        # Use correlation-based screening
        from scipy.stats import spearmanr
        
        # Calculate correlations between X and y
        correlations = np.zeros(p)
        for j in range(p):
            correlations[j] = abs(spearmanr(X[:, j], y)[0])
        
        # Get indices of top q correlations
        splits = [np.argsort(correlations)[-q:]]
    
    # Initialize results
    if verbose:
        print(f"Screening with {num_splits} splits, selecting {q} variables per split...")
    
    # Initialize containers for screened variables
    all_selected_var = []
    all_v_thresh = []
    all_rho_thresh = []
    
    # Process each split
    for i, split_indices in enumerate(splits):
        if verbose:
            print(f"Processing split {i+1}/{len(splits)} with {len(split_indices)} variables...")
        
        # Extract subset of predictors
        X_sub = X[:, split_indices]
        
        # Apply T-Rex to the subset
        res_sub = trex(
            X=X_sub,
            y=y,
            tFDR=tFDR,
            K=K,
            max_num_dummies=max_num_dummies,
            max_T_stop=max_T_stop,
            method=method,
            GVS_type=GVS_type,
            cor_coef=cor_coef,
            type=type,
            corr_max=corr_max,
            lambda_2_lars=lambda_2_lars,
            rho_thr_DA=rho_thr_DA,
            hc_dist=hc_dist,
            hc_grid_length=min(hc_grid_length, len(split_indices)),
            parallel_process=parallel_process,
            parallel_max_cores=parallel_max_cores,
            seed=seed + i if seed is not None else None,
            eps=eps,
            verbose=verbose
        )
        
        # Map selected variables back to original indices
        selected_var_orig = split_indices[res_sub["selected_var"]]
        all_selected_var.append(selected_var_orig)
        
        # Save thresholds
        all_v_thresh.append(res_sub["v_thresh"])
        if "rho_thresh" in res_sub:
            all_rho_thresh.append(res_sub["rho_thresh"])
    
    # Combine results from all splits
    combined_selected_var = np.concatenate(all_selected_var)
    
    # Remove duplicates (if any)
    unique_selected_var = np.unique(combined_selected_var)
    
    # Create final result
    result = {
        "selected_var": unique_selected_var,
        "tFDR": tFDR,
        "split_selected_var": all_selected_var,
        "v_thresh": all_v_thresh,
        "method": method,
        "num_splits": num_splits,
        "q": q
    }
    
    # Add rho_thresh if available
    if all_rho_thresh:
        result["rho_thresh"] = all_rho_thresh
    
    return result 