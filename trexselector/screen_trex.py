"""
Screening variant of the T-Rex selector for ultra-high dimensional datasets
"""

import numpy as np
from .random_experiments import random_experiments

def screen_trex(X, y, K=20, R=1000, method="trex", bootstrap=False,
                conf_level_grid=None, cor_coef=None, type="lar",
                corr_max=0.5, lambda_2_lars=None, rho_thr_DA=0.02,
                parallel_process=False, parallel_max_cores=None, seed=None,
                eps=np.finfo(float).eps, verbose=True):
    """
    Screening variant of the T-Rex selector based on bootstrapping.
    
    Parameters
    ----------
    X : ndarray, shape (n, p)
        Predictor matrix.
    y : ndarray, shape (n,)
        Response vector.
    K : int, default=20
        Number of random experiments.
    R : int, default=1000
        Number of bootstrap resamples.
    method : {'trex', 'trex+GVS', 'trex+DA+AR1', 'trex+DA+equi'}, default='trex'
        Method to use.
    bootstrap : bool, default=False
        If True, Screen-T-Rex is carried out with bootstrapping.
    conf_level_grid : ndarray, default=None
        Confidence level grid for the bootstrap confidence intervals. 
        If None, defaults to np.arange(0, 1.001, 0.001).
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
    parallel_process : bool, default=False
        If True, random experiments are executed in parallel.
    parallel_max_cores : int, default=None
        Maximum number of cores to be used for parallel processing.
    seed : int, default=None
        Seed for random number generator.
    eps : float, default=machine epsilon
        Numerical zero.
    verbose : bool, default=True
        If True, progress in computations is shown.
    
    Returns
    -------
    dict
        A dictionary containing the screening results.
    """
    # Set default for conf_level_grid if None
    if conf_level_grid is None:
        conf_level_grid = np.arange(0, 1.001, 0.001)

    # Error control from R version
    if method not in ["trex", "trex+GVS", "trex+DA+AR1", "trex+DA+equi"]:
        raise ValueError("'method' must be one of 'trex', 'trex+GVS', 'trex+DA+AR1', 'trex+DA+equi'.")

    # Scale X and center y
    X = (X - X.mean(axis=0)) / X.std(axis=0, ddof=1)
    y = y - y.mean()

    p = X.shape[1]
    T_stop = 1

    # Run random experiments
    rand_exp = random_experiments(
        X=X, y=y, K=K, T_stop=T_stop, num_dummies=p,
        method=method, type=type, corr_max=corr_max, lambda_2_lars=lambda_2_lars,
        early_stop=True, verbose=verbose, intercept=False, standardize=True,
        dummy_coef=True, parallel_process=parallel_process,
        parallel_max_cores=parallel_max_cores, seed=seed, eps=eps
    )

    beta_mat = rand_exp["rand_exp_last_betas_mat"]
    dummy_beta_mat = rand_exp["dummy_rand_exp_last_betas_mat"]
    Phi = rand_exp["Phi"]

    # Simplified bootstrap implementation
    if bootstrap:
        boot_data = dummy_beta_mat[np.abs(dummy_beta_mat) > eps]
        if len(boot_data) > 0:
            boot_means = np.array([np.mean(np.random.choice(boot_data, len(boot_data), replace=True)) for _ in range(R)])
            
            # Calculate confidence intervals for each level in the grid
            lower_bounds = np.percentile(boot_means, [(1-cl)*50 for cl in conf_level_grid])
            upper_bounds = np.percentile(boot_means, [(1+cl)*50 for cl in conf_level_grid])
            bootstrap_conf_int = np.column_stack([lower_bounds, upper_bounds])

            maj_vote_set_zero = Phi <= 0.5
            beta_means = np.mean(beta_mat, axis=0)
            R_no_boot = np.sum(~maj_vote_set_zero)
            
            R_with_boot = np.array([np.sum(~((beta_means >= bci[0]) & (beta_means <= bci[1]))) for bci in bootstrap_conf_int])
            
            valid_indices = np.where(R_with_boot <= R_no_boot)[0]
            conf_level_index = valid_indices.max() if len(valid_indices) > 0 else -1
            conf_level = conf_level_grid[conf_level_index]
            
            boot_set_zero = (beta_means >= bootstrap_conf_int[conf_level_index, 0]) & (beta_means <= bootstrap_conf_int[conf_level_index, 1])
            set_zero = boot_set_zero
        else:
            set_zero = Phi <= 0.5
            conf_level = np.nan
    else:
        set_zero = Phi <= 0.5
        conf_level = np.nan

    selected_var = np.where(~set_zero)[0]
    
    if len(selected_var) == 0:
        FDR_estimate = 0
    else:
        FDR_estimate = T_stop / len(selected_var)

    result = {
        "selected_var": selected_var,
        "FDR_estimate": FDR_estimate,
        "dummy_beta_mat": dummy_beta_mat,
        "beta_mat": beta_mat,
        "Phi": Phi,
        "K": K,
        "R": R,
        "method": method,
        "bootstrap": bootstrap,
        "conf_level": conf_level,
        "cor_coef": cor_coef,
        "type": type,
        "corr_max": corr_max,
        "lambda_2_lars": lambda_2_lars,
        "rho_thr_DA": rho_thr_DA
    }

    return result
 