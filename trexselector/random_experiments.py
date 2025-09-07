"""
Function for running K random experiments
"""

import numpy as np
from joblib import Parallel, delayed
from tlars import TLARS
from .lm_dummy import lm_dummy

def random_experiments(X, y, K=20, T_stop=1, num_dummies=None, method="trex",
                       GVS_type="EN", type="lar", corr_max=0.5, lambda_2_lars=None,
                       early_stop=True, lars_state_list=None, verbose=True,
                       intercept=False, standardize=True, dummy_coef=False,
                       parallel_process=False, parallel_max_cores=None, seed=None,
                       eps=np.finfo(float).eps):
    """
    Run K early terminated T-Rex random experiments and compute the matrix of relative
    occurrences for all variables and all numbers of included variables before stopping.
    
    Parameters
    ----------
    X : ndarray, shape (n, p)
        Predictor matrix.
    y : ndarray, shape (n,)
        Response vector.
    K : int, default=20
        Number of random experiments.
    T_stop : int, default=1
        Number of included dummies after which the random experiments are stopped.
    num_dummies : int, default=None
        Number of dummies that are appended to the predictor matrix.
        If None, it is set to the number of columns in X.
    method : {'trex', 'trex+GVS', 'trex+DA+AR1', 'trex+DA+equi', 'trex+DA+BT', 'trex+DA+NN'}, default='trex'
        Method to use.
    GVS_type : {'IEN', 'EN'}, default='EN'
        Type of group variable selection.
    type : {'lar', 'lasso'}, default='lar'
        Type of algorithm to use.
    corr_max : float, default=0.5
        Maximum allowed correlation between predictors from different clusters.
    lambda_2_lars : float, default=None
        Lambda_2 value for LARS-based Elastic Net.
    early_stop : bool, default=True
        If True, the forward selection process is stopped after T_stop dummies are included.
    lars_state_list : list, default=None
        List of TLARS models from previous computations (for warm starts).
    verbose : bool, default=True
        If True, progress in computations is shown.
    intercept : bool, default=False
        If True, an intercept is included.
    standardize : bool, default=True
        If True, the predictors are standardized and the response is centered.
    dummy_coef : bool, default=False
        If True, a matrix containing the terminal dummy coefficient vectors is returned.
    parallel_process : bool, default=False
        If True, random experiments are executed in parallel.
    parallel_max_cores : int, default=None
        Maximum number of cores to be used for parallel processing.
        If None, it is set to min(K, available_cores).
    seed : int, default=None
        Seed for random number generator.
    eps : float, default=machine epsilon
        Numerical zero.
    
    Returns
    -------
    dict
        A dictionary containing the results of the K random experiments.
    """
    # Error control
    if not isinstance(X, np.ndarray) or X.ndim != 2:
        raise ValueError("'X' must be a 2D numpy array.")
    
    if not isinstance(y, np.ndarray) or y.ndim != 1:
        raise ValueError("'y' must be a 1D numpy array.")
    
    if X.shape[0] != y.shape[0]:
        raise ValueError("Number of rows in X must match length of y.")
    
    if not isinstance(K, int) or K < 2:
        raise ValueError("The number of random experiments 'K' must be an integer larger or equal to 2.")
    
    if method not in ["trex", "trex+GVS", "trex+DA+AR1", "trex+DA+equi", "trex+DA+BT", "trex+DA+NN"]:
        raise ValueError("'method' must be one of 'trex', 'trex+GVS', 'trex+DA+AR1', 'trex+DA+equi', 'trex+DA+BT', 'trex+DA+NN'.")
    
    # Set default for num_dummies if None
    if num_dummies is None:
        num_dummies = X.shape[1]
    
    # Set default for parallel_max_cores if None
    if parallel_max_cores is None and parallel_process:
        import multiprocessing
        parallel_max_cores = min(K, multiprocessing.cpu_count())
    
    # Continue error control
    if method in ["trex", "trex+DA+AR1", "trex+DA+equi", "trex+DA+BT", "trex+DA+NN"]:
        if not isinstance(num_dummies, int) or num_dummies < 1:
            raise ValueError("'num_dummies' must be an integer larger or equal to 1.")
    else:  # method == "trex+GVS"
        p = X.shape[1]
        if not isinstance(num_dummies, int) or num_dummies % p != 0 or num_dummies < 1:
            raise ValueError("'num_dummies' must be a positive integer multiple of the total number of original predictors in X.")
    
    if not isinstance(T_stop, int) or T_stop < 1 or T_stop > num_dummies:
        raise ValueError(f"Value of 'T_stop' not valid. 'T_stop' must be an integer from 1 to {num_dummies}.")
    
    # Create empty lars_state_list if it's None
    if lars_state_list is None:
        lars_state_list = [None] * K
    elif len(lars_state_list) != K:
        raise ValueError("Length of 'lars_state_list' must be equal to number of random experiments 'K'.")
    
    # Set random seed if provided
    if seed is not None:
        np.random.seed(seed)
    
    # Define function to run a single random experiment
    def run_experiment(h, lars_state_dict=None):
        # Recreate tlars_cpp object if necessary (for parallel processing)
        if parallel_process and lars_state_dict is not None:
            lars_state = TLARS(lars_state_dict)
        else:
            lars_state = lars_state_list[h]
        
        # Run random experiment
        lars_state = lm_dummy(
            X=X,
            y=y,
            model_tlars=lars_state,
            T_stop=T_stop,
            num_dummies=num_dummies,
            method=method,
            GVS_type=GVS_type,
            type=type,
            corr_max=corr_max,
            lambda_2_lars=lambda_2_lars,
            early_stop=early_stop,
            verbose=verbose,
            intercept=intercept,
            standardize=standardize
        )
        
        # Extract T-LARS path
        lars_path = np.array(lars_state.coef_path_).T
        
        # Number of original variables
        p = X.shape[1]
        
        # Number of included dummies along solution path
        dummy_idx = np.arange(p, p + num_dummies)
        dummy_coefs = lars_path[dummy_idx, :]
        dummy_num_path = np.sum(np.abs(dummy_coefs) > eps, axis=0)
        
        # Number of included original variables along solution path
        var_idx = np.arange(p)
        var_coefs = lars_path[var_idx, :]
        var_num_path = np.sum(np.abs(var_coefs) > eps, axis=0)
        
        # Relative occurrences
        phi_T_mat = np.zeros((p, T_stop))
        for c in range(T_stop):
            if not np.any(dummy_num_path == c + 1):
                ind_sol_path = len(dummy_num_path) - 1
                if verbose:
                    print(f"Warning: For T_stop = {c}, LARS is running until k = min(n, p) and stops there before selecting {c} dummies.")
            else:
                ind_sol_path = np.where(dummy_num_path == c + 1)[0][0]
            
            phi_T_mat[:, c] = (1 / K) * (np.abs(lars_path[:p, ind_sol_path]) > eps)
        
        # Last coefficient vectors of all random experiments after termination
        rand_exp_last_betas = lars_path[:p, -1]
        
        # Dummy coefficients if requested
        if dummy_coef:
            dummy_rand_exp_last_betas = lars_path[p:p+num_dummies, -1]
        else:
            dummy_rand_exp_last_betas = None
        
        # For parallel processing, convert lars_state to dict
        if parallel_process:
            lars_state = lars_state.get_all()
        
        return phi_T_mat, rand_exp_last_betas, lars_state, dummy_rand_exp_last_betas
    
    # Run experiments in parallel or sequentially
    if parallel_process:
        # Convert lars_state_list to list of dictionaries for parallel processing
        lars_state_dicts = [lars_state.get_all() if lars_state is not None else None for lars_state in lars_state_list]
        results = Parallel(n_jobs=parallel_max_cores)(
            delayed(run_experiment)(h, lars_state_dicts[h]) for h in range(K)
        )
        phi_T_mats, rand_exp_last_betas_list, lars_state_list, dummy_rand_exp_last_betas_list = zip(*results)
    else:
        phi_T_mats = []
        rand_exp_last_betas_list = []
        lars_state_list_new = []
        dummy_rand_exp_last_betas_list = []
        
        for h in range(K):            
            phi_T_mat, rand_exp_last_betas, lars_state, dummy_rand_exp_last_betas = run_experiment(h)
            
            phi_T_mats.append(phi_T_mat)
            rand_exp_last_betas_list.append(rand_exp_last_betas)
            lars_state_list_new.append(lars_state)
            dummy_rand_exp_last_betas_list.append(dummy_rand_exp_last_betas)
        
        lars_state_list = lars_state_list_new
    
    # Combine results
    phi_T_mat = np.sum(phi_T_mats, axis=0)
    rand_exp_last_betas_mat = np.vstack(rand_exp_last_betas_list)
    Phi = np.mean(np.abs(rand_exp_last_betas_mat) > eps, axis=0)
    
    if dummy_coef:
        dummy_rand_exp_last_betas_mat = np.vstack(dummy_rand_exp_last_betas_list)
    else:
        dummy_rand_exp_last_betas_mat = None
    
    # Return results
    
    return {
        "phi_T_mat": phi_T_mat,
        "rand_exp_last_betas_mat": rand_exp_last_betas_mat,
        "dummy_rand_exp_last_betas_mat": dummy_rand_exp_last_betas_mat,
        "Phi": Phi,
        "lars_state_list": lars_state_list,
        "K": K,
        "T_stop": T_stop,
        "num_dummies": num_dummies,
        "method": method,
        "GVS_type": GVS_type,
        "type": type,
        "corr_max": corr_max,
        "lambda_2_lars": lambda_2_lars,
        "seed": seed,
        "eps": eps
    } 