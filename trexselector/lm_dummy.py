"""
Function for performing one random experiment
"""

import numpy as np
from sklearn.discriminant_analysis import StandardScaler
from sklearn.linear_model import RidgeCV
from tlars import TLARS
from .add_dummies import add_dummies
from .add_dummies_GVS import add_dummies_GVS

def lm_dummy(X, y, model_tlars=None, T_stop=1, num_dummies=None, 
             method="trex", GVS_type="IEN", type="lar", corr_max=0.5, 
             lambda_2_lars=None, early_stop=True, verbose=True, 
             intercept=False, standardize=True):
    """
    Run one random experiment of the T-Rex selector, i.e., generates dummies, 
    appends them to the predictor matrix, and runs the forward selection algorithm 
    until it is terminated after T_stop dummies have been selected.
    
    Parameters
    ----------
    X : ndarray, shape (n, p)
        Predictor matrix.
    y : ndarray, shape (n,)
        Response vector.
    model_tlars : TLARS, default=None
        TLARS model from a previous step (for warm starts).
    T_stop : int, default=1
        Number of included dummies after which the random experiments are stopped.
    num_dummies : int, default=None
        Number of dummies that are appended to the predictor matrix.
        If None, it is set to the number of columns in X.
    method : {'trex', 'trex+GVS', 'trex+DA+AR1', 'trex+DA+equi', 'trex+DA+BT', 'trex+DA+NN'}, default='trex'
        Method to use.
    GVS_type : {'IEN', 'EN'}, default='IEN'
        Type of group variable selection.
    type : {'lar', 'lasso'}, default='lar'
        Type of algorithm to use.
    corr_max : float, default=0.5
        Maximum allowed correlation between predictors from different clusters.
    lambda_2_lars : float, default=None
        Lambda_2 value for LARS-based Elastic Net.
    early_stop : bool, default=True
        If True, the forward selection process is stopped after T_stop dummies are included.
    verbose : bool, default=True
        If True, progress in computations is shown.
    intercept : bool, default=False
        If True, an intercept is included.
    standardize : bool, default=True
        If True, the predictors are standardized and the response is centered.
    
    Returns
    -------
    TLARS
        TLARS model after the random experiment.
    """
    # Machine epsilon
    eps = np.finfo(float).eps
    
    # Error control
    if not isinstance(X, np.ndarray) or X.ndim != 2:
        raise ValueError("'X' must be a 2D numpy array.")
    
    if not np.issubdtype(X.dtype, np.number):
        raise ValueError("'X' only allows numerical values.")
    
    if np.isnan(X).any():
        raise ValueError("'X' contains NaNs. Please remove or impute them before proceeding.")
    
    if not isinstance(y, np.ndarray) or y.ndim != 1:
        raise ValueError("'y' must be a 1D numpy array.")
    
    if not np.issubdtype(y.dtype, np.number):
        raise ValueError("'y' only allows numerical values.")
    
    if np.isnan(y).any():
        raise ValueError("'y' contains NaNs. Please remove or impute them before proceeding.")
    
    if X.shape[0] != y.shape[0]:
        raise ValueError("Number of rows in X does not match length of y.")
    
    if model_tlars is not None and not isinstance(model_tlars, TLARS):
        raise ValueError("'model_tlars' must be an object of class TLARS.")
    
    if method not in ["trex", "trex+GVS", "trex+DA+AR1", "trex+DA+equi", "trex+DA+BT", "trex+DA+NN"]:
        raise ValueError("'method' must be one of 'trex', 'trex+GVS', 'trex+DA+AR1', 'trex+DA+equi', 'trex+DA+BT', 'trex+DA+NN'.")
    
    if GVS_type not in ["IEN", "EN"]:
        raise ValueError("'GVS_type' must be one of 'IEN', 'EN'.")
    
    if type not in ["lar", "lasso"]:
        raise ValueError("'type' must be one of 'lar', 'lasso'.")
    
    # Set default num_dummies if None
    if num_dummies is None:
        num_dummies = X.shape[1]
    
    # Validate num_dummies based on method
    if method in ["trex", "trex+DA+AR1", "trex+DA+equi", "trex+DA+BT", "trex+DA+NN"]:
        if not isinstance(num_dummies, int) or num_dummies < 1:
            raise ValueError("'num_dummies' must be an integer larger or equal to 1.")
    
    if method == "trex+GVS":
        if not isinstance(num_dummies, int) or num_dummies % X.shape[1] != 0 or num_dummies < 1:
            raise ValueError(
                "'num_dummies' must be a positive integer multiple of the total number of original predictors in X."
            )
    
    # Validate T_stop
    if not isinstance(T_stop, int) or T_stop < 1 or T_stop > num_dummies:
        raise ValueError(
            f"Value of 'T_stop' not valid. 'T_stop' must be an integer from 1 to {num_dummies}."
        )
    
    # Validate corr_max for trex+GVS
    if method == "trex+GVS":
        if not isinstance(corr_max, (int, float)) or corr_max < 0 or corr_max > 1:
            raise ValueError("'corr_max' must have a value between zero and one.")
        
        # Validate lambda_2_lars for trex+GVS
        if lambda_2_lars is not None:
            if not isinstance(lambda_2_lars, (int, float)) or lambda_2_lars < eps:
                raise ValueError("'lambda_2_lars' must be a number larger than zero.")
    
    # Check if this is a fresh run or a continuation
    if T_stop == 1 or model_tlars is None:
        if method in ["trex", "trex+DA+AR1", "trex+DA+equi", "trex+DA+BT", "trex+DA+NN"]:
            # Add random dummies
            X_Dummy = add_dummies(X, num_dummies)
        else:  # method == "trex+GVS"
            # Add dummies with correlation constraints
            GVS_dummies = add_dummies_GVS(X, num_dummies, corr_max)
            X_Dummy = GVS_dummies["X_Dummy"]
            
            # Ridge regression to determine lambda_2 for elastic net
            if lambda_2_lars is None:
                n = X.shape[1]
                if standardize:
                    scaler = StandardScaler()
                    X_scaled = scaler.fit_transform(X)
                else:
                    X_scaled = X
                
                alphas = np.logspace(-6, 6, 100)
                cvfit = RidgeCV(cv=10, alphas=alphas, fit_intercept=intercept)
                cvfit.fit(X_scaled, y)
                
                mean_scores = cvfit.cv_values_.mean(axis=1)
                std_scores = cvfit.cv_values_.std(axis=1) / np.sqrt(10)
                min_mean = np.min(mean_scores)
                min_idx = np.argmin(mean_scores)
                min_se = std_scores[min_idx]
                threshold = min_mean + min_se
                
                idx_candidates = np.where(mean_scores <= threshold)[0]
                if len(idx_candidates) > 0:
                    best_idx = idx_candidates[np.argmax(cvfit.alphas_[idx_candidates])]
                    lambda_2_glmnet = cvfit.alphas_[best_idx]
                else:
                    best_idx = np.argmin(mean_scores)
                    lambda_2_glmnet = cvfit.alphas_[best_idx]
                
                lambda_2_lars = lambda_2_glmnet * n / 2
            
            # Data modification for Elastic Net (EN)
            if GVS_type == "EN":
                p_dummy = X_Dummy.shape[1]
                X_Dummy = (1 / np.sqrt(1 + lambda_2_lars)) * np.vstack([
                    X_Dummy,
                    np.sqrt(lambda_2_lars) * np.eye(p_dummy)
                ])
                y = np.concatenate([y, np.zeros(p_dummy)])
            
            # Data modification for Informed Elastic Net (IEN)
            if GVS_type == "IEN":
                p = X.shape[1]
                p_dummy = X_Dummy.shape[1]
                max_clusters = GVS_dummies["max_clusters"]
                cluster_sizes = GVS_dummies["cluster_sizes"]
                IEN_cl_id_vectors = GVS_dummies["IEN_cl_id_vectors"]
                
                # Create IEN augmentation
                aug_vectors = np.tile(IEN_cl_id_vectors, p_dummy // p)
                X_Dummy = np.sqrt(lambda_2_lars) * np.vstack([
                    (1 / np.sqrt(lambda_2_lars)) * X_Dummy,
                    (1 / np.sqrt(cluster_sizes.reshape(-1, 1))) * aug_vectors.T
                ])
                y = np.concatenate([y, np.zeros(max_clusters)])
            
            # Scale data again
            X_Dummy = (X_Dummy - X_Dummy.mean(axis=0)) / X_Dummy.std(axis=0, ddof=1)
            y = y - y.mean()
        
        # Create new TLARS model
        model_tlars = TLARS(
            X=X_Dummy,
            y=y,
            num_dummies=num_dummies,
            verbose=verbose,
            intercept=intercept,
            standardize=standardize,
            type=type,
            info=False
        )

    # Execute TLARS step
    model_tlars.fit(T_stop=T_stop, early_stop=early_stop, info=False)

    return model_tlars