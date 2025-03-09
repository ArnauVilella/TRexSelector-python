"""
Function for performing one random experiment
"""

import numpy as np
from tlars import TLARS
from sklearn.linear_model import ElasticNetCV
from .add_dummies import add_dummies, add_dummies_GVS

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
    # Error control
    if not isinstance(X, np.ndarray) or X.ndim != 2:
        raise ValueError("'X' must be a 2D numpy array.")
    
    if not isinstance(y, np.ndarray) or y.ndim != 1:
        raise ValueError("'y' must be a 1D numpy array.")
    
    if X.shape[0] != y.shape[0]:
        raise ValueError("Number of rows in X must match length of y.")
    
    if method not in ["trex", "trex+GVS", "trex+DA+AR1", "trex+DA+equi", "trex+DA+BT", "trex+DA+NN"]:
        raise ValueError("'method' must be one of 'trex', 'trex+GVS', 'trex+DA+AR1', 'trex+DA+equi', 'trex+DA+BT', 'trex+DA+NN'.")
    
    if GVS_type not in ["IEN", "EN"]:
        raise ValueError("'GVS_type' must be one of 'IEN', 'EN'.")
    
    if type not in ["lar", "lasso"]:
        raise ValueError("'type' must be one of 'lar', 'lasso'.")
    
    # Set default num_dummies if None
    if num_dummies is None:
        num_dummies = X.shape[1]
    
    # Machine epsilon
    eps = np.finfo(float).eps
    
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
                n = X.shape[0]
                alpha = 0  # Ridge regression
                en_cv = ElasticNetCV(l1_ratio=alpha, cv=10, random_state=0)
                en_cv.fit(X, y)
                lambda_2_glmnet = en_cv.alpha_
                lambda_2_lars = lambda_2_glmnet * n * (1 - alpha) / 2
            
            # Data modification for Elastic Net (EN)
            if GVS_type == "EN":
                p_dummy = X_Dummy.shape[1]
                X_aug = (1 / np.sqrt(1 + lambda_2_lars)) * np.vstack([
                    X_Dummy,
                    np.sqrt(lambda_2_lars) * np.eye(p_dummy)
                ])
                y_aug = np.concatenate([y, np.zeros(p_dummy)])
                X_Dummy = X_aug
                y = y_aug
            
            # Data modification for Informed Elastic Net (IEN)
            if GVS_type == "IEN":
                p = X.shape[1]
                p_dummy = X_Dummy.shape[1]
                max_clusters = GVS_dummies["max_clusters"]
                cluster_sizes = GVS_dummies["cluster_sizes"]
                IEN_cl_id_vectors = GVS_dummies["IEN_cl_id_vectors"]
                
                # Create IEN augmentation
                aug_vectors = np.tile(IEN_cl_id_vectors, p_dummy // p)
                X_aug = np.sqrt(lambda_2_lars) * np.vstack([
                    (1 / np.sqrt(lambda_2_lars)) * X_Dummy,
                    (1 / np.sqrt(cluster_sizes.reshape(-1, 1))) * aug_vectors.T
                ])
                y_aug = np.concatenate([y, np.zeros(max_clusters)])
                X_Dummy = X_aug
                y = y_aug
            
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