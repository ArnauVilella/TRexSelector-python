"""
Function for adding random dummy variables to the predictor matrix
"""

import numpy as np

def add_dummies(X, num_dummies, seed=None):
    """
    Add random dummy variables to the predictor matrix.
    
    Parameters
    ----------
    X : ndarray, shape (n, p)
        Predictor matrix.
    num_dummies : int
        Number of dummies to append to the predictor matrix.
    seed : int, optional
        Random seed for reproducibility. If None, no seed is set.
    
    Returns
    -------
    ndarray, shape (n, p + num_dummies)
        Predictor matrix with appended dummies.
    """
    # Error control
    if not isinstance(X, np.ndarray) or X.ndim != 2:
        raise ValueError("'X' must be a 2D numpy array.")
    
    if not np.issubdtype(X.dtype, np.number):
        raise ValueError("'X' only allows numerical values.")
    
    if np.any(np.isnan(X)):
        raise ValueError("'X' contains NaNs. Please remove or impute them before proceeding.")
    
    if not isinstance(num_dummies, int) or num_dummies < 1:
        raise ValueError("'num_dummies' must be an integer larger or equal to 1.")
    
    # Number of rows in X
    n = X.shape[0]
    
    # Set seed if provided
    if seed is not None:
        np.random.seed(seed)
    
    # Generate dummy variables
    dummies = np.random.normal(0, 1, size=(n, num_dummies))
    
    X_Dummy = np.hstack((X, dummies))
    
    return X_Dummy
