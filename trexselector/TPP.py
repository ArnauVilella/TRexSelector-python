"""
Function for computing true positive proportion (TPP)
"""

import numpy as np

def TPP(beta_hat, beta, eps=np.finfo(float).eps):
    """
    Compute the true positive proportion (TPP).
    
    Parameters
    ----------
    beta_hat : ndarray
        Estimated coefficient vector.
    beta : ndarray
        True coefficient vector.
    eps : float, default=machine epsilon
        Numerical zero.
    
    Returns
    -------
    float
        True positive proportion.
    """
    # Error control
    if not isinstance(beta_hat, np.ndarray) or beta_hat.ndim != 1:
        raise ValueError("'beta_hat' must be a 1D numpy array.")
    
    if not np.issubdtype(beta_hat.dtype, np.number):
        raise ValueError("'beta_hat' must be a 1D numpy array of numeric values.")
    
    if np.any(np.isnan(beta_hat)):
        raise ValueError("'beta_hat' contains NaNs. Please remove or impute them before proceeding.")
    
    if not isinstance(beta, np.ndarray) or beta.ndim != 1:
        raise ValueError("'beta' must be a 1D numpy array.")
    
    if not np.issubdtype(beta.dtype, np.number):
        raise ValueError("'beta' must be a 1D numpy array of numeric values.")
    
    if np.any(np.isnan(beta)):
        raise ValueError("'beta' contains NaNs. Please remove or impute them before proceeding.")
    
    if beta_hat.shape != beta.shape:
        raise ValueError("Shapes of 'beta_hat' and 'beta' must match.")
    
    num_actives = np.sum(np.abs(beta) > eps)
    num_true_positives = np.sum((np.abs(beta_hat) > eps) & (np.abs(beta) > eps))

    if num_actives == 0:
        return 0
    else:
        return num_true_positives / num_actives 