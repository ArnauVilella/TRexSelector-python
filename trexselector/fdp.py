"""
Functions for computing false discovery proportion (FDP) and true positive proportion (TPP)
"""

import numpy as np

def FDP(beta_hat, beta, eps=np.finfo(float).eps):
    """
    Compute the false discovery proportion (FDP).
    
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
        False discovery proportion.
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
    
    # Number of estimated variables
    R = np.sum(np.abs(beta_hat) > eps)
    
    # If no variable is selected, the FDP is defined as 0
    if R == 0:
        return 0.0
    
    # False discoveries
    V = np.sum((np.abs(beta_hat) > eps) & (np.abs(beta) <= eps))
    
    # False discovery proportion
    return V / R

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
    
    # Number of true non-zero coefficients
    S = np.sum(np.abs(beta) > eps)
    
    # If there are no true non-zero coefficients, the TPP is defined as 0
    if S == 0:
        return 0.0
    
    # True positives
    T = np.sum((np.abs(beta_hat) > eps) & (np.abs(beta) > eps))
    
    # True positive proportion
    return T / S

def fdp_hat(V, Phi, Phi_prime):
    """
    Compute the estimated FDP for a set of voting thresholds.
    
    Parameters
    ----------
    V : ndarray
        Voting thresholds.
    Phi : ndarray
        Vector of relative occurrences.
    Phi_prime : ndarray
        Vector of expected relative occurrences.
    
    Returns
    -------
    ndarray
        Estimated FDP for each voting threshold.
    """
    p = len(Phi)
    V_len = len(V)
    
    # Initialize estimated FDP
    FDP_hat = np.zeros(V_len)
    
    # Compute estimated FDP for each voting threshold
    for i, v in enumerate(V):
        R = np.sum(Phi >= v)
        if R == 0:
            FDP_hat[i] = 0.0
        else:
            FDP_hat[i] = np.sum(Phi_prime * (Phi >= v)) / R
    
    return FDP_hat 