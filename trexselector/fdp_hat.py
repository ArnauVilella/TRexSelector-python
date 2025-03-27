"""
Function for computing estimated false discovery proportion (FDP)
"""

import numpy as np

def fdp_hat(V, Phi, Phi_prime, eps=np.finfo(float).eps):
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
    eps : float, default=machine epsilon
        Numerical zero.
    
    Returns
    -------
    ndarray
        Estimated FDP for each voting threshold.
    """
    fdp_h = np.full(len(V), np.nan)
    for i in range(len(V)):
        num_sel_var = np.sum(Phi > V[i])
        if num_sel_var < eps:
            fdp_h[i] = 0
        else:
            indices = Phi > V[i]
            fdp_h[i] = min(1, np.sum((1 - Phi_prime)[indices]) / num_sel_var)
    
    return fdp_h 