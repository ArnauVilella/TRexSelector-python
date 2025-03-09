"""
Function for computing the expected relative occurrences
"""

import numpy as np

def Phi_prime_fun(p, T_stop, num_dummies, phi_T_mat, Phi, eps=np.finfo(float).eps):
    """
    Compute the expected relative occurrences.
    
    Parameters
    ----------
    p : int
        Number of original variables.
    T_stop : int
        Number of included dummies before stopping.
    num_dummies : int
        Number of dummies that are appended to the predictor matrix.
    phi_T_mat : ndarray, shape (p, T_stop)
        Matrix of relative occurrences for all variables and all numbers of included variables before stopping.
    Phi : ndarray, shape (p,)
        Vector of relative occurrences at T_stop.
    eps : float, default=machine epsilon
        Numerical zero.
    
    Returns
    -------
    ndarray, shape (p,)
        Vector of expected relative occurrences.
    """
    # Error checks
    if phi_T_mat.shape != (p, T_stop):
        raise ValueError(f"'phi_T_mat' must have dimension ({p}, {T_stop}).")
    
    if Phi.shape != (p,):
        raise ValueError(f"'Phi' must have length {p}.")
    
    # Initialize Phi_prime
    Phi_prime = np.zeros(p)
    
    # Compute expected relative occurrences
    for j in range(p):
        # Compute numerator
        numerator = num_dummies * 0.5
        
        # Compute denominator
        denominator = 0
        for t in range(T_stop):
            for i in range(p):
                denominator += phi_T_mat[i, t]
        
        # Assign Phi_prime[j]
        if Phi[j] < eps:
            Phi_prime[j] = 0
        else:
            Phi_prime[j] = min(1, numerator / denominator)
    
    return Phi_prime 