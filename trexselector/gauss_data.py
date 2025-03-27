"""
Gaussian data generator for testing the T-Rex selector
"""

import numpy as np

def generate_gaussian_data(n=50, p=100, seed=789):
    """
    Generate Gaussian data for testing the T-Rex selector.
    
    Parameters
    ----------
    n : int, default=50
        Number of observations.
    p : int, default=100
        Number of variables.
    seed : int, default=789
        Random seed for reproducibility.
    
    Returns
    -------
    X : ndarray, shape (n, p)
        Predictor matrix.
    y : ndarray, shape (n,)
        Response vector.
    beta : ndarray, shape (p,)
        True coefficient vector.
    """
    if seed is not None:
        np.random.seed(seed)
    
    X = np.random.normal(0, 1, size=(n, p))
    beta = np.zeros(p)
    beta[:3] = 3.0
    y = X @ beta + np.random.normal(0, 1, size=n)
    
    return X, y, beta 