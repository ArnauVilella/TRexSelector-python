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
    # Set seed for reproducibility
    np.random.seed(seed)
    
    # Generate predictor matrix
    X = np.random.normal(0, 1, size=(n, p))
    
    # Scale X
    X = (X - X.mean(axis=0)) / X.std(axis=0, ddof=1)
    
    # Generate true coefficient vector (3 non-zero coefficients)
    beta = np.zeros(p)
    beta[:3] = 3.0
    
    # Generate response vector
    eps = np.random.normal(0, 1, size=n)
    y = X @ beta + eps
    
    # Center y
    y = y - y.mean()
    
    return X, y, beta 