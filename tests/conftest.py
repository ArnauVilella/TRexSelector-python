"""
Pytest configuration file.
"""

import os
import sys
import pytest
import numpy as np

# Add the package root to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

@pytest.fixture
def gaussian_data():
    """
    Fixture providing Gaussian test data similar to the R package's Gauss_data.
    
    Returns:
        tuple: X, y, beta where:
            X is the predictor matrix (n x p)
            y is the response vector (n)
            beta is the true coefficient vector (p)
    """
    # Set seed for reproducibility
    np.random.seed(789)
    
    n, p = 50, 100
    
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