"""
Tests for the add_dummies function.
"""

import pytest
import numpy as np
from trexselector import add_dummies

def test_add_dummies_input_validation(gaussian_data):
    """Test error control for inputs X and num_dummies."""
    X, y, _ = gaussian_data
    
    # Tests for X
    # Test X must be a 2D array
    with pytest.raises(ValueError, match="'X' must be a 2D numpy array"):
        add_dummies(X=X[:, 0], num_dummies=10)
    
    # Create X with NaNs
    X_with_nan = X.copy()
    X_with_nan[0, 0] = np.nan
    
    with pytest.raises(ValueError, match="'X' contains NaNs"):
        add_dummies(X=X_with_nan, num_dummies=10)
    
    # Tests for num_dummies
    # Test num_dummies must be an integer
    with pytest.raises(ValueError, match="'num_dummies' must be an integer larger or equal to 1"):
        add_dummies(X=X, num_dummies=0.5)
    
    # Test num_dummies must be >= 1
    with pytest.raises(ValueError, match="'num_dummies' must be an integer larger or equal to 1"):
        add_dummies(X=X, num_dummies=0)
    
    # Test num_dummies must not be negative
    with pytest.raises(ValueError, match="'num_dummies' must be an integer larger or equal to 1"):
        add_dummies(X=X, num_dummies=-10)

def test_add_dummies_output_shape(gaussian_data):
    """Test that the output shape of add_dummies is correct."""
    X, _, _ = gaussian_data
    
    # Create a smaller test matrix
    n, p = 20, 5
    np.random.seed(42)
    X_small = np.random.randn(n, p)
    
    # Test with different numbers of dummies
    for num_dummies in [1, 2, 3]:
        X_Dummy = add_dummies(X=X_small, num_dummies=num_dummies)
        
        # Check shape
        assert X_Dummy.shape == (n, p + num_dummies), f"Expected shape {(n, p + num_dummies)}, got {X_Dummy.shape}"
        
        # Check that the first p columns are identical to X
        np.testing.assert_allclose(X_Dummy[:, :p], X_small)
        
        # Check that the dummy variables are standardized with more tolerance
        for j in range(p, p + num_dummies):
            assert abs(X_Dummy[:, j].mean()) < 1e-10, f"Dummy column {j} is not centered"
            assert abs(X_Dummy[:, j].std() - 1.0) < 1e-10, f"Dummy column {j} is not standardized" 