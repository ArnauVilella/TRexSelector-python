"""
Tests for the main trex function.
"""

import pytest
import numpy as np
from trexselector import trex

def test_trex_input_validation_method_type(gaussian_data):
    """Test error control for inputs method and type."""
    X, y, _ = gaussian_data
    
    # Test method must be one of the allowed values
    with pytest.raises(ValueError, match="'method' must be one of"):
        trex(X=X, y=y, method="test")
    
    # Test type must be one of the allowed values
    with pytest.raises(ValueError, match="'type' must be one of"):
        trex(X=X, y=y, type="test")

def test_trex_input_validation_X(gaussian_data):
    """Test error control for input X."""
    X, y, _ = gaussian_data
    
    # Test X must be a 2D array
    with pytest.raises(ValueError, match="'X' must be a 2D numpy array"):
        trex(X=X[:, 0], y=y)
    
    # Create X with NaNs
    X_with_nan = X.copy()
    X_with_nan[0, 0] = np.nan
    
    with pytest.raises(ValueError, match="'X' contains NaNs"):
        trex(X=X_with_nan, y=y)
    
    # Test X with non-numeric values - this will either raise when creating the X array
    # or during standardization, so we just check for any ValueError
    with pytest.raises(Exception):
        trex(X=X.astype(str), y=y)

def test_trex_input_validation_y(gaussian_data):
    """Test error control for input y."""
    X, y, _ = gaussian_data
    
    # Test y must be a 1D array
    with pytest.raises(ValueError, match="'y' must be a 1D numpy array"):
        trex(X=X, y=np.column_stack([y, y]))
    
    # Create y with NaNs
    y_with_nan = y.copy()
    y_with_nan[0] = np.nan
    
    # Check for a ValueError with a message containing "NaN" (case insensitive)
    with pytest.raises(ValueError, match="NaN"):
        trex(X=X, y=y_with_nan)
    
    # Test X and y must have compatible shapes
    with pytest.raises(ValueError, match="Number of rows in X must match length of y"):
        trex(X=X, y=y[:-1])

def test_trex_input_validation_parameters(gaussian_data):
    """Test error control for other parameters."""
    X, y, _ = gaussian_data
    
    # Test tFDR must be between 0 and 1
    with pytest.raises(ValueError, match="'tFDR' must be a number between 0 and 1"):
        trex(X=X, y=y, tFDR=-0.1)
    
    with pytest.raises(ValueError, match="'tFDR' must be a number between 0 and 1"):
        trex(X=X, y=y, tFDR=1.1)
    
    # Test K must be >= 2
    with pytest.raises(ValueError, match="The number of random experiments 'K' must be an integer larger or equal to 2"):
        trex(X=X, y=y, K=1)
    
    # Test max_num_dummies must be >= 1
    with pytest.raises(ValueError, match="'max_num_dummies' must be an integer larger or equal to 1"):
        trex(X=X, y=y, max_num_dummies=0)

def test_trex_basic_functionality(gaussian_data):
    """Test that trex produces the expected output."""
    X, y, beta = gaussian_data
    
    # Run trex with minimal parameters
    np.random.seed(1234)
    res = trex(X=X, y=y, tFDR=0.2, K=5, max_num_dummies=1, verbose=False)
    
    # Check that the output contains the expected keys
    expected_keys = [
        "selected_var", "tFDR", "T_stop", "num_dummies", "V", "v_thresh"
    ]
    for key in expected_keys:
        assert key in res, f"Expected key {key} not found in result"
    
    # Check that selected_var is a numpy array
    assert isinstance(res["selected_var"], np.ndarray), "selected_var should be a numpy array"
    
    # Check that T_stop is a positive integer
    assert isinstance(res["T_stop"], int), "T_stop should be an integer"
    assert res["T_stop"] > 0, "T_stop should be positive"
    
    # Check that the number of dummies is as expected
    assert res["num_dummies"] == X.shape[1], "num_dummies should be p in the first iteration" 