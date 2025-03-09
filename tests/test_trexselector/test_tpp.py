"""
Tests for the TPP (True Positive Proportion) function.
"""

import pytest
import numpy as np
from trexselector import TPP, trex

def test_tpp_input_validation_beta_hat(gaussian_data):
    """Test error control for input beta_hat."""
    X, y, beta = gaussian_data
    
    # Run T-Rex to get beta_hat
    np.random.seed(1234)
    res = trex(X, y, verbose=False)
    beta_hat = np.zeros(beta.shape[0])
    beta_hat[res["selected_var"]] = 1
    
    # Create beta_hat with NAs
    beta_hat_with_nan = beta_hat.copy()
    beta_hat_with_nan[np.random.choice(len(beta_hat), size=10)] = np.nan
    
    # Tests
    # Test beta_hat must be 1D array
    with pytest.raises(ValueError, match="'beta_hat' must be a 1D numpy array"):
        TPP(beta_hat=np.column_stack([beta_hat, beta_hat]), beta=beta)
    
    # Test beta_hat must be numeric
    with pytest.raises(ValueError, match="'beta_hat' must be a 1D numpy array"):
        TPP(beta_hat=beta_hat.astype(str), beta=beta)
    
    # Test beta_hat cannot contain NaNs
    with pytest.raises(ValueError, match="'beta_hat' contains NaNs"):
        TPP(beta_hat=beta_hat_with_nan, beta=beta)

def test_tpp_input_validation_beta(gaussian_data):
    """Test error control for input beta."""
    X, y, beta = gaussian_data
    
    # Run T-Rex to get beta_hat
    np.random.seed(1234)
    res = trex(X, y, verbose=False)
    beta_hat = np.zeros(beta.shape[0])
    beta_hat[res["selected_var"]] = 1
    
    # Create beta with NAs
    beta_with_nan = beta.copy()
    beta_with_nan[np.random.choice(len(beta), size=10)] = np.nan
    
    # Tests
    # Test beta must be 1D array
    with pytest.raises(ValueError, match="'beta' must be a 1D numpy array"):
        TPP(beta_hat=beta_hat, beta=np.column_stack([beta, beta]))
    
    # Test beta must be numeric
    with pytest.raises(ValueError, match="'beta' must be a 1D numpy array"):
        TPP(beta_hat=beta_hat, beta=beta.astype(str))
    
    # Test beta cannot contain NaNs
    with pytest.raises(ValueError, match="'beta' contains NaNs"):
        TPP(beta_hat=beta_hat, beta=beta_with_nan)
    
    # Test length of beta_hat must match length of beta
    with pytest.raises(ValueError, match="Shapes of 'beta_hat' and 'beta' must match"):
        TPP(beta_hat=np.concatenate([beta_hat, beta_hat]), beta=beta)

def test_tpp_value_range(gaussian_data):
    """Test that the value of TPP is in the interval [0, 1]."""
    X, y, beta = gaussian_data
    
    # Run T-Rex to get beta_hat
    np.random.seed(1234)
    res = trex(X, y, verbose=False)
    beta_hat = np.zeros(beta.shape[0])
    beta_hat[res["selected_var"]] = 1
    
    # Compute TPP
    tpp = TPP(beta_hat=beta_hat, beta=beta)
    
    # Test TPP is in [0, 1]
    assert 0 <= tpp <= 1, f"TPP value {tpp} is outside the expected range [0, 1]" 