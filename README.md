# TRexSelector-Python (trexselector)

A Python port of the [TRexSelector](https://github.com/jasinmachkour/TRexSelector) R package for high-dimensional variable selection with false discovery rate (FDR) control.

## Overview

TRexSelector performs fast variable selection in high-dimensional settings while controlling the false discovery rate (FDR) at a user-defined target level. The package implements the Terminating-Random Experiments Selector (T-Rex) as described in [Machkour, Muma, and Palomar (2025)](https://doi.org/10.1016/j.sigpro.2025.109894).

This Python package provides a port of the original R implementation, maintaining the same functionality while providing a more Pythonic interface. The Python port was created by Arnau Vilella (avp@connect.ust.hk).

## Installation

### Requirements

- Python >= 3.8
- numpy
- scipy >= 1.7.0
- scikit-learn >= 1.0.0
- tlars
- joblib >= 1.0.0
- matplotlib >= 3.4.0
- pandas >= 1.3.0

For manylinux (most Linux distributions), macOS, and Windows platforms, all dependencies including `tlars` will be installed automatically when installing the package. For other systems, you might need to build the `tlars` package from source.

```bash
pip install trexselector==0.6.14
```

## Usage

```python
import numpy as np
from trexselector import trex, generate_gaussian_data

# Generate some example data
X, y, beta = generate_gaussian_data(n=100, p=20, seed=1234)

# Run the T-Rex selector
res = trex(X=X, y=y)

# Get the selected variables
selected_var = res["selected_var"]
print(f"Selected variables: {selected_var}")
```

## Library Reference

### Main Functions

#### trex(X, y, tFDR=0.2, K=20, max_num_dummies=10, max_T_stop=True, method="trex", ...)

The main function for high-dimensional variable selection with FDR control.

- **X**: ndarray - Predictor matrix of shape (n, p).
- **y**: ndarray - Response vector of shape (n,).
- **tFDR**: float - Target FDR level (between 0 and 1).
- **K**: int - Number of random experiments.
- **max_num_dummies**: int - Factor determining maximum number of dummies.
- **max_T_stop**: bool - If True, maximum number of included dummies is set to ceiling(n/2).
- **method**: str - Method to use ('trex', 'trex+GVS', 'trex+DA+AR1', 'trex+DA+equi', 'trex+DA+BT', 'trex+DA+NN').
- **Returns**: dict - Contains selected variables and additional information.

#### screen_trex(X, y, tFDR=0.2, K=20, max_num_dummies=10, ...)

Screening variant of T-Rex for ultra-high dimensional datasets.

- **X**, **y**, **tFDR**, **K**: Same as trex().
- **q**: int - Number of variables to select in each split.
- **num_splits**: int - Number of splits of the original problem.
- **Returns**: dict - Contains selected variables and screening information.

#### random_experiments(X, y, K=20, T_stop=1, num_dummies=None, ...)

Run K random experiments with the T-Rex selector.

- **X**, **y**, **K**: Same as trex().
- **T_stop**: int - Number of included dummies before stopping.
- **num_dummies**: int - Number of dummies to append.
- **parallel_process**: bool - If True, experiments run in parallel.
- **Returns**: dict - Contains experiment results and statistics.

### Helper Functions

#### add_dummies(X, num_dummies)

Add random dummy variables to the predictor matrix.

- **X**: ndarray - Predictor matrix.
- **num_dummies**: int - Number of dummies to append.
- **Returns**: ndarray - Matrix with appended dummies.

#### add_dummies_GVS(X, num_dummies, corr_max=0.5)

Add dummy variables with correlation constraints for group variable selection.

- **X**: ndarray - Predictor matrix.
- **num_dummies**: int - Number of dummies to append.
- **corr_max**: float - Maximum allowed correlation between predictors.
- **Returns**: dict - Contains matrix with dummies and group information.

#### FDP(beta_hat, beta)

Compute the false discovery proportion.

- **beta_hat**: ndarray - Estimated coefficient vector.
- **beta**: ndarray - True coefficient vector.
- **Returns**: float - False discovery proportion.

#### TPP(beta_hat, beta)

Compute the true positive proportion.

- **beta_hat**: ndarray - Estimated coefficient vector.
- **beta**: ndarray - True coefficient vector.
- **Returns**: float - True positive proportion.

#### generate_gaussian_data(n=50, p=100, seed=789)

Generate synthetic Gaussian data for testing.

- **n**: int - Number of observations.
- **p**: int - Number of variables.
- **seed**: int - Random seed.
- **Returns**: tuple - (X, y, beta) containing predictor matrix, response, and true coefficients.

#### fdp_hat(V, Phi, Phi_prime)

Compute the estimated FDP for a set of voting thresholds.

- **V**: ndarray - Voting thresholds.
- **Phi**: ndarray - Vector of relative occurrences.
- **Phi_prime**: ndarray - Vector of expected relative occurrences.
- **Returns**: ndarray - Estimated FDP for each voting threshold.

#### Phi_prime_fun(p, T_stop, num_dummies, phi_T_mat, Phi)

Compute the expected relative occurrences for the T-Rex selector.

- **p**: int - Number of variables.
- **T_stop**: int - Number of included dummies before stopping.
- **num_dummies**: int - Number of dummies appended.
- **phi_T_mat**: ndarray - Matrix of relative occurrences.
- **Phi**: ndarray - Vector of relative occurrences.
- **Returns**: ndarray - Vector of expected relative occurrences.

#### select_var_fun(p, tFDR, T_stop, FDP_hat_mat, Phi_mat, V)

Select variables based on estimated FDP and voting thresholds for basic T-Rex variants.

- **p**: int - Number of variables.
- **tFDR**: float - Target FDR level.
- **T_stop**: int - Number of included dummies before stopping.
- **FDP_hat_mat**: ndarray - Matrix of estimated FDP values.
- **Phi_mat**: ndarray - Matrix of relative occurrences.
- **V**: ndarray - Voting thresholds.
- **Returns**: dict - Contains selected variables and selection information.

#### select_var_fun_DA_BT(p, tFDR, T_stop, FDP_hat_array_BT, Phi_array_BT, V, rho_grid)

Select variables based on estimated FDP and voting thresholds for dependency-aware T-Rex variants.

- **p**: int - Number of variables.
- **tFDR**: float - Target FDR level.
- **T_stop**: int - Number of included dummies before stopping.
- **FDP_hat_array_BT**: ndarray - Array of estimated FDP values.
- **Phi_array_BT**: ndarray - Array of relative occurrences.
- **V**: ndarray - Voting thresholds.
- **rho_grid**: ndarray - Grid of correlation thresholds.
- **Returns**: dict - Contains selected variables and selection information.

### Advanced Features

The package supports several variants of the T-Rex selector:

- **Basic T-Rex**: Standard variable selection with FDR control
- **T-Rex+GVS**: Group variable selection using correlation structure
- **T-Rex+DA variants**: Dependency-aware variants
  - AR1: Using AR(1) correlation structure
  - Equi: Using equicorrelation structure
  - BT: Using binary tree structure
  - NN: Using nearest neighbor structure

## References

- Machkour, J., Muma, M., & Palomar, D. P. (2025). The Terminating-Random Experiments Selector: Fast High-Dimensional Variable Selection with False Discovery Rate Control. Signal Processing, 231, 109894.

## License

This package is licensed under the GNU General Public License v3.0 (GPL-3.0).

## Acknowledgments

The original R package [TRexSelector](https://github.com/jasinmachkour/TRexSelector) was created by Jasin Machkour, Simon Tien, Daniel P. Palomar, and Michael Muma. This Python port was developed by Arnau Vilella (avp@connect.ust.hk).
