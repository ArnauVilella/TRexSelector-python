# TRexSelector-Python (trexselector)

A Python port of the [TRexSelector](https://github.com/jasinmachkour/TRexSelector) R package for high-dimensional variable selection with false discovery rate (FDR) control.

## Overview

TRexSelector performs fast variable selection in high-dimensional settings while controlling the false discovery rate (FDR) at a user-defined target level. The package is based on the paper [Machkour, Muma, and Palomar (2022)](https://arxiv.org/abs/2110.06048).

This Python package provides a port of the original R implementation, maintaining the same functionality while providing a more Pythonic interface. The Python port was created by Arnau Vilella (avp@connect.ust.hk).

## Installation

```bash
pip install trexselector==0.6.0
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

## Functions

The package contains the following main functions:

- `trex`: The main function for the T-Rex selector, which performs variable selection while controlling the FDR
- `screen_trex`: A screening variant of the T-Rex selector for ultra-high dimensional datasets
- `random_experiments`: Run K random experiments with the T-Rex selector
- `add_dummies`, `add_dummies_GVS`: Helper functions for adding dummy variables 
- `FDP`, `TPP`: Functions for computing false discovery and true positive proportions

## References

- Machkour, J., Muma, M., & Palomar, D. P. (2022). Model-Free Variable Selection with Directed Controls for the False Discovery Rate via Coefficient of Intrinsic Dependence. arXiv preprint arXiv:2110.06048.
- Machkour, J., Muma, M., & Palomar, D. P. (2022). High-dimensional variable selection for classification via binary random sketching. 2022 30th European Signal Processing Conference (EUSIPCO), 2202-2206.

## License

This package is licensed under the GNU General Public License v3.0 (GPL-3.0).

## Acknowledgments

The original R package [TRexSelector](https://github.com/jasinmachkour/TRexSelector) was created by Jasin Machkour, Simon Tien, Daniel P. Palomar, and Michael Muma. This Python port was developed by Arnau Vilella (avp@connect.ust.hk).
