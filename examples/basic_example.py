#!/usr/bin/env python3
"""
Basic example demonstrating the usage of the TRexSelector package
"""

import numpy as np
import matplotlib.pyplot as plt
from trexselector import trex, generate_gaussian_data

def main():
    # Generate synthetic Gaussian data
    print("Generating synthetic data...")
    X, y, beta_true = generate_gaussian_data(n=100, p=20, seed=1234)
    
    # Run the T-Rex selector
    print("Running T-Rex selector...")
    res = trex(
        X=X,
        y=y,
        tFDR=0.2,
        K=20,
        max_num_dummies=2,
        method="trex",
        type="lar",
        verbose=True
    )
    
    # Print results
    selected_var = res["selected_var"]
    print(f"\nSelected variables: {selected_var}")
    print(f"True non-zero variables: {np.where(beta_true != 0)[0]}")
    
    # Print some additional information
    print(f"\nInformation:")
    print(f"Target FDR: {res['tFDR']}")
    print(f"Number of dummies used: {res['num_dummies']}")
    print(f"T_stop: {res['T_stop']}")
    print(f"Voting threshold: {res['v_thresh']}")
    
    # Plot selected variables vs. true variables
    plt.figure(figsize=(12, 6))
    
    # True coefficients
    plt.subplot(1, 2, 1)
    plt.stem(beta_true, markerfmt='ro', linefmt='r-', basefmt='k-')
    plt.title('True Coefficients')
    plt.xlabel('Variable Index')
    plt.ylabel('Coefficient Value')
    plt.grid(True, alpha=0.3)
    
    # Selected variables
    plt.subplot(1, 2, 2)
    beta_selected = np.zeros_like(beta_true)
    beta_selected[selected_var] = 1
    plt.stem(beta_selected, markerfmt='bo', linefmt='b-', basefmt='k-')
    plt.title('Selected Variables')
    plt.xlabel('Variable Index')
    plt.ylabel('Selected (1) / Not Selected (0)')
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    # Save the figure instead of showing it
    plt.savefig('trex_results.png')
    print("\nPlot saved as 'trex_results.png'")
    # plt.show()  # Comment out or remove this line

if __name__ == "__main__":
    main() 