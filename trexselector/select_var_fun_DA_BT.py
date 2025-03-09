"""
Function for selecting variables based on estimated FDP and voting thresholds for the dependency-aware variant
"""

import numpy as np

def select_var_fun_DA_BT(p, tFDR, T_stop, FDP_hat_array_BT, Phi_array_BT, V, rho_grid):
    """
    Select variables based on estimated FDP and voting thresholds for the dependency-aware variant.
    
    Parameters
    ----------
    p : int
        Number of original variables.
    tFDR : float
        Target FDR level (between 0 and 1).
    T_stop : int
        Number of included dummies before stopping.
    FDP_hat_array_BT : ndarray, shape (T_stop, len(V), len(rho_grid))
        Array of estimated FDP values for each T_stop, voting threshold, and rho value.
    Phi_array_BT : ndarray, shape (T_stop, p, len(rho_grid))
        Array of relative occurrences for each T_stop, variable, and rho value.
    V : ndarray
        Vector of voting thresholds.
    rho_grid : ndarray
        Grid of rho values for the dependency-aware variant.
    
    Returns
    -------
    dict
        A dictionary containing:
        - selected_var: Indices of selected variables
        - v_thresh: Selected voting threshold
        - rho_thresh: Selected rho threshold
        - R_array: Number of selected variables for each T_stop, voting threshold, and rho value
    """
    # Error checks
    if FDP_hat_array_BT.shape[0] != T_stop or FDP_hat_array_BT.shape[1] != len(V) or FDP_hat_array_BT.shape[2] != len(rho_grid):
        raise ValueError(f"'FDP_hat_array_BT' must have dimensions ({T_stop}, {len(V)}, {len(rho_grid)}).")
    
    if Phi_array_BT.shape[0] != T_stop or Phi_array_BT.shape[1] != p or Phi_array_BT.shape[2] != len(rho_grid):
        raise ValueError(f"'Phi_array_BT' must have dimensions ({T_stop}, {p}, {len(rho_grid)}).")
    
    # Initialize R_array
    R_array = np.zeros((T_stop, len(V), len(rho_grid)))
    
    # Compute R_array: number of selected variables for each T_stop, voting threshold, and rho value
    for t in range(T_stop):
        for v_idx, v in enumerate(V):
            for rho_idx in range(len(rho_grid)):
                R_array[t, v_idx, rho_idx] = np.sum(Phi_array_BT[t, :, rho_idx] >= v)
    
    # Find all (v_idx, rho_idx) pairs where FDP_hat <= tFDR at the last T_stop
    valid_pairs = np.argwhere(FDP_hat_array_BT[T_stop-1, :, :] <= tFDR)
    
    if len(valid_pairs) == 0:
        # If no valid threshold is found, choose the highest voting threshold and lowest rho
        v_idx = len(V) - 1
        rho_idx = 0
    else:
        # Find the pair with the maximum R_array value
        max_R_idx = np.argmax([R_array[T_stop-1, pair[0], pair[1]] for pair in valid_pairs])
        v_idx, rho_idx = valid_pairs[max_R_idx]
    
    # Selected thresholds
    v_thresh = V[v_idx]
    rho_thresh = rho_grid[rho_idx]
    
    # Selected variables
    selected_var = np.where(Phi_array_BT[T_stop-1, :, rho_idx] >= v_thresh)[0]
    
    return {
        "selected_var": selected_var,
        "v_thresh": v_thresh,
        "rho_thresh": rho_thresh,
        "R_array": R_array
    } 