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

    # The selection process should use the last "good" T_stop.
    # The R code explicitly drops the results from the final T_stop value.
    if T_stop > 1:
        T_select = T_stop - 1
        FDP_hat_select = FDP_hat_array_BT[:T_select, :, :]
        Phi_array_select = Phi_array_BT[:T_select, :, :]
    else:
        T_select = 1
        FDP_hat_select = FDP_hat_array_BT
        Phi_array_select = Phi_array_BT

    # Generate R_array for the valid T_stop values, using > to match R
    R_array = np.zeros_like(FDP_hat_select)
    for t in range(T_select):
        for v_idx, v in enumerate(V):
            for rho_idx in range(len(rho_grid)):
                R_array[t, v_idx, rho_idx] = np.sum(Phi_array_select[t, :, rho_idx] > v)

    # Mask R_array where FDP > tFDR, making invalid entries negative
    R_array_masked = np.where(FDP_hat_select <= tFDR, R_array, -1)

    if np.all(R_array_masked == -1):
        # No combination satisfies the FDR. Default to most conservative threshold and select nothing.
        v_thresh = V[-1]
        rho_thresh = rho_grid[0]
        selected_var = np.array([], dtype=int)
    else:
        # Find the maximum number of selected variables in valid regions
        max_R = np.max(R_array_masked)
        
        # Find all indices (t, v, rho) where R_array is max
        max_R_indices = np.argwhere(R_array_masked == max_R)
        
        # Tie-breaking logic from R:
        # 1. Find max v_idx among the candidates
        max_v_idx = np.max(max_R_indices[:, 1])
        # 2. Filter for candidates with that max_v_idx
        v_filtered_indices = max_R_indices[max_R_indices[:, 1] == max_v_idx]
        
        # 3. In R, the tie-break is on rho, then t.
        # np.argwhere sorts by t, then v, then rho. To mimic R, we re-sort.
        # Sort by rho (dim 2), then t (dim 0)
        sorted_indices = v_filtered_indices[np.lexsort((v_filtered_indices[:, 0], v_filtered_indices[:, 2]))]
        t_idx, v_idx, rho_idx = sorted_indices[-1]
        
        v_thresh = V[v_idx]
        rho_thresh = rho_grid[rho_idx]
        selected_var = np.where(Phi_array_select[t_idx, :, rho_idx] > v_thresh)[0]

    # For consistency, compute the full R_array over the original T_stop range
    full_R_array = np.zeros((T_stop, len(V), len(rho_grid)))
    for t in range(T_stop):
        for v_idx, v in enumerate(V):
            for rho_idx in range(len(rho_grid)):
                full_R_array[t, v_idx, rho_idx] = np.sum(Phi_array_BT[t, :, rho_idx] > v)

    return {
        "selected_var": selected_var,
        "v_thresh": v_thresh,
        "rho_thresh": rho_thresh,
        "R_array": full_R_array
    } 