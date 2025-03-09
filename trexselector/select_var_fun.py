"""
Function for selecting variables based on estimated FDP and voting thresholds
"""

import numpy as np

def select_var_fun(p, tFDR, T_stop, FDP_hat_mat, Phi_mat, V):
    """
    Select variables based on estimated FDP and voting thresholds.
    
    Parameters
    ----------
    p : int
        Number of original variables.
    tFDR : float
        Target FDR level (between 0 and 1).
    T_stop : int
        Number of included dummies before stopping.
    FDP_hat_mat : ndarray, shape (T_stop, len(V))
        Matrix of estimated FDP values for each T_stop and voting threshold.
    Phi_mat : ndarray, shape (T_stop, p)
        Matrix of relative occurrences for each T_stop.
    V : ndarray
        Vector of voting thresholds.
    
    Returns
    -------
    dict
        A dictionary containing:
        - selected_var: Indices of selected variables
        - v_thresh: Selected voting threshold
        - R_mat: Number of selected variables for each T_stop and voting threshold
    """
    # Error checks
    if FDP_hat_mat.shape[0] != T_stop or FDP_hat_mat.shape[1] != len(V):
        raise ValueError(f"'FDP_hat_mat' must have dimensions ({T_stop}, {len(V)}).")
    
    if Phi_mat.shape[0] != T_stop or Phi_mat.shape[1] != p:
        raise ValueError(f"'Phi_mat' must have dimensions ({T_stop}, {p}).")
    
    # Initialize R_mat
    R_mat = np.zeros((T_stop, len(V)))
    
    # Compute R_mat: number of selected variables for each T_stop and voting threshold
    for t in range(T_stop):
        for v_idx, v in enumerate(V):
            R_mat[t, v_idx] = np.sum(Phi_mat[t] >= v)
    
    # Find maximum voting threshold with FDP <= tFDR
    valid_thresholds = np.where(FDP_hat_mat[T_stop-1] <= tFDR)[0]
    
    if len(valid_thresholds) == 0:
        # If no valid threshold is found, choose the highest voting threshold
        v_idx = len(V) - 1
    else:
        # Choose the minimum valid threshold (largest number of selected variables)
        v_idx = valid_thresholds[0]
    
    # Selected voting threshold
    v_thresh = V[v_idx]
    
    # Selected variables
    selected_var = np.where(Phi_mat[T_stop-1] >= v_thresh)[0]
    
    return {
        "selected_var": selected_var,
        "v_thresh": v_thresh,
        "R_mat": R_mat
    } 