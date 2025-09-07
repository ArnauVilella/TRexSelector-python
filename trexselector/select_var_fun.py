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

    # The selection process should use the last "good" T_stop.
    # The R code explicitly drops the results from the final T_stop value.
    if T_stop > 1:
        T_select = T_stop - 1
        FDP_hat_select = FDP_hat_mat[:T_select, :]
        Phi_mat_select = Phi_mat[:T_select, :]
    else:
        T_select = 1
        FDP_hat_select = FDP_hat_mat
        Phi_mat_select = Phi_mat

    # Generate R_mat for the valid T_stop values, using > to match R
    R_mat = np.zeros_like(FDP_hat_select)
    for t in range(T_select):
        for v_idx, v in enumerate(V):
            R_mat[t, v_idx] = np.sum(Phi_mat_select[t] > v)

    # Mask R_mat where FDP > tFDR, making invalid entries negative
    R_mat_masked = np.where(FDP_hat_select <= tFDR, R_mat, -1)

    if np.all(R_mat_masked == -1):
        # No combination satisfies the FDR. Default to most conservative threshold and select nothing.
        v_thresh = V[-1]
        selected_var = np.array([], dtype=int)
    else:
        # Find the maximum number of selected variables in valid regions
        max_R = np.max(R_mat_masked)
        
        # Find all indices (t, v) where R_mat is max
        valid_indices = np.argwhere(R_mat_masked == max_R)
        
        # Tie-breaking from R: select the index corresponding to the highest V, then highest T.
        # np.argwhere sorts by t (axis 0), then v (axis 1).
        # We need to sort by v, then t, to mimic R's column-major `which`.
        sorted_indices = valid_indices[np.lexsort((valid_indices[:, 0], valid_indices[:, 1]))]
        t_idx, v_idx = sorted_indices[-1]
        
        v_thresh = V[v_idx]
        selected_var = np.where(Phi_mat_select[t_idx] > v_thresh)[0]

    # For consistency, compute the full R_mat over the original T_stop range
    full_R_mat = np.zeros((T_stop, len(V)))
    for t in range(T_stop):
        for v_idx, v in enumerate(V):
            full_R_mat[t, v_idx] = np.sum(Phi_mat[t] > v)
            
    return {
        "selected_var": selected_var,
        "v_thresh": v_thresh,
        "R_mat": full_R_mat
    } 