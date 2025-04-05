import numpy as np

def Phi_prime_fun(p, T_stop, num_dummies, phi_T_mat, Phi, eps=np.finfo(float).eps):
    """
    Computes the Deflated Relative Occurrences (Python version of R function).

    Parameters
    ----------
    p : int
        Number of candidate variables.
    T_stop : int
        Number of included dummies when stopping (>= 1).
    num_dummies : int
        Total number of dummy variables used.
    phi_T_mat : numpy.ndarray, shape (p, T_stop)
        Matrix of relative occurrences.
    Phi : numpy.ndarray, shape (p,)
        Vector of relative occurrences at T = T_stop.
    eps : float, optional
        Numerical zero tolerance.

    Returns
    -------
    numpy.ndarray, shape (p,)
        Vector of deflated relative occurrences at T = T_stop.
    """
    if not isinstance(phi_T_mat, np.ndarray) or phi_T_mat.ndim != 2 or phi_T_mat.shape != (p, T_stop):
        raise ValueError(f"'phi_T_mat' must have shape ({p}, {T_stop})")
    if not isinstance(Phi, np.ndarray) or Phi.ndim != 1 or Phi.shape != (p,):
        raise ValueError(f"'Phi' must have shape ({p},)")
    if not isinstance(T_stop, int) or T_stop < 1:
        raise ValueError("T_stop must be an integer >= 1")
    if not isinstance(p, int) or p < 1:
         raise ValueError("p must be an integer >= 1")
    if not isinstance(num_dummies, int) or num_dummies < 0:
         raise ValueError("num_dummies must be a non-negative integer")

    av_num_var_sel = np.sum(phi_T_mat, axis=0)

    rows_gt_half = Phi > 0.5
    if np.any(rows_gt_half):
        delta_av_num_var_sel = np.sum(phi_T_mat[rows_gt_half, :], axis=0)
    else:
        delta_av_num_var_sel = np.zeros(T_stop)

    delta_av_num_var_sel_mod = delta_av_num_var_sel.copy()
    phi_T_mat_mod = phi_T_mat.copy()

    if T_stop > 1:
        delta_av_num_var_sel_mod[1:] = delta_av_num_var_sel[1:] - delta_av_num_var_sel[:-1]
        phi_T_mat_mod[:, 1:] = phi_T_mat[:, 1:] - phi_T_mat[:, :-1]

    phi_scale = np.zeros(T_stop)
    t_py = np.arange(T_stop)
    denominator_scaling = num_dummies - t_py

    valid_mask = (delta_av_num_var_sel_mod > eps) & (denominator_scaling > 0)

    if np.any(valid_mask):
        numerator_term = p - av_num_var_sel[valid_mask]
        divisor_term = delta_av_num_var_sel_mod[valid_mask]

        phi_scale[valid_mask] = 1 - (numerator_term / denominator_scaling[valid_mask]) / divisor_term

    Phi_prime = phi_T_mat_mod @ phi_scale

    return Phi_prime.flatten()