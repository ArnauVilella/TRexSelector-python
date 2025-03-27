"""
Function for adding dummy variables to the predictor matrix for the T-Rex+GVS selector
"""

import numpy as np
from scipy.cluster.hierarchy import linkage, fcluster
from scipy.spatial.distance import squareform
from scipy.stats import multivariate_normal

def add_dummies_GVS(X, num_dummies, corr_max=0.5, seed=None):
    """
    Add dummy predictors to the original predictor matrix for the T-Rex+GVS selector.

    Parameters
    ----------
    X : ndarray, shape (n, p)
        Real valued predictor matrix.
    num_dummies : int
        Number of dummies to append to the predictor matrix. 
        Must be a multiple of the number of original variables.
    corr_max : float, default=0.5
        Maximum allowed correlation between any two predictors from different clusters.
    seed : int, optional
        Random seed for reproducibility. If None, no seed is set.

    Returns
    -------
    dict
        A dictionary containing:
        - X_Dummy: ndarray, shape (n, p + num_dummies)
          Predictor matrix with appended dummies.
        - max_clusters: int
          Number of clusters found.
        - cluster_sizes: list
          Size of each cluster.
        - IEN_cl_id_vectors: ndarray
          Binary cluster identity vectors.
    """
    # Error control
    if not isinstance(X, np.ndarray) or X.ndim != 2:
        raise ValueError("'X' must be a 2D numpy array.")
    
    if not np.issubdtype(X.dtype, np.number):
        raise ValueError("'X' only allows numerical values.")
    
    if np.any(np.isnan(X)):
        raise ValueError("'X' contains NaNs. Please remove or impute them before proceeding.")
    
    # Dimensions of the data
    n, p = X.shape
    
    # Continue error control
    if not isinstance(num_dummies, int) or num_dummies % p != 0 or num_dummies < 1:
        raise ValueError("'num_dummies' must be a positive integer multiple of the total number of original predictors in X.")
    
    if not isinstance(corr_max, (int, float)) or corr_max < 0 or corr_max > 1:
        raise ValueError("'corr_max' must have a value between zero and one.")
    
    # Set seed if provided
    if seed is not None:
        np.random.seed(seed)

    # Single linkage hierarchical clustering using the sample correlation as the similarity measure
    sigma_X_dist = 1 - np.abs(np.corrcoef(X, rowvar=False))
    condensed_dist = squareform(sigma_X_dist, checks=False)
    fit = linkage(condensed_dist, method='single')
    clusters = fcluster(fit, t=1 - corr_max, criterion='distance')
    max_clusters = np.max(clusters)
    cluster_to_vars = {}
    for i, cluster_num in enumerate(clusters):
        if cluster_num not in cluster_to_vars:
            cluster_to_vars[cluster_num] = []
        cluster_to_vars[cluster_num].append(i)
    cluster_to_vars = dict(sorted(cluster_to_vars.items()))
    cluster_sizes = np.zeros(max_clusters, dtype=int)
    for j in range(max_clusters):
        cluster_sizes[j] = len(cluster_to_vars[j+1])
    
    # Binary cluster identity vectors for T-Rex+GVS+IEN
    IEN_cl_id_vectors = np.zeros((max_clusters, p), dtype=bool)
    for i, cluster_num in enumerate(range(1, max_clusters+1)):
        if cluster_num in cluster_to_vars:
            IEN_cl_id_vectors[i, cluster_to_vars[cluster_num]] = True
    
    # Generate dummy predictors and append them to the original predictor matrix X
    w_max = num_dummies // p
    X_p_sub_dummy = np.empty((n, p))
    X_Dummy = np.empty((n, p + num_dummies))
    X_Dummy[:, :p] = X

    # Generate dummy predictors for each set and cluster
    for w in range(w_max):
        idx = np.cumsum(cluster_sizes)
        for z in range(max_clusters):
            cluster_vars = cluster_to_vars[z+1]
            sub_X = X[:, cluster_vars]
            sigma_sub_X = np.cov(sub_X, rowvar=False)
            mu = np.zeros(cluster_sizes[z])
            if z == 0:
                X_p_sub_dummy[:, :idx[z]] = multivariate_normal.rvs(
                    mean=mu, 
                    cov=sigma_sub_X, 
                    size=n
                ).reshape(n, -1)
            else:
                X_p_sub_dummy[:, idx[z-1]:idx[z]] = multivariate_normal.rvs(
                    mean=mu, 
                    cov=sigma_sub_X, 
                    size=n
                ).reshape(n, -1)
        X_Dummy[:, (w+1)*p:(w+2)*p] = X_p_sub_dummy
    
    add_dummies_res = {
        'X_Dummy': X_Dummy,
        'max_clusters': max_clusters,
        'cluster_sizes': cluster_sizes,
        'IEN_cl_id_vectors': IEN_cl_id_vectors
    }

    return add_dummies_res 