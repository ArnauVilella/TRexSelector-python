"""
Functions for adding dummy variables to the predictor matrix
"""

import numpy as np
from scipy.cluster.hierarchy import linkage, fcluster
from scipy.spatial.distance import pdist

def add_dummies(X, num_dummies):
    """
    Add random dummy variables to the predictor matrix.
    
    Parameters
    ----------
    X : ndarray, shape (n, p)
        Predictor matrix.
    num_dummies : int
        Number of dummies to append to the predictor matrix.
    
    Returns
    -------
    ndarray, shape (n, p + num_dummies)
        Predictor matrix with appended dummies.
    """
    # Error control
    if not isinstance(X, np.ndarray) or X.ndim != 2:
        raise ValueError("'X' must be a 2D numpy array.")
    
    if not np.issubdtype(X.dtype, np.number):
        raise ValueError("'X' only allows numerical values.")
    
    if np.any(np.isnan(X)):
        raise ValueError("'X' contains NaNs. Please remove or impute them before proceeding.")
    
    if not isinstance(num_dummies, int) or num_dummies < 1:
        raise ValueError("'num_dummies' must be an integer larger or equal to 1.")
    
    # Number of rows and columns in X
    n, p = X.shape
    
    # Generate dummy variables
    np.random.seed(0)  # For reproducibility
    dummies = np.random.normal(0, 1, size=(n, num_dummies))
    
    # Scale dummies more carefully to ensure exact standardization
    dummies = dummies - dummies.mean(axis=0)
    stds = dummies.std(axis=0, ddof=1)
    dummies = dummies / stds
    
    # Verify standardization
    assert np.allclose(dummies.mean(axis=0), 0, atol=1e-10)
    assert np.allclose(dummies.std(axis=0, ddof=1), 1, atol=1e-10)
    
    # Append dummies to X
    X_Dummy = np.hstack((X, dummies))
    
    return X_Dummy

def add_dummies_GVS(X, num_dummies, corr_max=0.5):
    """
    Add dummy variables with correlation constraints for group variable selection.
    
    Parameters
    ----------
    X : ndarray, shape (n, p)
        Predictor matrix.
    num_dummies : int
        Number of dummies to append to the predictor matrix.
    corr_max : float, default=0.5
        Maximum allowed correlation between any two predictors from different clusters.
    
    Returns
    -------
    dict
        A dictionary containing:
        - X_Dummy: Predictor matrix with appended dummies
        - max_clusters: Maximum number of clusters
        - cluster_sizes: Size of each cluster
        - IEN_cl_id_vectors: Cluster identity vectors for Informed Elastic Net (IEN)
    """
    # Error control
    if not isinstance(X, np.ndarray) or X.ndim != 2:
        raise ValueError("'X' must be a 2D numpy array.")
    
    if not isinstance(num_dummies, int) or num_dummies < 1:
        raise ValueError("'num_dummies' must be an integer larger or equal to 1.")
    
    if not isinstance(corr_max, float) or corr_max < 0 or corr_max > 1:
        raise ValueError("'corr_max' must be a float between 0 and 1.")
    
    # Number of rows and columns in X
    n, p = X.shape
    
    if num_dummies % p != 0:
        raise ValueError("'num_dummies' must be a positive integer multiple of the number of original predictors in X.")
    
    # Number of dummies per predictor
    num_dummies_per_pred = num_dummies // p
    
    # Compute correlation matrix
    corr_mat = np.corrcoef(X, rowvar=False)
    
    # Compute distance matrix: 1 - |corr_mat|
    dist_mat = 1 - np.abs(corr_mat)
    
    # Hierarchical clustering
    Z = linkage(pdist(X.T), method='single')
    
    # Determine clusters: cut tree at height 1 - corr_max
    clusters = fcluster(Z, 1 - corr_max, criterion='distance') - 1  # 0-based indexing
    
    # Number of clusters
    max_clusters = len(np.unique(clusters))
    
    # Cluster sizes
    unique_clusters, cluster_sizes = np.unique(clusters, return_counts=True)
    
    # Create IEN cluster identity vectors
    IEN_cl_id_vectors = np.zeros((p, max_clusters))
    for j in range(p):
        IEN_cl_id_vectors[j, clusters[j]] = 1
    
    # Initialize X_Dummy
    X_Dummy = np.copy(X)
    
    # Add dummies for each original predictor
    for j in range(p):
        # Generate dummies for this predictor
        x_dummies = np.random.normal(0, 1, size=(n, num_dummies_per_pred))
        
        # Scale dummies
        x_dummies = (x_dummies - x_dummies.mean(axis=0)) / x_dummies.std(axis=0, ddof=1)
        
        # Append to X_Dummy
        X_Dummy = np.hstack((X_Dummy, x_dummies))
    
    return {
        "X_Dummy": X_Dummy,
        "max_clusters": max_clusters,
        "cluster_sizes": cluster_sizes,
        "IEN_cl_id_vectors": IEN_cl_id_vectors
    } 