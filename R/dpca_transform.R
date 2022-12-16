#' A function to transform the high dimensional data (neural population data) via Demixed PCA.
#'
#' @param X An array, dim=c(n_features_1, n_features_2, ..., n_neurons), where n_features_j is the number of the j-features (where the axis correspond to different parameters).
#' @param marginalizations String or None. Marginalization subspace upon which to project, if None return dict with projections on all marginalizations.
#' @param P A list mapping strings to matrix. Element dim = c(num_neuron, n_components). Holds encoding matrices for each term in variance decompositions.
#' @param F A list mapping strings to matrix. Element dim = c(num_neuron, n_components). Holds decoding matrices for each term in variance decompositions.
#'
#' @return A list mapping strings to matrix. Each element is a demixed pca transformed (decoding) 2D matrix.

dpca_transform <- function(X, marginalizations, P, F){
  # X shape: ...xDxTxSxN


  X = dpca_zero_mean(X)

  X_transformed = list()

  # marginalizations = all parameter combinations or selected parameter
  for (name in marginalizations){
    F_value = F[name][[1]]
    # projecting array
    arr = t(F_value) %*% dpca_flat2d(X)
    X_transformed[name] = list(arr)
  }

  return (X_transformed)
}
