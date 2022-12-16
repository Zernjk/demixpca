#' A function to fit the high dimensional data (neural population data) via Demixed PCA.
#'
#' @param X An array, dim=c(n_features_1, n_features_2, ..., n_neurons), where n_features_j is the number of the j-features (where the axis correspond to different parameters).
#' @param n_components The number of components required. Defult=5.
#' @param regularizer The coefficient for the Ridge regularization. A non-negative number. Defult to be 0, no regularization.
#' @param method The method chose for decomposition. 'rsvd': Randomized SVD, 'svd': SVD, 'qr': QR decomposition. Defult='rsvd'.
#' @param label_lst A list of strings presenting the task parameters. The order of element in the string should consistent with the dimension of the array data. 'ts': time and stimulus, 'dts': decision, time and stimulus, etc.
#' @return P: A list mapping strings to matrix. Element dim = c(num_neuron, n_components). Holds encoding matrices for each term in variance decompostions ( can be used in inverse_transform to map from low-dimensional representation back to original data space).
#' @return F: A list mapping strings to matrix. Element dim = c(num_neuron, n_components). Holds decoding matrices for each term in variance decompostions (used to transform data to low-dimensional space).

dpca_fit <- function(X, n_components=5, regularizer=0, method='rsvd', label_lst){

  # center data
  X = dpca_zero_mean(X)


  # marginalize data

  mXs = dpca_marginalize(X=X, label_lst=label_lst)


  if (regularizer > 0){
    # add regularization
    res_reg = dpca_add_regularization(X,mXs,regularizer*sum(X**2))
    regX = res_reg$regY; regmXs = res_reg$regmYs; pregX = res_reg$pregY
  }
  else {
    # without regularization
    library(pracma)
    regX=dpca_flat2d(X); regmXs=mXs; pregX=pracma::pinv(dpca_flat2d(X))
  }

  # compute closed-form solution
  res_ = dpca_decomposition(regX,regmXs,pinvX=pregX, n_components=n_components, method=method)

  return (list(P=res_$P, F=res_$F))
}
