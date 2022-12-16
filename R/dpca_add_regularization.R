#' A function to add regularization in the Demixed PCA
#' Utilized in `dpca_fit()` and `dpca_fit_transform()`.
#'
#' @param Y An array, dim=c(n_features_1, n_features_2, ..., n_neurons), where n_features_j is the number of the j-features (where the axis correspond to different parameters).
#' @param mYs List with values in the shape of flattened Y. Marginalized data, should be the result of dpca_marginalize().
#' @param lam The coefficient for the Ridge regularization. A non-negative number. lam equal to the product of regularizer and l2 norm of the data.
#'
#' @return return the regularized flattened matrix, marginalizations, and the inversed matrix.

dpca_add_regularization <- function(Y,mYs,lam){
  N = dim(Y)[length(dim(Y))]
  flatY = dpca_flat2d(Y)
  regY = cbind(flatY, lam*diag(N))

  regmYs = list()
  for (key in names(mYs)){
    regmYs[key] = list(cbind(mYs[key][[1]], matrix(0, N, N)))
  }


  flatregY = regY
  pregY = t(flatregY) %*% solve(flatregY %*% t(flatregY) + lam*2*diag(N))

  return (list(regY=regY, regmYs=regmYs, pregY=pregY))
}
