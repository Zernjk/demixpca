#' Fit the model with the neural population data (a high dimensional array X) and apply the dimensionality reduction on X.
#'
#' @param X An array, dim=c(n_features_1, n_features_2, ..., n_neurons), where n_features_j is the number of the j-features (where the axis correspond to different parameters).
#' @param regularizer The coefficient for the Ridge regularization. A non-negative number. Default to be 0, no regularization.
#' @param method The method chose for decomposition. 'rsvd': Randomized SVD, 'svd': SVD, 'qr': QR decomposition. Default='rsvd'.
#' @param n_components The number of components required. Default=5.
#' @param label A string presenting the task parameters. The order of element in the string should consistent with the dimension of the array data. 'ts': time and stimulus, 'dts': decision, time and stimulus, etc.
#' @param plot A bool parameter, whether to plot the First component on margins. Default to be FALSE. Can handle neural population data with 3 or 4 task parameters.
#' @param plot_margin A string presenting the margin (task parameter) to plot the components. Default to be 's', stimulus.
#' @param ylim The y axis limitation. Same as `plot()` function in R. Default to be c(-3,3).
#'
#' @return A list mapping strings to matrix. Each element is a demixed pca transformed 2D matrix. Can be reshaped to the origin dimension by using `dim()`function in R.

dpca_fit_transform <- function(X, regularizer = 0, label, method='rsvd', n_components = 5,
                          plot=FALSE, plot_margin='s',ylim = c(-3,3)){
  # X shape: ...xDxTxSxN
  label_lst = as.list(strsplit(label, "")[[1]])
  dim_len = length(dim(X))

  re_fit = dpca_fit(X = X, n_components = n_components, regularizer = regularizer, method = method, label_lst=label_lst)
  marginalizations = names(re_fit$P)
  re_transform = dpca_transform(X = X, marginalizations = marginalizations, P=re_fit$P, F=re_fit$F)

  if(plot & dim_len %in% c(3,4)){
    dpca_plot_component(X = X, re_transform = re_transform, k_component = 1, plot_margin = plot_margin,
                   ylim = ylim)
  }

  return (re_transform)
}
