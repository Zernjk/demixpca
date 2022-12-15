#' Fit the model with the neural population data (a high dimensional array X) and apply the dimensionality reduction on X.
#'
#' @param X An array, dim=c(n_neurons, n_features_1, n_features_2, ...), where n_samples in the number of neurons and n_features_j is the number of the j-features (where the axis correspond to different parameters).
#' @param regularizer The coefficient for the Ridge regularization. A non-negative number. Default to be 0, no regularization.
#' @param method The method chose for decomposition. 'rsvd': Randomized SVD, 'svd': SVD, 'qr': QR decomposition. Default='rsvd'.
#' @param label A string presenting the task parameters. The order of element in the string should consistent with the dimension of the array data. 'ts': time and stimulus, 'dts': decision, time and stimulus, etc.
#' @param plot A bool parameter, whether to plot the components on margins. Default to be FALSE. Can handle neural population data with 3 or 4 task parameters.
#' @param plot_margin A string presenting the margin (task parameter) to plot the components. Default to be 's', stimulus.
#' @param k_component The `k`th component to plot. Default to be 1.
#' @param ylim The y axis limitation. Same as `plot()` function in R. Default to be c(-3,3).
#'
#' @return A list mapping strings to matrix. Each element is a demixed pca transformed (decoding) 2D matrix. Can be reshaped to the origin dimension by using `dim()`function in R.

dpca_fit_transform <- function(X, regularizer = 0, label, method='rsvd',
                          plot=FALSE, plot_margin='s', k_component=1, ylim = c(-3,3)){
  # X shape: ...xDxTxSxN
  label_lst = as.list(strsplit(label, "")[[1]])
  dim_len = length(dim(X))

  re_fit = fit(X = X, regularizer = regularizer, method = method, label_lst=label_lst)
  marginalizations = names(re_fit$P)
  re_transform = dpca_transform(X = X, marginalizations = marginalizations, P=re_fit$P, F=re_fit$F)

  if(plot & dim_len %in% c(3,4)){
    plot_component(X = X, re_transform = re_transform, k_component = 1, plot_margin = plot_margin,
                   ylim = ylim)
  }

  return (re_transform)
}
