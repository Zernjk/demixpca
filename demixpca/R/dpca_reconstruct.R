#' Reconstruct the data: Transform data first into reduced space before projecting it back into data space.
#' 
#' @param X An array, dim=c(n_neurons, n_features_1, n_features_2, ...), where n_samples in the number of neurons and n_features_j is the number of the j-features (where the axis correspond to different parameters).
#' @param margin_on A string presenting the margin (task parameter) to be inversed transformed on.
#' @param label A string presenting the task parameters. The order of element in the string should consistent with the dimension of the array data. 'ts': time and stimulus, 'dts': decision, time and stimulus, etc.
#' @param regularizer The coefficient for the Ridge regularization. A non-negative number. Deault to be 0, no regularization.
#' @param method The method chose for decomposition. 'rsvd': Randomized SVD, 'svd': SVD, 'qr': QR decomposition. Default='rsvd'.
#' 
#' @return An array with the same shape as the original data.

dpca_reconstruct <- function(X, margin_on, label, regularizer = 0, method='rsvd'){
  # X: origin data
  # margin_on: str, the parameter reconstructed on
  label_lst = as.list(strsplit(label, "")[[1]])
  
  re_fit = dpca_fit(X = X, regularizer = regularizer, method = method, label=label_lst)
  marginalizations = names(re_fit$P)
  
  re_transform = dpca_transform(X = X, marginalizations = marginalizations, P=re_fit$P, F=re_fit$F)
  
  transformedX = re_transform[margin_on][[1]]
  
  re_inverse_transformed = dpca_inverse_transform(X = X, transformedX = transformedX, margin_back = margin_on, P=re_fit$P)
  
  return (re_inverse_transformed)
}
