#' Transform data back to its original space. This function is utilized in the `dpca_reconstruct()` function.
#' 
#' @param X An array, dim=c(n_neurons, n_features_1, n_features_2, ...), where n_samples in the number of neurons and n_features_j is the number of the j-features (where the axis correspond to different parameters).
#' @param transformedX A transformed matrix on certain margin (task parameter) dim = c(n_components, n_features)
#' @param margin_back A string presenting the margin (task parameter) to be inversed transformed on.
#' @param P: A list mapping strings to matrix. Element dim = c(num_neuron, n_components). Holds encoding matrices for each term in variance decompositions.
#' 
#' @return An array with the same shape as the original data.

dpca_inverse_transform <- function(X, transformedX, margin_back, P){
  K = dim(transformedX)[1]  ## number of components
  num_Fetures = dim(transformedX)[2]  ## number of features (num_Fetures = ...*D*T*S)
  
  P_margin = P[margin_back][[1]]  ## NxK dim
  
  X_ = transformedX - rowMeans(transformedX)
  
  X_inverse_transformed = P_margin %*% X_ ## dim = NxF
  
  dim(X_inverse_transformed)  = dim(X)
  
  return(X_inverse_transformed)
}