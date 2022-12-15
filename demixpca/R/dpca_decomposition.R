#' Doing the decomposition of matrix with different methods
#' 
#' @param flatX A 2D matrix flattened from a high dimensional array. Dim = c(num_neuron, num_features).
#' @param mXs List with values in the shape of flatX. Marginalized data, should be the result of dpca_marginalize()
#' @param pinvX Inverse of the flatX matrix
#' @param n_components The number of components required. Default=10.
#' @param method The method chose for decomposition. 'rsvd': Randomized SVD, 'svd': SVD, 'qr': QR decomposition. Default='rsvd'.
#' @return P: A list mapping strings to matrix. Element dim = c(num_neuron, n_components). Holds encoding matrices for each term in variance decompostions ( can be used in inverse_transform to map from low-dimensional representation back to original data space).
#' @return F: A list mapping strings to matrix. Element dim = c(num_neuron, n_components). Holds decoding matrices for each term in variance decompostions (used to transform data to low-dimensional space).

dpca_decomposition <- function(flatX, mXs, pinvX, n_components=10, method='rsvd'){
  
  rX = flatX
  P = list()
  F = list()
  for (key in names(mXs)){
    mX = mXs[key][[1]]
    
    flatmX = mX
    C = flatmX %*% pinvX
    
    if (method=='rsvd'){
      library(rsvd)
      # randomized svd
      print('Using Randomized SVD Method')
      U = rsvd(C %*% rX, k=n_components)$u
    }
    
    else if (method=='svd') {
      # svd
      print('Using SVD Method')
      U = svd(C %*% rX)$u[, 1:n_components]
    }
    
    else{
      # qr
      print('Using QR decomposition Method')
      U = qr.Q(qr(C %*% rX))[, 1:n_components]
    }
    
    # U shape: Nxn_components
    
    P[key] = list(U)
    F[key] = list(t(t(U) %*% C))
    
  }
  
  return (list(P=P, F=F))
  
}