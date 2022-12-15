#' Marginalize the data matrix
#' 
#' @param X An array, dim=c(n_neurons, n_features_1, n_features_2, ...), where n_samples in the number of neurons and n_features_j is the number of the j-features (where the axis correspond to different parameters).
#' @param label_lst A string presenting the task parameters. The order of element in the string should consistent with the dimension of the array data. 'ts': time and stimulus, 'dts': decision, time and stimulus, etc.
#' 
#' @return A list mapping strings to matrices, with values corresponding to the marginalized data and mixed residual data (and the key refers to the marginalization label, mixed residual data is called by 'mix')

dpca_marginalize <- function(X, label_lst){
  # X shape: ...xDxTxSxN
  
  dim_lst = dim(X)
  dim_len = length(dim_lst)
  
  if (length(label_lst) >1 & length(label_lst) != dim_len-1) print('Label Length Error!')
  
  x = X
  fx = dpca_flat2d(x)
  xmix_bar = fx
  Xmargs = list()
  
  i=1
  
  for (label in label_lst){
    if (i<length(label_lst)){
      x_i = apply(x, c(i, dim_len), mean)
      tmp = rep(t(x_i), prod(dim_lst[-c(i, dim_len)]))
      xi_bar = matrix(tmp, nr=dim_lst[dim_len], nc=prod(dim_lst[-dim_len]))
      xmix_bar = xmix_bar - xi_bar
      Xmargs[label] = list(xi_bar)
    }
    else{
      x_i = apply(x, c(i, dim_len), mean)
      tmp = rep( x_i, 1, each=prod(dim_lst[-c(i, dim_len)]) )
      xi_bar = matrix(tmp, nr=dim_lst[dim_len], nc=prod(dim_lst[-dim_len]), byrow = TRUE)
      xmix_bar = xmix_bar - xi_bar
      Xmargs[label] = list(xi_bar)
    }
    
    i = i + 1
    
  }
  
  Xmargs['mix'] = list(xmix_bar)
  
  return(Xmargs)
}