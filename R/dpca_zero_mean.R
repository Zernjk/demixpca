#' Subtracts the mean from each observable
#' 
#' @param A a High dimensional array
#' @return a same dimension array that already subtracted the mean

dpca_zero_mean <- function(A){
  flatA = dpca_flat2d(A)
  rm = rowMeans(flatA)
  res = t(flatA-rm)
  dim(res) = dim(A)
  return (res)  
}