#' Flatten array function
#' 
#' This function allows you to flatten a high-dimension array into a 2-dimension matrix
#' @param A The high-dimension array.
#' @return A matrix flatten from the array
#' @examples flat2d(array(1, dim=c(3, 3, 3)))

dpca_flat2d <- function(A){
  d_lst = dim(A) 
  if (length(d_lst)>2){
    At = matrix(unlist(as.list(A)), d_lst[length(d_lst)], prod(d_lst[-length(d_lst)]), byrow = TRUE)
  }
  else{At = A}
  return (At)
}
