#' Plot the `k`th components. Utilized in the `dpca_fit_transform()` function. Can handle neural population data with 3 or 4 task parameters.
#' 
#' @param X An array, dim=c(n_neurons, n_features_1, n_features_2, ...), where n_samples in the number of neurons and n_features_j is the number of the j-features (where the axis correspond to different parameters).
#' @param re_transform A list mapping strings to matrix. Each element is a demixed pca transformed (decoding) 2D matrix. 
#' @param k_component The `k`th component to plot. Default to be 1.
#' @param plot_margin A string presenting the margin (task parameter) to plot the components.
#' @param ylim The y axis limitation. Same as `plot()` function in R. Default to be c(-3,3).
#' 
#' @return A plot with the component on margin

dpca_plot_component <- function(X, re_transform, k_component=1, plot_margin, ylim = c(-3,3)){
  # X shape: ...xDxTxSxN
  dim_lst = dim(X)
  dim_len = length(dim_lst)
  
  
  if (dim_len==3){
    # X shape: TxSxN
    T = dim(X)[1]
    S = dim(X)[2]
    N = dim(X)[3]
    
    res = re_transform[plot_margin][[1]][k_component,]
    
    mat = matrix(res, T, S)
    
    time = c(1:T)
    
    plot(time,mat[,1], type='l', col=rainbow(S)[1], ylab='', ylim = ylim,
         main=paste('The First component on', plot_margin))
    for (s in c(2:S)){
      lines(time, mat[,s], col=rainbow(S)[s])
    }
  }
  
  else{
    D = dim(X)[1]
    T = dim(X)[2]
    S = dim(X)[3]
    N = dim(X)[4]
    
    res = re_transform[plot_margin][[1]][k_component,]
    
    mat = array(res, dim=c(D, T, S))
    
    time = c(1:T)
    light = seq(0.2, 1, length.out=D)
    
    plot(time,mat[1,,1], type='l', col=rainbow(S, alpha = light[1])[1], ylab='', ylim = ylim,
         main=paste('The First component on', plot_margin))
    for (di in c(2:D))
      lines(time, mat[di,,1], col=rainbow(S, alpha = light[di])[1])
    for (si in c(2:S)){
      for (di in c(1:D))
        lines(time, mat[di,,si], col=rainbow(S, alpha = light[di])[si])
    }
    
  }
}