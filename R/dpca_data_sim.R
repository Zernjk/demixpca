#' Generate the simulation data
#' 
#' @param N number of neuron samples. Default to be 50.
#' @param n_samples number of trail samples. Default to be 10.
#' @param noise noise in the trail experiments. Default to be 0.2.
#' @param pars_lst A list of number containing the number of each task parameter features
#' @param save A bool parameter, whether to save the simulated data as `.rds` file or not.
#' @param file_name The name of file to be save as. Default to be 'simR.rds'
#' 
#' @return A simulated neural population trail data 
#' 

dpca_data_sim <- function(N=50, n_samples=10, noise=0.2, pars_lst, save=TRUE, file_name='simR.rds'){

  # number of parameters to be generated
  num_pars = length(pars_lst)
  
  # create an array from standard norm
  r = rnorm(prod(pars_lst)*N*n_samples, mean = 0, sd = 1)
  # combine parameter dimension and neurons and number of samples
  total_dim = c(pars_lst, N, n_samples)
  A = array(r, dim = total_dim)
  trailR = noise*A
  
  order_org = c(1:(num_pars+2))
  
  for (p in 1:num_pars){
    par_dim = pars_lst[p]
    # create latent variable
    z = seq(0, par_dim-1, 1)/par_dim # length(Z)=par_dim
    
    order_p = c(p, order_org[-p])
    order_dim_p = total_dim[order_p]
    order_back = match(order_org, order_p)
    
    rnp = array(rep(rnorm(N), each=prod(pars_lst)), dim=order_dim_p)
    rp = array(rep(z),dim=order_dim_p)
    rrp = aperm(rp * rnp, order_back)
    
    trailR = trailR + rrp
    
  }
  
  
  # trial-average data
  rr = apply(trailR, c(1:(num_pars+1)), mean)
  rmf = rowMeans(dpca_flat2d(rr))
  
  R = rr - array(rep(rmf, 1, each = prod(pars_lst)), dim=c(pars_lst, N))
  print("Data Dimension: ")
  print(dim(R))
  if (save) saveRDS(R, 'simR.rds')
  
  return(R)
  
}
