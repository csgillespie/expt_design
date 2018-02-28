library(parallel)
compiler::enableJIT(3)
source("R/death_1d/death_utility.R")
simulate = function(theta=1, J=1, ds) rbinom(J, 50, exp(-theta*ds))
collapse_d = function(m) {
  m_means = tapply(m[,3], m[,2], mean)
  m_collapse = matrix(-1, ncol=3, nrow=length(m_means))
  m_collapse[,3] = as.vector(m_means)
  m_collapse[,2] = as.numeric(names(m_means))
  m_collapse[,1] = as.vector(tapply(m[,1], m[,2], sum))
  colnames(m_collapse) = c("n", "d", "u")
  
  ## Remove designs from outside design space
  m_collapse = m_collapse[m_collapse[,3] > 0,]
  return(m_collapse)
}

pre_screen = function(N=10, ds, p) {
  
  ud_mat = matrix(1, ncol=3, nrow=N)
  d_prop = ud_mat[,2] = sample(1:length(ds), N, replace=TRUE)
  theta_prop = rlnorm(N, -0.005, sqrt(0.01))
  
  y_prop = simulate(theta_prop, N, ds[d_prop])
  util_prop = sapply(seq_along(y_prop),function(i) get_utility(y_prop[i],  d_prop[i]))
  ud_mat[,3] = util_prop
  ud_mat = collapse_d(ud_mat)
  ud_mat = ud_mat[order(ud_mat[,2]), ]  # Don't need to order. 
  
  m = matrix(0, ncol=3, nrow(ud_mat)*2)
  colnames(m) = colnames(ud_mat)
  m[1:nrow(ud_mat),] = ud_mat
  attr(m, "values") = nrow(ud_mat) 
  m
}



g = function(N = c(10,10), seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  
  ds = seq(0.01, 10, 0.01)
  l = vector("list", length(N))
  ud_mat = pre_screen(N[1], ds)
  attr(ud_mat, "m") = 0
  l[[1]] = ud_mat[rowSums(ud_mat) > 0, ]
  N = N[-1]; 
  i = 1
  for(i in seq_along(N)) {
    k = 1
    n_row = attr(ud_mat, "values") 
    for(k in 1:N[i]) {
      limits = quantile(ud_mat[1:n_row, 3], 1 - 2^(-i))
      
      prob = ud_mat[1:n_row,3]
      prob[prob < limits] = 0
      
      row = sample(1:n_row, 1, prob = prob)
      d_prop = d = ud_mat[row, 2]
      
      d_prop = d_prop + rpois(1, 2) - rpois(1, 2)
      if(d_prop > length(ds) || d_prop < 1) d_prop = d
      where = which(ud_mat[,2] == d_prop)
      ## Has the point been sampled before
      if(length(where)> 0) {
        row = where
      } else {
        n_row = n_row + 1
        row = n_row
        ud_mat[row,2] = d_prop
      }
      
      n = ud_mat[row, 1]; u_pre = ud_mat[row, 3]
      
      theta_prop = rlnorm(1, -0.005, sqrt(0.01))
      y_prop = simulate(theta_prop, 1, ds[d_prop])
      util_prop = get_utility(y_prop, d_prop)
      ud_mat[row,] = c(n+1, d_prop, (u_pre*n + util_prop)/(n+1))
    }
    attr(ud_mat, "values") = n_row; attr(ud_mat, "m") = i
    l[[i+1]] = ud_mat[rowSums(ud_mat) > 0, ]
    message("m= ", i)
  }
  l
}

## Plot focusing in
m = res = g(c(4800,4800,4800,4800,4800))
saveRDS(m, file="output/death_1d/gil_single.rds")

setnicepar(mfrow=c(2, 3))
plot(m[[1]][,2], m[[1]][,1])
plot(m[[2]][,2], m[[2]][,1])
plot(m[[3]][,2], m[[3]][,1])
plot(m[[4]][,2], m[[4]][,1])
plot(m[[5]][,2], m[[5]][,1])


## Calculcate MSE - run in parallel
get_optimal_g = function(i, N = c(4800,4800,4800,4800,4800)) {
  res = g(N)
  ud_mat = res[[length(res)]]
  x = ud_mat[,2]
  optim1 = x[which.max(ud_mat[,3])]/100   
  sp  = smooth.spline(ud_mat[,3]~ud_mat[,2])
  optim2 = sp$x[which.max(sp$y)]
  c(optim1, optim2/100)
}

res_constant = mclapply(1:500, get_optimal_g, N = c(4800,4800,4800,4800,4800), mc.cores = 8)
res_incr = mclapply(1:500, get_optimal_g, N = rev(c(12000,  6000,  3000,  1500, 750)), mc.cores = 8)
res_de = mclapply(1:500, get_optimal_g, N = c(12000,  6000,  3000,  1500, 750), mc.cores = 8)


saveRDS(res_constant, file="output/death_1d/gil_constant.rds")
saveRDS(res_incr, file="output/death_1d/gil_incr.rds")
saveRDS(res_de, file="output/death_1d/gil_de.rds")


