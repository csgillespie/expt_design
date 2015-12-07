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




J = 1:2
N = c(10, 10)

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

next_J = function(ud_mat, cur_J) { 
  
  d =  ud_mat[,2]/100
  u =  ud_mat[,3]
  
  vs = sapply(cur_J:(cur_J+30), function(i) sum(d^2*u^i/sum(u^i)) - sum(d*u^i/sum(u^i))^2)
  next_J = which(vs/vs[1] < 0.5)[1] + (cur_J-1)
  if(is.na(next_J)) 
    J = cur_J + 30
  else
    J = next_J
  J
}

g = function(N = c(10,10), p=0.2, seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  J = 1
  ds = seq(0.01, 10, 0.01)
  l = vector("list", length(N))
  ud_mat = pre_screen(N[1], ds)
  attr(ud_mat, "J") = J
  l[[1]] = ud_mat[rowSums(ud_mat) > 0, ]
  N = N[-1]; 
  (J = next_J(l[[1]], cur_J=1))
  message("J= ", J)
  i = 1
  for(i in seq_along(N)) {
    k = 1
    n_row = attr(ud_mat, "values") 
    for(k in 1:N[i]) {
      
      row = sample(1:n_row, 1, prob = ud_mat[1:n_row,3]^J)
      d_prop = d = ud_mat[row, 2]
      
      if(runif(1) < p) {
        d_prop = d_prop + sample(-10:10, 1)
        if(d_prop > length(ds) || d_prop < 1) d_prop = d
        where = which(ud_mat[,2] == d_prop)
        if(length(where)> 0) {
          row = where
        } else {
          n_row = n_row + 1
          row = n_row
          ud_mat[row,2] = d_prop
        }
      }
      n = ud_mat[row, 1]; u_pre = ud_mat[row, 3]
      
      theta_prop = rlnorm(1, -0.005, sqrt(0.01))
      y_prop = simulate(theta_prop, 1, ds[d_prop])
      util_prop = get_utility(y_prop, d_prop)
      ud_mat[row,] = c(n+1, d_prop, (u_pre*n + util_prop)/(n+1))
    }
    attr(ud_mat, "values") = n_row; attr(ud_mat, "J") = J
    l[[i+1]] = ud_mat[rowSums(ud_mat) > 0, ]
    (J = next_J(l[[i+1]], cur_J=J))
    message("J= ", J)
  }
  l
}

get_optimal_g = function(i, N = c(4800,4800,4800,4800,4800)) {
  res = g(N)
  ud_mat = res[[length(res)]]
  x = ud_mat[,2]
  optim1 = x[which.max(ud_mat[,3])]/100   
  sp  = smooth.spline(ud_mat[,3]~ud_mat[,2])
  optim2 = sp$x[which.max(sp$y)]
  c(optim1, optim2/100)
}

optim = unlist(res)
5*4800
dd = readRDS("output/death_1d/muller_300.RData")

setnicepar(mfrow=c(2, 2))
boxplot(optim, ylim=c(1, 2.5))
boxplot(dd[,1], ylim=c(1, 2.5))
boxplot(dd[,2], ylim=c(1, 2.5))
boxplot(dd[,3], ylim=c(1, 2.5))

mean((optim-1.61)^2)
mean((dd[,1]-1.61)^2)
mean((dd[,2]-1.61)^2)
mean((dd[,3]-1.61)^2)

setnicepar()
plot(density(optim))
lines(density(dd[,1]), col=2)
lines(density(dd[,2]), col=3)
lines(density(dd[,3]), col=4)

library(parallel)
res_constant = mclapply(1:300, get_optimal_g, N = c(4800,4800,4800,4800,4800), mc.cores = 8)
res_incr = mclapply(1:300, get_optimal_g, N = rev(c(12000,  6000,  3000,  1500, 750)), mc.cores = 8)
res_de = mclapply(1:300, get_optimal_g, N = c(12000,  6000,  3000,  1500, 750), mc.cores = 8)


saveRDS(unlist(res)[seq(2, 600, 2)], file="output/death_1d/gil_increase.RData")

optim = unlist(res)



optim = readRDS("output/gil_1d.RData")
saveRDS(optim, file="output/death_1d/gil_1d.RData")

24000/c(2, 4, 8, 16, 32)

tmp = g(c(1000, 1000, 1000, 1000, 1000))

ud_mat = tmp[[2]]
ud_mat = ud_mat[order(ud_mat[,2]), ]  

plot(ud_mat[,2], ud_mat[,3])
lines(loess(ud_mat[,3]~ud_mat[,2]), col=3)

x = ud_mat[,2]
optim1 = x[which.max(ud_mat[,3])]/100   
sp  = smooth.spline(ud_mat[,3]~ud_mat[,2])
optim2 = sp$x[which.max(sp$y)]
c(optim1, optim2)
