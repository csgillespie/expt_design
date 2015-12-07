compiler::enableJIT(3)
source("R/death_1d/death_utility.R")


simulate = function(theta=1, J=1, ds) rbinom(J, 50, exp(-theta*ds))

J = 3
muller = function(J=1, N = 10, seed=NULL){
  if(!is.null(seed))set.seed(seed)
  d_samples = matrix(0, ncol=2, nrow=N)
  ds = seq(0.01, 10, 0.01)
  d_cur = sample(1:length(ds), 1)
  (theta_cur = rlnorm(J, -0.005, sqrt(0.01)))
  (y_cur = simulate(theta_cur, J, ds[d_cur]))
  (util_cur = prod(sapply(y_cur, get_utility,  d_cur)))
  
  i = 1
  for(i in 1:N) {
    # set.seed(i)
    d_prop = sample(1:length(ds), 1)
    theta_prop = rlnorm(J, -0.005, sqrt(0.01))
    y_prop = simulate(theta_prop, J, ds[d_prop])
    util_prop = prod(sapply(y_prop, get_utility,  d_prop))
    
    if(util_prop/util_cur > runif(1)) {
      y_cur = y_prop
      theta_cur = theta_prop
      d_cur = d_prop
      util_cur = util_prop
    }  
    d_samples[i,] = c(ds[d_cur], util_cur)
  }
  d_samples
}
muller()

get_optimal = function(out){
  den = density(out[,1])
  den$x[which.max(den$y)] 
}

get_muller_optims = function(i, N=24000) {
  muller1 = muller(J=1, N=N)
  muller8 = muller(J=8, N=ceiling(N/8))
  muller16 = muller(J=16, N=ceiling(N/16))
  c(get_optimal(muller1), get_optimal(muller8), get_optimal(muller16))
}

mullers = parallel::mclapply(1:225, get_muller_optims, mc.cores = 4)




system.time(m8 <- muller(J=8, 24000/8))


