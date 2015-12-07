library("parallel")
compiler::enableJIT(3)
source("R/death_4d/death_utility.R")


#simulate = function(theta=1, J=1, ds) rbinom(J, 50, exp(-theta*ds))

J = 1; N= 10; n_d = 4
muller = function(J=1, N = 10, seed=NULL, n_d =4){
  if(!is.null(seed))set.seed(seed)
  (d_cur = c(1, sort(sample(2:501, n_d-1))))
  ds = seq(0.0, 50, 0.01)
  
  d_samples = matrix(0, ncol=length(d_cur) + 1, nrow=N)
  
  (theta_cur = rlnorm(J, -0.005, sqrt(0.01)))
  (y_cur = simulate(theta_cur, J, ds[d_cur]))
  (util_cur = prod(apply(y_cur, 2, get_utility, ds[d_cur])))
  
  i = 1
  for(i in 1:N) {
    #set.seed(i)
    d_prop = c(1, sort(sample(2:501, n_d-1)))
    theta_prop = rlnorm(J, -0.005, sqrt(0.01))
    y_prop = simulate(theta_prop, J, ds[d_prop])
    util_prop = prod(apply(y_cur, 2, get_utility, ds[d_cur]))
    
    if(util_prop/util_cur > runif(1)) {
      y_cur = y_prop
      theta_cur = theta_prop
      d_cur = d_prop
      util_cur = util_prop
    }  
    d_samples[i,] = c(ds[d_cur], util_cur^(1/J))
  }
  d_samples
}



cl = makeCluster(6)
clusterExport(cl, c("muller", "simulate", "get_utility", "K"))
res1 = parLapply(cl, 1:100, function(i) muller(J=1, N=10^5, i))
saveRDS(res1, file="output/death_3d/muller1.RData")
res16 = parLapply(cl, 1:100, function(i) muller(J=16, N=ceiling(10^5/16), i))
saveRDS(res16, file="output/death_3d/muller16.RData")
res16_par = parLapply(cl, 1:100, function(i) muller(J=16, N=10^5/2, i))
saveRDS(res16_par, file="output/death_3d/muller16_par.RData")
stopCluster(cl)


head(res1[[1]])
plot(res1[[1]][,2], type="l")
hist(res1[,1], breaks=600, freq=F)



plot(res1[,2]^(1/J), type="l")

hist(res[,1])
