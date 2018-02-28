library(parallel)
library(acebayes)
compiler::enableJIT(3)
source("R/death_1d/death_utility.R")
simulate = function(theta=1, J=1, ds) rbinom(J, 50, exp(-theta*ds))

ace_utility_wrapper = function(d, B) {
  theta_prop = rlnorm(B, -0.005, sqrt(0.01))
  d_prop = round(d, 0)
  ds = seq(0.01, 10, 0.01)
  y_prop = sapply(theta_prop, simulate, 1, ds[d_prop])
  util_prop = sapply(y_prop, get_utility, d_prop)
  util_prop  
}

ace_bayes = function(i) {
  start.d = matrix(sample(1:1000, 1),nrow=1)
  ace(utility = ace_utility_wrapper, start.d = start.d, 
      B=c(200,19), N2 = 0, lower = 1, upper = 1000)
}

cl = makeCluster(7)
clusterCall(cl, function() library("acebayes"))
clusterExport(cl, c("ace_bayes", "ace_utility_wrapper", "simulate", "get_utility", "K"))
res = parLapply(cl, 1:500, ace_bayes)
res1 = unlist(lapply(res, function(i) round(i$phase1.d)/100))
beepr::beep(8)
saveRDS(res, file="output/death_1d/cx_ex.rds")
