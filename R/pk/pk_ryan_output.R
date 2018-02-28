
.libPaths(c(.libPaths(), "/home/ubuntu/Rpackages"))
library(doParallel)
(no_of_cores = parallel::detectCores())
#cl = makeCluster(6)

compiler::enableJIT(3)

source("R//pk/pk.R")
source("R/pk/pk_prior.R")
source("R/pk/pk_utility.R")
source("R/pk/pk_aws.R")
registerDoParallel(no_of_cores)
MAX_X = 100; MAX_Y = 800
pars = c(18, 410)
q = seq(0, 1, length.out=12)[-12]
(x = qbeta(q, pars[1], pars[2])*720)

## Ryan
s=1/11
x = qbeta(c(s,2*s, 3*s, 4*s, 5*s, 6*s, 7*s, 8*s, 9*s, 10*s),0.73,3.1)*720


res = foreach(k = 1:30, .combine = c) %dopar% {
  pars = propose_pars()
  sigma2 = propose_sigma2()
  ke = pars[1]; V = pars[2]
  y = get_data(ke, V, x=x, sigma2=sigma2)
  estimate_utility(x, y, sigma2, pars, N=5000)
}
mean(res)
# 0.6457

