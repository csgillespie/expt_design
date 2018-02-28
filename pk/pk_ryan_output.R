library("doParallel")
library("coda")
(no_of_cores = parallel::detectCores())
registerDoParallel(no_of_cores)

source("R/pk/pk.R")
source("R/pk/pk_prior.R")
source("R/pk/pk_utility.R")

# Our optimal - mean utility of around 1
pars = c(0.25, 9)
q = seq(0, 1, length.out=12)[-12]
(x = qbeta(q, pars[1], pars[2])*720)

## Ryan - mean utility of around 0.6
s=1/11
x = qbeta(c(s,2*s, 3*s, 4*s, 5*s, 6*s, 7*s, 8*s, 9*s, 10*s),0.73,3.1)*720

res = foreach(k = 1:no_of_cores, .combine = c) %dopar% {
  pars = propose_pars()
  sigma2 = propose_sigma2()
  ke = pars[1]; V = pars[2]
  y = get_data(ke, V, x=x, sigma2=sigma2)
  estimate_utility(x, y, sigma2, pars, N=5000)
}
mean(res)
beepr::beep(3)
