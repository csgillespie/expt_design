source("utility.R")
library(parallel)
library(tidyverse)
library(acebayes)
compiler::enableJIT(3)
doParallel::registerDoParallel(5)


counter = 0
ace_utility_wrapper = function(d, B) {
  pars = get_pars()
  g = pars["g"]; h = pars["h"]
  c = pars["c"]; b = pars["b"]
  counter <<- counter + B
  tau = tau_prior(B, g, h)
  beta = theta_prior(B, b, c, h, g)
  t = as.vector(d)
  sims = sapply(seq_len(B), function(i) simulate(t, beta[i], tau[i]))
  apply(sims, 2, function(i) get_utility(c, b, h, g, t, i))
}


counter
ace_bayes = function(i = NULL) {
  start.d = matrix(runif(2),nrow=1)
  a = ace(utility = ace_utility_wrapper, start.d = start.d, 
          B = c(200,20), N1 = 14, N2 = 0, lower = 0, upper = 1)
  c(a$phase1.d, a$phase1.trace[length(a$phase1.trace)])
}

l = list()
for(i in 1:500) {
  message(i)
  l[[i]] = ace_bayes(i)
}

r = l %>% 
  reduce(rbind) %>%
  as_tibble()

saveRDS(r, file = "../data/sin/ace.rds")



