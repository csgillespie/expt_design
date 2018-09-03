library(mvtnorm)
library(hash)
library(foreach)

no_d = 15
counter = 0
get_utility = function(x, epsilon = 0.03) {
  m = seq_len(length(x)) - 0.5
  sigma = diag(10, nrow = length(x), ncol = length(x))
  exp(15 * log(20 * pi)/2 + dmvnorm(x, mean = m, sigma, log = TRUE)) * rlnorm(1, 0, sdlog = epsilon)
  # 
  #   u = dmvnorm(x, mean = m, sigma, log = TRUE)
  # exp(u + 5 + rnorm(1, sd = epsilon))
}

create_hash = function(designs, percentiles) {
  utils = designs$util
  cut_off = quantile(utils, prob = percentiles)
  designs = designs[designs$util > cut_off,]
  h = hash()
  for (i in 1:nrow(designs)) {
    h[[paste(designs[i,], collapse = " ")]] = i
  }
  list(designs, h)
}

initialise_design = function(N, no_d) {
  initial = lhs::randomLHS(N, no_d)
  initial = round(initial, 2)
  initial = initial * no_d
  initial = initial[!duplicated(initial),]
  initial = t(apply(initial, 1, sort))

  utils = vapply(1:N, function(i) get_utility(initial[i, ]), FUN.VALUE = numeric(1))

  designs = as.data.frame(initial)
  designs$util = utils
  designs$util2 = utils^2
  designs$n = 1
  
  create_hash(designs, 0)
}

perturb_times = function(x, lambda) {
  x + (rpois(length(x), lambda) - rpois(length(x), lambda)) * 0.01
}

