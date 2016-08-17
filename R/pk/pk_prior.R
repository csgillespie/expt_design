eval_sigma2_prior = function(x){
  m = 0.01; v = 10^(-5)
  scale = v/m;shape = m^2/v
  dgamma(x, shape = shape, scale=scale, log=TRUE)
}

eval_pars_prior = function(x) {
 mvtnorm::dmvnorm(log(x), c(-3.26, 8.99), 
                matrix(c(0.0071, -0.0057, -0.0057, 0.0080), ncol=2), log=TRUE)
}

propose_sigma2 = function(log=FALSE) {
  m = 0.01; v = 10^(-5)
  scale = v/m;shape = m^2/v
  rgamma(1, shape = shape, scale=scale)
}

propose_pars = function(d=1) {
  pars = MASS::mvrnorm(1, mu = c(-3.26, 8.99), 
                       Sigma = matrix(c(0.0071, -0.0057, -0.0057, 0.0080)*d, ncol=2))
  pars = exp(pars)
  names(pars) = c("ke", "V")
  pars
}
