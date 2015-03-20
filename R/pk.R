
get_y = function(t, D, T_inf, ke, V, sigma2){
  eps_t = rnorm(1, 0, sqrt(sigma2))
  y = D/T_inf*1/(ke*V)*(1 + eps_t)
  if(t<=T_inf) { 
    y = y*(1- exp(-ke*t))
  } else {
    y = y*(1- exp(-ke*T_inf))*exp(-ke*(t-T_inf))
  }
  y
}

get_pars = function(log = FALSE) {
  pars = MASS::mvrnorm(1, mu = c(-3.26, 8.99), 
          Sigma = matrix(c(0.0071, -0.0057, -0.0057, 0.0080), ncol=2))
  if(!log) pars = exp(pars)
  pars  
}

get_data = function(ke, V, sigma2, x=0:300, D=500, T_inf=30){
  y = sapply(x, get_y, D, T_inf, ke, V, sigma2)  
  as.vector(y)
}

get_cmax = function(ke, V,D=500, T_inf=30) D/T_inf*1/(ke*V)*(1-exp(-ke*T_inf))

# 
# 
# plot(0, 0, type="n", xlim=c(0, 300), ylim=c(0, 60))
# for(i in 1:1000){
#   pars = get_pars(log=FALSE)
#   
#   ke = pars[1]; V = pars[2]
# 
#   lines(t, 1000*y, col=i)
# }
# 
# x = t
# 
# y
# 
