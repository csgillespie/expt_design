compiler::enableJIT(3)
# ds = ds[d_prop[i,]]
# ds = c(0, ds)
simulate = function(theta=1, J=1, ds) {
  n = matrix(50, ncol=J, nrow=length(ds))
  for(i in 2:(length(ds)))
    n[i,] = rbinom(J, n[i-1,], exp(-theta*(ds[i]-ds[i-1])))
  n
}

#ds = ds[d_prop]


## ds should include 0; y should include 50
K = function(beta, i=0, y, ds)
  beta^i*
  prod(dbinom(y[-1], y[-length(y)], exp(-beta*diff(ds))))*
  dlnorm(beta, -0.005, sqrt(0.01))

K = Vectorize(K, "beta")

get_total_utility = function(ds){
  subd = 100000000L
  abs_tol = 1e-15
  lower = 0.5; upper = 2.5
  K0 = sapply(0:50, function(y)integrate(K, lower=lower, upper=upper, i=0, y=y, ds=ds, 
                                         subdivisions = subd, abs.tol=abs_tol)$value)
  K1 = sapply(0:50, function(y)integrate(K, lower=lower, upper=upper, i=1, y=y, ds=ds, 
                                         subdivisions = subd, abs.tol=abs_tol)$value)
  K2 = sapply(0:50, function(y)integrate(K, lower=lower, upper=upper, i=2, y=y,  ds=ds, 
                                         subdivisions = subd, abs.tol=abs_tol)$value)
  
  message(sum(K0^3/(K2*K0-K1^2)))
  sum(K0^3/(K2*K0-K1^2))
}

get_utility = function(y, ds) {
  lower = 0; upper = 3
  subd = 100000000L
  abs_tol = 1e-60
  K0 = integrate(K, lower=lower, upper=upper, i=0, y=y, ds=ds, 
                 subdivisions = subd, abs.tol=abs_tol)$value
  K1 = integrate(K, lower=lower, upper=upper, i=1, y=y, ds=ds, 
                 subdivisions = subd, abs.tol=abs_tol)$value
  K2 = integrate(K, lower=lower, upper=upper, i=2, y=y,  ds=ds, 
                 subdivisions = subd, abs.tol=abs_tol)$value
  K0^2/(K2*K0-K1^2)
}

#  y = c(50, 20,  7,  0)
#  d = c(0, ds[d_prop])
#  get_utility(y, d)

#y = d_prop[,1];ds = d_prop
# y
# diff(ds)
#  ds = c(0, 100,321,  476)/100
#  theta = 1; J=1
#  y = simulate(1, 1, ds)
# # # K(1, 0, y, ds)
# # # 
#   get_utility(y[,1], ds)
# # 
# i = 99
# values = numeric(100)
# for(i in 1:100) {
#   set.seed(i)
#   ds = c(0, 5, 147, 293)/100
#   y = simulate(rlnorm(1, -0.005, sqrt(0.01)), 1,  ds)
#   values[i] = get_utility(y[,1], ds)
# }
#  
# mean(values)
#  hist(values)
# y
