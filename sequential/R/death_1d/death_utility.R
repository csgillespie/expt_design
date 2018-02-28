#compiler::enableJIT(0)

K = function(beta, i=0, y, ds, n0=50)
  beta^i*dbinom(y, n0, exp(-beta*ds))*dlnorm(beta, -0.005, sqrt(0.01))

get_total_utility = function(ds, n0 = 50){
  subd = 100000000L
  abs_tol = 1e-15
  lower = 0.5; upper = 2.5
  K0 = sapply(0:n0, function(y)integrate(K, lower=lower, upper=upper, i=0, y=y, ds=ds, n0=n0,
                                         subdivisions = subd, abs.tol=abs_tol)$value)
  K1 = sapply(0:n0, function(y)integrate(K, lower=lower, upper=upper, i=1, y=y, ds=ds, n0=n0,
                                         subdivisions = subd, abs.tol=abs_tol)$value)
  K2 = sapply(0:n0, function(y)integrate(K, lower=lower, upper=upper, i=2, y=y,  ds=ds, n0=n0,
                                         subdivisions = subd, abs.tol=abs_tol)$value)
  
  message(sum(K0^3/(K2*K0-K1^2)))
  sum(K0^3/(K2*K0-K1^2))
}



get_utility = local({
  subd = 100000000L
  abs_tol = 1e-14
  ds = seq(0.01, 10, 0.01)
  # Species: X, Time Y,

  function(i, j) {

    # i: species, j: Time
    if(m[i+1, j] != -1) return(m[i+1, j])

    d = ds[j]
    lower = 0.5; upper = 1.5
    K0 = integrate(K, lower=lower, upper=upper, i=0, y=i, ds=d,
                   subdivisions = subd, abs.tol=abs_tol)$value
    K1 = integrate(K, lower=lower, upper=upper, i=1, y=i, ds=d,
                   subdivisions = subd, abs.tol=abs_tol)$value
    K2 = integrate(K, lower=lower, upper=upper, i=2, y=i,  ds=d,
                   subdivisions = subd, abs.tol=abs_tol)$value
    (m[i+1, j] = K0^2/(K2*K0-K1^2))
    m[i+1, j]
  }
})

ds = seq(1.55, 1.65, 0.01)
i = 1.6
dd = matrix(0, ncol=5, nrow = length(ds))
n0 = 45:55
for(i in seq_along(ds)){
  for(j in seq_along(n0)) {
    u = get_total_utility(ds[i], n0[j])
    dd[i,j] = u
  }
}

d = apply(dd, 2, function(i)i/max(i))

plot(d[,1],  col=1)
lines(d[,2], type="l", col=2)
lines(d[,3], type="l", col=3)
lines(d[,4], type="l", col=4)
lines(d[,5], type="l", col=5)


apply(dd, 2, which.max)

# subd = 100000000L
# abs_tol = 1e-14
# ds = seq(0.01, 10, 0.01)
# ## Species: X, Time Y,
# m = matrix(-1, ncol=1000, nrow=51)  
# 
# j = 1000; i = 50
# for(i in 0:50){
#   for(j in 1:1000) {
#     
#     
#     d = ds[j]  
#     lower = 0.5; upper = 1.5
#     K0 = integrate(K, lower=lower, upper=upper, i=0, y=i, ds=d, 
#                    subdivisions = subd, abs.tol=abs_tol)$value
#     K1 = integrate(K, lower=lower, upper=upper, i=1, y=i, ds=d, 
#                    subdivisions = subd, abs.tol=abs_tol)$value
#     K2 = integrate(K, lower=lower, upper=upper, i=2, y=i,  ds=d, 
#                    subdivisions = subd, abs.tol=abs_tol)$value
#     (m[i+1, j] = K0^2/(K2*K0-K1^2))
#   }
#   message(i)
# }
# 
# 
# saveRDS(m, "output/death_1d/utility_matrix.rds")
# get_total_utility(1.61)
# ds = seq(0.01, 5, 0.01)
# utils = sapply(ds, get_total_utility)
# 
# 
# 
# # 
# 
# setnicepar(mfrow=c(1, 2))
# u = utils
# u_diff = u-min(u)
# plot(ds, utils/sum(utils), type="l", ylim=c(0, 0.003))
# lines(ds, u_diff/sum(u_diff), col=2)
# 
# plot(ds, utils^10/sum(utils^10), type="l")
# 
# p = utils/sum(utils)
# lines(ds, p^10/sum(p^10), col=2)
# 
# plot(ds, utils/sum(utils), type="l", ylim=c(0, 0.003))
# 
# 
# 
# plot(ds, u_diff^220/sum(u_diff^220), type="l", xlim=c(1, 2))
# 
# plot(ds, (u_diff/sum(u_diff)^200), type="l", xlim=c(1, 2))
# 
# l_utils = log(utils)



