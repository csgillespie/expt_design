ll = function(y, ds, beta)
  sum(dbinom(y[-1], y[-length(y)], exp(-beta*diff(ds)), log=TRUE))

ds=c(0, .22, 2.85, 4.9)
y = c(50, 38,7,2)
theta_cur = 1
N = 100000

ll_cur = -1000
res = matrix(0, ncol=2, nrow=N)
for(i in 1:N) {
  theta_prop = theta_cur + rnorm(1, 0, 0.06)
  ll_prop = ll(y, ds, theta_prop)
  
  if(ll_prop-ll_cur + 
     dlnorm(theta_prop, -0.005, sqrt(0.01), log=TRUE) - 
     dlnorm(theta_cur, -0.005, sqrt(0.01), log=TRUE)  > log(runif(1))) {
    theta_cur = theta_prop
    ll_cur =ll_prop
  }
  res[i,] = c(ll_cur, theta_cur)
}
res
plot(res[,2], type="l")
1/var(res[,2])

get_utility(y[-1], c(0,ds))


diff(c(0, ds))
beta=1
dbinom(y[-1], y[-length(y)], exp(-beta*diff(ds)))

dbinom(y[-1], y[-length(y)], exp(-beta*diff(ds)), log=TRUE)
diff(ds)
ds
