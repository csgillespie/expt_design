set.seed(1)
source("R/pk.R")
pars = propose_pars()
sigma2 = propose_sigma2()
ke = pars[1]; V = pars[2]
x = seq(0, 300, length.out=10)
y = get_data(ke, V, x=x, sigma2=sigma2)
plot(sim-y)
lines(x, sim, col=2)
get_ll = function(x, y, ke, V, sigma2, D=500, T_inf=30) {
  sim = get_data(x=x,D=D, T_inf=T_inf, ke=ke, V=V, sigma2=0)
  sum(sapply(2:length(y), function(i) dnorm(y[i], sim[i], sim[i]*sqrt(sigma2), log=TRUE)))
  
  #sum(dnorm(y, sim, sim*sqrt(sigma2), log=TRUE))
}



sigma2_cur = sigma2_prop =sigma2
pars_cur = pars_prop = pars#propose_pars()
ll_cur = ll_prop = -Inf
N = 10000
dd = data.frame(sigma2 = numeric(N), ke = pars[1], V = pars[2], ll = 0, cmax=0)
for(i in 1:N) {
  sigma2_prop = propose_sigma2()
  (pars_prop = pars_cur +rnorm(2, 0, c(0.001, 300)))
  (ll_prop = get_ll(x, y, ke=pars_prop[1], V=pars_prop[2], sigma2=sigma2_prop))
  
  u = log(runif(1))
  if(ll_prop  > u+ ll_cur) {
    sigma2_cur = sigma2_prop
    pars_cur = pars_prop
    ll_cur = ll_prop
  }
  dd[i,] = c(sigma2_cur, pars_cur, ll_cur, get_cmax(pars_cur[1], pars_cur[2]))
  
}
head(dd)
setnicepar(mfrow=c(2,2))
plot(dd[,1], type="l")
plot(dd[,2], type="l")
plot(dd[,3], type="l")
plot(density(dd[,5]*1000))
head(dd)

sum(diff(dd[,1])!=0)/nrow(dd)*100
