compiler::enableJIT(3)
cores = 6
options("mc.cores" = cores)
#set.seed(1)
source("R/pk.R")
source("R/pk_prior.R")
pars = propose_pars()
sigma2 = propose_sigma2()
ke = pars[1]; V = pars[2]
x = seq(0, 300, length.out=100)
y = get_data(ke, V, x=x, sigma2=sigma2)

plot(x, y)
lines(x, y, col=2)
get_ll = function(x, y, ke, V, sigma2, D=500, T_inf=30) {
  sim = get_data(x=x,D=D, T_inf=T_inf, ke=ke, V=V, sigma2=0)
  sum(sapply(2:length(y), function(i) dnorm(y[i], sim[i], sim[i]*sqrt(sigma2), log=TRUE)))
  
  #sum(dnorm(y, sim, sim*sqrt(sigma2), log=TRUE))
}

y
get_utility = function(x, y, sigma2, pars, N=10000) {
  pars = as.vector(pars)
  sigma2_cur = sigma2_prop =sigma2
  pars_cur = pars_prop = pars#propose_pars()
  ll_cur = ll_prop = -Inf
  dd = data.frame(sigma2 = numeric(N), ke = pars[1], V = pars[2], ll = 0, cmax=0)
  for(i in 1:N) {
    sigma2_prop = propose_sigma2()
    (pars_prop = pars_cur +rnorm(2, 0, c(0.0001, 300)))
    (ll_prop = get_ll(x, y, ke=pars_prop[1], V=pars_prop[2], sigma2=sigma2_prop))
    u = log(runif(1))
    if(ll_prop  > u+ ll_cur) {
      sigma2_cur = sigma2_prop
      pars_cur = pars_prop
      ll_cur = ll_prop
    }
    dd[i,] = c(sigma2_cur, pars_cur, ll_cur, get_cmax(pars_cur[1], pars_cur[2]))
  }
  1/var(get_cmax(dd$ke, dd$V))
  
}


get_us = function(s, m) {
  x = pbeta(q, m[s, 1], m[s, 2])*300
  pars = propose_pars()
  sigma2 = propose_sigma2()
  ke = pars[1]; V = pars[2]
  y = get_data(ke, V, x=x, sigma2=sigma2)
  u = get_utility(x, y, sigma2, pars, N=1000)
  u
}



n = 1000
x = seq(0, 1, length.out=100)
m_g = expand.grid(x, x)
m = matrix(0, ncol=4, nrow=nrow(m_g))
m[,1] = m_g[,1]
m[,2] = m_g[,2]
m = m[m[,1] > 0.001 & m[,2] > 0.001, ]
prior = exp(MASS::mvrnorm(1e5, mu = c(-3.26, 8.99), 
                          Sigma = matrix(c(0.0071, -0.0057, -0.0057, 0.0080), ncol=2)))

min_cmax = 1/var(get_cmax(prior[,1], prior[,2]))
min_cmax = 500
m[,4] = min_cmax

q = seq(0, 1, length.out = 11)
nr = nrow(m)
for(i in 1:n) {
  set.seed(i)
  s = sample(1:nr, 4*cores, prob=m[,4])
  s = s + sample(-10:10, 4*cores, replace=TRUE)
  s[s < 1] = 1;s[s > nr] = nr 
  res = unlist(parallel::mclapply(s, get_us, m))
  m[s,4] = (m[s,4] * m[s,3] + res)/(m[s,3] + 1)
  m[s,3] = m[s, 3] + 1
  
  message(i)
}
min(m[,4])

sapply(1:10, function(i) var(m[,4]^i/max(m[,4]^i)))
0.0327/2

#save(m, file="m.RData")


plot(cumsum(m[,4]/sum(m[,4])))

plot(m[,3])
plot(m[,4], type="l")
which.max(m[,4])
s= 384


which.min(m[,4])
s = 8120
m[8120,]



min(m[m[,4] > 0, 4])

head(m[m[,4] > 0, ])











dd = dd[-(1:100), ]
setnicepar(mfrow=c(2,2))
plot(dd$sigma2, type="l"); abline(h=sigma2, col=2)
plot(dd$ke, type="l");abline(h=ke, col=2)
plot(dd$V, type="l"); abline(h=V, col=2)
plot(density(dd[,5]*1000))
head(dd)

sum(diff(dd[,1])!=0)/nrow(dd)*100
