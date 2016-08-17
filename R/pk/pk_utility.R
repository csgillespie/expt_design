get_ll = function(x, y, ke, V, sigma2, D=500, T_inf=30) {
  sim = get_data(x=x,D=D, T_inf=T_inf, ke=ke, V=V, sigma2=0)
  sum(sapply(2:length(y), function(i) dnorm(y[i], sim[i], sim[i]*sqrt(sigma2), log=TRUE)))
}

check_timepoints = function(x) {
  if(sum(x < 120) < 7) return(FALSE)
  if(any(abs(diff(x)) < 1)) return(FALSE)
  return(TRUE)
}

get_utility = function(x, y, sigma2_cur, pars_cur, ll_cur, N=10000) {
  dd = data.frame(sigma2 = numeric(N), ke = numeric(N), V =numeric(N), ll = 0, cmax=0)
  for(i in 1:N) {
    sigma2_prop = propose_sigma2()
    #  (pars_prop = pars_cur +rnorm(2, 0, c(0.0001, 300)))
    pars_prop = propose_pars(d=2.5)
    
    (ll_prop = get_ll(x, y, ke=pars_prop[1], V=pars_prop[2], sigma2=sigma2_prop))
    u = log(runif(1))
    prior = eval_pars_prior(pars_prop) - eval_pars_prior(pars_cur) + 
      eval_sigma2_prior(sigma2_prop) - eval_sigma2_prior(sigma2_cur)
    
    if(ll_prop  + prior> u+ ll_cur) {
      sigma2_cur = sigma2_prop
      pars_cur = pars_prop
      ll_cur = ll_prop
    }
    dd[i,] = c(sigma2_cur, pars_cur, ll_cur, get_cmax(pars_cur[1], pars_cur[2]))
  }
  return(dd)
}

estimate_utility = function(x, y, sigma2, pars, N) {
  dd = NULL;ll = -Inf; ess = 0
  while(ess < 50) {
    dd_tmp = get_utility(x, y, sigma2, pars, ll, N)
    dd = rbind(dd, dd_tmp)
    (ess = coda::effectiveSize(dd[,ncol(dd)]))
    sigma2 = dd[nrow(dd), 1]; pars = unlist(dd[nrow(dd), 2:3]); 
  }
  (1/var(dd[,ncol(dd)]))
}

get_parameters = function(m, s=NULL) {
  x = seq(0, 1, length.out=MAX_X+1)
  y = seq(0, 12, length.out=MAX_Y+1)
  if(!is.null(s))
    pars = c(x[m[s, 1]+1], y[m[s, 2]+1])
  else 
    pars =  matrix(c(x[m[,1]+1], y[m[,2]+1]), ncol=2)
  return(pars)
}

get_us = function(s, m) {
  pars = get_parameters(m, s)
  q = seq(0, 1, length.out=12)[-12]
  (x = round(qbeta(q, pars[1], pars[2])*720))
  if(!check_timepoints(x)) return(0)
  
  pars = propose_pars()
  sigma2 = propose_sigma2()
  ke = pars[1]; V = pars[2]
  y = get_data(ke, V, x=x, sigma2=sigma2)
  estimate_utility(x, y, sigma2, pars, N=5000)
}