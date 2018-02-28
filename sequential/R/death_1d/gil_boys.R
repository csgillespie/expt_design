compiler::enableJIT(3)
source("R/death_utility.R")
simulate = function(theta=1, J=1, ds) rbinom(J, 50, exp(-theta*ds))
collapse_d = function(m, no_of_ps) {
  m_means = tapply(m[,3], m[,2], mean)
  m_collapse = matrix(-1, ncol=3, nrow=length(m_means))
  m_collapse[,3] = as.vector(m_means)
  m_collapse[,2] = as.numeric(names(m_means))
  m_collapse[,1] = as.vector(tapply(m[,1], m[,2], sum))
  colnames(m_collapse) = c("n", "d", "u")
  
  ## Remove designs from outside design space
  m_collapse = m_collapse[m_collapse[,3] > 0,]
  return(m_collapse)
}

merge_d = function(m1, m2) {
  ## Merge on design points
  m_merge = merge(m1, m2, "d", all=TRUE)
  m_merge[is.na(m_merge)] = 0
  m = matrix(-1, ncol=3, nrow=nrow(m_merge))
  m[,1] = m_merge[,2]+m_merge[,4]
  m[,2] = m_merge[,1]
  m[,3] = m_merge[,3]*(m_merge[,2]/m[,1])+m_merge[,5]*(m_merge[,4]/m[,1])
  colnames(m) = c("n", "d", "u")
  m
}



g = function(J=1, N = 10, seed=NULL,  no_of_ps=1){
  if(!is.null(seed)) set.seed(seed)
  
  ds = seq(0.01, 50, 0.01)
  d_cur = 161
  
  (theta_cur = rlnorm(no_of_ps[1]*J[1], -0.005, sqrt(0.01)))
  (y_cur = simulate(theta_cur, no_of_ps[1]*J[1], ds[d_cur]))
  (levels = factor(rep(1:J[1], each=no_of_ps[1])))
  (util_cur = prod(tapply(sapply(y_cur, get_utility,  d_cur), levels, mean )))
  
  l = vector("list", length(J))
  i = 2
  for(i in seq_along(J)) {
    l[[i]] = matrix(-1, ncol=3, nrow=N[i])
    (levels = factor(rep(1:J[i], each=no_of_ps[i])))
    for(k in 1:N[i]) {
      if(i == 1){
        d_prop = d_cur + sample(-4:4, 1)
      } else {
        row = sample(1:nrow(l[[i-1]]), 1, prob = l[[i-1]][,3]^(J[i]/J[i-1]))
        d_prop = l[[i-1]][row, 2]
      }
      if(d_prop < 1 || d_prop > 500){
        util_prop = util_prop_mean = util_mean = 0
      } else {
        theta_prop = rlnorm(no_of_ps[i]*J[i], -0.005, sqrt(0.01))
        y_prop = simulate(theta_prop, no_of_ps[i]*J[i], ds[d_prop])
        util_prop = tapply(sapply(y_prop, get_utility,  d_prop), levels, mean)
        if(i == 1) {
          n =0; u_pre =0
        } else {
          n = l[[i-1]][row, 1]; u_pre = l[[i-1]][row, 3]
        }
        util_mean = mean(util_prop)*no_of_ps[i]
        util_prop_mean = (util_mean + n*u_pre)/(n + no_of_ps[i])
        util_prop = prod(util_prop)
      }
      
      if(util_prop/util_cur > runif(1)) {
        y_cur = y_prop
        theta_cur = theta_prop
        d_cur = d_prop
        util_cur = util_prop
      }  
      ## Store the proposed values!
      ## sample_size, design, u(d)
      l[[i]][k,] = c(J[i]*no_of_ps[i], d_prop, util_mean/no_of_ps[i])
    }
    l[[i]] = collapse_d(l[[i]], no_of_ps[i])
    if(i != 1)
      l[[i]] = merge_d(l[[i]], l[[i-1]])
  }
  l
}
g()
J = c(1, 2, 4, 8, 16); N = J = c(1, 2, 4, 8, 16); N = rep(4800,5);no_of_ps=c(1, 1, 1, 1, 1)
sum(J*N)
res1 = g(J, N = N, no_of_ps = no_of_ps)
saveRDS(res1, file="output/Gil_P1.RData")

N = N*c(1, 2, 4, 8, 8)
no_of_ps = c(8, 4, 2, 1, 1)

res8 = g(J, N = N, no_of_ps=c(8, 4, 2, 1, 1))
saveRDS(res8, file="output/Gil_P8.RData")



no_of_ps = 4
J = c(1, 2, 4, 8, 16); N = c(20000, 10000, 10000, 2500, 2500)
#J = c(1, 2, 4); N = c(100, 100, 100)
res4 = g(J, N = N, no_of_ps=no_of_ps)

head(res4[[1]])
res4[[1]]
sum(res4[[1]][,1])
sum(res4[[2]][,1])
sum(res4[[3]][,1])

setnicepar(mfrow =c(2, 3))
plot(res4[[1]][,2], res4[[1]][,3]/4, type="l")
plot(res4[[2]][,2], res4[[2]][,3]/4, type="l")
plot(res4[[3]][,2], res4[[3]][,3]/4, type="l")
plot(res4[[4]][,2], res4[[4]][,3]/4, type="l")
plot(res4[[5]][,2], res4[[5]][,3]/4, type="l")
plot(res4[[5]][,2], res4[[5]][,3]/4, type="l")
lines(lowess(res4[[5]][,2], res4[[5]][,3]/4))

setnicepar()

setnicepar(mfrow =c(2, 3))
plot(res4[[1]][,2], res4[[1]][,1])
plot(res4[[2]][,2], res4[[2]][,1])
plot(res4[[3]][,2], res4[[3]][,1])
plot(res4[[4]][,2], res4[[4]][,1])
plot(res4[[5]][,2], res4[[5]][,1])


setnicepar(mfrow =c(1, 3))
plot(res4[[1]][,2], res4[[1]][,3], ylim=c(100, 140))
plot(res4[[2]][,2], res4[[2]][,3], ylim=c(100, 140))
plot(res4[[3]][,2], res4[[3]][,3], ylim=c(100, 140))
which.max(res4[[3]][,3])
res4[[3]][which.max(res4[[3]][,3]),]







