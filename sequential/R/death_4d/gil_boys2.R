library("parallel")
compiler::enableJIT(3)
source("R/death_4d/death_utility.R")

collapse_d = function(m, no_of_ps) {
  n_d = ncol(m) - 4
  
  ## sample size of three hard coded
  n = aggregate(m[,7], by=list(m[,3], m[,4], m[,5], m[,6]), sum)
  u = aggregate(m[,8], by=list(m[,3], m[,4], m[,5], m[,6]), mean)
  
  m_collapse = matrix(0, ncol=n_d + 4, nrow=nrow(n))
  m_collapse[,1] = n[, n_d+1] # Sample size
  m_collapse[,2] = u[, n_d+1] # u(d)
  m_collapse[,3:(n_d + 2)] = as.matrix(u[,1:n_d]) # d
  
  colnames(m_collapse) = c("n", "u", paste0("d", (1:n_d)-1), "n_pr", "u_pr")
  
  ## Remove designs from outside design space
  m_collapse = m_collapse[m_collapse[,2] > 0,]
  return(m_collapse)
}

merge_d = function(m) {
  n_d = ncol(m) - 4
  ## Merge on design points
  m[,2] = (m[,2]*m[,1] + m[,8]*m[,7])/(m[,1] + m[,7])
  m[,1] = m[,1] + m[,7]
  m[,7:8] = m[,7:8]*0
  colnames(m) = c("n", "u", paste0("d", (1:n_d)-1), "n_pr", "u_pr")
  m
}

J = 2; no_of_ps = 3
J = 1:2; N= c(10, 10); no_of_ps = c(1, 1); n_d = 4
g = function(J=1, N = 10, seed=NULL,  no_of_ps=1, n_d = 4){
  if(!is.null(seed)) set.seed(seed)
  
  (d_cur = c(1, sort(sample(2:501, n_d-1))))
  ds = seq(0.01, 10, 0.01)

  (theta_cur = rlnorm(no_of_ps[1]*J[1], -0.005, sqrt(0.01)))
  (y_cur = simulate(theta_cur, no_of_ps[1]*J[1], ds[d_cur]))
  (levels = factor(rep(1:J[1], each=no_of_ps[1])))
  (util_cur = prod(tapply(apply(y_cur, 2, get_utility, ds[d_cur]), levels, mean)))
  
  l = vector("list", length(J))
  i = 1;k = 1
  for(i in seq_along(J)) {
    if(i == 1)
      l[[i]] = matrix(-1, ncol=n_d+4, nrow=N[i])
    else
      l[[i]] = l[[i-1]]
    (levels = factor(rep(1:J[i], each=no_of_ps[i])))
    for(k in 1:N[i]) {
      #set.seed(k)
      
      if(i == 1){
        #d_prop = sort(d_cur + sample(-4:4, n_d))
        (d_prop = c(1, sort(sample(2:501, n_d-1))))
        l[[1]][k,] = c(0, 0, d_prop, 0, 0)
        row = k
      } else {
        row = sample(1:nrow(l[[i-1]]), 1, prob = l[[i-1]][,2]^J[i])
        d_prop = l[[i-1]][row, 3:(2+n_d)]
      }
      
      if(any(d_prop < 1) || any(d_prop > 501) || length(unique(d_prop)) != n_d){
        util_prop = util_prop_mean = util_mean = 0
      } else {
        (theta_prop = rlnorm(no_of_ps[i]*J[i], -0.005, sqrt(0.01)))
        (y_prop = simulate(theta_prop, no_of_ps[i]*J[i], ds[d_prop]))
        (util_prop = tapply(apply(y_prop, 2, get_utility, ds[d_prop]), levels, mean))
        if(i == 1) {
          n =0; u_pre =0
        } else {
          n = l[[i-1]][row, 1]; u_pre = l[[i-1]][row, 2]
        }
        util_mean = mean(util_prop)
        util_prop_mean = (util_prop*no_of_ps[i] + n*u_pre)/(no_of_ps[i] + n)
        util_prop = prod(util_prop_mean)
      }
      
      if(util_prop/util_cur > runif(1)) {
        y_cur = y_prop
        theta_cur = theta_prop
        d_cur = d_prop
        util_cur = util_prop
      }  
      ## Store the proposed values!
      ## sample_size, design, u(d)
      l[[i]][row, 4+n_d] =  (n*u_pre + l[[i]][row, 3+n_d]*l[[i]][row, 4+n_d] + no_of_ps[i]*J[i]*util_mean)
      l[[i]][row, 4+n_d] = l[[i]][row, 4+n_d]/(n + l[[i]][row, 3+n_d] + no_of_ps[i]*J[i])
      l[[i]][row, 3+n_d] = (n + l[[i]][row, 3+n_d] + no_of_ps[i]*J[i])
    }
    if(i == 1)
      l[[i]] = collapse_d(l[[i]], no_of_ps[i])
    else
      l[[i]] = merge_d(l[[i]])
  }
  l
}
J = c(1, 2, 4, 8, 16); 
N = c(20000, 10000, 5000, 2500, 1250);no_of_ps=c(8, 4, 2, 1, 1)
res = g(J=J, N =N, no_of_ps=no_of_ps, n_d = 4)
res


head(res[[1]])
head(res[[5]])
setnicepar(mfrow=c(2, 2))
tmp = res[[5]]; i = 1
tail(tmp)
d1 = unlist(by(tmp[, 1:2], tmp[,i+3], function(i) sum(i[,1]*i[,2])/sum(i[,1]), simplify = FALSE))
u = as.vector(d1)^16
t1 = as.numeric(names(d1))
#plot(t1, u/sum(u), type="l", ylim=c(0, 0.005))
lines(t1, u/sum(u), col=2)


plot(t1, cumsum(u/sum(u)), type="l")
abline(h=c(0.25, 0.75))

i=2;lines(t1, u^i/sum(u^i), type="l", col=2)
i=4;lines(t1, u^i/sum(u^i), type="l", col=3)
i=8;lines(t1, u^i/sum(u^i), type="l", col=4)
i=16;lines(t1, u^i/sum(u^i), type="l", col=4)


plot(sapply(1:50, function(i) sum(u^2*u^i/sum(u^i)) - sum(u*u^i/sum(u^i))^2))


tmp = res[[1]];
d1 = unlist(by(tmp[, 1:2], tmp[,i+3], function(i) sum(i[,1]*i[,2])/sum(i[,1]), simplify = FALSE))
u = as.vector(d1)
t1 = as.numeric(names(d1))
lines(t1, u, type="l", col=2)

length(unique(dd[,3]))
?tapply

res[[1]]


tmp[[1]]
J = c(1, 2, 4, 8, 16); N = J = c(1, 2, 4, 8, 16); N = c(20000, 10000, 5000, 2500, 1250);no_of_ps=c(1, 1, 1, 1, 1)
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







