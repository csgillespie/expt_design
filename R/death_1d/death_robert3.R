compiler::enableJIT(3)
library(parallel)
source("R/death_1d/death_utility.R")

simulate = function(theta=1, J=1, ds) rbinom(J, 50, exp(-theta*ds))

initalise_robert = function(N) {
  
  ds = seq(0.01, 10, 0.01)
  d_cur = sample(seq_along(ds), 1)
  
  m = matrix(-1, ncol=4, nrow=N)
  for(i in 1:N) {
    d_prop = sample(seq_along(ds), size=1)
    theta_prop = rlnorm(1, -0.005, sqrt(0.01))  
    
    y_prop = simulate(theta_prop, 1, ds[d_prop])
    util_prop = prod(sapply(y_prop, get_utility,  d_prop))
    m[i,] = c(1, d_prop, util_prop, 0)
  }
  m
}

power_up = function(m) {
  ds = seq(0.01, 10, 0.01)
  J = m[1,1]
  for(i in 1:nrow(m)) {
    theta_prop = rlnorm(m[i, 1], -0.005, sqrt(0.01))
    d_prop = m[i, 2]
    y_prop = simulate(theta_prop, J, ds[d_prop])
    util_prop = prod(sapply(y_prop, get_utility,  d_prop))
    m[i, 3] = m[i, 3]*util_prop
  }
  m[,1] = 2*m[,1]
  m[,4] = m[,3]/sum(m[,3])
  m
}


robert = function(J=1, N = 10, seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  
  ds = seq(0.01, 10, 0.01)
  d_cur = sample(seq_along(ds), 1)
  
  m = initalise_robert(N)
  m = power_up(m)
  l = vector("list", length(J))
  l[[1]] = m
  
  re_sam = sample(1:nrow(m), 
                  size = nrow(m), 
                  prob = m[,4], 
                  replace=TRUE)
  l[[1]] = l[[1]][re_sam, ]
  
  i = 1
  k = 1
  
  for(i in seq_along(J[-1])) {
    message(i)
    i = i + 1
    l[[i]] = l[[i-1]]
    for(k in 1:N) {
      d_prop = l[[i-1]][k, 2]
      d_prop = d_prop + rpois(1, lambda=3) - rpois(1, lambda=3)
      if(d_prop > 1 && d_prop <= length(ds)) {
      
        J_i = l[[i-1]][1, 1]
        theta_prop = rlnorm(J_i, -0.005, sqrt(0.01))
        y_prop = simulate(theta_prop, J_i, ds[d_prop])
        util_prop = prod(sapply(y_prop, get_utility,  d_prop))
        
        alpha = util_prop/m[k, 3]
        if(alpha > runif(1)) {
          l[[i]][k,] = c(J[i], d_prop, util_prop, 0)
        } 
      }
    }

    m = power_up(l[[2]])
    re_sam = sample(1:nrow(m), 
                    size = nrow(m), 
                    prob = m[,4], 
                    replace=TRUE)
    l[[i]] = m[re_sam, ]
  }
  l
}


get_optimal_robert = function(seed, J, N) {
  res = robert(J, N[1], seed=seed)
  ud_mat = res[[length(res)]]
  x = ud_mat[,2]cd
  optim1 = x[which.max(ud_mat[,3])]/100   
  optim1
}

J = c(1, 2, 4); N = c(2400, 2400, 2400); seed = NULL
sum(c(N*J, (N*J)[-3]))
rob1 = mclapply(1:500, get_optimal_robert, J, N, mc.cores = 8)
beepr::beep(8)


J = c(1, 2, 4, 8); N = rep(1090, 4)
rob2 = mclapply(1:500, get_optimal_robert, J, N, mc.cores = 8)

J = c(1, 2, 4, 8, 16); N = rep(522, 5)
rob3 = mclapply(1:500, get_optimal_robert, J, N, mc.cores = 4)


saveRDS(rob1, file="output/death_1d/robert3.rds")
saveRDS(rob2, file="output/death_1d/robert4.rds")
saveRDS(rob3, file="output/death_1d/robert5.rds")
head(rob1)


J = c(1, 2, 4)
N = c(4000, 4000, 4000)/2.15
sum(c(N*J, (N*J)[-1]))



# N = c(20000, 12000, 20000, 16000, 16000)
# no_of_ps = c(8, 4, 2, 1, 1)

# 
# sum(N*J)
# l = robert(J = J, N = N)

setnicepar(mfrow=c(3, 2))
hist(l[[1]][,2]/100, breaks="fd", ylim=c(0, 1), freq=F)
hist(l[[2]][,2]/100, breaks="fd", ylim=c(0, 1), freq=F)
hist(l[[3]][,2]/100, breaks="fd", ylim=c(0, 1), freq=F)
hist(l[[4]][,2]/100, breaks="fd", ylim=c(0, 1), freq=F)
hist(l[[5]][,2]/100, breaks="fd", ylim=c(0, 1), freq=F)
