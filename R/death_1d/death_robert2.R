compiler::enableJIT(3)
library(parallel)
source("R/death_1d/death_utility.R")

simulate = function(theta=1, J=1, ds) rbinom(J, 50, exp(-theta*ds))

robert = function(J=1, N = 10, seed=NULL){
  if(!is.null(seed)) set.seed(seed)
  
  ds = seq(0.01, 10, 0.01)
  d_cur = sample(seq_along(ds), 1)
  
  (theta_cur = rlnorm(J[1], -0.005, sqrt(0.01)))
  (y_cur = simulate(theta_cur, J[1], ds[d_cur]))
  (util_cur = prod(sapply(y_cur, get_utility,  d_cur)))
  
  l = vector("list", length(J))
  i = 1
  for(i in seq_along(J)) {
    l[[i]] = matrix(-1, ncol=3, nrow=N[i])
    for(k in 1:N[i]) {
      if(i == 1){
        d_prop = sample(seq_along(ds), 1)
      } else {
        row = sample(1:nrow(l[[i-1]]), 1)
        d_prop = l[[i-1]][row, 2]
      }
      theta_prop = rlnorm(J[i], -0.005, sqrt(0.01))
      y_prop = simulate(theta_prop, J[i], ds[d_prop])
      util_prop = prod(sapply(y_prop, get_utility,  d_prop))
      
      
      if(util_prop/util_cur > runif(1)) {
        y_cur = y_prop
        theta_cur = theta_prop
        d_cur = d_prop
        util_cur = util_prop
      }  
      l[[i]][k,] = c(J[i], d_cur, util_cur)
    }
    if(i != length(J)) {
      ## Powering up the proposal
      for(k in 1:N[i]) {
        theta_prop = rlnorm(J[i+1] - J[i], -0.005, sqrt(0.01))
        d_prop = l[[i]][k, 2]
        y_prop = simulate(theta_prop, J[i], ds[d_prop])
        util_prop = prod(sapply(y_prop, get_utility,  d_prop))
        l[[i]][k,3] = l[[i]][k,3]*util_prop
      }
    }
    ## Resample
    re_sam = sample(1:nrow(l[[i]]), size = nrow(l[[i]]), prob = l[[i]][,3], replace=TRUE)
    l[[i]] = l[[i]][re_sam, ]
  }
  l
}

get_optimal_robert = function(seed, J, N) {
  res = robert(J, N, seed=seed)
  ud_mat = res[[length(res)]]
  x = ud_mat[,2]
  optim1 = x[which.max(ud_mat[,3])]/100   
  optim1
}

J = c(1, 2, 4); N = c(2400, 2400, 2400)

#m = get_optimal_robert(seed = 12, J, N)
#beepr::beep(8)

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
