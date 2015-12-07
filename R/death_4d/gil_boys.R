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


pre_screen = function(N=10, ds, n_d) {
  
  ud_mat = matrix(1, ncol=n_d + 2, nrow=N)
  d_prop = matrix(sample(1:length(ds), n_d*N, replace=TRUE), ncol=n_d)
  d_prop = ud_mat[,2:(1+n_d)] = t(apply(d_prop, 1, sort))
  theta_prop = rlnorm(N, -0.005, sqrt(0.01))
  i = 1
  #simulate(theta_prop[i], 1, c(0, ds[d_prop[i,]]))
  y_prop = t(sapply(1:N, function(i) simulate(theta_prop[i], 1, c(0, ds[d_prop[i,]]))))
  i = 1
  get_utility(y_prop[i,],  c(0, ds[d_prop[i,]]))  
  
  util_prop = sapply(1:N, function(i) get_utility(y_prop[i,],  c(0, ds[d_prop[i,]])))
  ud_mat[,ncol(ud_mat)] = util_prop
  n = aggregate(ud_mat[,1], by=list(ud_mat[,2], ud_mat[,3], ud_mat[,4], ud_mat[,5]), sum)[,n_d+1]
  u = aggregate(ud_mat[,5], by=list(ud_mat[,2], ud_mat[,3], ud_mat[,4], ud_mat[,5]), sum)
  
  m = matrix(0, ncol=n_d+2, nrow=length(n)*2)
  m[1:length(n),1] = n
  m[1:length(n),2:(1+n_d)] = as.matrix(u[,1:n_d])
  m[1:length(n),n_d+2] =u[,n_d+1]

  attr(m, "values") = length(n)
  m
}

get_det = function(d, u, n_d) {
  cov_mat = matrix(0, ncol=n_d, nrow=n_d)
  u = u/sum(u)
  for(i in 1:n_d) {
    for(j in 1:n_d){
      cov_mat[i,j] = sum(d[,i]*d[,j]*u) - sum(d[,i]*u)*sum(d[,j]*u)
    }
    cov_mat[j,i] = cov_mat[i,j]
  }
  det(cov_mat)
}

next_J = function(ud_mat, cur_J, n_d) { 
  d =  ud_mat[,2:(1+n_d)]/100
  u =  ud_mat[,2+n_d]

  base = get_det(d, u, n_d)
  vs = sapply(cur_J:(cur_J+30), function(i) get_det(d, u^i, n_d))
  j_prop = which(vs/vs[1] < 0.5/4)[1] + (cur_J-1)
  j_prop = j_prop[1]
  if(is.na(j_prop)) 
    J = cur_J + 30
  else
    J = j_prop
  J
}


N= c(10, 10); n_d = 4;J = 1; p=0.2
g = function(N = c(10, 10), p=0.2, seed=NULL, n_d = 4, J=1){
  if(!is.null(seed)) set.seed(seed)
  
  ds = seq(0.0, 10, 0.01)
  ud_mat = pre_screen(N[1], ds, n_d)
  attr(ud_mat, "J") = J
  l = vector("list", length(N))
  l[[1]] = ud_mat[rowSums(ud_mat) > 0, ]
  N = N[-1]; 
  (J = next_J(l[[1]], cur_J=J, n_d))
  message("J = ", J)
  tmp = ud_mat
  i = 1;k = 1
  for(i in seq_along(N)) {
  
    k = 1
    n_row = attr(ud_mat, "values") 
    for(k in 1:N[i]) {
      row = sample(1:n_row, 1, prob = ud_mat[1:n_row,2+n_d]^J)
      d_prop = d = ud_mat[row, 2:(1+n_d)]
      if(runif(1) < p) {
        d_prop = sort(d_prop + sample(-10:10, n_d))
        if(any(d_prop > length(ds)) || any(d_prop < 1)) d_prop = d
        where = which(apply(ud_mat[,2:(1+n_d)], 1, function(i) all(i == d_prop)))
        
        if(length(where)> 0) {
          row = where
        } else {
          n_row = n_row + 1
          row = n_row
          ud_mat[row,2:(1+n_d)] = d_prop
        }
      }
      n = ud_mat[row, 1]; u_pre = ud_mat[row, n_d+2]
      theta_prop = rlnorm(1, -0.005, sqrt(0.01))
      y_prop = simulate(theta_prop, 1, c(0, ds[d_prop]))
      util_prop = get_utility(y_prop[,1], c(0, ds[d_prop]))
      if(util_prop < 0){
        util_prop = 100
      }
      ud_mat[row,] = c(n+1, d_prop, (u_pre*n + util_prop)/(n+1))
    }
    attr(ud_mat, "values") = n_row; attr(ud_mat, "J") = J
    l[[i+1]] = ud_mat[rowSums(ud_mat) > 0, ]
    attr(l[[i+1]], "values") = n_row; attr(l[[i+1]], "J") = J
    (J = next_J(l[[i+1]], cur_J=J, n_d))
    message("J= ", J)
  }
  l
}
tmp = g(N = c(20000, 20000,20000,20000,20000), n_d = 4)

cl = makeCluster(6)
clusterExport(cl, c("g", "simulate", "get_utility", "K", "pre_screen", "next_J", "get_det"))
res1 = parLapply(cl, 1:10, function(i) g(N = c(20000, 20000,20000,20000,20000), n_d = 4))
stopCluster(cl)
saveRDS(res1, file="output/death_3d/gill.RData")

sort(table(d1))

m = tmp[[5]]
head(m)
m[which.max(m[,6]),]

den = density(m[,2])
den$x[which.max(den$y)]

plot(density(m[,2]))


n = aggregate(m[,1], by=list(m[,2]), sum)
u = aggregate(m[,6]^100*m[,1], by=list(m[,2]), sum)
dd = cbind(n,u) 
dd$u = dd[,4]/dd[,2]
which.max(dd$u)
dd[666,]


head(dd)
plot(dd[,1], dd$u^(1/100))

samply(tmp[])

n = tmp[[5]][,1]
d1 =tmp[[5]][,2]
u = tmp[[5]][,6]

plot(d1, u^16)


table(d1)
d = as.data.frame(tmp[[4]])
colnames(d) = c("n", "d1", "d2", "d3", "d4", "u")

library(caret)
mSmooth = train(u^50 ~ d1+d2+d3+d4, data=d,method = "gamSpline")

library(nloptr)

f = function(x) {
  if(any(x < 0) || any(x > 1000)) return(Inf)
  x = sort(x)
  -predict(mSmooth, data.frame(d1=x[1], d2=x[2], d3=x[3], d4=x[4]))
}
f
f(x = c(75, 200, 275, 4350))
optim(c(75, 200, 275, 430), f)

plot(d$d2, d$u)

