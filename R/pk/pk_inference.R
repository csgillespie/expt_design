compiler::enableJIT(3)
options("mc.cores" = 6)
source("R//pk/pk.R")
source("R/pk/pk_prior.R")
source("R/pk/pk_utility.R")
MAX_X = 100; MAX_Y = 800

get_utility_det = function(j, m) {
  pars = get_parameters(m)
  x = pars[,1]; y = pars[,2]
  p = m[,4]^j/sum(m[,4]^j)
  (v1 = sum(x^2*p) - sum(x*p)^2)
  (v2 = sum(y^2*p) - sum(y*p)^2)
  cov = sum(x*y*p) - sum(x*p)* sum(y*p)
  1/(v1*v2- cov^2)
}

# m: columns 1 & 2: shape1, shape2 of beta
# 3, 4: n, E(u(d))
# 5, 6: E(u(d)^2), has_visited
optimal = function(n = 167, max_j = 5) {
  m_g = expand.grid(0:MAX_X, 0:MAX_Y)
  m = matrix(0, ncol=6, nrow=nrow(m_g))
  m[,1] = m_g[,1];m[,2] = m_g[,2];
  m = m[m[,1] > 0.00 & m[,2] > 0.0, ]
  m[,4] = 1 # Store E[X]. Not zero so it samples in i=1
  threads = 10*getOption("mc.cores")
  J = 1; lambda=4
  (nr = nrow(m))
  for(j in 1:max_j) {
    for(i in 1:n) {
      if(sum(m[,4]) < 0.0000000001) {
        prob =1 
      } else {
        prob = m[,4]^J
      }
      s = sample(1:nr, threads, prob = prob, replace=TRUE)
      ## Move in x & y direction      
      s_x =  rpois(threads, lambda) - rpois(threads, lambda)
      s_y =  rpois(threads, lambda) - rpois(threads, lambda)
      s_trans = s + s_x  + MAX_Y*s_y
      s_trans[s_trans < 1 | s_trans > MAX_X*MAX_Y] = s[s_trans < 1 | s_trans > MAX_X*MAX_Y]
      s = sort(s_trans) ## Update key

      (res = unlist(parallel::mclapply(s, get_us, m)))
      if(any(res > 3)) stop()
      
      ## Work out average if we choose multiple points
      u = tapply(res, s, mean)
      u2 = tapply(res^2, s, mean)
      n_u = as.vector(tapply(res, s, length))
      s = as.numeric(names(u))
      
      m[s,4] = (m[s,4] * m[s,3] + u*n_u)/(m[s,3] + n_u)
      m[s,5] = (m[s,5] * m[s,3] + u2*n_u)/(m[s,3] + n_u)
      m[s,3] = m[s, 3] + n_u
      m[s,6] = 1
      m[m[,6]==0,4] = sum(m[,3]*m[,4])/sum(m[,3])
      message(i)
    }
    message("#### ", J)
    base = get_utility_det(J, m)
    JS = (J+1):(J+11)
    ratios = base/sapply(JS, function(i) get_utility_det(i,m= m))
    J = JS[min(which(ratios < 0.5^2), 11)]
    message("#### ", J)
    #saveRDS(m, file=paste0("/tmp/tmp", j, ".RData"))
  }
  attr(m, "J") = J
  return(m)
}



for(i in 1:1000) {
  set.seed(i)
  m = optimal(n=167, 5)
  dir.create("output/pk", showWarnings = FALSE)
  fname = file.path("output","pk", paste0("m_",i, ".RData" ))
  saveRDS(m, file=fname)
}











