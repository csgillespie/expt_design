library(doParallel)
(no_of_cores = parallel::detectCores())
#cl = makeCluster(6)
registerDoParallel(no_of_cores)

compiler::enableJIT(3)

source("R//pk/pk.R")
source("R/pk/pk_prior.R")
source("R/pk/pk_utility.R")
source("R/pk/pk_aws.R")

MAX_X = 100; MAX_Y = 800

get_utility_det = function(j, m) {
  pars = get_parameters(m)
  x = pars[,1]; y = pars[,2]
  m[,4] = m[,4]/max(m[,4])
  p = m[,4]^j/sum(m[,4]^j)
  (v1 = sum(x^2*p) - sum(x*p)^2)
  (v2 = sum(y^2*p) - sum(y*p)^2)
  cov = sum(x*y*p) - sum(x*p)* sum(y*p)
  1/(v1*v2- cov^2)
}

update_m = function(m, s, res) {
  ## Work out average if we choose multiple points
  u = tapply(res, s, mean)
  u2 = tapply(res^2, s, mean)
  n_u = as.vector(tapply(res, s, length))
  s = as.numeric(names(u))
  
  m[s,4] = (m[s,4] * m[s,3] + u*n_u)/(m[s,3] + n_u)
  m[s,5] = (m[s,5] * m[s,3] + u2*n_u)/(m[s,3] + n_u)
  m[s,3] = m[s, 3] + n_u
  m[s,6] = 1
  m[m[,6]==0,4] = sum((m[,3] > 0.5)*m[,4])/sum(m[,3])
  return(m)
}


# m: columns 1 & 2: shape1, shape2 of beta
# 3, 4: n, E(u(d))
# 5, 6: E(u(d)^2), has_visited
optimal = function(n = 50000, j = 2^(0:6)) {

  m_g = expand.grid(0:MAX_X, 0:MAX_Y)
  m = matrix(0, ncol=6, nrow=nrow(m_g))
  m[,1] = m_g[,1];m[,2] = m_g[,2];
  m = m[m[,1] > 0.00 & m[,2] > 0.0, ]
  m[,4] = 1 # Store E[X]. Not zero so it samples in i=1
  threads = 10*no_of_cores                                   ####XXXXX
  J = 1; lambda=4
  (nr = nrow(m))
  n = ceiling((n/length(j))/threads)
  
  m_global = m
  for(J in j) {
    #last_j_change = 0
    for(i in seq_len(n)) {
      if(sum(m_global[,4]) < 0.0000000001) {
        prob = m_global[,4]^0
      } else {
        prob = (m_global[,4]/max(m_global[,4]))^J
      }
      s = sample(1:nr, threads, prob = prob, replace=TRUE)
      ## Move in x & y direction      
      s_x =  rpois(threads, lambda) - rpois(threads, lambda)
      s_y =  rpois(threads, lambda) - rpois(threads, lambda)
      s_trans = s + s_x  + MAX_Y*s_y
      s_trans[s_trans < 1 | s_trans > MAX_X*MAX_Y] = s[s_trans < 1 | s_trans > MAX_X*MAX_Y]

      s = sort(s_trans)
      res = foreach(k = s, .combine = c) %dopar%  get_us(k, m)
      if(any(res > 3)) stop("Big res")
      m = update_m(m, s, res)
      m_global = update_m(m_global, s, res)

      message(i)
    }
    
    upload_item(m)
    m_global = combine_items()
    
    message("#### ", J)

  }
  upload_item(m)
  attr(m, "J") = J
  return(m)
}

m = optimal(n = 1)
# 
# for(i in 2:1000) {
#   set.seed(i)
#   m = optimal()

