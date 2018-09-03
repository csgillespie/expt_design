source("utility.R")
library(tidyverse)
library(doParallel)
doParallel::registerDoParallel(5)


compiler::enableJIT(3)
MAX_X = MAX_Y = 1000
utility_wrapper = function(s, m, pars) {
  g = pars["g"]; h = pars["h"]
  c = pars["c"]; b = pars["b"]
  tau = tau_prior(1, g, h)
  beta = theta_prior(1, b, c, h, g)
  t = as.vector(m[s,1:2])/MAX_X
  sim = simulate(t, beta, tau)
  get_utility(c, b, h, g, t, sim)
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
  # m[m[,6]==0,4] = sum((m[,3] > 0.5)*m[,4])/sum(m[,3])
  return(m)
}

# m: columns 1 & 2: shape1, shape2 of beta
# 3, 4: n, E(u(d))
# 5, 6: E(u(d)^2), has_visited
optimal = function(n = 24000, j = 0:9) {
  pars = get_pars()
  m_g = expand.grid(0:MAX_X, 0:MAX_Y)
  m = matrix(0, ncol = 6, nrow = nrow(m_g))
  m[,1] = m_g[,1];m[,2] = m_g[,2];
  m = m[m[,1] > 0.00 & m[,2] > 0.0, ]
  #m[,4] = 1 # Store E[X]. Not zero so it samples in i=1.
  threads = min(60, n)
  J = 0; lambda = 4
  (nr = nrow(m))
  (n = ceiling((n/length(j))/threads))
  message("n = ", n)
  
  prob = rep(1, nr)
  i = 1
  for (J in j) {
    for (i in seq_len(n)) {
      if (J == j[length(j)]) lambda = 0
      if (sum(m[,3]) > n) { 
        utils = m[,4] - min(m[,4])
        utils[m[,6] == 0] = 0
        q = quantile(utils[m[,3] > 0], prob = 1 - 1/2^J, na.rm = TRUE)
        prob = utils >= q
        prob = prob * utils
      } 
      s = sample(1:nr, threads, prob = prob , replace = TRUE)
      
      ## Move in x & y direction      
      s_x = rpois(threads, lambda) - rpois(threads, lambda)
      s_y = rpois(threads, lambda) - rpois(threads, lambda)
      s_trans = s + s_x  + MAX_Y*s_y
      s_trans[s_trans < 1 | s_trans > MAX_X*MAX_Y] = s[s_trans < 1 | s_trans > MAX_X*MAX_Y]
      
      s = sort(s_trans)
      # d1 < d2
      k = s[3]
      for (k in seq_along(s)) {
        if (m[s[k],1] > m[s[k],2]) s[k] = m[s[k],2] + (m[s[k],1] - 1)*MAX_Y
      }
      s
      res = sapply(s, function(i) utility_wrapper(i, m, pars))
      #res = foreach(k = s, .combine = c) %dopar%  {utility_wrapper(k, m, pars)}
      m = update_m(m, s, res)
      message(J, ":", i)
    }
  }
  attr(m, "J") = J
  
  m = m %>% 
    as_tibble() %>%
    mutate(V1 = V1/MAX_X, V2 = V2/MAX_Y) %>%
    filter(V6 == 1) %>%
    arrange(desc(V4)) %>%
    head(1)
  return(m)
}



n = 24000
#system.time(o <- suppressMessages(optimal(n = n, j = 0:5)))
o


n = 24000
res = parallel::mclapply(1:250, function(i) {
  suppressMessages(optimal(n = n, j = 0:7))
  }
)
beepr::beep()


#results = res %>% bind_rows()


saveRDS(results, file= "data/sin/new.rds")
gr = readRDS("data/sin/exact.rds")
ggplot(gr, aes(Var1, Var2)) + 
  geom_raster(aes(fill = util)) + 
  geom_point(data = results, aes(V1, V2), alpha = 0.1)


# 
