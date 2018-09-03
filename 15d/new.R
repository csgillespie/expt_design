library(hash)
library(doParallel)
library(tidyverse)
no_of_cores = 4
doParallel::registerDoParallel(no_of_cores)

source("initialise.R")
m = 1:14
no_d = 15
N = 24000/(length(m) + 1) * no_d

fit = function(N, no_d, m) {
  percentiles = 1 - seq(0, (length(m)+1)*N, N) * 0.5^(0:(length(m) + 1))/seq(0,(length(m)+1)*N, N)
  percentiles[1] = 0
  
  #m = 0 in initialise_design
  initial = initialise_design(N, no_d)
  hash_tab = initial[[2]]
  designs = initial[[1]]
  tmp_mat = matrix(0, ncol = ncol(designs), nrow = N)
  colnames(tmp_mat) = colnames(designs)
  no_of_threads = 10 # Not really parallel since utility is so fast
  j = 1
  lambda = 1
  for (j in m) {
    nr = which.max(cumsum(designs$n > 0))
    designs = designs[1:nr, ]
    l = create_hash(designs, percentiles[j])
    hash_tab = l[[2]]
    designs = l[[1]]
    nr = nrow(designs)
    designs = rbind(designs, tmp_mat)
    i = 1
    if (j == m[length(m)]) lambda = 0 # Final run
    for (i in 1:(N/no_of_threads)) {
      message(i, " n = ", sum(designs$n), " nrow = ", nrow(designs))
      
      utils = designs$util[1:nr]
      q = quantile(utils, prob = percentiles[j + 1])
      prob = utils >= q
      s = sample(1:nr, no_of_threads, prob = prob*utils , replace = TRUE)
      (new_designs = t(apply(designs[s, 1:no_d], 1, perturb_times, lambda = lambda)))
      new_designs = t(apply(new_designs, 1, sort))
      
      res = sapply(seq_len(no_of_threads), function(i) get_utility(new_designs[i, ]))

      k = 1
      for (k in seq_len(no_of_threads)) {
        key = paste(new_designs[k,1:no_d], collapse = " ")
        row_number = hash_tab[[key]]
        if (is.null(row_number)) {
          nr = nr + 1
          designs[nr, ] = c(new_designs[k, 1:no_d], res[k], res[k]*res[k], 1)
          hash_tab[[key]] = nr
        } else {
          ## Update value
          n = designs[row_number, "n"] 
          util = designs[row_number, "util"]
          designs[row_number, "util"] = (util*n + res[k])/(n + 1)
          
          util2 = designs[row_number, "util2"]
          designs[row_number, "util2"] = (util2*n + res[k]^2)/(n + 1)
          designs[row_number, "n"] = n + 1
        }
      }
    }
  }
  designs = designs[1:nr,] %>%
    as_tibble() %>%
    arrange(-util) %>%
    head(1)
  designs
}

l = parallel::mclapply(1:100, function(i) suppressMessages(fit(N, no_d, m)))
beepr::beep()
l



#saveRDS(dd, "data/15d/new.rds")
