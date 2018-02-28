#' @importFrom beepr beep
#' @importFrom doParallel registerDoParallel
#' @export
mcmc = function(N, J, no_of_cores, dir=".") {
  no_d = 2
  doParallel::registerDoParallel(no_of_cores)
  no_of_threads = 4*no_of_cores
  message("Total evaluations: ", no_of_threads * N * (length(J)+1))
  
  initial = initialise_design(no_of_threads*N, no_d)
  hash_tab = initial[[2]]
  designs = initial[[1]]
  tmp_mat = matrix(0, ncol = ncol(designs), nrow=N*no_of_threads)
  colnames(tmp_mat) = colnames(designs)

  for(j in J) {
    nr = sum(designs$n > 0)
    designs = designs[1:nr, ]
    designs = rbind(designs, tmp_mat)
    for(i in 1:N) {
      message(i)
      
      utils = designs$util[1:nr]
      q = quantile(utils, prob = 1-1/2^j)
      #prob = (designs$util/sum(designs$util))^J
      prob = utils >= q
      s = sample(1:nr, no_of_threads, prob = prob*utils , replace=TRUE)
      #t_pts = sample(2:6, no_of_threads)  
      (new_designs = t(apply(designs[s, 1:no_d], 1, perturb_times)))
      seeds = runif(1000)*2^31
      res = foreach(k = seq_len(no_of_threads), .combine = c) %dopar% {
        get_utility(k, seeds[k], new_designs[k,])    
      }
      
      k = 1
      for(k in seq_len(no_of_threads)) {
        key = paste(new_designs[k,1:no_d], collapse = " ")
        row_number = hash_tab[[key]]
        if(is.null(row_number)) {
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
    saveRDS(designs[1:nr,],  file.path(dir, paste0("aphids", j, ".Rds")))
    
  }
  beepr::beep()
  designs[1:nr,]
}

