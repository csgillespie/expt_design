#' @export
#' @import hash
#' @importFrom foreach foreach %dopar%
initialise_design = function(N, no_d) {
  #requireNamespace("doParallel")

  n = ceiling(N/1000)
  N = n*1000
  
  initial = matrix(t(replicate(N, sample(1:50, no_d -1 ))), ncol=no_d-1)
  initial = cbind(0, initial)
  for(i in 1:N) 
    initial[i,] = sort(initial[i,])
  
  utils = NULL; i = 1
  for(i in 1:n) {
      seeds = runif(1000)*2^32
      res = foreach(k = 1:1000, .combine = c) %dopar% get_utility(k, seeds[k], initial[1000*(i-1) + k,])
      # Yes, I know I'm growing, but who cares. 
      # If only someone had written a book on Efficient R programming
      utils = c(utils, res)
  }
  
  h = hash()
  for(i in 1:N) {
    h[[paste(initial[i,], collapse = " ")]] = i
  }
  
  designs = as.data.frame(initial)
  designs$util = utils
  designs$util2 = utils^2
  designs$n = 1
  list(designs, h)
}
