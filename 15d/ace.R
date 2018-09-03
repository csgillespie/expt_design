library(parallel)
library(acebayes)
compiler::enableJIT(3)
doParallel::registerDoParallel(5)
source("initialise.R")

no_d = 15
#N = 360000

counter = 0
ace_utility_wrapper = function(d, B) {
#  counter <<- counter + B
  sapply(1:B, function(i) get_utility(d))
}

N = 360000

r = mclapply(1:30, function(i) {
  start.d = matrix(sort(runif(no_d, max = no_d)), nrow = 1)
  ace(utility = ace_utility_wrapper, start.d = start.d, 
      B = c(200,10), N1 = 29, 
      N2 = 0, lower = 0, upper = 15)
#  counter
}
)
counter
beepr::beep()
saveRDS(r, "data/15d/ace.rds")

