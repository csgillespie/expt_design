# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Script to implement INSH algorithm for the Deaht model example,           #
#                                                                           #
# Author: David J. Price                                                    #
# Date: 21st March, 2018                                                    #
# Updated: Colin Gillespie                                                                           #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(acebayes)
library(truncnorm)
library(foreach)
library(doParallel)
source("death_model/death_1d/death_utility.R")

ds = seq(0.01, 10, 0.01)
simulate = function(theta=1, ds) rbinom(1, 50, exp(-theta*ds))
# Function to propose discrete designs according to a truncated MVN distribution centred at a previous design

discrete_trunc_sample <- function(current_d, minmaxT, sd_T, stepsize, m, mindiff, ntimes = 1){
  new_design <- matrix(-1, nrow = m, ncol=length(current_d))
  for (i in 1:m){
    
    if (length(current_d)==1){
      new_design[i,] = matrix(rtruncnorm(n = 1, a = minmaxT[1], b = minmaxT[2], mean = as.numeric(current_d), sd = sd_T), nrow = 1, ncol=ntimes)
    } else{
      # Check order of proposed observation times
      while( any(diff(new_design[i,])<mindiff) ){
        # # To propose designs on a grid
        # new_design[i,] = round(matrix(rtruncnorm(n = 1, a = minmaxT[1], b = minmaxT[2], mean = as.numeric(current_d), sd = sd_T), nrow = 1, ncol=ntimes)/stepsize)*stepsize
        # # To propose continuous designs
        new_design[i,] = matrix(rtruncnorm(n = 1, a = minmaxT[1], b = minmaxT[2], mean = as.numeric(current_d), sd = sd_T), nrow = 1, ncol=ntimes)
      }
    }
  }
  return(new_design)
}

insh_estimate = function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  
  # Range for observation times
  minmaxT <- c(0, 10)
  
  # Specify number of observation times
  ntimes <- 1
  
  # INSH relevant inputs for each number of observations (as in "An Induced Natural Selection Heuristic for...", Price et al. (2018))
  # W=8; # Number of waves to run INSH
  # m=c(rep(3,W/2), rep(5,W/2)); # Number of designs to sample around each retained design, at each wave
  # ntokeep = c(rep(10,W/2), rep(6,W/2)); # Number of designs to keep at each wave
  # n_init=20; # Number of designs to randomly allocate in first wave
  
  W=5;
  m = rep(3,W);  
  ntokeep = rep(8,W);
  n_init=24;
  
  # Standard deviation of perturbation kernel
  pert_sd <- 0.1
  sample_sd <- rep(pert_sd, times = ntimes)
  
  # Grid over which to search for obs times if sampling across grid
  gridsize <- 0.05

  # Minimum time allowed between subsequent samples
  min_difference <- 0
  
  # Sample initial designs uniformly/randomly from grid over 0:gridsize:24
  current_wave_designs <- matrix(NA, nrow = n_init, ncol = ntimes);
  
  for (i in 1:n_init){
    # Make sure subsequent times are atleast 15 minutes apart
    current_wave_designs[i,] <- sort(runif(ntimes, min = minmaxT[1], max = minmaxT[2]))
  }

  keep_all <- NULL
  cl <- makeCluster(1)
  on.exit(stopCluster(cl))
  registerDoParallel(cl)
  w = 1
  tot = 0
  util <- vector("numeric", length = nrow(current_wave_designs))
  for (w in 1:W){
    # Evaluate utility for each of the designs in the current wave using utilcomp15sig from acebayes package
    d = 1
    util <- foreach(d=1:nrow(current_wave_designs), .packages = "acebayes", .combine = c,
                    .export = c("simulate", "get_utility")) %dopar% {
      obs_t = signif(current_wave_designs[d], 3)
      obs_t = max(obs_t, 0.01)
      obs_t = min(obs_t, 10)
      
      theta_prop = rlnorm(150, -0.005, sqrt(0.01))
      x = sapply(theta_prop, simulate,  obs_t)
      mean(sapply(x, get_utility, obs_t * 100))
    }
    tot = tot + length(util)
    
    # THIS WAS A SAFETY CHECK FOR THE ACEBAYES utilcomp15sig FUNCTION -- PERHAPS NOT NECESSARY
    # If utility returns Inf for any design, remove that design
    if(any(is.na(util) | is.infinite(util))){
      omit <- which(is.na(util) | is.infinite(util))
      current_wave_designs <- current_wave_designs[-omit,]
      util <- util[-omit]
    }
    # Establish cutoff for which designs to retain
    tmp_util <- sort(util, decreasing = TRUE)
    cutoff <- tmp_util[ntokeep[w]]
    
    # Retain designs
    update_designs <- current_wave_designs[util >= cutoff,, drop = FALSE]
    
    # Store ALL designs considered, the corresponding utility, and the wave
    keep_all <- rbind(keep_all, cbind(current_wave_designs,util, w))
    
    # IF the optimal design occured in a previous iteration, add back into the designs for 
    # # consideration so new designs are sampled around this region
    if (keep_all[which.max(keep_all[,ntimes+1]),ntimes+2]!=w){
      update_designs <- rbind(update_designs, keep_all[which.max(keep_all[,ntimes+1]), 1:ntimes])
    }
    
    # Sample new designs
    current_wave_designs <- foreach(r=1:nrow(update_designs), 
                                    .combine = "rbind", 
                                    .packages = "truncnorm", 
                                    .export = "discrete_trunc_sample") %dopar% {
      discrete_trunc_sample(update_designs[r,], minmaxT, sample_sd, gridsize, m[w], min_difference)
    }
  }
  message(tot)
  opt_loc <- which.max(keep_all[,ntimes+1])
  keep_all[opt_loc,1:ntimes]
}
insh_estimate()

x = vapply(1:500, function(i) insh_estimate(), FUN.VALUE = double(1) )
beepr::beep(3)
boxplot(x)

saveRDS(x, file= "data/death_insh.rds")
