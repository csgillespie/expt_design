library("aphids")
dir.create("output5")
system.time(d <- mcmc(2000, 1:8, no_of_cores = 6, dir="output5"))

