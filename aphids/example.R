library(Rcpp)
library(RcppGSL)
sourceCpp("aphids/example.cpp")
ode()

sourceCpp("aphids/aphids_init.cpp")
