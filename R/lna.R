library(deSolve)
source("R/data.R")

###################################################################
## Moment closure LV equations. 
## Additional parameter is_mc used to switch to LNA
###################################################################
aphid_model = function(t, x, parms) {
  with(as.list(c(x, parms)),{
    is_mc = 0    
    ##Means - notice the covariance term
    dk10 = lambda*k10 - mu*(k10*k01 + k11*is_mc)
    dk01 = lambda*k10
    
    ##Variances and covariance with the Normal approximation
    dk20 = lambda*(k10+2*k20) + mu*(k11 -2*k10*k11 +k01*(k10-2*k20))
    
    dk11 = lambda*(k10+k20 +k11) - mu*(k10*k02+k01*k11)
    
    dk02 = lambda*(k10+2*k11)
    
    res = c(dk10, dk01, dk20, dk11, dk02)
    list(res)
  })
}


## LNA simulation
#pars = c(lambda=4.75, mu=0.00095)

pars = c(lambda=0.246, mu=0.000134)
#pars = c(lambda=0.35, mu=0.000028)
yini = c(k10=28, k01=28, k20=0, k11=0, k02=0)
times = c(0, 9,17,24,32,42)
times = seq(0, 60, 1)

## At extreme points the MC approximation breaks down
## Catch error and do something sensible
out = ode(yini, times, aphid_model, pars)
plot(out[,1], out[,2], type="l")
points(c(17, 30),c(0,0))

out[,1:3]

points(sim$Time*7, sim$N)


146.000000 7.000000
663.000000 15.000000
535.000000 26.000000
131.000000 33.000000
19.000000 41.000000
> DING! clyde: ...github/experimental_design/C >                                                                                                                                                                    $ make run
./main
ODE: 0.246000,0.000134
ODE: 28.000000,0.000000
146.000000 7.000000
819.000000 17.000000
707.000000 24.000000
131.000000 33.000000
12.000000 43.000000
