gillespie = function(n=28, cumul=28, tincr=1, maxtime=10, lambda, mu)
{
  time = 0; tstep=0;
  i=1 
  N=vector(); Cumul=vector(); Time=vector()
  while(time < maxtime)
  {
    #Calculate the overall rate
    rate = lambda*n + mu*n*cumul
    if(rate == 0)#everything is dead.
      time = maxtime 
    else
      time = time + rexp(1, rate)
    
    #Store values at discrete intervals
    while(time > tstep && tstep < (maxtime+tincr/2))
    {
      N[i] = n; Cumul[i] = cumul; Time[i] = tstep
      tstep = tstep + tincr
      i = i+1
    }
    
    #Don't go by maxtime
    if(time >= maxtime)
      break;
    
    #Choose which reaction happens
    u = runif(1)
    if(u<(lambda*n)/rate)#birth
    {
      n = n+1
      cumul = cumul+1
    }else if(u< (lambda*n + mu*n*cumul)/rate){
      n = n-1
    }
    
  }
  return(data.frame(Time, N,Cumul))
}
