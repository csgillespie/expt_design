library(gtools)

load_files = function(dir=".") {
  f = list.files(path=dir, pattern="*.csv$", full.names = TRUE)
  
  f = mixedsort(f)
  l = list()
  for(i in seq_along(f)) {
    fname = paste0(f[i])
    l[[i]] = read.csv(fname, header=FALSE)
  }
  l
}
get_acceptance = function(d) round(sum(diff(d$V2) !=0)/length(d$V2)*100)

get_plot = function(l) {
  no_tps = ncol(l[[1]])-2
  no_parts = length(l)
  for(k in 1:no_tps){
    op = setnicepar(mfrow=c(ceiling(no_parts/4), 4), ask=TRUE)
    on.exit(par(op))
    for(i in 1:no_parts){
      hist(l[[i]][,k+1], xlab=paste0("t:", k), main=paste("J = ", 2^(i-1)),  
           breaks=length(unique(l[[i]][,k+1])))
    }
    
  }
}
l = load_files(dir = "/tmp/6pta/")
get_plot(l)
get_plot(l2)


l1 = load_files(dir = "/data/ncsg3/github/experimental_design/C/")
l2 = load_files(dir = "/data/ncsg3/github/experimental_design/C_no_weight//")
#get_plot(l)
l3 = load_files(dir = "/data/ncsg3/github/experimental_design/C_no_weight_no_power_j_3/")
#get_plot(l)
l4 = load_files(dir = "/data/ncsg3/github/experimental_design/C_no_weigth_no_power//")
#get_plot(l)

setnicepar(mfrow=c(2, 3))
r = c(0, 3)
hist(l1[[1]]$V2, breaks="fd", freq=F, ylim=r);
hist(l1[[2]]$V2, breaks=100, freq=F, ylim=r);
hist(l1[[3]]$V2, breaks=100, freq=F, ylim=c(0, 11));

hist(l2[[1]]$V2, breaks="fd", freq=F, ylim=r);
hist(l2[[2]]$V2, breaks="fd", freq=F, ylim=r);
hist(l2[[3]]$V2, breaks="fd", freq=F, ylim=c(0, 11));


hist(l2[[1]]$V2, breaks="fd", freq=F, ylim=r);
hist(l2[[2]]$V2, breaks="fd", freq=F, ylim=r);
hist(l2[[3]]$V2, breaks="fd", freq=F, ylim=c(0, 11));

hist(l3[[1]]$V2, breaks="fd", freq=F, ylim=r);
hist(l4[[1]]$V2, breaks="fd", freq=F, ylim=r);






########################
#Re weighting
########################
setnicepar(mfrow=c(1, 2))

p2 = tapply(exp(l[[1]]$V6)^2, l[[1]]$V2, mean) 
p2 = p2/sum(p2)
p3 = tapply(l[[1]]$V6, l[[1]]$V2, mean) 
p3 = p3/sum(p3)
p3
t1 = table(l[[2]]$V2)/sum(table(l[[2]]$V2))
plot(as.numeric(names(t1)), as.vector(t1), type="l")
lines(p2, type="l", col=2)
lines(p3, type="l", col=3)

t1 = table(l[[7]]$V2)/sum(table(l[[2]]$V2))
lines(as.numeric(names(t1)), as.vector(t1), col=4)


p2 = tapply(exp(l[[2]]$V3)^1.5, l[[2]]$V2, mean) 
p2 = p2/sum(p2)

p3 = tapply(exp(l[[1]]$V3)^3, l[[1]]$V2, mean) 
p3 = p3/sum(p3)

t1 = table(l[[3]]$V2)/sum(table(l[[3]]$V2))
plot(as.numeric(names(t1)), as.vector(t1), type="l")
lines(p2, type="l", col=2)
lines(p3, type="l", col=3)




