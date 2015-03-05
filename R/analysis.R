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
      hist(l[[i]][,k+1], xlab=paste("T:", i), main=NULL,  
           breaks="fd")
    }
    
  }
}
l = load_files(dir = "/tmp/4pt/")
get_plot(l)




l = load_files(dir = "/data/ncsg3/github/experimental_design/C/")
get_plot(l)

l = load_files(dir = "/data/ncsg3/github/experimental_design/C_no_weight//")
get_plot(l)





tab = table(l[[2]]$V2)
plot(tab/sum(tab))

p = tapply(l[[2]]$V3/sum(l[[2]]$V3), l[[2]]$V2, sum)
p = p/sum(p)

setnicepar(mfrow=c(1, 2))
plot(tab/sum(tab))
plot(p*tab/(sum(p*tab)))

setnicepar()
plot(density(tab/sum(tab)), ylim=c(0, 30))
lines(density(p*tab/(sum(p*tab))), col=2)

