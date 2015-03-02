get_acceptance = function(d) round(sum(diff(d$V2) !=0)/length(d$V2)*100)

f = list.files(path="C/", pattern="*.csv$")
i = 1
l = list()
for(i in 1:length(f)) {
  fname = paste0("C/parts_", i-1, ".csv")
  l[[i]] = read.csv(fname, header=FALSE)
}

table(l[[1]]$V2)

setnicepar(mfrow=c(3, length(f)))
for(i in 1:length(f)){
  hist(l[[i]]$V2, xlab=expression(t[1]), main=NULL, freq=FALSE, xlim=c(0, 50), 
       breaks="fd", ylim=c(0, 0.5))
}

for(i in 1:length(f)){
  hist(l[[i]]$V3, xlab=expression(t[2]), main=NULL, freq=FALSE, xlim=c(15, 50), breaks="fd", 
       ylim=c(0, 0.25))
}

#setnicepar(mfrow=c(2, 3))
for(i in 1:length(f)){
  hist(l[[i]]$V4/(i+2), xlab="Utility", main=NULL, freq=FALSE, breaks="fd", 
       xlim=c(35.5, 38), ylim=c(0, 3) )
}





unlist(lapply(l, get_acceptance))
