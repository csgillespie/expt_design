dd = read.table("R/mcmc.txt", header=T)

l = dd$l/7;m = dd$m/7


mean(l);mean(m)
sd(l); sd(m)
cor(l, m)



library(mvtnorm)
r = rmvnorm(100000, mean = c(mean(l), mean(m)), 
        sigma = matrix(c(var(l)*100, cov(l,m), cov(l,m), var(m)*100), ncol=2))

setnicepar()
plot(r[,2], r[,1], type="n")        
points(r[,2], r[,1], cex=0.1, bg=2, pch=21)
points(m, l, cex=0.1, bg=2, pch=21, col=2)

out
