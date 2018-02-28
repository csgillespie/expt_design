
u = readRDS("output/death_1d/exact.RData")
d = seq(0.01, 10, length.out = length(u))


cum_u = cumsum(u)
add_line = function(u, d, p, col=1) {
  values= which(u > quantile(u, probs=p[1]))
  lines(d[values], u[values], col=col, lwd=2)  
}
  
plot(d, u, type="l")
add_line(u, d, 1-1/2^1, 2)
add_line(u, d, 1-1/2^2, 3)
add_line(u, d, 1-1/2^3, 4)
add_line(u, d, 1-1/2^4, 5)



u = u/sum(u)

plot(d, cumsum(u), type="l")
lines(d, cumsum(u^2)/sum(u^2), col=2)
lines(d, cumsum(u^4)/sum(u^4), col=3)
lines(d, cumsum(u^8)/sum(u^8), col=4)
lines(d, cumsum(u^16)/sum(u^16), col=5)


plot(d, u^64/sum(u^64))
