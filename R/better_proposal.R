f = function(x, mu=1) 50*exp(-mu*x)
x = seq(0, 10, 0.01)

n = round(f(x, 0.95), 0)
plot(x, n, type="l")
points(x[162], n[162], col=2)

normalised = diff(n)/n[-length(n)]
normalised = normalised[1:(which(is.nan(normalised))[1]-2)]
d = cumsum(normalised)/sum(normalised)
plot(d)

which(d > 0.5)[1]

which(n==25)[1]


plot(d)
plot(cumsum(diff(n))/sum(diff(n)), type="l")


plot()
