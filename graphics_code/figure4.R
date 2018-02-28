source("graphics_code/aphids_simulation.R")
set.seed(1)
mu = c(0.246, 0.000134)
lambda_sd = 0.00079*10; mu_sd = 0.000002*10;
cov = 0.356 * lambda_sd * mu_sd
cov_mat = matrix(c(lambda_sd^2, cov, cov,  mu_sd^2), ncol=2)

par = MASS::mvrnorm(100, mu, cov_mat)
sims = apply(par, 1, function(i) gillespie(lambda=i[1], mu=i[2], maxtime=50))

get_data = function(fname, type, N = NULL) {
  o = readRDS(fname)
  o = o[o$n > 2,]# Bug re-running
  o$type = type
  o = o[order(-o$util),]  
  o$id = 1:nrow(o)
  o$se = sqrt((o$util2 - o$util^2)/(o$n-1))/sqrt(o$n)
  o$mean = o$util
  if(!is.null(N)) o = o[1:min(N,nrow(o)), ]
  o
}

N = 1
o2 = get_data(fname = "data/output2/aphids8.Rds", type="One design point", N)
o3 = get_data(fname = "data/output3/aphids8.Rds", type="Two design points", N)
o4 = get_data(fname = "data/output4/aphids8.Rds", type="Three design points", N)
o5 = get_data(fname = "data/output5/aphids8.Rds", type="Four design points", N)

o2 = unlist(o2[,c("V1", "V2")])
o3 = unlist(o3[, c("V1", "V2", "V3")])
o4 = unlist(o4[, c("V1", "V2", "V3", "V4")])
o5 = unlist(o5[, c("V1", "V2", "V3", "V4", "V5")])

mypalette(1)
fname = "graphics/figure4.pdf"
pdf(fname, width=6, height=4)
par(mar=c(3,3,2,1), 
    mgp=c(2,0.4,0), tck=-.01,
    cex.axis=0.9, las=1)

plot(0, 0, frame.plot = FALSE, axes=FALSE, ylim=c(-150, 1500), xlim=c(0, 50), type="n", 
     xlab="Time (days)", ylab="Aphid population", 
     panel.first = abline(h=c(0, 500, 1000, 1500), lty=3, col="grey80"))

i = 4
for(i in 1:length(sims)) {
  s = sims[[i]]
  lines(s$Time, s$N, col=rgb(150, 150, 150, alpha=60,maxColorValue=255))
}

y_values = seq(0, -190, length.out=4)#c(-5, -55, -95, -135)


segments(0, y_values[1], o2[length(o2)], y_values[1], col="grey80", lwd=2)
segments(0, y_values[2], o3[length(o3)], y_values[2], col="grey80", lwd=2)
segments(0, y_values[3], o4[length(o4)], y_values[3], col="grey80", lwd=2)
segments(0, y_values[4], o5[length(o5)], y_values[4], col="grey80", lwd=2)

points(o2, rep(y_values[1], length(o2)), pch=21, bg="black")
points(o3, rep(y_values[2], length(o3)), pch=21, bg="black")
points(o4, rep(y_values[3], length(o4)), pch=21, bg="black")
points(o5, rep(y_values[4], length(o5)), pch=21, bg="black")


axis(1, tick = F)
axis(2, at = c(0, 500, 1000, 1500), tick=FALSE)
dev.off()
system(paste("pdfcrop", fname))

