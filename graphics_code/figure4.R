source("sin/utility.R")
pars = get_pars()
set.seed(1)
t = seq(0, 1, 0.001)

dd = NULL
for(i in 1:100) {
  tau = tau_prior(1, pars["g"], h = pars["h"])
  beta = theta_prior(1, b = pars["b"], c = pars["c"], h = pars["h"], g = pars["g"])
  s = simulate(t, beta, tau)
  dd_tmp = data.frame(t = t, s = s, id = i)
  dd = rbind(dd, dd_tmp)
}

fname = "graphics/figure4.pdf"
pdf(fname, width = 6, height = 4)
par(mar=c(3,3,2,1), mgp=c(2,0.4,0), tck=-.01,
    cex.axis=0.9, las=1, xaxs='i',yaxs='i', mfrow = c(1, 1))
plot(0, 0, ylim=c(-60, 60), xlim=c(0, 1), 
     xlab="d", ylab=expression(y[t]), 
     panel.first = {abline(h=seq(-60, 60, 20), lty=3, col="grey80")}, 
     axes=FALSE, frame=FALSE, pch=21, col=NA, cex = 0.7)
for(i in seq_along(unique(dd$id))) {
  lines(dd[dd$id == i,]$t, dd[dd$id ==i,]$s, 
        col = rgb(130, 130, 130, alpha=60, maxColorValue=255))
}
lines(t, simulate(t, theta = 10, Inf), col= "steelblue", lwd = 2)
axis(2, seq(-60, 60, 20), tick=FALSE,  col.axis="grey10", cex.axis = 0.8)
axis(1, seq(0, 1, 0.25), tick=F,  col.axis="grey10", cex.axis = 0.8)
title("100 stochastic realisations", adj=0.5, 
      cex.main=0.9, font.main=2, col.main="black")


dev.off()
system(paste("pdfcrop", fname))