get_y = function(t, D, T_inf, ke, V, sigma2){
  eps_t = rnorm(1, 0, sqrt(sigma2))
  y = D/T_inf*1/(ke*V)*(1 + eps_t)
  if(t<=T_inf) { 
    y = y*(1- exp(-ke*t))
  } else {
    y = y*(1- exp(-ke*T_inf))*exp(-ke*(t-T_inf))
  }
  y
}

get_pars = function(log = FALSE) {
  pars = MASS::mvrnorm(1, mu = c(-3.26, 8.99), 
                       Sigma = matrix(c(0.0071, -0.0057, -0.0057, 0.0080), ncol=2))
  if(!log) pars = exp(pars)
  pars  
}

get_data = function(ke, V, sigma2, x=0:400, D=500, T_inf=30){
  y = sapply(x, get_y, D, T_inf, ke, V, sigma2)  
  as.vector(y)
}

N = 100
l = list()
for(i in 1:N) {
  pars  = get_pars(FALSE)
  pars[3] = rnorm(1, 0.01, sqrt(10^(-5)))
  l[[i]] = get_data(pars[1], pars[2], pars[3])
}

col = rgb(85,130,169, alpha=150, maxColorValue=255)
mypalette(1)
s = 1/11
ryan_design = c(0, qbeta(c(s,2*s, 3*s, 4*s, 5*s, 6*s, 7*s, 8*s, 9*s, 10*s),0.73,3.1))* 720
(csg_design = c(0, qbeta(c(s,2*s, 3*s, 4*s, 5*s, 6*s, 7*s, 8*s, 9*s, 10*s),0.25,9))* 720)

fname = "graphics/figure7.pdf"
pdf(fname, width=8, height=5)
par(mar=c(3,3,2,1), mgp=c(2,0.4,0), tck=-.01,
    cex.axis=0.9, las=1, xaxs='i',yaxs='i')

plot(0:400, l[[1]]*1000, col=col, type="n", ylim=c(-10, 60), xlim=c(-5, 400), 
     xlab="Time (mins)", ylab = "Concentration", axes=FALSE, frame=FALSE, 
     panel.first = abline(h=seq(0, 60, 20), lty=3, col="grey80"))
for(i in seq_along(l)) {
  lines(0:400, l[[i]]*1000, col=rgb(200, 200, 200, 50, maxColorValue = 255))
}
axis(2, seq(0, 60, 20), tick=FALSE,  col.axis="grey50", cex.axis = 0.8)
axis(1, seq(0, 400, 100), tick=F,  col.axis="grey50", cex.axis = 0.8)
# title("PK (100 simulations)", adj=1, 
#       cex.main=0.9, font.main=2, col.main="black")

pars = c(exp(-3.26), exp(8.99), 0.01)
lines(seq(0, 400, length.out=1000), 
      get_data(pars[1], pars[2], 0, x= seq(0, 400, length.out=1000))*1000, 
      col="steelblue", lwd=2)
text(200,2, "Mean curve", col="steelblue")
points(ryan_design, rep(-3, 11), pch=21, bg=1)
points(csg_design, rep(-6, 11), pch=21, bg=2)

text(340, -3, "Ryan design", pos = 4, cex=0.9, col=1)
text(340, -6, "New design", pos = 4, cex=0.9, col=2)
dev.off()
system(paste("pdfcrop", fname))