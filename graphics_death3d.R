tmp = readRDS("death3d.RData")

dd1 = tmp[[1]][tmp[[1]][,5] > 50,]
dd2 = tmp[[2]][tmp[[2]][,5] > 50,]
dd3 = tmp[[3]][tmp[[3]][,5] > 50,]
dd4 = tmp[[4]][tmp[[4]][,5] > 50,]
dd5 = tmp[[5]][tmp[[5]][,5] > 50,]

alpha = 175
palette(c(rgb(200,79,178, alpha=alpha,maxColorValue=255), 
          rgb(105,147,45, alpha=alpha, maxColorValue=255),
          rgb(85,130,169, alpha=alpha, maxColorValue=255),
          rgb(204,74,83, alpha=alpha, maxColorValue=255),
          rgb(183,110,39, alpha=alpha, maxColorValue=255),
          rgb(131,108,192, alpha=alpha, maxColorValue=255),
          rgb(63,142,96, alpha=alpha, maxColorValue=255)))
par(mar=c(3,3,2,1), 
    mgp=c(2,0.4,0), tck=-.01,
    cex.axis=0.9, las=1,mfrow=c(2, 3))
plot(dd1[,1], dd1[,5], panel.first=grid(), xlab="n", 
     ylab=expression(paste(hat(u), "(d)")), pch=21, bg=3, cex=0.5, main = "J=1")
plot(dd2[,1], dd2[,5], ylim=c(50, 185), cex=0.5, panel.first=grid(), xlab="n", 
     ylab=expression(paste(hat(u), "(d)")), pch=21, bg=2, xlim=c(0, 20), main = "J=16")
plot(dd3[,1], dd3[,5], ylim=c(50, 185), cex=0.5, panel.first=grid(), xlab="n", 
     ylab=expression(paste(hat(u), "(d)")), pch=21, bg=2, main = "J=42")
plot(dd4[,1], dd4[,5], ylim=c(50, 185), cex=0.5, xlim=c(0, 130), panel.first=grid(), xlab="n", 
     ylab=expression(paste(hat(u), "(d)")), pch=21, bg=2, main = "J=72")
plot(dd5[,1], dd5[,5], ylim=c(50, 185), xlim=c(0, 130),
     cex=0.5, panel.first=grid(), xlab="n", 
     ylab=expression(paste(hat(u), "(d)")), pch=21, bg=2, main = "J=98")
