res = readRDS("data/death1d/death_gil_single.rds")
unlist(lapply(res, function(i) max(i[,1])))
fname = "graphics/figure1.pdf"
pdf(fname, width=6, height=4)

par(mar=c(3,3,2,1), mgp=c(2,0.4,0), tck=-.01,
    cex.axis=0.9, las=1, xaxs='i',yaxs='i', mfrow=c(2, 3))

bg = rgb(85,130,169, alpha=160, maxColorValue=255)

plot(res[[1]][,2]/100, res[[1]][,1], ylim=c(0, 60), xlim=c(0, 10), 
     xlab="d", ylab="No. of realisations", 
     panel.first = {abline(h=seq(0, 60, 20), lty=3, col="grey80");abline(v = 1.61, col="grey80", lwd=2)}, 
     axes=FALSE, frame=FALSE, pch=21, col=NA, bg=bg, cex = 0.7)
axis(2, seq(0, 60, 20), tick=FALSE,  col.axis="grey10", cex.axis = 0.8)
axis(1, seq(0, 10, 2), tick=F,  col.axis="grey10", cex.axis = 0.8)
title("Initialisation", adj=0.5, 
      cex.main=0.9, font.main=2, col.main="black")
text(1.61, 57, "Optimal design", col="grey10", pos = 4, cex=0.8)


plot(res[[2]][,2]/100, res[[2]][,1], ylim=c(0, 60), xlim=c(0, 10), 
     xlab="d", ylab="", 
     panel.first = {abline(h=seq(0, 60, 20), lty=3, col="grey80");abline(v = 1.61, col="grey80", lwd=2)}, 
     axes=FALSE, frame=FALSE, pch=21, col=NA, bg=bg, cex = 0.7)
axis(2, seq(0, 60, 20), tick=FALSE,  col.axis="grey10", cex.axis = 0.8)
axis(1, seq(0, 10, 2), tick=F,  col.axis="grey10", cex.axis = 0.8)
title("m = 1", adj=0.5, 
      cex.main=0.9, font.main=2, col.main="black")

plot(res[[3]][,2]/100, res[[3]][,1], ylim=c(0, 60), xlim=c(0, 10), 
     xlab="d", ylab="", 
     panel.first = {abline(h=seq(0, 60, 20), lty=3, col="grey80");abline(v = 1.61, col="grey80", lwd=2)}, 
     axes=FALSE, frame=FALSE, pch=21, col=NA, bg=bg, cex = 0.7)
axis(2, seq(0, 60, 20), tick=FALSE,  col.axis="grey10", cex.axis = 0.8)
axis(1, seq(0, 10, 2), tick=F,  col.axis="grey10", cex.axis = 0.8)
title("m = 2", adj=0.5, 
      cex.main=0.9, font.main=2, col.main="black")

plot(res[[4]][,2]/100, res[[4]][,1], ylim=c(0, 165), xlim=c(0, 10), 
     xlab="d", ylab="No. of realisations", 
     panel.first = {abline(h=seq(0, 180, 40), lty=3, col="grey80");abline(v = 1.61, col="grey80", lwd=2)}, 
     axes=FALSE, frame=FALSE, pch=21, col=NA, bg=bg, cex = 0.7)
axis(2, seq(0, 180, 40), tick=FALSE,  col.axis="grey10", cex.axis = 0.8)
axis(1, seq(0, 10, 2), tick=F,  col.axis="grey10", cex.axis = 0.8)
title("m = 3", adj=0.5, 
      cex.main=0.9, font.main=2, col.main="black")

plot(res[[5]][,2]/100, res[[5]][,1], ylim=c(0, 165), xlim=c(0, 10), 
     xlab="d", ylab="", 
     panel.first = {abline(h=seq(0, 180, 40), lty=3, col="grey80");abline(v = 1.61, col="grey80", lwd=2)}, 
     axes=FALSE, frame=FALSE, pch=21, col=NA, bg=bg, cex = 0.7)
axis(2, seq(0, 180, 40), tick=FALSE,  col.axis="grey10", cex.axis = 0.8)
axis(1, seq(0, 10, 2), tick=F,  col.axis="grey10", cex.axis = 0.8)
title("m = 4", adj=0.5, 
      cex.main=0.9, font.main=2, col.main="black")


dev.off()
system(paste("pdfcrop", fname))
