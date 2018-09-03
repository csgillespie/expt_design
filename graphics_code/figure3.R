p = readRDS("data/death1d/exact.rds")
p = sort(p/sum(p), decreasing = T)

q = seq(0, 1, length.out = 100)
j = 1
values = matrix(0, nrow=100, ncol=4)
med_values = numeric(100)
for(j in seq_len(nrow(values))) {
  values[j,] = c(which(cumsum(p^j/sum(p^j)) > 2^-1)[1],
                 which(cumsum(p^j/sum(p^j)) > 2^-2)[1],
                 which(cumsum(p^j/sum(p^j)) > 2^-3)[1],
                 which(cumsum(p^j/sum(p^j)) > 2^-4)[1])
  d = p^j/sum(p^j)
}

head(values)
plot(values[,1], type="l", ylim=c(1, 500), log="xy")
lines(values[,2], type="l")
lines(values[,3], type="l")
lines(values[,4], type="l")


#plot(values, med_values, type="l")

fname = "graphics/figure3.pdf"
pdf(fname, width=6, height=4)

par(mar=c(3,3,2,1), mgp=c(2,0.4,0), tck=-.01,
    cex.axis=0.9, las=1, yaxs='i')

plot(seq_len(nrow(values)), values[,1], log="xy",
     ylim=c(1, 500),
     xlab="J", ylab=expression(n[up]), 
     panel.first = abline(h=c(1, 5,10, 50, 100, 500), lty=3, col="grey80"), 
     axes=FALSE, frame=FALSE, pch = 21, bg="steelblue", type="l")
lines(seq_len(nrow(values)), values[,2], lty=2)
lines(seq_len(nrow(values)), values[,3], lty=3)
#lines(seq_len(nrow(values)), values[,4], lty=4)
axis(2, c(1, 5,10, 50, 100, 500), tick=FALSE,  col.axis="grey50", cex.axis = 0.8)
axis(1, c(1, 2, 5, 10, 20, 50, 100), tick=F,  col.axis="grey50", cex.axis = 0.8)
text(35, 100,expression(paste(alpha, "=0.5")), cex=0.9)
text(45, 40,expression(paste(alpha, "=0.25")), cex=0.9)
text(55, 19,expression(paste(alpha, "=0.125")), cex=0.9)

#title("", adj=1, 
#      cex.main=0.9, font.main=2, col.main="black")
dev.off()
system(paste("pdfcrop", fname))
