set.seed(1)
source("15d/initialise.R")

x = seq_len(15) - 0.5
x_tweak = seq(6.5, 8.5, by = 0.01)
util = util_true = numeric(length(x_tweak))

for (i in 1:length(x_tweak)) {
  x[8] = x_tweak[i]
  util[i] = get_utility(x)
  util_true[i] = get_utility(x, 0)
}

fname = "graphics/figure6.pdf"
pdf(fname, width = 6, height = 4)
par(mar = c(3,3.6,2,1), mgp = c(2,0.4,0), tck = -.01,
    cex.axis = 0.9, las = 1, yaxs = 'i')

plot(x_tweak, util, pch = 21, bg = "grey", xlab = expression(d[8]), 
     axes = FALSE, frame = FALSE, ylab = "Utility", 
     panel.first = abline(h = c(0.9, 0.95, 1.0,1.05, 1.1), lty = 3, col = "grey80"), 
     ylim = c(0.89, 1.1))
     
lines(x_tweak, util_true, lwd = 2, col = "steelblue")

axis(1, c(6.5, 7.0, 7.5,8.0,  8.5), 
     tick = FALSE,  col.axis = "grey50", cex.axis = 0.8)
axis(2, c(0.9, 1.0, 1.1), 
     c(0.9, "1.0", 1.1), tick = FALSE,  col.axis = "grey50", cex.axis = 0.8)

dev.off()
system(paste("pdfcrop", fname))