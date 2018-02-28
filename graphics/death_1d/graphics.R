source("R/death_1d/death_utility.R")
mypalette(1)

ds = seq(0.01, 10, 0.01)
exact = readRDS("output/death_1d/exact.RData")

pdf("graphics/death_1d/exact.pdf", width=14, height=6)
setnicepar(mfrow=c(1, 3))
plot(ds, exact, type="l", ylab="u(d)", xlab="d", 
     panel.first = grid())
plot(ds, exact/sum(exact), type="l", ylab="p(D=d)", xlab="d", 
     ylim=c(0, 0.0012), panel.first=grid())

plot(ds, cumsum(exact/sum(exact)), type="l",
     ylab="Pr(D > d)", xlab="d", 
     ylim=c(0, 1), panel.first=grid())

lines(ds, punif(ds, 0, 10), col=2)
text(7, 0.5, "U(0, 10)", col=2)

lines(ds, pnorm(ds, 2, 0.5), col=3)
text(4, 0.9, "N(2, 0.25)", col=3)
dev.off()

gil = readRDS("output/death_1d/gil_constant.RData")
gil_de = readRDS(file="output/death_1d/gil_decrease.RData")
gil_incr = readRDS(file="output/death_1d/gil_increase.RData")





muller = readRDS("output/death_1d/muller_300.RData")
r1 = unlist(readRDS("output/death_1d/robert1.RData"))
r2 = unlist(readRDS("output/death_1d/robert2.RData"))

######################################
pdf("graphics/death_1d/comparisona.pdf", width=4.5, height=4.5)
setnicepar()
plot(density(gil, adj=2), main="", xlim=c(0.5, 3),ylim=c(0, 8.5),
     xlab="d", panel.first=grid(), lwd=2)
lines(density(muller[,1]), col=2, lwd=2)
text(2.5, 0.5, "J=1", col=2)
lines(density(muller[,2]), col=3, lwd=2)
text(2, 2, "J=8", col=3)
lines(density(muller[,3]), col=4, lwd=2)
text(1, 1, "J=16", col=4)
#text(0.6, 8, "(a)")
dev.off()


pdf("graphics/death_1d/comparisonb.pdf", width=4.5, height=4.5)
setnicepar()

plot(density(gil, adj=2), main="", xlim=c(0.5, 3), 
     xlab="d", panel.first=grid(), ylim=c(0, 8.5), lwd=2)
lines(density(r1, adj=2), col=2, lwd=2)
lines(density(r2, adj=2), col=3, lwd=2)
#text(0.6, 8, "(b)")
dev.off()

pdf("graphics/death_1d/comparisonc.pdf", width=4.5, height=4.5)
setnicepar()
plot(density(gil, adj=2), main="", xlim=c(0.5, 3),ylim=c(0, 12),
     xlab="d", panel.first=grid(), lwd=2)
lines(density(gil_incr, adj=2), col=2, lwd=2)
lines(density(gil_de, adj=2), col=3, lwd=2)
#text(0.6, 11, "(c)")
dev.off()


