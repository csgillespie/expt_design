m1 = readRDS("output/death_3d/muller1.RData")
m16 = readRDS("output/death_3d/muller16.RData")


tmp = m16[[1]]

setnicepar(mfrow=c(3, 3))
hist(m16[[1]][,1], breaks="fd", xlim=c(0 ,2.5))
hist(m16[[2]][,1], breaks="fd", xlim=c(0 ,2.5))
hist(m16[[3]][,1], breaks="fd", xlim=c(0 ,2.5))
hist(m16[[4]][,1], breaks="fd", xlim=c(0 ,2.5))
hist(m16[[5]][,1], breaks="fd", xlim=c(0 ,2.5))
hist(m16[[6]][,1], breaks="fd", xlim=c(0 ,2.5))
hist(m16[[7]][,1], breaks="fd", xlim=c(0 ,2.5))
hist(m16[[8]][,1], breaks="fd", xlim=c(0 ,2.5))
hist(m16[[9]][,1], breaks="fd", xlim=c(0 ,2.5))

