library(ggplot2)
library("aws.s3")

MAX_X = 100; MAX_Y = 800
x = seq(0, 1, length.out=MAX_X+1)
y = seq(0, 12, length.out=MAX_Y+1)

packageVersion("aws.s3")
bucketlist()
bucket = get_bucket(bucket = 'bayesianexpdesign')
(bucket_elements = unlist(lapply(bucket, `[[`, "Key")))


l = lapply(bucket_elements, s3readRDS,  bucket = "bayesianexpdesign")
## Only 24K
initial = as.data.frame(l[[1]])
initial$x = x[initial$V1]
initial$y = y[initial$V2]

dd = l[[1]];
dd[dd[,1] ==24 & dd[,2] == 754,]
for(i in seq_along(l)[-1]){
  message(i, " : ", sum(l[[i]][,3]))
  dd_tmp = l[[i]]
  dd[,4] = (dd[,4]*dd[,3] + dd_tmp[,4]*dd_tmp[,3])/(dd[,3] + dd_tmp[,3])
  dd[,5] = (dd[,5]*dd[,3] + dd_tmp[,5]*dd_tmp[,3])/(dd[,3] + dd_tmp[,3])
  #dd[is.nan(dd[,4]),4] = 0
  dd[,6] = max(dd[,6], dd_tmp[,6])
  dd[,3] = dd[,3] + dd_tmp[,3]
  dd[dd[,1] ==24 & dd[,2] == 754,]
}
dd[is.nan(dd[,4]),4] = 0
dd = as.data.frame(dd)
dd$x = x[dd$V1]
dd$y = y[dd$V2]

nr = sample(1:nrow(initial), size = 2*nrow(initial), replace=TRUE, prob=initial[,4]^5)
initial_samp = initial[nr,]
# Contour plot
g1 = ggplot(initial_samp, aes(x, y)) + stat_density2d(bins=10) + 
  xlim(c(0, 1)) + ylim(c(0, 12.5)) + 
  theme_bw() + xlab("a") + ylab("b") + 
  ggtitle("Utility surface") + annotate("point", x=0.73, y=3.1, cex=2) + 
  annotate("text", x=0.75, y=3.5, label="Ryan et al") +
  annotate("text", x=01, y=12, label = "(a)")
g1

## Visiting
g2 = ggplot(initial, aes(x, y)) + 
  geom_raster(aes(fill=V3>0)) + 
  theme_bw() + xlab("a") + ylab("b") + 
  scale_x_continuous(expand=c(0, 0), limit=c(0, 1)) +
  scale_y_continuous(expand=c(0, 0), limit=c(0, 12)) + 
  scale_fill_manual(values = c("TRUE"="steelblue", "FALSE"="white"), guide=FALSE) + 
  ggtitle("Visited designs") + 
  annotate("text", x=0.95, y=11.2, label = "(b)")
g2


initial_o = initial[initial[,3] > 3,]
initial_o = initial_o[order(-initial_o$V4)[1:100],]
g3 = ggplot(initial_o) + 
  geom_point(aes(1:100, V4)) + 
  geom_errorbar(aes(1:100, ymin = V4 - 2*(V5- V4^2), ymax = V4 + 2*(V5- V4^2) )) + 
  ylim(c(1, 1.4)) + theme_bw() + xlab(NULL) + ylab("U(d)") + 
  ggtitle("Top 100 designs (24K utility evaluations)") + 
  annotate("text", x=100, y=1.4, label = "(c)")
g3

dd_o = dd[dd[,3] > 3,]
dd_o = dd_o[order(-dd_o$V4)[1:100],]
g4 = ggplot(dd_o) + geom_point(aes(1:100, V4)) + 
  geom_errorbar(aes(1:100, ymin = V4 - 2*(V5- V4^2), ymax = V4 + 2*(V5- V4^2) )) + 
  ylim(c(1, 1.4)) + theme_bw() + xlab(NULL) + ylab("U(d)") + 
  ggtitle("Top 100 designs (56K utility evaluations)") + 
  annotate("text", x=100, y=1.4, label = "(d)")
g4

pdf("graphics/")
gridExtra::grid.arrange(g1, g2, g3, g4, ncol=2)

dd_o[13,]

pars = c(18, 410)

ggplot(dd_o) + 

pdf("/tmp/pk_cloud.pdf", width=8, height=6)
print(g )
dev.off()


head(dd_samp)
s=1/11
qbeta(c(s,2*s, 3*s, 4*s, 5*s, 6*s, 7*s, 8*s, 9*s, 10*s),0.73,3.1)*720
qbeta(c(s,2*s, 3*s, 4*s, 5*s, 6*s, 7*s, 8*s, 9*s, 10*s),0.25,7.5)*720



