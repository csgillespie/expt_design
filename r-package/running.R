library(aphids)
dir.create("output2")
system.time(d <- mcmc(2000, 1:8, no_of_cores = 6, dir="output5"))

d = readRDS("output2/aphids8.Rds")
d1 = d#[[8]]
d1 = d1[d1$n > 1,]
sqrt((d1$util2 - d1$util^2)/(d1$n-1))

err = data.frame(se = sqrt((d1$util2 - d1$util^2)/(d1$n-1))/sqrt(d1$n), 
                 mean = d1$util, 
                 n = d1$n)
err = err[order(-err$mean), ]
err$id = 1:nrow(err)
head(err)
library(ggplot2)
ggplot(err) + 
  geom_errorbar(aes(x=id, ymin=mean-1.96*se, ymax=mean+1.96*se)) + 
  geom_point(aes(id, mean)) + 
  coord_flip()


## Output2
tmp = d1[order(d1$V2),]
plot(tmp$V2, tmp$util)


d = c(0 ,5,19,24,32)# 35.83
d = c(0,18 ,27, 34, 38)
res1 = sapply(1:200, function(i) get_utility(i, i, d))
mean(res1)
