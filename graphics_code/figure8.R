library(ggplot2)
library(hrbrthemes)                       

get_data = function(fname, type, N = NULL) {
  o = readRDS(fname)
  o = o[o$n > 2,]# Bug re-running
  o$type = type
  o = o[order(-o$util),]  
  o$id = 1:nrow(o)
  o$se = sqrt((o$util2 - o$util^2)/(o$n-1))/sqrt(o$n)
  o$mean = o$util
  if(!is.null(N)) o = o[1:min(N,nrow(o)), ]
  o
}

N = 100
o2 = get_data(fname = "data/aphids/output2/aphids8.Rds", type="One design point", N)
o3 = get_data(fname = "data/aphids/output3/aphids8.Rds", type="Two design points", N)
o4 = get_data(fname = "data/aphids/output4/aphids8.Rds", type="Three design points", N)
o5 = get_data(fname = "data/aphids/output5/aphids8.Rds", type="Four design points", N)
o5[2,]

col_names = c("n", "mean", "se", "n", "type", "id")
o = rbind(o2[, col_names], o3[, col_names])
o = rbind(o, o4[,col_names])
o = rbind(o, o5[,col_names])


err = data.frame(se = o$se, mean = o$mean, 
                 n = o$n, 
                 type=o$type, id = o$id, stringsAsFactors = F)

err$type = factor(err$type, levels  = 
                    c("One design point", "Two design points", "Three design points", "Four design points"))


head(o2)
ggplot(subset(o2, type=="One design point")) + 
  geom_point(aes(V2, util), size=0.5) 

g2 = ggplot(subset(err, type=="Two design points")) + 
  geom_errorbar(aes(x=id, ymin=mean-2*se, ymax=mean+2*se), size=0.1) + 
  geom_point(aes(id, mean), size=0.5) + 
  theme_ipsum() + 
  labs(y="u(d)", x=NULL, title="Two design points") 
g2
g3 = ggplot(subset(err, type=="Three design points")) + 
  geom_errorbar(aes(x=id, ymin=mean-2*se, ymax=mean+2*se), size=0.1) + 
  geom_point(aes(id, mean), size=0.5) + 
  theme_ipsum() + 
  labs(y="u(d)", x=NULL, title="Three design points") 

g4 = ggplot(subset(err, type=="Four design points")) + 
  geom_errorbar(aes(x=id, ymin=mean-2*se, ymax=mean+2*se), size=0.1) + 
  geom_point(aes(id, mean), size=0.5) + 
  theme_ipsum() + 
  labs(y="u(d)", x=NULL, title="Four design points") 

g = ggplot(err) + 
  geom_errorbar(aes(x=id, ymin=mean-2*se, ymax=mean+2*se), size=0.1) + 
  geom_point(aes(id, mean), size=0.5) + 
  facet_wrap(~type, scale="free_y") + 
  theme_ipsum_rc() + 
  labs(y="u(d)", x=NULL) 
g
fname = "graphics/figure8.pdf"
pdf(fname, width=8, height=6)
suppressMessages(extrafont::loadfonts())
print(g)
dev.off()

system(paste("pdfcrop", fname))