gil_constant = readRDS("data/death_gil_constant.rds")
gil_incr = readRDS("data/death_gil_incr.rds")
gil_de = readRDS("data/death_gil_de.rds")
muller = readRDS("data/death_mullers.rds")

gil_constant = unlist(gil_constant)[seq(2, 2*length(gil_constant), 2)]
gil_incr = unlist(gil_incr)[seq(2, 2*length(gil_incr), 2)]
gil_de = unlist(gil_de)[seq(2, 2*length(gil_de), 2)]

muller1 = unlist(muller)[seq(1, 3*length(muller), 3)]
muller8 = unlist(muller)[seq(2, 3*length(muller), 3)]
muller16 = unlist(muller)[seq(3, 3*length(muller), 3)]

robert3 = unlist(readRDS("data/robert3.rds"))
robert4 = unlist(readRDS("data/robert4.rds"))
robert5 = unlist(readRDS("data/robert5.rds"))
cx = readRDS("data/cx.rds")

length(muller1)

dd = data.frame(values = c(gil_constant, gil_incr, gil_de, 
                           muller1, muller8, muller16, 
                           robert3, robert4, robert5, 
                           cx), 
                group = rep(c("SEB Con", "SEB Inc", "SEB DE", 
                              "Muller 1", "Muller 8", "Muller 16", 
                              "Robert 1", "Robert 2", "Robert 3", 
                              "CX"), each=500), 
                alg = factor(c(rep(1:3, each=1500), rep(4, 500))))

dd$group = factor(dd$group, levels=rev(c("SEB Con", "SEB Inc", "SEB DE", 
                          "Muller 1", "Muller 8", "Muller 16", 
                          "Robert 1", "Robert 2", "Robert 3", 
                          "CX")))
dd_text = data.frame(values = rep(3.5, 9), 
                     group = c("SEB Con", "SEB Inc", "SEB DE", 
                               "Muller 1", "Muller 8", "Muller 16", 
                               "Robert 1", "Robert 2", "Robert 3"), 
                     label=c("N[m] ~'= 4800, 4800, 4800, 4800, 4800'", 
                             "N[m] ~'= 12000, 6000, 3000, 1500, 750'",
                             "N[m] ~'= 750, 1500, 3000, 6000, 12000'",
                             "J ~'= 1'","J ~'= 8'","J ~'= 16'",
                             "J ~'= 1, 2, 4'","J ~'= 1, 2, 4, 8'","J ~'= 1, 2, 4, 8, 16'"))
library(ggplot2)
library(hrbrthemes)         
res = 900
fname = "graphics/figure2.png"
png(fname, width=10*0.8*res, height=7*0.8*res, res = res)
g = ggplot(dd) + 
  geom_boxplot(aes(y=values, x=group, fill=alg)) + 
  theme_bw() + coord_flip() + xlab(NULL) + ylab("Design: d") + 
  scale_fill_manual(values=c("1"="steelblue", "2"="forestgreen", "3"="firebrick", "4"="tan3")) + 
  guides(fill=FALSE)
g  
g1 = g + theme_ipsum_rc(grid="X", base_family = "Arial") + 
  scale_x_discrete(labels = c("ACE", "", "", "Amzal", "", "", "Muller", "", "", "New"), 
                   expand=c(0,0.7)) + 
  scale_y_continuous(expand=c(0.01, 0),  limits = c(0.8, 3.5)) 
g2 = g1 + geom_label(data=dd_text, aes(y=values, x=group, label=label), 
              colour="grey10", fill="white", hjust=1, size=3, vjust="bottom", 
              nudge_x = -0.25, parse=TRUE) 
g2

dev.off()
#system(paste("pdfcrop", fname))

g2 +annotate("segment", x = 0, xend = 10.5, y = 1.61, yend = 1.61,
             colour = "blue")
?geom_hline
?annotate

####
# MSE 
library(dplyr)
dd %>%
  group_by(group) %>%
  summarise(mse = sqrt(sum((values - 1.61)^2)/length(values)),
            l1 = sum(abs(values - 1.61))/length(values)) %>%
  mutate(mse = signif(mse, 2)) 
# 

