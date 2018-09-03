library(viridis)
library(ggplot2)
df = read.csv("data/cotton_data.csv")
df = df[df$Year == 2,]
df$No.aphids.leaf = df$No.aphids.leaf*60
colnames(df) = c("Year", "Time", "Water", "Nitrogen", "Block", "Aphids")
df = df[df$Time >2, ]
df$Time = (df$Time -3)*7
df$Block = factor(df$Block)
df$Nitrogen = factor(df$Nitrogen, labels = c("Zero", "Variable", "Blanket"))
df$Water = factor(df$Water, labels = c("Low", "Medium", "High"))

fname = "graphics/aphids_data.pdf"
pdf(fname, width=8, height=8)
ggplot(df, aes(Time, Aphids, colour = Block)) + 
  geom_point(alpha = 0.7) + 
  facet_grid(Water ~ Nitrogen) + 
  ylim(c(0, 3000)) + 
  guides(colour=FALSE) + 
  xlab("Time (days)") + 
  xlim(c(0, 30)) + 
  theme_bw() + 
  scale_color_viridis(discrete=TRUE)

dev.off()
system(paste("pdfcrop", fname))
