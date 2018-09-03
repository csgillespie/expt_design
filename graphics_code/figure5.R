library(hrbrthemes)
library(tidyverse)

ace = readRDS("data/sin/ace.rds")
exact = readRDS("data/sin/exact.rds")
new = readRDS("data/sin/new.rds")

ace = ace %>% 
  mutate(d1 = pmin(V1, V2), d2 = pmax(V1, V2)) %>%
  select(d1, d2)

new = new %>%
  as_tibble() %>%
  rename(d1 = V1, d2=V2) %>%
  select(d1, d2)

exact = exact %>%
  rename(d1 = Var1, d2 = Var2) %>%
  filter(d2 >= d1)


fname = "graphics/figure5a.pdf"
pdf(fname, width = 6, height = 4)
ggplot(exact, aes(d1, d2)) + 
  geom_raster(aes(fill = util)) + 
  geom_point(data = ace, aes(d1, d2)) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) + 
  scale_fill_viridis_c(name = "Utility") + 
  theme(panel.background = element_rect(fill = NA)) + 
  xlab(expression(d[1])) + ylab(expression(d[2]))
dev.off()
system(paste("pdfcrop", fname))

fname = "graphics/figure5b.pdf"
pdf(fname, width = 6, height = 4)
ggplot(exact, aes(d1, d2)) + 
  geom_raster(aes(fill = util)) + 
  geom_point(data = new, aes(d1, d2), alpha = 0.1) + 
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) + 
  scale_fill_viridis_c(name = "Utility") + 
  theme(panel.background = element_rect(fill = NA)) + 
  xlab(expression(d[1])) + ylab(expression(d[2]))
dev.off()
system(paste("pdfcrop", fname))

