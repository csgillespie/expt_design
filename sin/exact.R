source("utility.R")
library("tidyverse")


i = 2
t = seq(0, 1, length.out = 750)
gr = expand.grid(t, t)
gr = gr[gr[,1] >= gr[,2],]
u = u_sd = numeric(nrow(gr))
pars = get_pars()
for (i in 1:nrow(gr)) {
  t = unlist(gr[i,])
  y = replicate(10, simulate(t))
  us = apply(y, 2, function(i) get_utility(pars["c"], pars["b"], pars["h"], pars["g"], t, i))
  u[i] = mean(us)
  u_sd[i] = sd(us)
  if((i %% 1000)== 0) message(i)
}

gr = gr %>%
  as_tibble() %>%
  mutate(util = u, sd = u_sd)
gr = gr[,c(2, 1, 3:4)] %>%
  rename(Var2 = Var1, Var1 = Var2) %>%
  bind_rows(gr)



ggplot(gr, aes(Var1, Var2)) + 
  geom_raster(aes(fill = util))

saveRDS(gr, file = "../data/sin/exact.rds")

