collapse_d = function(m, no_of_ps=1) {
  if(ncol(m) == 2){
    m_tmp = matrix(no_of_ps, ncol=3, nrow=nrow(m))
    m_tmp[,2:3] = m
    m = m_tmp
  }
  
  m_means = tapply(m[,3], m[,2], mean)
  m_collapse = matrix(-1, ncol=3, nrow=length(m_means))
  m_collapse[,3] = as.vector(m_means)
  m_collapse[,2] = as.numeric(names(m_means))
  m_collapse[,1] = as.vector(tapply(m[,1], m[,2], sum))
  colnames(m_collapse) = c("n", "d", "u")
  
  ## Remove designs from outside design space
  m_collapse = m_collapse[m_collapse[,3] > 0,]
  return(m_collapse)
}
g1 = readRDS(file="output/death_1d/Gil_P1.RData")
g8 = readRDS(file="output/death_1d/Gil_P8.RData")
r1 = readRDS(file="output/death_1d/robert_p1.RData")
r8 = readRDS(file="output/death_1d/robert_p8.RData")

setnicepar(mfrow=c(2, 2))
plot(g1[[5]][,2], g1[[5]][,3], type="l")
m = collapse_d(r1[[5]])
plot(m[,2], m[,3]^(1/16), type="l")
plot(g8[[5]][,2], g8[[5]][,3], type="l")
m = collapse_d(r8[[5]])
plot(m[,2], m[,3]^(1/16), type="l")

head(m1)
colSums(m1)
m1 = collapse_d(readRDS(file="output/death_1d/muller1.RData"))
m16 = collapse_d(readRDS(file="output/death_1d/muller16_p1.RData"))
m16_par = collapse_d(readRDS(file="output/death_1d/muller16_p8.RData"), 8)

setnicepar(mfrow=c(2, 2))
plot(m1[,2], m1[,3], type="l")
plot(m16[,2]*100, m16[,3]^(1/16), type="l")
plot(m16_par[,2]*100, m16_par[,3]^(1/16), type="l")


setnicepar(mfrow=c(2, 2))
plot(g8[[5]][,2], g8[[5]][,1], type="l")
plot(m[,2], m[,1], type="l")
plot(m1[,2]*100, m1[,1], type="l")
plot(m16_par[,2]*100, m16_par[,1], type="l")

