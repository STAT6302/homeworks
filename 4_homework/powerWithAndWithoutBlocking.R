n = 48
X = matrix(0,nrow=n,ncol=13)
colnames(X) = c('mu','A','B','Control','F1','F2','F3','B1','B2','B3','B4','B5','B6')
X[,1] = rep(1,n)
X[,2] = c(rep(1,n/2),rep(0,n/2))
X[,3] = c(rep(0,n/2),rep(1,n/2))
X[,4] = rep(c(rep(1,n/8),rep(0,n/8),rep(0,n/8),rep(0,n/8)),2)
X[,5] = rep(c(rep(0,n/8),rep(1,n/8),rep(0,n/8),rep(0,n/8)),2)
X[,6] = rep(c(rep(0,n/8),rep(0,n/8),rep(1,n/8),rep(0,n/8)),2)
X[,7] = rep(c(rep(0,n/8),rep(0,n/8),rep(0,n/8),rep(1,n/8)),2)
X[1:6,8:13] = diag(1,6)
X[7:12,8:13] = diag(1,6)
X[13:18,8:13] = diag(1,6)
X[19:24,8:13] = diag(1,6)
X[25:30,8:13] = diag(1,6)
X[31:36,8:13] = diag(1,6)
X[37:42,8:13] = diag(1,6)
X[43:48,8:13] = diag(1,6)

XdfBlock = data.frame('species' = c(rep('A',n/2),rep('B',n/2)),
                      'fertilizer' = c(rep('Control',6),rep('F1',6),rep('F2',6),rep('F3',6),
                                         rep('Control',6),rep('F1',6),rep('F2',6),rep('F3',6)),
                      'block' = rep(c('B1','B2','B3','B4','B5','B6'),8))


###
# sim parms
###
nSweep = 1000
alpha  = 0.05
sig    = 1
####

####
alphaVec_species = rep(0,nSweep)
alphaVec_fert    = rep(0,nSweep)
set.seed(10)
for(j in 1:nSweep){
  #mu = c(12,1,2,0,2,1,-3,-6,-4,-2,0,3,6)# Substantial block effect
  mu = c(12,1,2,0,2,1,-3,rep(0,6))# No block effect
  Y = X %*% mu + rnorm(n,0,sig)
  model = lm(Y ~ species + fertilizer + block,
             data = XdfBlock)
  alphaVec_species[j] = anova(model)[[5]][1]
  alphaVec_fert[j]    = anova(model)[[5]][2]
}
mean(alphaVec_fert < alpha)
mean(alphaVec_species < alpha)
