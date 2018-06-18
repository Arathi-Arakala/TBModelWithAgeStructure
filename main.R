####################################################################
## Create an age stratified model with age classes aligned to Dawson Paper
## As this is aimed for is pre-vaccination data, the model will not have
## vaccination or case detection rate.
## Create two versions of model 
## - one where Latent compartments have flow rates into higher age classes
## - second where Latent compartments don't have flow into higher age classes
## Compare the mortality rates with that of the Dawson Paper
## Can we prove that the second model is inaccurate? 
## This has implications on possible treatment strategies in high endemic settings
####################################################################

require(deSolve)
require(graphics)
require(ggplot2)
require(xlsx)

source("basicFunctions.R")
source("modelDefine.R")
source("mcmc_fitting.R")
M<-master_mcmc_runner(20)
index<-which.max(M$log_lh)
M_best<-M[index,]

mult<-M_best$mult
beta1<-M_best$beta1

nstart=c(rep(3*10^6, times=9), rep(0, times=9), rep(0, times=9), c(0,1000, rep(0, times=7)), rep(0, times=9))
# mult<-40
# beta1=3*24/365
parameters<-getParameters(9, mult, beta1)
time<-seq(from=1, to=365*100, by=1 )

output_pre<-as.data.frame(ode(nstart,time,TBmodel_9ageclasses,parameters))
eqbm<-output_pre[dim(output_pre)[1], ]
#eqbm
#eqbm[29:37]
#plotOutputbyDiseaseClass(output_pre)
#plotOutputByAge(output_pre)

modelOP<-as.numeric(getEqbmMortalitybyAgeClass(output_pre, parameters$mu_I))
modelOP*365

# Plot the DAW table for 1851-1860
daw<-getDAWtable()
min_age<-c(0, seq(from=5, to=75, by=10))
quartz()
par(mfrow=c(1,1), oma=c(0,0,2,0))
yrange<-range(c(as.numeric(daw$t1Total[1,]/2), modelOP*365))
plot(min_age, daw$t1Total[1,]/2, type='b', pch=16, lty=1,col="darkblue", lwd=2, xlab="Age Groups", ylab="Annual death rates per million", main="year 1851-1860", sub="source:Daw 1950", ylim=yrange)
lines(min_age, modelOP*365, type='b', pch=16, lty=2, lwd=2, col="darkgreen")
title(paste("beta =", round(beta1,4)," per day, ", "k =", round(mult,4), sep=" "  ), outer=TRUE)
legend(x=40, y=5000, legend=c("DAW data", "Model output"), lty=c(1,2), lwd=2, col=c("darkblue", "darkgreen"))
