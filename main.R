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
###################################################################

require(deSolve)
require(graphics)
require(ggplot2)
require(xlsx)

source("basicFunctions.R")
source("modelDefine.R")
source("mcmc_fitting_3params.R")

# Daw data
#global values that can be read by the functions in this file.
daw<-getDAWtable()
daw_years<-seq(from=1860, to=1940, by=10)
t_year<-1870
row_index<-which(daw_years==t_year)
my_data<-daw$t1Total[row_index,]/2

### initial starting values
beta_o<-0.1001
tau<-0.01
beta1=getBeta_decayExp(beta_o, tau, t_year)
mult<-40
parameters<-getParameters(9, mult, beta1)
year_step<-5
nstart=c(rep(3*10^6, times=9), rep(0, times=9), rep(0, times=9), c(0,1000, rep(0, times=7)), rep(0, times=9))
time<-seq(from=1, to=365*year_step, by=1 )
### Run the model to equilibrium by running in steps of "year_step" years
### year_step is 5, running in steps of 5 years
### t_period to check for eqbm is 365 days
### change_limit as tol for change is 1%
t_period<-365
change_limit<-1
eqbmFlag<-FALSE
eqbm<-numeric()
t_last<-numeric()
output_pre_all<-numeric()
repeat{
  output_pre<-as.data.frame(ode(nstart,time,TBmodel_9ageclasses,parameters))
  eqbmFlag<-checkEqbm(output_pre, t_period, change_limit)
  eqbm<-output_pre[dim(output_pre)[1],]
  t_last<-as.numeric(eqbm[1])
  output_pre_all<-rbind(output_pre_all, output_pre)
  tail(output_pre_all[,1])

  time<-seq(from=t_last+1, to=t_last+(365*year_step), by=1)
  nstart<-as.numeric(eqbm[-1])
  if(eqbmFlag==TRUE) 
    break
}


### This eqbm will be read by the function my_log_lh_func_3params
eqbm
plotOutputByAge(output_pre_all)
  

# Now run the MCMC function to get the best parameters after 5 accepted runs
M<-master_mcmc_runner(5)
index<-which.max(M$log_lh)
M_best<-M[index,]

# get the best parameters
mult<-M_best$mult
beta_o<-M_best$beta_o
tau<-M_best$tau



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
yrange<-range(c(as.numeric(daw$t1Total[row_index,]/2), modelOP*365))
plot(min_age, daw$t1Total[row_index,]/2, type='b', pch=16, lty=1,col="darkblue", lwd=2, xlab="Age Groups", ylab="Annual death rates per million", main=paste0("year ", t_year-9, " - ", t_year), sub="source:Daw 1950", ylim=yrange)
lines(min_age, modelOP*365, type='b', pch=16, lty=2, lwd=2, col="darkgreen")
title(paste0("beta = ", round(beta1,4)," per day, (beta_o=",beta_o, ",tau= ", tau, ") ", "k =", round(mult,4)), outer=TRUE)
legend(x=40, y=5000, legend=c("DAW data", "Model output"), lty=c(1,2), lwd=2, col=c("darkblue", "darkgreen"))
