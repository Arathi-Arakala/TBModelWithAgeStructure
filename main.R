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
library(reshape2)

source("basicFunctions.R")
source("modelDefine.R")
source("mcmc_fitting_3params.R")
#source("mcmc_fitting_4params.R")

alpha<-1860

# plot daw data
daw<-getDAWtable()
daw_total<-daw$t1Total
daw_years<-seq(from=1850, to=1930, by=10)
daw_total<-cbind(daw_years, daw_total)
daw_total_df<-melt(daw_total, id.vars = "daw_years", measure.vars =  c("age0", "age5", "age15", "age25", "age35", "age45", "age55", "age65", "age75" ) )

#daw_plot<-ggplot(daw_total_df, )
# # Daw data
# #global values that can be read by the functions in this file.
# daw<-getDAWtable()
# daw_years<-seq(from=1860, to=1940, by=10)
# t_year<-1870
# row_index<-which(daw_years==t_year)
# my_data<-daw$t1Total[row_index,]/2

# ### initial starting values
# beta_o<-0.1
# tau<-0.1
# gamma<-0.00001
# alpha<-1860
# 
# plotBeta_decayExp(beta_o, tau,gamma,1)
# beta1=getBeta_decayExp(beta_o, tau, gamma, t_year-1850)
# mult<-40
# parameters<-getParameters(9, mult, beta1)
# EqbmOutput<-IsEqbm(parameters)
# plotOutputByAge(EqbmOutput$output_pre_all)
# plotDiseaseClassbyAge(EqbmOutput$output_pre_all)
# 
# EqbmOutput$eqbm
# 
# Fit to a single year
# Daw data
# #global values that can be read by the functions in this file.
# daw<-getDAWtable()
# min_age<-c(0, seq(from=5, to=75, by=10))
# daw_years<-seq(from=1860, to=1940, by=10)
# 
# for( t in 2:length(daw_years)){
#   t_year<-daw_years[t]
#   row_index<-which(daw_years==t_year)
#   my_data<-daw$t1Total[row_index,]/2
#   # Now run the MCMC function to get the best parameters after 5 accepted runs, set the second argument to TRUE for single year fit
#   M<-master_mcmc_runner_3params(15, TRUE)
#   index<-which.max(M$log_lh)
#   M_best<-M[index,]
#   
#   mult<-M_best$mult
#   beta_o<-M_best$beta_o
#   tau<-M_best$tau
#   
#   #plotBeta_decayExp(beta_o, tau,gamma, 1)
#   
#   nstart<-as.numeric(eqbm[-1])
#   t_last<-as.numeric(eqbm[1])
#   n_years<-t_year-1850
#   time<-seq(from=1, to=365*n_years,by=1 )
#   
#   parameters_timevarybeta<-getParameters_timevarybeta(9, mult, beta_o, tau, gamma, alpha)
#   #parameters_timevarybeta<-getParameters_timevarybeta_alpha(9, mult, beta_o, tau, alpha1, gamma, alpha)
#   op<-ageStrTBModel_mortality_3params(mult, beta_o, tau, gamma, alpha, eqbm, t_year)
#   
#   if(t==2){
#     quartz()
#     par(mfrow=c(1,1), oma=c(0,0,2,0))
#     yrange<-range(c(daw$t1Total/2, op))
#     plot(min_age, daw$t1Total[row_index,]/2, type='b', pch=16, lty=1,col=t, lwd=2, xlab="Age Groups", ylab="Annual death rates per million", main="Fitting to year wise DAW data", sub="source:Daw 1950", ylim=yrange)
#     lines(min_age, op, type='b', pch=16, lty=2, lwd=2, col=t)
#     legend(x=40, y=5000, legend=c("DAW data", "Model output"), lty=c(1,2), lwd=2)
#     
#   }
#   if(t>2){
#     lines(min_age, daw$t1Total[row_index,]/2,type='b', pch=16, lty=1,col=t, lwd=2)
#     lines(min_age, op, type='b', pch=16, lty=2, lwd=2, col=t)
#   }
#    print(t)
# }
# 

### fitting to all DAW data points
daw<-getDAWtable()
min_age<-c(0, seq(from=5, to=75, by=10))
daw_years<-seq(from=1860, to=1940, by=10)
gamma<-0.0001

starttime<-timestamp()
M<-master_mcmc_runner_3params(30, FALSE) #second argument determines fitting to all DAW data points.
endtime<-timestamp()

print(c(starttime, endtime))


index<-which.max(M$log_lh)
#index<-which.min(M$log_lh)
M_best<-M[index,]

mult<-M_best$mult
beta_o<-M_best$beta_o
tau<-M_best$tau
c(mult, beta_o, tau)
# nstart<-as.numeric(eqbm[-1])
# t_last<-as.numeric(eqbm[1])

#plot best beta curve
plotBeta_decayExp(beta_o, tau,gamma,1)


#plot all the beta curves from accepted values
M_accepted<-M[which(M$accepted==1),]

for(m in 1:dim(M_accepted)[1]){
  # if(m==1){
  #   plotBeta_decayExp(M_accepted$beta_o[m], M_accepted$tau[m],gamma, 1)
  # }
 # if(m>1){
    plotBeta_decayExp(M_accepted$beta_o[m], M_accepted$tau[m],gamma, 0)
    
 # }
  
}

# plot histogram of accepted parameter values
quartz()
par(mfrow=c(2,2), oma=c(0,0,2,0))
hist(M_accepted$mult)
hist(M_accepted$beta_o)
hist(M_accepted$tau)


# plot all the 90 points
for(t in 1:length(daw_years)){
  t_year<-daw_years[t]
  row_index<-which(daw_years==t_year)
  
  op<-ageStrTBModel_mortality_3params(mult, beta_o, tau, gamma, alpha, t_year)
  
  if(t==1){
    quartz()
    par(mfrow=c(1,1), oma=c(0,0,2,0))
    yrange<-range(c(daw$t1Total/2, op))
    plot(min_age, daw$t1Total[row_index,]/2, type='b', pch=16, lty=1,col=t, lwd=2, xlab="Age Groups", ylab="Annual death rates per million", main="Parameter fitting to ALL DAW data", sub="source:Daw 1950", ylim=yrange)
    lines(min_age, op, type='b', pch=16, lty=2, lwd=2, col=t)
    legend(x=40, y=5000, legend=c("DAW data", "Model output"), lty=c(1,2), lwd=2)
  }
  
  if(t>2){
    lines(min_age, daw$t1Total[row_index,]/2,type='b', pch=16, lty=1,col=t, lwd=2)
    lines(min_age, op, type='b', pch=16, lty=2, lwd=2, col=t)
  }
  
}

quartz()
par(mfrow=c(2,1),oma=c(0,0,2,0))
plot(1:dim(M)[1], M$beta_o, type='p', pch=(M$accepted+16), col=3 )
plot(1:dim(M)[1], M$tau, type='p', pch=(M$accepted+16), col=4)
M$log_lh
M$accepted


##### Now that we got the best fit, let us use these parameters to see the disease dynamics
beta_o<-M_best$beta_o
tau<-M_best$tau
gamma<-0.0001
mult<-M_best$mult
alpha<-1860

# run to eqbm for the given beta_o n tau at the starting year 1860
beta1=getBeta_decayExp(beta_o, tau, gamma, alpha-1850)
#mult<-40
parameters<-getParameters(9, mult, beta1)
EqbmOutput<-IsEqbm(parameters)
eqbm<-EqbmOutput$eqbm

#use eqbm as starting conditions for ode run
nstart<-as.numeric(eqbm[-1])
parameters_timevarybeta<-getParameters_timevarybeta(9, mult, beta_o, tau, gamma, alpha)

#Run the TB model till 1940
t_year<-1940
n_years<-t_year-1850 # start year
time<-seq(from=1, to=365*n_years, by=100 ) #timestep output monthly to speed up
output<-as.data.frame(ode(nstart,time,TBmodel_9ageclasses_timevarybeta_closedpopln,parameters_timevarybeta))
# plotOutputByAge(output)
# plotDiseaseClassbyAge(output)
# plotTotalPopln(output)
plotCompartmentalDisbnByAgeGroup(output, t_year)