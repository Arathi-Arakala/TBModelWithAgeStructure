#### This file will contain a list of commonly used functions ####
##################################################################

source("modelDefine.R")

### 1. Function to display the table in DAW paper #####
## Excerpt from DAW1950
## "The data used are set out in Table I and consist of the 
## mortality rates in England and Wales for all forms of tuberculosis. 
## For the years 1851-1920, the rates were taken or derived from 
## Table 12 of the Registrar-General’sDecennial Supplement, 1921, Part III. 
## For 1921-40, the rates have been calculated from the numbers of deaths 
## and mean populations given in the annual volumes of the 
## Registrar-General's StatisticRaelview."


getDAWtable<-function(){
  table1Male<-data.frame(age0=c(6323,6018,5798,5004,4347,3129,1942,1067,612), age5=c(1166,967,828,727,615,552,545,321,184),
                         age15=c(3400, 3157,2493,1976,1641,1353, 1299,1083, 817), age25=c(4163, 4206, 3785, 3164, 2541,2158, 1840,1413, 1009),
                         age35=c(4119, 4244, 4198, 3685, 3251, 2622, 2204, 1645, 1134), age45=c(3957, 3969,3928,3611, 3296,2934,2335,1729,1390),
                         age55=c(3479, 3433, 3285, 3027, 2768, 2574, 2135, 1437, 1287), age65=c(2573, 2174, 2025, 1913, 1706, 1686, 1390, 972, 808),
                         age75=c(1061, 740, 650, 732,629, 668, 585, 398, 353))
  table1Female<-data.frame(age0=c(5232, 4917, 4663, 3987, 3516, 2636, 1619, 881, 522), age5=c(1388, 1109, 956, 949, 780, 704, 682, 408, 214), 
                           age15=c(4079, 3689, 2907, 2267, 1670, 1338, 1470, 1353, 1061), age25=c(4690, 4482, 3631, 2932, 2086, 1651, 1484, 1230, 929),
                           age35=c(4293, 3988, 3475, 2846, 2264, 1710, 1401, 936, 639), age45=c(3236, 2954, 2535, 2146, 1753, 1449, 1156, 729, 481), 
                           age55=c(2523, 2178, 1866, 1597, 1344, 1186, 943, 617, 423), age65=c(1783, 1354, 1193, 1058, 906, 894, 750, 511, 358),
                           age75=c(834, 528, 452, 452, 427, 494, 437, 326, 236))
  table1Total<-table1Male+table1Female
  
  list(t1Male=table1Male, t1Female=table1Female, t1Total=table1Total)
}
### plot the data in table per calendar year
plotTablePerCY<-function(table, type){
  startYears<-seq(from=1851, to=1931, by=10)
  endYears<-seq(from=1860, to=1940, by=10)
  
  startAge<-c(0, 5, 15, 25,35,45,55,65,75)
  quartz()
  par(mfrow=c(1,1), oma=c(0,0,2,0))
  for(i in 1:dim(table)[1]){
    if(i==1)
       plot(startAge, table[i,], type='b', pch=16, col=i, ylab="Deaths per Million", xlab="Age (mid point)", ylim=range(table) )
    if(i>1)
      lines(startAge, table[i,], type='b', pch=16, col=i)
  }
  legend(x=55,y=max(table)-1000, legend=as.character(startYears), col=1:dim(table)[1], lwd=2 )
  title(paste("DAW TABLE of", type, "by calendar year", sep=" "), outer=TRUE)
  
}

# plot the data in the table by mean birth year
plotTableMeanBirthYear<-function(table, type){
  meanBirthYears<-seq(from=1855, to=1935, by=10)
  tableByBirthYear<-matrix(0, nrow=9, ncol=9)
  for(i in 1:dim(table)[1]){
  tmp<-table[i, 1]
  if(i<9){
    for(t in 1:(9-i)){
      tmp<-c(tmp, table[i+t,t+1 ])
    }#end of t loop
  }#end of if
  tableByBirthYear[i,1:length(tmp)]<-tmp
  }#end of i loop
  
  startAge<-c(0, 5, 15, 25,35,45,55,65,75)
  quartz()
  par(mfrow=c(1,1), oma=c(0,0,2,0))
  for(i in 1:dim(tableByBirthYear)[1]){
    if(i==1)
      plot(startAge, tableByBirthYear[i,], type='b', pch=16, col=i, ylab="Deaths per Million", xlab="Age (mid point)", ylim=range(tableByBirthYear) )
    if(i>1)
      lines(startAge, tableByBirthYear[i,], type='b', pch=16, col=i)
  }
  legend(x=55,y=max(tableByBirthYear)-1000, legend=as.character(meanBirthYears), col=1:dim(tableByBirthYear)[1], lwd=2 )
  title(paste("DAW TABLE of", type, "by mean birth year", sep=" "), outer=TRUE)
  
}

# table<-table1Male
# plotTablePerCY(table, "male")
# plotTableMeanBirthYear(table, "male")

############################################################################

plotOutputbyDiseaseClass<-function(output_pre){
  quartz()
  par(mfrow=c(3,2), oma=c(0,0,2,0))
  
for(c in 2:dim(output_pre)[2]){
  
  if(c %in% 2:10){
    yrange<-range(output_pre[,2:10])
    if(c==2){
      plot(output_pre$time, output_pre[,c], type='l', lty=c, col=c, lwd=2, ylab=" Numbers", ylim=yrange, main="Susceptibles")
      legend(x=output_pre$time[2]-100, y=yrange[2]-10, legend=c("0-5", "5-15", "15-25", "25-35", "35-45", "45-55", "55-65","65-75", ">75"), lty=2:10, col=2:10)
    }
    if(c>2)  
      lines(output_pre$time, output_pre[,c], lty=c, lwd=1, col=c)
  }

  
  if(c %in% 11:19){
    yrange<-range(output_pre[,11:19])
    if(c==11){
      plot(output_pre$time, output_pre[,c], type='l', lty=c, col=c, lwd=2, ylab=" Numbers", ylim=yrange, main="Latent_a")
      legend(x=output_pre$time[2]-100, y=yrange[2], legend=c("0-5", "5-15", "15-25", "25-35", "35-45", "45-55", "55-65","65-75", ">75"), lty=11:19, col=11:19)
    }
    if(c>11)  
      lines(output_pre$time, output_pre[,c], lty=c, lwd=1, col=c)
  }
  
  if(c %in% 20:28){
    yrange<-range(output_pre[,20:28])
    if(c==20){
      plot(output_pre$time, output_pre[,c], type='l', lty=c, col=c, lwd=2, ylab=" Numbers", ylim=yrange, main="Latent_b")
      legend(x=output_pre$time[2]-100, y=yrange[2], legend=c("0-5", "5-15", "15-25", "25-35", "35-45", "45-55", "55-65","65-75", ">75"), lty=20:28, col=20:28)
    }
    if(c>20)  
      lines(output_pre$time, output_pre[,c], lty=c, lwd=1, col=c)
  }
  
  if(c %in% 29:37){
    yrange<-range(output_pre[,29:37])
    if(c==29){
      plot(output_pre$time, output_pre[,c], type='l', lty=c, col=c, lwd=2, ylab=" Numbers", ylim=yrange, main="Infected")
      legend(x=output_pre$time[2]-100, y=yrange[2], legend=c("0-5", "5-15", "15-25", "25-35", "35-45", "45-55", "55-65","65-75", ">75"), lty=29:37, col=29:37)
    }
    if(c>29)  
      lines(output_pre$time, output_pre[,c], lty=c, lwd=1, col=c)
  }
  
  if(c %in% 38:46){
    yrange<-range(output_pre[,38:46])
    if(c==38){
      plot(output_pre$time, output_pre[,c], type='l', lty=c, col=c, lwd=2, ylab=" Numbers", ylim=yrange, main="Recovered")
      legend(x=output_pre$time[2]-100, y=yrange[2], legend=c("0-5", "5-15", "15-25", "25-35", "35-45", "45-55", "55-65","65-75", ">75"), lty=38:46, col=38:46)
    }
    if(c>38)  
      lines(output_pre$time, output_pre[,c], lty=c, lwd=1, col=c)
  }
  
  
}# end of 'for' loop

 title("Output by Disease Class", outer=TRUE) 
}

plotOutputByAge<-function(output_pre){
  quartz()
  par(mfrow=c(3,3), oma=c(0,0,2,0))
  maintext<-""
  for(c in 2:10){
    if(c==2) maintext<-"Age 0-5"
    if(c==3) maintext<-"Age 5-15"
    if(c==4) maintext<-"Age 15-25"
    if(c==5) maintext<-"Age 25-35"
    if(c==6) maintext<-"Age 35-45"
    if(c==7) maintext<-"Age 45-55"
    if(c==8) maintext<-"Age 55-65"
    if(c==9) maintext<-"Age 65-75"
    if(c==10) maintext<-"Age >75"
    yrange<-range(output_pre[,c(c,c+9,c+18,c+27,c+36)])
    plot(output_pre$time, output_pre[,c],type='l', lty=c, lwd=1, col=1, ylim=yrange, ylab="Numbers", main=maintext)
    legend(x=output_pre$time[2]-100, y=yrange[2], legend=c("S", "L_a", "L_b", "I", "S_r"), lty=c, col=1:5 )
    for(i in 1:4){
      lines(output_pre$time, output_pre[,c+(i*9)], lty=c, lwd=2, col=i+1)
    }
    
  }# end of 'for' loop
  title("Output by Age Class", outer=TRUE)
}

getEqbmMortalitybyAgeClass<-function(output_pre, mu_I){
  eqbm<-output_pre[dim(output_pre)[1], ]
  
  #calculate total population in each age group
  N_tot<-rep(0, times=9)
  for(i in 2:10){
    N_tot[i-1]<-sum(eqbm[c(i,i+9, i+18, i+27, i+36)])
  }
  
  
  #columns 29:37 are the Infected classes of the 9 age classes
  modelMortality<-(eqbm[29:37]/N_tot)*mu_I*10^6
  modelMortality
}

ageStrTBModel_mortality<-function(mult, beta1){
  nstart=c(rep(3*10^6, times=9), rep(0, times=9), rep(0, times=9), c(0,1000, rep(0, times=7)), rep(0, times=9))
  # mult<-40
  # beta1=3*24/365
  parameters<-getParameters(9, mult, beta1)
  time<-seq(from=1, to=365*20, by=1 )
  
  output_pre<-as.data.frame(ode(nstart,time,TBmodel_9ageclasses,parameters))
  eqbm<-output_pre[dim(output_pre)[1], ]
  modelOP<-as.numeric(getEqbmMortalitybyAgeClass(output_pre, parameters$mu_I))
  modelOP*365
}
# mult=40
# beta1<-3*24/365
# ageStrTBModel_mortality(mult, beta1)
# 

# beta_o and tau are parameters of the time varying beta function, diff to tau_in and tau_out
ageStrTBModel_mortality_3params<-function(mult, beta_o, tau, gamma, alpha, eqbm, t_year){
 # nstart=c(rep(3*10^6, times=9), rep(0, times=9), rep(0, times=9), c(0,1000, rep(0, times=7)), rep(0, times=9))
  nstart<-as.numeric(eqbm[-1])
  # mult<-40
  # beta1=3*24/365
  # t_year can be any year, to compare with DAW table it could be 1860 (column 1) to 1940(column 9), in steps of 10.
  # beta1<-getBeta_decayExp(beta_o, tau, t_year)
  # parameters<-getParameters(9, mult, beta1)
  parameters_timevarybeta<-getParameters_timevarybeta(9, mult, beta_o, tau, gamma, alpha)
  n_years<-t_year-1850 # start year
  time<-seq(from=1, to=365*n_years, by=1 )
  
  output_pre<-as.data.frame(ode(nstart,time,TBmodel_9ageclasses_timevarybeta,parameters_timevarybeta))
  modelOP<-as.numeric(getEqbmMortalitybyAgeClass(output_pre, parameters_timevarybeta$mu_I))
  modelOP*365
}
#   
## This function returns the value of beta given the starting value and the decay
## It defines beta as a decaying exponential function.
## t will be a year from 1850 to 1940
## gamma is the value that beta will asymtotically converge to
getBeta_decayExp<-function(beta_o, tau,gamma, t){
#beta_o<-0.1001
#tau<-0.01
t_values<-seq(from=1850, to=1940, by=10)
beta<-(beta_o * exp( - tau * ( t_values-1850)) ) + gamma
beta_func<-approxfun(t_values-1850, beta, method="linear")
return(beta_func(t))

}

#flag indicates if we start a new plot (=1) or overlay on existing plot(=0)
plotBeta_decayExp<-function(beta_o, tau,gamma, flag){
 
    
    t_values<-seq(from=1850, to=1940, by=10)
    beta<-(beta_o * exp( - tau * ( t_values-1850))) + gamma
    beta_func<-approxfun(t_values-1850, beta, method="linear")

    if(flag==1){
      quartz()
      plot(t_values-1850, beta_func(t_values-1850), type='b', lwd=2, pch=16, lty=1, main="time varying beta", ylim=c(0,1))
      
    }
    if(flag==0){
      lines(t_values-1850, beta_func(t_values-1850),type='b', lwd=2, lty=2)
    }
     
}

#here alpha1 refers to the intensity of re-infection after first infection. Don't confuse with alpha here, which is asssociated with the time varying beta function
ageStrTBModel_mortality_4params<-function(mult, beta_o, tau, alpha1, gamma, alpha, eqbm, t_year){
  # nstart=c(rep(3*10^6, times=9), rep(0, times=9), rep(0, times=9), c(0,1000, rep(0, times=7)), rep(0, times=9))
  nstart<-as.numeric(eqbm[-1])
  # mult<-40
  # beta1=3*24/365
  # t_year can be any year, to compare with DAW table it could be 1860 (column 1) to 1940(column 9), in steps of 10.
  # beta1<-getBeta_decayExp(beta_o, tau, t_year)
  # parameters<-getParameters(9, mult, beta1)
  #parameters_timevarybeta<-getParameters_timevarybeta(9, mult, beta_o, tau, gamma, alpha)
  parameters_timevarybeta<-getParameters_timevarybeta_alpha(9, mult, beta_o, tau, alpha1, gamma, alpha) #alpha1 is the alpha we refer to
  
  n_years<-t_year-1860
  time<-seq(from=1, to=365*n_years, by=1 )
  
  output_pre<-as.data.frame(ode(nstart,time,TBmodel_9ageclasses_timevarybeta,parameters_timevarybeta))
  modelOP<-as.numeric(getEqbmMortalitybyAgeClass(output_pre, parameters_timevarybeta$mu_I))
  modelOP*365
}
#   
## This function returns the value of beta given the starting value and the decay
# ## It defines beta as a decaying exponential function.
# ## t will be a year from 1850 to 1940
# getBeta_decayExp<-function(beta_o, tau, t){
#   #beta_o<-0.1001
#   #tau<-0.01
#   t_values<-seq(from=1850, to=1940, by=10)
#   beta<-beta_o * exp( - tau * ( t_values-1860))
#   beta_func<-approxfun(t_values-1860, beta, method="linear")
#   return(beta_func(t))
#   
#   
#   quartz()
#   plot(t_values-1860, beta_func(t_values-1860), type='b', lwd=2, pch=16, lty=1, main="time varying beta")
#   points(t, beta_func(t), pch=18, col="red")
# }
# 
# This function checks if we have reached equilibrium. 
# t_period is the time period over which we check the population change, expressed in days.
# change_limit is the largest change in population over the time period that
# can be considered as eqbm. Expressed as a percentage.

checkEqbm<-function(output_pre, t_period, change_limit){
  isEqbm_flag<-FALSE
  t_max<-output_pre[dim(output_pre)[1],1]
  output_allCompartments<-matrix(0, nrow = dim(output_pre)[1], ncol = 5)
  CompIndex<- c(2,11,20,29,38)
  for(c in 1:length(CompIndex))
    output_allCompartments[,c]<-as.matrix(rowSums(output_pre[,c:(c+8)]) )
  
  total_pop<-rowSums(output_allCompartments)
  
  Infected<-output_allCompartments[,4]/total_pop
    
  percentage_change<-100*( abs(Infected[dim(output_pre)[1]] - Infected[ dim(output_pre)[1] - t_period])/Infected[ dim(output_pre)[1] - t_period])
  
  if(percentage_change<change_limit)
    isEqbm_flag <- TRUE
  
  isEqbm_flag  

}

#function to plot each disease class across age at a point in time
#disease class Index varies from 1 to 5 ( S, L_a, L_b, I, R)
plotDiseaseClassbyAge<-function(output_all){
  
  output_last<-output_all[dim(output_all)[1], ]
  AgeClass<-1:9
  
  output_mx<-matrix(as.numeric(output_last[-1]), nrow=5, ncol=9, byrow=TRUE)
  output_total<-colSums(output_mx)
  
  
  quartz()
  par(mfrow=c(3,2), oma=c(0,0,2,0))
  for(diseaseClassIndex in 1:5){
    
    titleString<-character()
    if(diseaseClassIndex==1) titleString<-"S"
    if(diseaseClassIndex==2) titleString<-"L_a"
    if(diseaseClassIndex==3) titleString<-"L_b"
    if(diseaseClassIndex==4) titleString<-"I"
    if(diseaseClassIndex==5) titleString<-"R"
    
    
    plot(AgeClass, output_mx[diseaseClassIndex,]/output_total, type='b', lty=1, col="blue", pch=16, ylab="Numbers", xlab="Age Classes", main=paste0("Disease Class ",titleString) )
    
  }
   titleString<-" ALL "
   plot(AgeClass, output_total, type='b', lty=1, col="blue", pch=16, ylab="Numbers", xlab="Age Classes", main=paste0("Disease Class ",titleString) )
   
  
  
}