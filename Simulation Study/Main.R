set.seed(25091991)
library(dplyr)
library(ggplot2)
library(tidyr)
library(parallel)

#setwd("/home/nadine/Documents/UHasselt/Year 2/Thesis/Multithread")
source("/home/nadine/Documents/UHasselt/Year 2/Thesis/Experiments/sim_1.R")

nsim = 1000                    # number of simulations 

start.time <- Sys.time()

number_of_simulations = c(1:nsim)
results = mclapply(number_of_simulations, sim, mc.cores = 5)

end.time <- Sys.time()
round(end.time - start.time,2)

setwd("/home/nadine/Documents/UHasselt/Year 2/Thesis/Experiments/Scenario 7")

###############################################################################################################
################################################### Results ################################################### 
###############################################################################################################

iterations = list()
new_iterations = list()
number_of_days = c()

for(s in 1:nsim){
  iterations[[s]] = (results[[s]][[1]])
  new_iterations[[s]] = (results[[s]][[2]])
}


number_of_days = (results[[1]][[3]])
N = (results[[1]][[4]])



# Look at total number of infections
## create a data frame containing the total number of symptomatic and asymptomatic infections for diseases 1 and 2 for each time step and simulation
save_infections = function(){
  infections = data.frame()
  for(z in 1:nsim){
    temp = as.data.frame(iterations[z])  
    inf = data.frame(sim = NA, day = c(0:number_of_days), i_a_1 = NA, i_s_1 = NA, i_a_2 = NA, i_s_2 = NA, e_1 = NA, e_2 = NA, i_1 = NA, i_2 = NA, i_s_12 = NA, i_12 = NA)
    inf$sim = z
    for(d in 0:number_of_days+1){
      inf$i_a_1[d] = temp$I1a_S1S2[d] + temp$I1a_S1R2[d] + temp$I1a_V1S2[d] + temp$I1a_V1R2[d] + temp$I1a_S1V2[d] + temp$I1a_S1VR2[d] + temp$I1a_V1V2[d] + temp$I1a_V1VR2[d]
      inf$i_s_1[d] = temp$I1s_S1S2[d] + temp$I1s_S1R2[d] + temp$I1s_V1S2[d] + temp$I1s_V1R2[d] + temp$I1s_S1V2[d] + temp$I1s_S1VR2[d] + temp$I1s_V1V2[d] + temp$I1s_V1VR2[d]
      inf$i_a_2[d] = temp$I2a_S1S2[d] + temp$I2a_R1S2[d] + temp$I2a_V1S2[d] + temp$I2a_VR1S2[d] + temp$I2a_S1V2[d] + temp$I2a_R1V2[d] + temp$I2a_V1V2[d] + temp$I2a_VR1V2[d]
      inf$i_s_2[d] = temp$I2s_S1S2[d] + temp$I2s_R1S2[d] + temp$I2s_V1S2[d] + temp$I2s_VR1S2[d] + temp$I2s_S1V2[d] + temp$I2s_R1V2[d] + temp$I2s_V1V2[d] + temp$I2s_VR1V2[d]
      inf$e_1[d] = temp$E1_S1R2[d] + temp$E1_S1S2[d] + temp$E1_S1V2[d] + temp$E1_S1VR2[d] + temp$E1_V1R2[d] + temp$E1_V1S2[d] + temp$E1_V1V2[d] + temp$E1_V1VR2[d]
      inf$e_2[d] = temp$E2_R1S2[d] + temp$E2_R1V2[d] + temp$E2_S1S2[d] + temp$E2_S1V2[d] + temp$E2_V1S2[d] + temp$E2_V1V2[d] + temp$E2_VR1S2[d] + temp$E2_VR1V2[d]
      inf$i_1[d] = inf$i_a_1[d] + inf$i_s_1[d]
      inf$i_2[d] = inf$i_a_2[d] + inf$i_s_2[d]
      inf$i_s_12[d] = inf$i_s_1[d] + inf$i_s_2[d]
      #inf$i_12[d] = inf$i_1[d]+inf$i_2[d]
    }
    infections = rbind(infections,inf)
  }
  return(infections)
}

infections = save_infections()
infections$i_12 = infections$i_1 + infections$i_2

write.csv(infections, "scenario_7_0_infections.csv", row.names=FALSE)


######################################################################################################################


## create a data frame containing the total number of NEW symptomatic and asymptomatic infections for diseases 1 and 2 for each time step and simulation

save_new_infections = function(){
  new_infections = data.frame()
  for(z in 1:nsim){
    temp = as.data.frame(new_iterations[z])  
    inf = data.frame(sim = NA, day = c(0:number_of_days), new_i_a_1 = NA, new_i_s_1 = NA, new_i_a_2 = NA, new_i_s_2 = NA, V1 = NA, V2 = NA)
    inf$sim = z
    for(d in 0:number_of_days+1){
      inf$new_i_a_1[d] = temp$I1a_S1S2[d] + temp$I1a_S1R2[d] + temp$I1a_V1S2[d] + temp$I1a_V1R2[d] + temp$I1a_S1V2[d] + temp$I1a_S1VR2[d] + temp$I1a_V1V2[d] + temp$I1a_V1VR2[d]
      inf$new_i_s_1[d] = temp$I1s_S1S2[d] + temp$I1s_S1R2[d] + temp$I1s_V1S2[d] + temp$I1s_V1R2[d] + temp$I1s_S1V2[d] + temp$I1s_S1VR2[d] + temp$I1s_V1V2[d] + temp$I1s_V1VR2[d]
      inf$new_i_a_2[d] = temp$I2a_S1S2[d] + temp$I2a_R1S2[d] + temp$I2a_V1S2[d] + temp$I2a_VR1S2[d] + temp$I2a_S1V2[d] + temp$I2a_R1V2[d] + temp$I2a_V1V2[d] + temp$I2a_VR1V2[d]
      inf$new_i_s_2[d] = temp$I2s_S1S2[d] + temp$I2s_R1S2[d] + temp$I2s_V1S2[d] + temp$I2s_VR1S2[d] + temp$I2s_S1V2[d] + temp$I2s_R1V2[d] + temp$I2s_V1V2[d] + temp$I2s_VR1V2[d]
    }
    inf$V1 = cumsum(temp$V1_new + temp$V12_new_2)
    inf$V2 = cumsum(temp$V2_new + temp$V12_new_1)
    new_infections = rbind(new_infections,inf)
  }
  return(new_infections)
}


new_infections = save_new_infections()
write.csv(new_infections, "scenario_7_0_new_infections.csv", row.names=FALSE)

###############################################################################################################################################################

# final size 

final_size = function(){
  total_final_size = 0
  for(i in 1:nsim){
    temp = as.data.frame(iterations[i])
    f = temp[temp$day == number_of_days,]$R12_S1S2 + 
      temp[temp$day == number_of_days,]$R12_S1V2 + 
      temp[temp$day == number_of_days,]$R12_V1S2 + 
      temp[temp$day == number_of_days,]$R12_V1V2 + 
      temp[temp$day == number_of_days,]$R1_S1S2 + 
      temp[temp$day == number_of_days,]$R1_S1V2 + 
      temp[temp$day == number_of_days,]$R1_V1S2 + 
      temp[temp$day == number_of_days,]$R1_V1V2 + 
      temp[temp$day == number_of_days,]$D1_S1S2 + 
      temp[temp$day == number_of_days,]$D1_S1V2 + 
      temp[temp$day == number_of_days,]$D1_V1S2 + 
      temp[temp$day == number_of_days,]$D1_V1V2 +
      temp[temp$day == number_of_days,]$R2_S1S2 + 
      temp[temp$day == number_of_days,]$R2_S1V2 + 
      temp[temp$day == number_of_days,]$R2_V1S2 + 
      temp[temp$day == number_of_days,]$R2_V1V2 + 
      temp[temp$day == number_of_days,]$D2_S1S2 + 
      temp[temp$day == number_of_days,]$D2_S1V2 + 
      temp[temp$day == number_of_days,]$D2_V1S2 + 
      temp[temp$day == number_of_days,]$D2_V1V2
    total_final_size = c(total_final_size, f)
  }
  
  final_size_1 = 0
  for(i in 1:nsim){
    temp = as.data.frame(iterations[i])
    f = temp[temp$day == number_of_days,]$R1_S1S2 + 
      temp[temp$day == number_of_days,]$R1_S1V2 + 
      temp[temp$day == number_of_days,]$R1_V1S2 + 
      temp[temp$day == number_of_days,]$R1_V1V2 + 
      temp[temp$day == number_of_days,]$D1_S1S2 + 
      temp[temp$day == number_of_days,]$D1_S1V2 + 
      temp[temp$day == number_of_days,]$D1_V1S2 + 
      temp[temp$day == number_of_days,]$D1_V1V2
    final_size_1 = c(final_size_1, f)
  }
  
  final_size_2 = 0
  for(i in 1:nsim){
    temp = as.data.frame(iterations[i])
    f = temp[temp$day == number_of_days,]$R2_S1S2 + 
      temp[temp$day == number_of_days,]$R2_S1V2 + 
      temp[temp$day == number_of_days,]$R2_V1S2 + 
      temp[temp$day == number_of_days,]$R2_V1V2 + 
      temp[temp$day == number_of_days,]$D2_S1S2 + 
      temp[temp$day == number_of_days,]$D2_S1V2 + 
      temp[temp$day == number_of_days,]$D2_V1S2 + 
      temp[temp$day == number_of_days,]$D2_V1V2
    final_size_2 = c(final_size_2, f)
  }
  
  final_size_12 = 0
  for(i in 1:nsim){
    temp = as.data.frame(iterations[i])
    f = temp[temp$day == number_of_days,]$R12_S1S2 + 
      temp[temp$day == number_of_days,]$R12_S1V2 + 
      temp[temp$day == number_of_days,]$R12_V1S2 + 
      temp[temp$day == number_of_days,]$R12_V1V2
    final_size_12 = c(final_size_12, f)
  }
  return(data.frame(sim = c(0:nsim), COVID = final_size_1, Influenza = final_size_2, Both = final_size_12, Total = total_final_size))
}

final_size = final_size()

write.csv(final_size, "scenario_7_0_finalsize.csv", row.names=FALSE)

##############################################################################################################################################"

# calculate CP for each simulation

CP = function(){
  CP = data.frame(sim = rep(c(1:nsim),3), 
                  disease = c(rep(1,nsim), rep(2,nsim), rep(12,nsim)), CP = NA, piek = NA)
  for(s in 1:nsim){
    CP[CP$sim==s & CP$disease==1,]$CP = max(infections[infections$sim == s,]$i_1)
    piek_temp_1 = as.vector(infections[infections$sim==s & infections$i_1==CP[CP$sim==s & CP$disease==1,]$CP,]$day)
    CP[CP$sim==s & CP$disease==1,]$piek = median(piek_temp_1)
    CP[CP$sim==s & CP$disease==2,]$CP = max(infections[infections$sim == s,]$i_2)
    piek_temp_2 = as.vector(infections[infections$sim==s & infections$i_2==CP[CP$sim==s & CP$disease==2,]$CP,]$day)
    CP[CP$sim==s & CP$disease==2,]$piek = median(piek_temp_2)
    CP[CP$sim==s & CP$disease==12,]$CP = max(infections[infections$sim == s,]$i_12)
    piek_temp_12 = as.vector(infections[infections$sim==s & infections$i_12==CP[CP$sim==s & CP$disease==12,]$CP,]$day)
    CP[CP$sim==s & CP$disease==12,]$piek = median(piek_temp_12)
  }
  return(CP)
}

CP = CP()
write.csv(CP, "scenario_7_0_CP.csv", row.names=FALSE)

#############################################################################################################################

vacc_cov_1 = 0.85
vacc_cov_2 = 0.226

vaccinations = NA
vaccination_COVID_threshold = NA
vaccination_Influenza_threshold = NA

for(s in 1:nsim){
  
  compartments = as.data.frame(results[[s]][[1]])
  compartment_flow = as.data.frame(results[[s]][[2]])
  N = as.numeric(results[[s]][[4]])
  
  degree_of_vaccination = data.frame(sim = s, day = compartment_flow$day,
                                     COVID = (cumsum(compartment_flow$V1_new)+cumsum(compartment_flow$V12_new_1))/N,
                                     Influenza = (cumsum(compartment_flow$V2_new) + cumsum(compartment_flow$V12_new_2))/N,
                                     Both = (cumsum(compartment_flow$V12_new_1)+cumsum(compartment_flow$V12_new_2))/N)
  vaccinations = rbind(vaccinations, degree_of_vaccination)
  
  vacc_COVID = min(degree_of_vaccination[degree_of_vaccination$COVID>vacc_cov_1,]$day)
  vacc_Influenza = min(degree_of_vaccination[degree_of_vaccination$Influenza>vacc_cov_2,]$day)
  vaccination_COVID_threshold = c(vaccination_COVID_threshold, vacc_COVID)
  vaccination_Influenza_threshold = c(vaccination_Influenza_threshold, vacc_Influenza)
}

write.csv(vaccinations, "scenario_6_0_vaccinations.csv", row.names=FALSE)



  
##############################################################################################################################

fs = data.frame(sim = c(1:250), final_size_both = NA, final_size_1 = NA, final_size_2 = NA)
for(i in 1:250){
  temp_1 = mean((final_size$COVID[1:i]+final_size$Both[1:i])/N)
  temp_2 = mean((final_size$Influenza[1:i]+final_size$Both[1:i])/N)
  temp_both = temp_1+temp_2
  if(i>1){
    fs$final_size_both[i] = temp_both
    fs$final_size_1[i] = temp_1
    fs$final_size_2[i] = temp_2
  }
}
fs


ggplot(fs)+
  #geom_line(aes(x=sim, y=final_size_both), color = "purple")+
  geom_line(aes(x=sim, y=final_size_1), color = "red")+
  geom_line(aes(x=sim, y=final_size_2), color = "blue")+
  labs(title="Final size with increasing number of simulations", x ="number of simulations", y = "final size")


###############################################################

day = data.frame(sim = c(1:nsim), end_1 = NA)

for(i in 1:nsim){
  temp = infections[infections$sim==i,]
  t = NA
  for(d in 1:500){
    if((temp$e_1[d] + temp$i_1[d] + temp$e_2[d] + temp$i_2[d])==0){
      t = c(t,d)
    }
  }
  t = t[-1]
  day$end_1[i] = min((t))
}

day

hist(day$end_1)

