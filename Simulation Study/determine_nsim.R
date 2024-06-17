library(ggplot2)
library(dplyr)
library(tidyr)

setwd("/home/nadine/Documents/UHasselt/Year 2/Thesis/Experiments/Scenario 2A")

infections_0 = read.csv("scenario_1_0_infections.csv")
new_infections_0 = read.csv("scenario_1_0_new_infections.csv")
final_size_0 = read.csv("scenario_1_0_finalsize.csv")
CP_0 = read.csv("scenario_1_0_CP.csv")

final_size_COVID = data.frame(sim = c(0:1000),
                              q = NA, 
                              disease = "COVID-19",
                              quantity = ("Final Size"))

final_size_Influenza = data.frame(sim = c(0:1000),
                                  q = NA, 
                                  disease = "Influenza",
                                  quantity = ("Final Size"))

for(i in 1:nsim){
  final_size_COVID$q[i] = mean(final_size_0$COVID[1:i]/10000)
}

for(i in 1:nsim){
  final_size_Influenza$q[i] = mean(final_size_0$Influenza[1:i]/10000)
}

final_sizes = rbind(final_size_COVID, final_size_Influenza)

ggplot(final_sizes)+
  aes(x=sim, y = final_size, group = disease, color = disease)+
  geom_line()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))+
  scale_color_manual(values=c("#CA0020", "#00A9FF"))+
  labs(x = "Number of Simulations", y = "Final Size", color = "Disease")+
  theme(legend.position = c(0.8,0.5))


#######################################################################################"
  
CP_COVID = data.frame(sim = c(0:1000),
                                q = NA,
                                disease = "COVID-19",
                                quantity = ("Cases at Peak"))

CP_Influenza = data.frame(sim = c(0:1000),
                                  q = NA, 
                                  disease = "Influenza",
                                  quantity = ("Cases at Peak"))

day_COVID = data.frame(sim = c(0:1000),
                      q = NA,
                      disease = "COVID-19",
                      quantity = ("Day of the Peak"))

day_Influenza = data.frame(sim = c(0:1000),
                          q = NA,
                          disease = "Influenza",
                          quantity = ("Day of the Peak"))

for(i in 1:nsim){
  CP_COVID$q[i] = mean(CP_0[CP_0$disease==1,]$CP[1:i]/10000)
  day_COVID$q[i] = mean(CP_0[CP_0$disease==1,]$piek[1:i]/500)
}

for(i in 1:nsim){
  CP_Influenza$q[i] = mean(CP_0[CP_0$disease==2,]$CP[1:i]/10000)
  day_Influenza$q[i] = mean(CP_0[CP_0$disease==2,]$piek[1:i]/500)
}

CP = rbind(CP_COVID, CP_Influenza)

ggplot(CP)+
  aes(x=sim, y = CP, group = disease, color = disease)+
  geom_line()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))+
  scale_color_manual(values=c("#CA0020", "#00A9FF"))+
  labs(x = "Number of Simulations", y = "Cases at Peak", color = "Disease")+
  theme(legend.position = c(0.8,0.5))

ggplot(CP)+
  aes(x=sim, y = peak, group = disease, color = disease)+
  geom_line()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))+
  scale_color_manual(values=c("#CA0020", "#00A9FF"))+
  labs(x = "Number of Simulations", y = "Day of the Peak", color = "Disease")+
  theme(legend.position = c(0.8,0.8))



summmary = rbind(final_size_COVID,
                 final_size_Influenza,
                 CP_COVID,
                 CP_Influenza,
                 day_COVID,
                 day_Influenza)

ggplot(summmary)+
  aes(x=sim, y = q, group = interaction(disease, quantity), color = disease)+
  geom_line()+
  facet_wrap(~quantity, ncol = 1)+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))+
  scale_color_manual(values=c("#CA0020", "#00A9FF"))+
  labs(x = "Number of Simulations", y = "Standardized Quantity", color = "Disease")+
  theme(legend.position = c(0.2,0.8))

##################################################################################################

infections_summary = rbind(data.frame(sim = infections_0$sim, day = infections_0$day, infections = infections_0$i_1, disease = "COVID-19"),
                           data.frame(sim = infections_0$sim, day = infections_0$day, infections = infections_0$i_2, disease = "Influenza"))

length_of_outbreaks_COVID = c()
for(i in 1:nsim){
  temp = infections_summary[infections_summary$sim==i & infections_summary$disease=="COVID-19",]
  circulating = temp[temp$infections != 0,]$day
  end_of_outbreak = max(circulating)
  if(end_of_outbreak>=490){
    end_of_outbreak = 450
  }
  length_of_outbreaks_COVID = c(length_of_outbreaks_COVID, end_of_outbreak)
}

length_of_outbreaks_Influenza = c()
for(i in 1:nsim){
  temp = infections_summary[infections_summary$sim==i & infections_summary$disease=="Influenza",]
  circulating = temp[temp$infections != 0,]$day
  end_of_outbreak = max(circulating)
  if(end_of_outbreak>=490){
    end_of_outbreak = 450
  }
  length_of_outbreaks_Influenza = c(length_of_outbreaks_Influenza, end_of_outbreak)
}

results_outbreaks = rbind(data.frame(sim = c(1:1000), length = length_of_outbreaks_COVID, disease = "COVID-19"),
                          data.frame(sim = c(1:1000), length = length_of_outbreaks_Influenza, disease = "Influenza"))

ggplot(data = results_outbreaks)+
  aes(x = length, fill = disease)+
  geom_histogram(binwidth = 7)+
  scale_fill_manual(values=c("#CA0020", "#00A9FF"))+
  labs(x = "Day of the Last Infection", y = "Count", fill = "Disease")+
  theme(legend.position = c(0.8,0.8))+
  geom_vline(xintercept = max(results_outbreaks$length))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14))








