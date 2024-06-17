library(ggplot2)
library(dplyr)
library(tidyr)

setwd("/home/nadine/Documents/UHasselt/Year 2/Thesis/Experiments/Scenario 1")

infections_01 = read.csv("scenario_01_infections.csv", sep = ",")
new_infections_01 = read.csv("scenario_01_new_infections.csv", sep = ",")
final_size_01 = read.csv("scenario_01_finalsize.csv")
CP_01 = read.csv("scenario_01_CP.csv")

infections_02 = read.csv("scenario_02_infections.csv", sep = ",")
new_infections_02 = read.csv("scenario_02_new_infections.csv", sep = ",")
final_size_02 = read.csv("scenario_02_finalsize.csv")
CP_02 = read.csv("scenario_02_CP.csv")

setwd("/home/nadine/Documents/UHasselt/Year 2/Thesis/Experiments/Scenario 2A")

infections_0 = read.csv("scenario_1_0_infections.csv")
new_infections_0 = read.csv("scenario_1_0_new_infections.csv")
final_size_0 = read.csv("scenario_1_0_finalsize.csv")
CP_0 = read.csv("scenario_1_0_CP.csv")

N = 10002
nsim = 1000

##################################################################################################################################################
# Plot Prevalence
##################################################################################################################################################

# create data frames:
infections_summary = rbind(data.frame(sim = infections_01$sim, day = infections_01$day, infections = infections_01$i_1, disease = "COVID-19", interaction = "Independence"),
                           data.frame(sim = infections_02$sim, day = infections_02$day, infections = infections_02$i_2, disease = "Influenza", interaction = "Independence"),
                           data.frame(sim = infections_0$sim, day = infections_0$day, infections = infections_0$i_1, disease = "COVID-19", interaction = "Co-circulation"),
                           data.frame(sim = infections_0$sim, day = infections_0$day, infections = infections_0$i_2, disease = "Influenza", interaction = "Co-circulation"),
                           data.frame(sim = infections_0$sim, day = infections_0$day, infections = infections_0$i_12, disease = "Sum", interaction = "Co-circulation"))

# plot the prevalences of the random selection of simulations
ggplot(infections_summary[infections_summary$disease %in% c("COVID-19","Influenza") & infections_summary$sim %in% round(runif(30,1,1000),0),])+
  geom_line(aes(x=day, y=infections/N, group = interaction(sim, disease, interaction), color = disease),size = 0.2)+
  facet_wrap(~factor(interaction, levels = c("Independence", "Co-circulation")), ncol = 1)+
  labs(x ="Day", y = "Relative Prevalence", color = "Disease")+
  theme(legend.position = c(0.9,0.8), legend.background = element_rect(fill = "white", color = "black"))+
  scale_y_continuous(breaks = seq(0,0.3,0.05))+
  scale_color_manual(values=c("#CA0020", "#00A9FF"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14))
  

##################################################################################################################################################
# Plot Attack Rate
##################################################################################################################################################

# create a data frame that contains the necessary information about the final sizes
AR_summary = rbind(data.frame(sim = final_size_01$sim, final_size = final_size_01$COVID + final_size_01$Both, disease = factor("COVID-19"), interaction = factor("Independence")),
                   data.frame(sim = final_size_02$sim, final_size = final_size_02$Influenza + final_size_02$Both, disease = factor("Influenza"), interaction = factor("Independence")),
                   data.frame(sim = final_size_0$sim, final_size = final_size_0$COVID + final_size_0$Both, disease = factor("COVID-19"), interaction = factor("Co-circulation")),
                   data.frame(sim = final_size_0$sim, final_size = final_size_0$Influenza + final_size_0$Both, disease = factor("Influenza"), interaction = factor("Co-circulation")))
AR_summary$AR = AR_summary$final_size/N

# createa data frame that contains the mean and 95\% percentile range of the AR (for all simulations and for outbreaks only)
AR_summary_statistics = AR_summary[AR_summary$disease %in% c("COVID-19", "Influenza"),] %>% 
  group_by(interaction, disease) %>% 
  summarise(mean_AR = round(mean(AR),4), 
            AR_025 = round(quantile(AR,0.025),4), 
            AR_975 = round(quantile(AR,0.975),4),
            AR_median = round(quantile(AR,0.5),4))
AR_summary_statistics$extinct = "All simulations"

AR_summary_statistics_nonextinct = AR_summary[AR_summary$AR>0.1 & AR_summary$disease %in% c("COVID-19", "Influenza"),] %>% 
  group_by(interaction, disease) %>% 
  summarise(mean_AR = round(mean(AR),4), 
            AR_025 = round(quantile(AR,0.025),4), 
            AR_975 = round(quantile(AR,0.975),4),
            AR_median = round(quantile(AR,0.5),4))
AR_summary_statistics_nonextinct$extinct = "Outbreaks"

AR_summary_statistics = rbind(AR_summary_statistics, AR_summary_statistics_nonextinct)
AR_summary_statistics$int = c(0,0,1,1,0,0,1,1)

# plot mean AR with 95\% percentile interval for both diseases, with and without interaction, all simulations and only outbreaks
ggplot()+
  geom_segment(data = AR_summary_statistics[AR_summary_statistics$disease=="COVID-19",], 
               aes(x=AR_025,xend=AR_975,y=int+0.1,yend=int+0.1, group = interaction(int, extinct), color = disease))+
  geom_point(data = AR_summary_statistics[AR_summary_statistics$disease=="COVID-19",],
             aes(x = mean_AR, y = int+0.1, group = interaction(int, extinct), color = disease))+
  geom_segment(data = AR_summary_statistics[AR_summary_statistics$disease=="Influenza",], 
               aes(x=AR_025,xend=AR_975,y=int-0.1,yend=int-0.1, group = interaction(int, extinct), color = disease))+
  geom_point(data = AR_summary_statistics[AR_summary_statistics$disease=="Influenza",],
             aes(x = mean_AR, y = int-0.1, group = interaction(int, extinct), color = disease))+  labs(x ="Relative Attack Rate", y = " ", color = "Disease")+
  scale_color_manual(values=c("#CA0020", "#00A9FF"))+
  facet_grid(~extinct)+
  scale_y_continuous(breaks=c(0,1), labels = c("Independence", "Co-circulation"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14))+
  theme(legend.background = element_rect(fill = "white", color = "black"))
  

# create a histogram of the attack rates separated by disease and scenario
ggplot(AR_summary[AR_summary$disease %in% c("COVID-19", "Influenza"),])+
  aes(x=AR, group = interaction(disease, interaction), fill = disease)+
  facet_grid(interaction~disease)+
  geom_histogram(binwidth = 0.02)+
  theme(legend.position=c(0.9,0.85), legend.background = element_rect(fill = "white", color = "black"))+
  labs(x="Relative Attack Rate", y="Count", fill = "Disease")+
  scale_fill_manual(values=c("#CA0020", "#00A9FF"))+
theme(axis.text.x = element_text(size=12),
      axis.text.y = element_text(size=12),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14),
      legend.text = element_text(size=12),
      legend.title = element_text(size=12),
      strip.text.x = element_text(size=14),
      strip.text.y = element_text(size=14))

# determine which simulations cause an outbreak
nonextinct_COVID_without_interaction = AR_summary[AR_summary$disease=="COVID-19" & AR_summary$interaction == "Independence" & AR_summary$AR > 0.1,]$sim
nonextinct_COVID_with_interaction = AR_summary[AR_summary$disease=="COVID-19" & AR_summary$interaction == "Co-circulation" & AR_summary$AR > 0.1,]$sim
nonextinct_Influenza_without_interaction = AR_summary[AR_summary$disease=="Influenza" & AR_summary$interaction == "Independence" & AR_summary$AR > 0.1,]$sim
nonextinct_Influenza_with_interaction = AR_summary[AR_summary$disease=="Influenza" & AR_summary$interaction == "Co-circulation" & AR_summary$AR > 0.1,]$sim


# determine the extinction probabilities and store them in an data frame
extinction_probability = data.frame(disease = rep(c("COVID-19", "Influenza"),2), 
                                    interaction = c(rep("Independence",2), rep("Co-circulation",2)),
                                    p = NA)
extinction_probability[extinction_probability$disease=="COVID-19" & extinction_probability$interaction=="Co-circulation",]$p = 1-length(nonextinct_COVID_with_interaction)/nsim
extinction_probability[extinction_probability$disease=="COVID-19" & extinction_probability$interaction=="Independence",]$p = 1-length(nonextinct_COVID_without_interaction)/nsim
extinction_probability[extinction_probability$disease=="Influenza" & extinction_probability$interaction=="Co-circulation",]$p = 1-length(nonextinct_Influenza_with_interaction)/nsim
extinction_probability[extinction_probability$disease=="Influenza" & extinction_probability$interaction=="Independence",]$p = 1-length(nonextinct_Influenza_without_interaction)/nsim


# create a violin plot of the attack rates separated by disease and scenario
ggplot(data = AR_summary[AR_summary$disease %in% c("COVID-19", "Influenza"),], 
       aes(x=interaction,y=AR, group = interaction(disease, interaction), fill=disease))+
  geom_violin()+
  labs(x=" ", y="Relative Attack Rate", fill = "Disease")+
  #geom_point(aes(x=interaction,y=mean_AR, group = interaction(disease, interaction)), data = AR_summary_statistics, size = 2)+
  stat_summary(fun = "mean", geom = "point", color = "black")+
  geom_text(data = AR_summary_statistics[AR_summary_statistics$extinct == "All simulations",], aes(x=interaction, y=round(mean_AR,2), group = interaction(disease, interaction), label=round(mean_AR,3)), hjust=-0.2, vjust=-0.5, size=4)+
  theme(legend.position=c(0.85,0.85), legend.background = element_rect(fill = "white", color = "black"))+
  geom_point(data=extinction_probability, aes(x=interaction, y=p, group = interaction(disease, interaction)), shape=8)+
  geom_text(data = extinction_probability, aes(x=interaction, y=p, group = interaction(disease, interaction), label=p), hjust=-0.2, vjust=-0.5, size=4)+
  facet_grid(~disease)+
  scale_fill_manual(values=c("#CA0020", "#00A9FF"))+
  scale_y_continuous(breaks = c(seq(0,1,0.1)))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))
  

# calculate the range of the epidemic (with mean and 95% CI) for outbreaks
outbreaks_summary = data.frame(disease = rep(c("COVID-19", "Influenza"),2), 
                               interaction = c(rep("Independence",2), rep("Co-circulation",2)),
                               mean = NA,
                               CI_lb = NA,
                               CI_up = NA)
## for COVID 
### Independence
ranges_COVID_no = c()
for(i in nonextinct_COVID_without_interaction){
  temp = infections_01[infections_01$sim==i,]
  ranges_COVID_no = c(ranges_COVID_no,min(temp[temp$i_1 == 0 & temp$e_1==0,]$day))
}
outbreaks_summary[outbreaks_summary$disease=="COVID-19" & outbreaks_summary$interaction=="Independence",]$mean = mean(ranges_COVID_no)
outbreaks_summary[outbreaks_summary$disease=="COVID-19" & outbreaks_summary$interaction=="Independence",]$CI_lb = quantile(ranges_COVID_no, 0.025)
outbreaks_summary[outbreaks_summary$disease=="COVID-19" & outbreaks_summary$interaction=="Independence",]$CI_up = quantile(ranges_COVID_no, 0.975)

### Co-circulation
ranges_COVID_yes = c()
for(i in nonextinct_COVID_with_interaction){
  temp = infections_0[infections_0$sim==i,]
  #temp = infections_summary[infections_summary$disease=="COVID-19" & infections_summary$interaction=="Independence" & infections_summary$sim == i,]
  ranges_COVID_yes = c(ranges_COVID_yes,min(temp[temp$i_1 == 0 & temp$e_1==0,]$day))
}
outbreaks_summary[outbreaks_summary$disease=="COVID-19" & outbreaks_summary$interaction=="Co-circulation",]$mean = mean(ranges_COVID_yes)
outbreaks_summary[outbreaks_summary$disease=="COVID-19" & outbreaks_summary$interaction=="Co-circulation",]$CI_lb = quantile(ranges_COVID_yes, 0.025)
outbreaks_summary[outbreaks_summary$disease=="COVID-19" & outbreaks_summary$interaction=="Co-circulation",]$CI_up = quantile(ranges_COVID_yes, 0.975)

## for Influenza 
### Independence
ranges_Influenza_no = c()
for(i in nonextinct_Influenza_without_interaction){
  temp = infections_02[infections_02$sim==i,]
  if(min(temp[temp$i_2 == 0 & temp$e_2==0,]$day) == Inf){
    ranges_Influenza_no = c(ranges_Influenza_no,500)
  }else{
    ranges_Influenza_no = c(ranges_Influenza_no,min(temp[temp$i_2 == 0 & temp$e_2==0,]$day))
  }
}
outbreaks_summary[outbreaks_summary$disease=="Influenza" & outbreaks_summary$interaction=="Independence",]$mean = mean(ranges_Influenza_no)
outbreaks_summary[outbreaks_summary$disease=="Influenza" & outbreaks_summary$interaction=="Independence",]$CI_lb = quantile(ranges_Influenza_no, 0.025)
outbreaks_summary[outbreaks_summary$disease=="Influenza" & outbreaks_summary$interaction=="Independence",]$CI_up = quantile(ranges_Influenza_no, 0.975)

### Co-circulation
ranges_Influenza_yes = c()
for(i in nonextinct_Influenza_with_interaction){
  temp = infections_0[infections_0$sim==i,]
  if(min(temp[temp$i_2 == 0 & temp$e_2==0,]$day) == Inf){
    ranges_Influenza_yes = c(ranges_Influenza_yes,500)
  }else{
    ranges_Influenza_yes = c(ranges_Influenza_yes,min(temp[temp$i_2 == 0 & temp$e_2==0,]$day))
  }
}
outbreaks_summary[outbreaks_summary$disease=="Influenza" & outbreaks_summary$interaction=="Co-circulation",]$mean = mean(ranges_Influenza_yes)
outbreaks_summary[outbreaks_summary$disease=="Influenza" & outbreaks_summary$interaction=="Co-circulation",]$CI_lb = quantile(ranges_Influenza_yes, 0.025)
outbreaks_summary[outbreaks_summary$disease=="Influenza" & outbreaks_summary$interaction=="Co-circulation",]$CI_up = quantile(ranges_Influenza_yes, 0.975)
outbreaks_summary$int = c(0,0,1,1)

# plot the ranges of an epidemic with 95\% percentile interval
ggplot()+
  geom_segment(data = outbreaks_summary[outbreaks_summary$disease=="COVID-19",], 
               aes(x=CI_lb, xend=CI_up, y=int, yend=int, group = interaction, color = disease))+
  geom_point(data = outbreaks_summary[outbreaks_summary$disease=="COVID-19",],
             aes(x = mean, y = int, group = interaction, color = disease))+
  geom_segment(data = outbreaks_summary[outbreaks_summary$disease=="Influenza",], 
               aes(x=CI_lb, xend=CI_up, y=int, yend=int, group = interaction, color = disease))+
  geom_point(data = outbreaks_summary[outbreaks_summary$disease=="Influenza",],
             aes(x = mean, y = int, group = interaction, color = disease))+
  labs(x ="Duration of an Outbreak (in days)", y = " ", color = "Disease", size = 14)+
  scale_color_manual(values=c("#CA0020", "#00A9FF"))+
  scale_y_continuous(breaks=c(0,1), labels = c("Independence", "Co-circulation"), limits = c(-0.2,1.2))+
  scale_x_continuous(breaks = c(seq(100,500,100)))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))+
  theme(legend.background = element_rect(fill = "white", color = "black"))
  


#### Look at outbreaks only 
# create a histogram of AR of outbreaks only
ggplot(AR_summary[AR_summary$disease %in% c("Influenza") & AR_summary$AR>0.1,])+
  aes(x=AR, group = interaction(disease, interaction), fill = disease)+
  facet_grid(interaction~disease)+
  geom_histogram(binwidth = 0.005)+
  theme(legend.position="none")+
  labs(x="Relative Attack Rate", y="Count", fill = "Disease")+
  scale_fill_manual(values=c("#CA0020", "#00A9FF"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))+
  theme(legend.position=c(0.85,0.85), legend.background = element_rect(fill = "white", color = "black"))
  

# create a violin plot of AR of outbreaks only
ggplot()+
  geom_violin(data = AR_summary[AR_summary$AR>0.1 & AR_summary$disease %in% c("COVID-19", "Influenza"),], 
              aes(x=interaction,y=AR, group = interaction(disease, interaction), fill=disease))+
  labs(x=" ", y="Relative Attack Rate", fill = "Disease")+
  geom_point(aes(x=interaction,y=mean_AR, group = interaction(disease, interaction)), 
             data = AR_summary_statistics_nonextinct, size = 2)+
  theme(legend.position=c(0.85,0.85), legend.background = element_rect(fill = "white", color = "black"))+
  geom_text(data = AR_summary_statistics[AR_summary_statistics$extinct == "Outbreaks",], aes(x=interaction, y=round(mean_AR,2), group = interaction(disease, interaction), label=round(mean_AR,3)), hjust=-0.2, vjust=-0.8, size=4)+
  stat_summary(fun = "mean", geom = "point", color = "black")+
  geom_point(data=extinction_probability, aes(x=interaction, y=p, group = interaction(disease, interaction)), shape=8)+
  geom_text(data = extinction_probability, aes(x=interaction, y=p, group = interaction(disease, interaction), label=p), hjust=0, vjust=-0.5, size=4)+
  facet_grid(~disease)+
  scale_fill_manual(values=c("#CA0020", "#00A9FF"))+
  scale_y_continuous(breaks = c(seq(0,1,0.1)))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))


##################################################################################################################################################
# Plot Cases at Peak
##################################################################################################################################################

# store the information of the different scenarios in data frames
CP_COVID_no = CP_01[CP_01$disease==1,]
CP_COVID_no$disease = factor("COVID-19")
CP_COVID_no$interaction = factor("Independence")
CP_COVID_no_nonextinct = CP_COVID_no[CP_COVID_no$sim %in% nonextinct_COVID_without_interaction,]

CP_Influenza_no = CP_02[CP_02$disease==2,]
CP_Influenza_no$disease = factor("Influenza")
CP_Influenza_no$interaction = factor("Independence")
CP_Influenza_no_nonextinct = CP_Influenza_no[CP_Influenza_no$sim %in% nonextinct_Influenza_without_interaction,]
  
CP_COVID_yes = CP_0[CP_0$disease==1,]
CP_COVID_yes$disease = factor("COVID-19")
CP_COVID_yes$interaction = factor("Co-circulation")
CP_COVID_yes_nonextinct = CP_COVID_yes[CP_COVID_yes$sim %in% nonextinct_COVID_with_interaction,]

CP_Influenza_yes = CP_0[CP_0$disease==2,]
CP_Influenza_yes$disease = factor("Influenza")
CP_Influenza_yes$interaction = factor("Co-circulation")
CP_Influenza_yes_nonextinct = CP_Influenza_yes[CP_Influenza_yes$sim %in% nonextinct_Influenza_with_interaction,]

CP_Both_yes = CP_0[CP_0$disease==12,]
CP_Both_yes$disease = factor("Both")
CP_Both_yes$interaction = factor("Co-circulation")
CP_Both_yes_nonextinct = CP_Both_yes[CP_Both_yes$sim %in% c(nonextinct_COVID_with_interaction, nonextinct_Influenza_with_interaction),]


#create data frame to store all information about cases at peak for all simulations
CP_summary = rbind(CP_COVID_no,
                   CP_Influenza_no,
                   CP_COVID_yes,
                   CP_Influenza_yes, 
                   CP_Both_yes)
CP_summary$CP = CP_summary$CP/N

# create a data frame with mean cases at peak and 95% percentile interval for all simulations
CP_summary_statistics = CP_summary[CP_summary$disease %in% c("COVID-19", "Influenza", "Both"),] %>% 
  group_by(interaction, disease) %>% 
  summarise(mean_CP = round(mean(CP),4), 
            CP_025 = round(quantile(CP,0.025),4), 
            CP_975 = round(quantile(CP,0.975),4),
            CP_median = round(quantile(CP,0.5),4))
CP_summary_statistics$ext = "All simulations"

# create histograms of the cases at peak for all simulations
ggplot(CP_summary[CP_summary$disease %in% c("COVID-19", "Influenza", "Both"),])+
  aes(x=CP, group = interaction(disease, interaction), fill = disease)+
  facet_grid(interaction~disease)+
  geom_histogram(binwidth = 0.006)+
  theme(legend.position = c(0.85,0.85))+
  labs(x="Relative Cases at Peak", y="Count", fill= "Disease")+
  scale_fill_manual(values=c("#CA0020", "#00A9FF", "#E68613"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))+
  theme(legend.position=c(0.85,0.85), legend.background = element_rect(fill = "white", color = "black"))
  
# create violin plots of the cases at peak for all simulations
ggplot()+
  geom_violin(data = CP_summary[CP_summary$disease %in% c("COVID-19", "Influenza", "Both"),],
              aes(x=interaction, y=CP, group = interaction(disease, interaction), fill=disease))+
  labs(x=" ", y="Relative Cases at Peak", fill = "Disease")+
  geom_point(aes(x=interaction,y=mean_CP, group = interaction(disease, interaction)), 
             data = CP_summary_statistics, size = 2)+
  geom_text(data = CP_summary_statistics, aes(x=interaction,y=mean_CP, group = interaction(disease, interaction), label = round(mean_CP,3)), hjust=-0.3, vjust=-0.5, size=3.5)+
  theme(legend.position = c(0.5,0.85))+
  facet_grid(~disease)+
  scale_fill_manual(values=c("#CA0020", "#00A9FF", "#E68613"))+
  scale_y_continuous(breaks = c(seq(0,1,0.1)))+
  theme(axis.text.x = element_text(size=12),
      axis.text.y = element_text(size=12),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14),
      legend.text = element_text(size=12),
      legend.title = element_text(size=12),
      strip.text.x = element_text(size=14),
      strip.text.y = element_text(size=14))+
  theme(legend.position=c(0.5,0.85), legend.background = element_rect(fill = "white", color = "black"))

#### Look at outbreaks only 
#create data frame to store all information about cases at peak for outbreaks only
CP_summary_nonextinct = rbind(CP_COVID_no_nonextinct,
                              CP_Influenza_no_nonextinct,
                              CP_COVID_yes_nonextinct,
                              CP_Influenza_yes_nonextinct,
                              CP_Both_yes_nonextinct)
CP_summary_nonextinct$CP = CP_summary_nonextinct$CP/N

# create a data frame with mean cases at peak and 95% percentile interval for outbreaks only
CP_summary_statistics_nonextinct = CP_summary_nonextinct[CP_summary_nonextinct$disease %in% c("COVID-19", "Influenza", "Both"),] %>% 
  group_by(interaction, disease) %>% 
  summarise(mean_CP = round(mean(CP),4), 
            CP_025 = round(quantile(CP,0.025),4), 
            CP_975 = round(quantile(CP,0.975),4),
            CP_median = round(quantile(CP,0.5),4))
CP_summary_statistics_nonextinct$ext = "Outbreaks"

# create histograms of the cases at peak for outbreaks only
ggplot(CP_summary_nonextinct[CP_summary_nonextinct$disease %in% c("Influenza"),])+
  aes(x=CP, group = interaction(disease, interaction), fill = disease)+
  facet_grid(interaction~disease)+
  geom_histogram(binwidth = 0.0005)+
  theme(legend.position = c(0.9,0.85))+
  labs(x="Relative Cases at Peak", y="Count", fill= "Disease")+
  scale_fill_manual(values=c("#CA0020", "#00A9FF", "#E68613"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))+
  theme(legend.position=c(0.85,0.85), legend.background = element_rect(fill = "white", color = "black"))


# create violin plot of cases at peak for outbreaks only
ggplot()+
  geom_violin(data = CP_summary_nonextinct[CP_summary_nonextinct$disease %in% c("COVID-19", "Influenza", "Both"),],
              aes(x=interaction, y=CP, group = interaction(disease, interaction), fill=disease))+
  labs(x=" ", y="Relative Cases at Peak", fill = "Disease")+
  geom_point(aes(x=interaction,y=mean_CP, group = interaction(disease, interaction)), 
             data = CP_summary_statistics_nonextinct[CP_summary_statistics_nonextinct$disease %in% c("COVID-19", "Influenza"),], size = 2)+
  geom_text(data = CP_summary_statistics_nonextinct, aes(x=interaction,y=mean_CP, group = interaction(disease, interaction), label = round(mean_CP,3)), hjust=-0.2, vjust=0.3, size=4)+
  theme(legend.position=c(0.50,0.85), legend.background = element_rect(fill = "white", color = "black"))+
facet_grid(~disease)+
  scale_fill_manual(values=c("#CA0020", "#00A9FF", "#E68613"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))+
  scale_y_continuous(breaks = c(seq(0,1,0.1)))

# prepare summary data frame for plotting intervals
CP_summary_statistics_all = rbind(CP_summary_statistics, CP_summary_statistics_nonextinct)
CP_summary_statistics_all$int = c(0,0,1,1,1,0,0,1,1,1)

# plot mean CP with 95\% percentile interval for both diseases, with and without interaction, all simulations and only outbreaks
ggplot()+
  geom_segment(data = CP_summary_statistics_all[CP_summary_statistics_all$disease=="COVID-19",], 
               aes(x = CP_025, xend = CP_975, y = int+0.1, yend = int+0.1, group = interaction(int, ext), color = disease))+
  geom_point(data = CP_summary_statistics_all[CP_summary_statistics_all$disease=="COVID-19",],
             aes(x = mean_CP, y = int+0.1, group = interaction(int, ext), color = disease))+
  geom_segment(data = CP_summary_statistics_all[CP_summary_statistics_all$disease=="Influenza",], 
               aes(x = CP_025, xend = CP_975, y = int-0.1, yend = int-0.1, group = interaction(int, ext), color = disease))+
  geom_point(data = CP_summary_statistics_all[CP_summary_statistics_all$disease=="Influenza",],
             aes(x = mean_CP, y = int-0.1, group = interaction(int, ext), color = disease))+  labs(x ="Relative Cases at Peak", y = " ", color = "Disease")+
  geom_segment(data = CP_summary_statistics_all[CP_summary_statistics_all$disease=="Both",], 
               aes(x = CP_025, xend = CP_975, y = int, yend = int, group = interaction(int, ext), color = disease))+
  geom_point(data = CP_summary_statistics_all[CP_summary_statistics_all$disease=="Both",],
             aes(x = mean_CP, y = int, group = interaction(int, ext), color = disease))+  labs(x ="Relative Cases at Peak", y = " ", color = "Disease")+
  scale_color_manual(values=c("#E68613", "#CA0020", "#00A9FF"))+
  facet_grid(~ext)+
  scale_y_continuous(breaks=c(0,1), labels = c("Independence", "Co-circulation"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))+
  theme(legend.background = element_rect(fill = "white", color = "black"))
  


##################################################################################################################################################
# Plot Day of the Peak
##################################################################################################################################################

# create a data frame with summary statistics for the day of the peak for all simulations
day_peak_summary = CP_summary[CP_summary$disease %in% c("COVID-19", "Influenza", "Both"),] %>% 
  group_by(interaction, disease) %>% 
  summarise(mean_day_peak = round(mean(piek),4), 
            day_peak_025 = round(quantile(piek,0.025),4), 
            day_peak_975 = round(quantile(piek,0.975),4),
            day_peak_median = round(quantile(piek,0.5),4))
day_peak_summary$ext = "All Simulations"

# plot histograms of the day of the peak for all simulations
ggplot(CP_summary[CP_summary$disease %in% c("COVID-19", "Influenza", "Both"),])+
  aes(x=piek, group = interaction(disease, interaction), fill = disease)+
  facet_grid(interaction~disease)+
  geom_histogram(binwidth = 10)+
  theme(legend.position=c(0.9,0.85))+
  labs(x="Day of the Peak", y="Count", fill = "Disease")+
  scale_fill_manual(values=c("#CA0020", "#00A9FF", "#E68613"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))+
  theme(legend.position=c(0.85,0.85), legend.background = element_rect(fill = "white", color = "black"))

  
# create violin plots of the day of the peak for all simulations
ggplot()+
  geom_violin(data = CP_summary[CP_summary$disease %in% c("COVID-19", "Influenza", "Both"),],
              aes(x=interaction, y=piek, group = interaction(disease, interaction), fill=disease))+
  labs(x=" ", y="Day of the Peak", fill = "Disease")+
  geom_point(aes(x=interaction,y=mean_day_peak, group = interaction(disease, interaction)), 
             data = day_peak_summary, size = 2)+
  geom_text(data = day_peak_summary, aes(x=interaction, y = mean_day_peak, group = interaction(disease, interaction), label = round(mean_day_peak,3)), hjust=-0.2, vjust=1, size=4)+
  theme(legend.position = c(0.15,0.85))+
  facet_grid(~disease)+
  scale_fill_manual(values=c("#CA0020", "#00A9FF", "#E68613"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))+
  theme(legend.position=c(0.85,0.85), legend.background = element_rect(fill = "white", color = "black"))



#### Look at outbreaks only 
# create a data frame with summary statistics for the day of the peak for outbreaks only
day_peak_summary_nonextinct = CP_summary_nonextinct[CP_summary_nonextinct$disease %in% c("COVID-19", "Influenza", "Both"),] %>% 
  group_by(interaction, disease) %>% 
  summarise(mean_day_peak = round(mean(piek),4), 
            day_peak_025 = round(quantile(piek,0.025),4), 
            day_peak_975 = round(quantile(piek,0.975),4),
            day_peak_median = round(quantile(piek,0.5),4))
day_peak_summary_nonextinct$ext = "Outbreaks"

# plot histograms of the day of the peak for outbreaks only
ggplot(CP_summary_nonextinct[CP_summary_nonextinct$disease %in% c("COVID-19", "Influenza", "Both"),])+
  aes(x=piek, group = interaction(disease, interaction), fill = disease)+
  facet_grid(interaction~disease)+
  geom_histogram(binwidth = 8)+
  theme(legend.position = c(0.9,0.85))+
  labs(x="Day of the Peak", y="Count", fill = "Disease")+
  scale_fill_manual(values=c("#CA0020", "#00A9FF", "#E68613"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))+
  theme(legend.position=c(0.85,0.85), legend.background = element_rect(fill = "white", color = "black"))


# create violin plots of the day of the peak for outbreaks only
ggplot()+
  geom_violin(data = CP_summary_nonextinct[CP_summary_nonextinct$disease %in% c("COVID-19", "Influenza", "Both"),],
              aes(x=interaction, y=piek, group = interaction(disease, interaction), fill=disease))+
  labs(x=" ", y="Day of the Peak", fill = "Disease")+
  geom_point(aes(x=interaction, y=mean_day_peak, group = interaction(disease, interaction)), 
             data = day_peak_summary_nonextinct, size = 2)+
  geom_text(data = day_peak_summary_nonextinct, aes(x=interaction, y = mean_day_peak, group = interaction(disease, interaction), label = round(mean_day_peak,1)), hjust=-0.2, vjust=0.5, size=4)+
  theme(legend.position = c(0.15,0.85))+
  facet_grid(~disease)+
  scale_fill_manual(values=c("#CA0020", "#00A9FF", "#E68613"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))+
  theme(legend.position=c(0.85,0.85), legend.background = element_rect(fill = "white", color = "black"))

# prepare summary data frame for plotting intervals
day_peak_summary_statistics_all = rbind(day_peak_summary, day_peak_summary_nonextinct)
day_peak_summary_statistics_all$int = c(0,0,1,1,1,0,0,1,1,1)

# plot mean day of the peak with 95\% percentile interval for both diseases, with and without interaction, all simulations and only outbreaks
ggplot()+
  geom_segment(data = day_peak_summary_statistics_all[day_peak_summary_statistics_all$disease=="COVID-19",], 
               aes(x = day_peak_025, xend = day_peak_975, y = int+0.1, yend = int+0.1, group = interaction(int, ext), color = disease))+
  geom_point(data = day_peak_summary_statistics_all[day_peak_summary_statistics_all$disease=="COVID-19",],
             aes(x = mean_day_peak, y = int+0.1, group = interaction(int, ext), color = disease))+
  geom_segment(data = day_peak_summary_statistics_all[day_peak_summary_statistics_all$disease=="Influenza",], 
               aes(x = day_peak_025, xend = day_peak_975, y = int-0.1, yend = int-0.1, group = interaction(int, ext), color = disease))+
  geom_point(data = day_peak_summary_statistics_all[day_peak_summary_statistics_all$disease=="Influenza",],
             aes(x = mean_day_peak, y = int-0.1, group = interaction(int, ext), color = disease))+  
  labs(x ="Day of the Peak", y = " ", color = "Disease")+
  geom_segment(data = day_peak_summary_statistics_all[day_peak_summary_statistics_all$disease=="Both",], 
               aes(x = day_peak_025, xend = day_peak_975, y = int, yend = int, group = interaction(int, ext), color = disease))+
  geom_point(data = day_peak_summary_statistics_all[day_peak_summary_statistics_all$disease=="Both",],
             aes(x = mean_day_peak, y = int, group = interaction(int, ext), color = disease))+
  scale_color_manual(values=c("#E68613", "#CA0020", "#00A9FF"))+
  facet_grid(~ext)+
  scale_y_continuous(breaks=c(0,1), labels = c("Independence", "Co-circulation"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size=14),
        strip.text.y = element_text(size=14))+
  theme(legend.background = element_rect(fill = "white", color = "black"))

