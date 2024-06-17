set.seed(25091991)
library(parallel)


source("//home/nadine/Documents/UHasselt/Year 2/Thesis/Estimation of Beta/SEIR_MCMC.R")

# initialize the parameters for MCMC
sd_proposal = c(0.40,0.18)
beta_0 = c(0.5,0.8,1,1.5,2) # starting value
niter = 100000
nchains = length(beta_0)
# number of MCMC iterations
burnin = 5000    # MCMC burn in

stored = NA
acceptance = NA
acceptance_probability = NA

MCMC = function(c){
  system(paste0("echo 'chain ", c, " started.'"))
  beta_old = beta_0[c]
  target_old = target_MSE(beta_old, data$cases)
  for(i in 1:niter){
    system(paste0("echo 'chain ", c, " iteration'",i))
    beta_new = proposed(beta_old)
    target_new = target_MSE(beta_new, data$cases)
    ratio = min(1,target_old/target_new)
    accept = runif(1) < ratio
    acceptance[i] = accept
    stored[i] = ifelse(accept, beta_new, beta_old)
    beta_old = ifelse(accept, beta_new, beta_old)
    target_old = ifelse(accept, target_new, target_old)
  }
  acceptance_probability = sum(acceptance)/niter
  results = list(stored, acceptance_probability)
  return(results)
  system(paste0("echo 'chain ", c, " ended'"))
}

###############################################################################"

start.time <- Sys.time()

number_of_chains = c(1:nchains)
results = mclapply(number_of_chains, MCMC, mc.cores = 5)
betas = list()

end.time <- Sys.time()
round(end.time - start.time,2)

###############################################################################"

#results = results_influenza
  
par(mfrow = c(1, 1)) 

chain_1 = results[[1]][[1]]
chain_1_final = chain_1[((niter-burnin):niter)]
chain_2 = results[[2]][[1]]
chain_2_final = chain_2[((niter-burnin):niter)]
chain_3 = results[[3]][[1]]
chain_3_final = chain_3[((niter-burnin):niter)]
chain_4 = results[[4]][[1]]
chain_4_final = chain_4[((niter-burnin):niter)]
chain_5 = results[[5]][[1]]
chain_5_final = chain_5[((niter-burnin):niter)]

chains = cbind(chain_1, chain_2, chain_3, chain_4, chain_5)
#write.csv(chains, "/home/nadine/Documents/UHasselt/Year 2/Thesis/Estimation of Beta/MCMC_Influenza_chains.csv", row.names=FALSE)



#stored
#write.csv(stored[,1:2], "/home/nadine/Documents/UHasselt/Year 2/Thesis/Estimation of Beta/MCMC_COVID_results_4.csv", row.names=FALSE)
#write.csv(stored, "/home/nadine/Documents/UHasselt/Year 2/Thesis/Estimation of Beta/MCMC_COVID_results.csv", row.names=FALSE)

#stored = as.data.frame(read.csv("/home/nadine/Documents/UHasselt/Year 2/Thesis/Estimation of Beta/MCMC_ILI_results_1.csv"))
#sample_final = stored[burnin:niter,]

beta_1 = data.frame(iteration = c(0:(niter-burnin)),
                    beta = chain_1_final,
                    chain = "chain 1")
beta_2 = data.frame(iteration = c(0:(niter-burnin)),
                    beta = chain_2_final,
                    chain = "chain 2")
beta_3 = data.frame(iteration = c(0:(niter-burnin)),
                    beta = chain_3_final,
                    chain = "chain 3")
beta_4 = data.frame(iteration = c(0:(niter-burnin)),
                    beta = chain_4_final,
                    chain = "chain 4")
beta_5 = data.frame(iteration = c(0:(niter-burnin)),
                    beta = chain_5_final,
                    chain = "chain 5")

plot = rbind(beta_1, beta_2, beta_3, beta_4, beta_5)
means = data.frame(chain = c("chain 1", "chain 2", "chain 3", "chain 4", "chain 5"),
                   mean = c(mean(beta_1$beta), mean(beta_2$beta), mean(beta_3$beta), mean(beta_4$beta), mean(beta_5$beta)))

ggplot(data = plot)+
  aes(x = iteration, y = beta, group = chain)+
  geom_line()+
  facet_wrap(~chain, nrow = 5)+
  geom_abline(data = means, aes(slope = 0, intercept = mean), colour = "red")+
  labs(x ="Iteration", y = "Transmission Rate")+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))


ggplot(data = plot)+
  aes(x = beta, group = chain)+
  geom_histogram(binwidth = 0.08, fill = "darkgray", color = "black")+
  facet_wrap(~chain, nrow = 5)+
  #geom_vline(data = means, aes(xintercept = mean), colour = "red")+
  #geom_text(data = means, aes(x = mean, y = 5, label=round(mean,4)), hjust = -0.35, vjust = -7, color = "red")+
  labs(x ="Transmission Rate", y = "Count")+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))




median_beta = c(median(chain_1), median(chain_2), median(chain_3), median(chain_4), median(chain_5))
median_beta
beta_final = mean(median_beta)
beta_final

beta_s = beta_final
beta_a = beta_s * beta_hat[x]
#R_0 = (beta_final * beta_hat[x] * gamma_a[x] + beta_final * (1-gamma_a[x])) / delta[x]
R_0 = (beta_a * gamma_a[x] + beta_s * (1-gamma_a[x])) / delta[x]
R_0

(1.2* delta[x])/(beta_hat[x] * gamma_a[x] + 1 - gamma_a[x])

final_simulation = SIR(1.12)
final_simulation_summary = final_simulation %>% group_by(week) %>% summarize(S = mean(S), E = mean(E), Ia = mean(Ia), Is = mean(Is), R = mean(R), cases = mean(cases)) %>% filter(week != 0)

cols = c("mean (simulated)" = "red", "simulated" = "darkgray", "real" = "black")
ggplot()+
  geom_point(data=data, aes(x=week, y=cases, color = "real"))+
  geom_line(data=data,aes(x=week, y=cases , color = "real"))+
  geom_line(data = final_simulation %>% filter(week>=1), aes(x=week, y=cases, group = sim, color = "simulated"), size = 0.1)+
  geom_point(data = final_simulation_summary, aes(x=week, y=cases, color = "mean (simulated)"))+
  geom_line(data = final_simulation_summary, aes(x=week, y=cases, color = "mean (simulated)"), linewidth = 0.5)+
  labs(x ="Week", y = "Incidence")+
  scale_colour_manual(name="Data",values=cols)+
  theme(legend.position = c(0.15,0.75))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))
  
  



