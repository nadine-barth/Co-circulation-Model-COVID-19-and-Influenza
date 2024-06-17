library(dplyr)
library(ggplot2)
set.seed(25091991)
#####################################################################################################################
########################################### Load and prepare the data set ###########################################
#####################################################################################################################

######################################################## ILI #########################################################

read_ili_data = function(){
  NL_ILI = read.csv("/home/nadine/Documents/UHasselt/Year 2/Thesis/Estimation of Beta/NL_incidence.csv")
  # group by dates
  NL_ILI_data = NL_ILI %>% group_by(week = yw) %>% summarise(cases = sum(count))
  NL_ILI_data$week = seq(1,nrow(NL_ILI_data),1)
  NL_ILI_data = NL_ILI_data %>% filter(week >= 111 & week <= 116)
  return(NL_ILI_data)
}
######################################################## COVID19 #########################################################

read_covid_data = function(){
  # read in data
  COVID19_data = read.csv("/home/nadine/Documents/UHasselt/Year 2/Thesis/Estimation of Beta/COVID19_data.csv")
  # group by dates
  COVID19 = COVID19_data %>% group_by(date = DATE) %>% summarise(cases = sum(CASES))
  # merge data for each week to obtain weekly incidence
  COVID19 = COVID19[1:1421,]
  COVID19$week = rep(1:203, each=7)
  COVID19 = COVID19 %>% group_by(week) %>% summarise(cases=sum(cases), date = min(date))
  
  # select a subset of data (first wave)
  # dates between march 1st 2020 and june 21st 2020
  COVID19_subset = subset(COVID19, week >= 1 & week <= 4)
  return(COVID19_subset)  
}

# read in the data
data_ILI = read_ili_data()
data_COVID = read_covid_data()
datasets = list(data_COVID, data_ILI)

# choose which disease to work with
x = 1       # 1 = COVID19, 2 = ILI

data = as.data.frame(datasets[x])
data$week = seq(1,nrow(data),1)

# plot, look at the incidence 
ggplot(data)+
  aes(x=week, y=cases)+
  geom_point()+
  geom_line()+
  #scale_x_continuous(name="Week (Year)", labels=c("1" = "9 (2020)", "2" = "10 (2020)", "3" = "11 (2020)", "4" = "12 (2020)"))+
  scale_x_continuous(name="Week (Year)", breaks = c(1,2,3,4,5,6), labels=c("1" = "34 (2022)", "2" = "35 (2022)", "3" = "36 (2022)", "4" = "37 (2020)", "5" = "38 (2022)", "6"= "(39 (2022)"))+
  #ggtitle("Weekly Incidence of COVID-19 (Belgium)")
  ggtitle("Weekly Incidence of Influenza (The Netherlands)")

time_steps = nrow(data)

#####################################################################################################################
##################################################### Parameters #####################################################
#####################################################################################################################

# Define some parameters for the simulations
## Initial number of individuals in classes for both diseases
E_0 = c(300,500)      # exposed
I_0 = c(150,600)      # infected
R_0 = c(0,0)          # recovered
D_0 = c(0,0)          # dead

N = c(11700000, 17000000)   # total population size

length_of_time_step = 7  # length of one time step in days
nsim = 100 # number of simulations


initial_state = c(I_0[x], R_0[x], N[x], E_0[x], D_0[x])
specifications = c(time_steps, length_of_time_step, nsim)

## Parameters related to the disease

beta_hat = c(0.55,0.51)    # relative infectiousness
delta = c(1/10.2, 1/4.8)   # inverse of infectious period
gamma = c(1/2, 1/1.6)      # inverse of latent period
gamma_a = c(0.308, 0.331)  # probability of asymptomatic infection
mu = c(0.015,0.004)        # death rate

######################################################################################################################
############################################ Define some useful functions ############################################
######################################################################################################################

SIR = function(beta_s){
  
  simulations = data.frame() # create an empty data frame to store information about this simulation
  
  time_steps = specifications[1]
  length_of_time_step = specifications[2]
  nsim = specifications[3]
  
  delta = delta[x]
  gamma= gamma[x]
  gamma_a = gamma_a[x]
  mu = mu[x]
  beta_a = beta_s*beta_hat[x]
  
  for(s in 1:nsim){
    #print(paste0("SEIR simulation ", s))
    # initialize the parameters
    E = initial_state[4]
    I = initial_state[1]
    Ia = round(I * gamma_a,0)
    Is = I-Ia
    #print(paste0("I = ", I, ", Ia = ", Ia, ", Is = ", Is, " (beta_a = ", beta_a, ")"))
    R = initial_state[2]
    N = initial_state[3]
    D = initial_state[5]
    S = N-I-R-E-D

    
    # placeholders
    S_vec = S
    E_vec = E
    Ia_vec = Ia
    Is_vec = Is
    R_vec = R
    D_vec = D
    new_cases = 0
    
    # simulation
    for(t in 1:(time_steps)){
      #print(paste0("time_step = ", t))
      #print(paste0("E = ", E, ", Ia = ", Ia, ", Is = ", Is, ", R = ", R, "D = ", D))
      
      E_new = rbinom(1,S,1-exp(-length_of_time_step/N * (beta_a*Ia + beta_s*Is)))
      #print(paste0("Is = ", Is, ", Ia = ", Ia, ", beta_s = ", beta_s, ", beta_a = ", beta_a))
      Ia_new = rbinom(1,E,1-exp(-length_of_time_step*gamma*gamma_a))
      #print(paste0("gamma = ", gamma, ", gamma_a = ", gamma_a, ", E = ", E))
      Is_new = rbinom(1,E,1-exp(-length_of_time_step*gamma*(1-gamma_a)))
      #print(paste0("E = ", E, ", E_new = ", E_new))
      if(Ia_new+Is_new > E+E_new){
        #print("problem with I")
        #print(paste0("Ia_new + Is_new (", Ia_new, "+", Is_new, ") > E + E_new (", E, "+", E_new, ")"))
        I_NEW = rbinom(1,E,1-exp(-length_of_time_step*gamma))
        #print(paste0("I_NEW = ", I_NEW))
        u = runif(1,0,1)
        if(u>0.5){
          #print("Ia first")
          Ia_new = round(I_NEW * gamma_a,0)
          #print(paste0("gamma_a = ", gamma_a))
          Is_new = I_NEW - Ia_new
          #print(paste0("Ia_new = ", Ia_new, ", Is_new = ", Is_new))
        } else {
          #print("Is first")
          Is_new = round(I_NEW * (1-gamma_a),0)
          #print(paste0("gamma_a = ", gamma_a))
          Ia_new = I_NEW - Is_new
          #print(paste0("Ia_new = ", Ia_new, ", Is_new = ", Is_new))
          
        }
      }
      
      Rs_new = rbinom(1,Is,(1-exp(-delta*length_of_time_step*(1-mu))))
      Ra_new = rbinom(1,Ia,1-exp(-length_of_time_step*delta))
      D_new = rbinom(1,Is,1-exp(-length_of_time_step*mu))
      #print(paste0("E_new = ", E_new, ", Ia_new = ", Ia_new, ", Is_new = ", Is_new, ", Rs_new = ", Rs_new, ", Ra_new = ", Ra_new, "D_new = ", D_new))
      
      
      S = S - E_new
      E = E + E_new - Ia_new - Is_new 
      Ia = Ia + Ia_new - Ra_new
      #print(paste0("Ia = ", Ia, "+", Ia_new, "-", Ra_new))
      Is = Is + Is_new - Rs_new
      #print(paste0("Is = ", Is, "+", Is_new, "-", Rs_new))
      R = R + Ra_new + Rs_new
      D = D+D_new
      
      # store results per time step in the different compartments
      S_vec = c(S_vec, S)
      E_vec = c(E_vec, E)
      Ia_vec = c(Ia_vec, Ia)
      Is_vec = c(Is_vec, Is)
      R_vec = c(R_vec, R)
      D_vec = c(D_vec, D_new)
      new_cases = c(new_cases, (Is_new))
      
    }
    # construct a data frame to store the information
    simulation = data.frame(sim = s,
                            week = seq(0,time_steps,1),
                            S = S_vec,
                            E = E_vec,
                            Ia = Ia_vec,
                            Is = Is_vec,
                            R = R_vec,
                            D = D_vec,
                            cases = new_cases)
    simulation[2:time_steps,]
    simulations = rbind(simulations, simulation)
  }
  return(simulations)
}


MSE = function(data, simulation){
  mse = sqrt(sum((data-simulation)^2)/time_steps)
  return(mse)
}

LIKELIHOOD = function(data, simulation){
  for(s in 1:nsim){
    simulation = test_simulation[test_simulation$sim==s,]
    lik = sum(log(dbinom(data$cases, simulation$E[1:6], 1-exp(-length_of_time_step*gamma[x]*(1-gamma_a[x])))))
    print(lik)
  }
}



target_MSE = function(beta, data){
  simulation = SIR(beta)
  simulation = simulation %>% filter(week != 0)
  mse_vec = c()
  for(e in 1:n_distinct(simulation$sim)){
    mse = MSE(data, simulation[simulation$sim==e,]$cases)
    mse_vec = c(mse_vec,mse)
  }
  mse_new = mean(mse_vec)
  return(mse_new)
}

target_LIKELIHOOD = function(beta, data){
  simulation = SIR(beta)
  likelihood_vec = c()
  for(e in 1:n_distinct(simulation$sim)){
    likelihood = LIKELIHOOD(data, simulation[simulation$sim==e,]$cases)
    likelihood_vec = c(likelihood_vec,likelihood)
    likelihood_new = mean(likelihood_vec)
    return(likelihood_new)
  }
}

proposed = function(mean){
  beta_new = rnorm(1, mean, sd_proposal[x])
  #print(paste0("beta_new = ", beta_new))
  # check if beta is >0
  while(beta_new<0){
    beta_new = rnorm(1, mean, sd_proposal[x])
    #print("negative beta_new")
    #print(paste0("new beta_new = ", beta_new))
  }
  return(beta_new)
}

######################################################################################################################
######################################################## MCMC ########################################################
######################################################################################################################  

