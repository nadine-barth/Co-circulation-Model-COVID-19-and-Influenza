
sim = function(i){
  
  # console print to keep track of the progress of the simulations
  system(paste0("echo 'simulation ", i, " started.'"))
  
  ###############################################################################################################
  ################################## Set up parameters for running simulations ################################## 
  ###############################################################################################################
  
  length_of_time_step = 1       # length of one time step (in days) in the simulation
  number_of_days = 1000         # number of days in one simulation
  time_steps = number_of_days * 1/length_of_time_step   # number of time steps in the entire simulation
  introduction_time_1 = 1       # day at which COVID-19 is introduced
  introduction_time_2 = 1       # day at which Influenza is introduced
  initial_infections_1 = 1      # number infected individuals introduced (COVID-19) 
  initial_infections_2 = 1      # number infected individuals introduced (Influenza)
  S_12 = 10000                  # number of susceptible individuals at the start of the simulation
  
  ###############################################################################################################
  ####################################### Set up parameters for the model ####################################### 
  ###############################################################################################################
  
  # transmission rates
  beta_1s = 0.375                 # transmission rate for symptomatic infection of COVID-19
  beta_hat_1 = 0.55               # relative transmission rate for symptomatic infection of COVID-19
  beta_1a = beta_hat_1 * beta_1s  # transmission rate for asymptomatic infection of COVID-19
  
  beta_2s = 0.2996                # transmission rate for symptomatic infection of Influenza
  beta_hat_2 = 0.51               # relative transmission rate for symptomatic infection of Influenza
  beta_2a = beta_hat_2 * beta_2s  # transmission rate for asymptomatic infection of Influenza
  
  # factors that influence the transmission rate
  lambda_hat_1i = 1         # heterologous effect on infectiousness, COVID-19
  lambda_hat_1I = 1         # homologous effect on infectiousness, COVID-19
  Lambda_hat_1i = 1         # heterologous effect on susceptibility, COVID-19
  Lambda_hat_1I = (1-0.863) # homologous effect on susceptibility, COVID-19
  
  lambda_hat_2i = 1         # heterologous effect on infectiousness, Influenza
  lambda_hat_2I = 1         # homologous effect on infectiousness, Influenza
  Lambda_hat_2i = 1         # heterologous effect on susceptibility, Influenza
  Lambda_hat_2I = (1-0.349) # homologous effect on susceptibility, Influenza
  
  # inverse of the latent period
  gamma_1 = 1/2             # COVID-19
  gamma_2 = 1/1.6           # Influenza
  
  # probability of asymptomatic infection
  gamma_hat_1a = 0.308      # COVID-19
  gamma_hat_2a = 0.331      # Influenza
  
  # inverse of the infectious period
  delta_1 = 1/10.2          # COVID-19 
  delta_2 = 1/4.8           # Influenza
  
  # mortality probability because of a symptomatic infection
  mu_1 = 0.015              # COVID-19 
  mu_2 = 0.004              # Influenza
  
  # vaccination rate
  omega_1 = 0               # COVID-19 
  omega_2 = 0               # Influenza
  
  ###############################################################################################################
  ############################################### Initialize Model ############################################## 
  ###############################################################################################################
  
  # create a data frame to store the initial model state
  initial_model_state = data.frame(t = c(0:time_steps), day = rep(0:number_of_days, each=length_of_time_step),
                                   S12 = NA, E1_S1S2 = NA, I1a_S1S2 = NA, I1s_S1S2 = NA, R1_S1S2 = NA, D1_S1S2 = NA, E1_S1R2 = NA, I1a_S1R2 = NA,
                                   I1s_S1R2 = NA, E2_S1S2 = NA, I2a_S1S2 = NA, I2s_S1S2 = NA, R2_S1S2 = NA, D2_S1S2 = NA, E2_R1S2 = NA, I2a_R1S2 = NA,
                                   I2s_R1S2 = NA, R12_S1S2 = NA, V1 = NA, E1_V1S2 = NA, I1a_V1S2 = NA, I1s_V1S2 = NA, R1_V1S2 = NA, D1_V1S2 = NA,
                                   E1_V1R2 = NA, I1a_V1R2 = NA, I1s_V1R2 = NA, E2_V1S2 = NA, I2a_V1S2 = NA, I2s_V1S2 = NA, R2_V1S2 = NA, D2_V1S2 = NA,
                                   E2_VR1S2 = NA, I2a_VR1S2 = NA, I2s_VR1S2 = NA, R12_V1S2 = NA, V2 = NA, E1_S1V2 = NA, I1a_S1V2 = NA, I1s_S1V2 = NA,
                                   R1_S1V2 = NA, D1_S1V2 = NA, E1_S1VR2 = NA, I1a_S1VR2 = NA, I1s_S1VR2 = NA, E2_S1V2 = NA, I2a_S1V2 = NA, I2s_S1V2 = NA,
                                   R2_S1V2 = NA, D2_S1V2 = NA, E2_R1V2 = NA, I2a_R1V2 = NA, I2s_R1V2 = NA, R12_S1V2 = NA, V12 = NA, E1_V1V2 = NA,
                                   I1a_V1V2 = NA, I1s_V1V2 = NA, R1_V1V2 = NA, D1_V1V2 = NA, E1_V1VR2 = NA, I1a_V1VR2 = NA, I1s_V1VR2 = NA, E2_V1V2 = NA,
                                   I2a_V1V2 = NA, I2s_V1V2 = NA, R2_V1V2 = NA, D2_V1V2 = NA, E2_VR1V2 = NA, I2a_VR1V2 = NA, I2s_VR1V2 = NA, R12_V1V2 = NA)

  # set all compartments at time step 0 to 0
  initial_model_state[1,] = 0
  # set number of susceptible individuals 
  initial_model_state$S12[1] = S_12
  
  # create a data frame to store the number of new individuals of each compartment at each time step
  initial_new_individuals = data.frame(t = c(0:time_steps), day = rep(0:number_of_days, each=length_of_time_step),
                                       E1_S1S2_new = NA, I1a_S1S2_new = NA, I1s_S1S2_new = NA, R1a_S1S2_new = NA, R1s_S1S2_new = NA, D1_S1S2_new = NA, E1_S1R2_new = NA, I1a_S1R2_new = NA,
                                       I1s_S1R2_new = NA, D1_S1R2_new = NA, E2_S1S2_new = NA, I2a_S1S2_new = NA, I2s_S1S2_new = NA, D2_S1S2_new = NA, R2a_S1S2_new = NA, R2s_S1S2_new = NA,
                                       E2_R1S2_new = NA, I2a_R1S2_new = NA, I2s_R1S2_new = NA, D2_R1S2_new = NA, R12a_S1S2_new = NA, R12s_S1S2_new = NA, R1a2_S1S2_new = NA, R1s2_S1S2_new = NA,
                                       V1_new = NA, E1_V1S2_new = NA, I1a_V1S2_new = NA, I1s_V1S2_new = NA, R1a_V1S2_new = NA, R1s_V1S2_new = NA, D1_V1S2_new = NA, E1_V1R2_new = NA,
                                       I1a_V1R2_new = NA, I1s_V1R2_new = NA, D1_V1R2_new = NA, E2_V1S2_new = NA, I2a_V1S2_new = NA, I2s_V1S2_new = NA, D2_V1S2_new = NA, R2a_V1S2_new = NA,
                                       R2s_V1S2_new = NA, E2_VR1S2_new = NA, I2a_VR1S2_new = NA, I2s_VR1S2_new = NA, D2_VR1S2_new = NA, R12a_V1S2_new = NA, R12s_V1S2_new = NA, R1a2_V1S2_new = NA,
                                       R1s2_V1S2_new= NA, V2_new = NA, E1_S1V2_new = NA, I1a_S1V2_new = NA, I1s_S1V2_new = NA, R1a_S1V2_new = NA, R1s_S1V2_new = NA, D1_S1V2_new = NA, E1_S1VR2_new = NA,
                                       I1a_S1VR2_new = NA, I1s_S1VR2_new = NA, D1_S1VR2_new = NA, E2_S1V2_new = NA, I2a_S1V2_new = NA, I2s_S1V2_new = NA, D2_S1V2_new = NA, R2a_S1V2_new = NA,
                                       R2s_S1V2_new = NA, E2_R1V2_new = NA, I2a_R1V2_new = NA, I2s_R1V2_new = NA, D2_R1V2_new = NA, R12a_S1V2_new = NA, R12s_S1V2_new = NA, R1a2_S1V2_new = NA,
                                       R1s2_S1V2_new = NA, V12_new_1 = NA, V12_new_2 = NA, E1_V1V2_new = NA, I1a_V1V2_new = NA,
                                       I1s_V1V2_new = NA, R1a_V1V2_new = NA, R1s_V1V2_new = NA, D1_V1V2_new = NA, E1_V1VR2_new = NA, I1a_V1VR2_new = NA, I1s_V1VR2_new = NA, D1_V1VR2_new = NA,
                                       E2_V1V2_new = NA, I2a_V1V2_new = NA, I2s_V1V2_new = NA, D2_V1V2_new = NA, R2a_V1V2_new = NA, R2s_V1V2_new = NA, E2_VR1V2_new = NA, I2a_VR1V2_new = NA,
                                       I2s_VR1V2_new = NA, D2_VR1V2_new = NA, R12a_V1V2_new = NA, R12s_V1V2_new = NA, R1a2_V1V2_new = NA, R1s2_V1V2_new = NA)
  # set all compartments at time step 0 to 0
  initial_new_individuals[1,] = 0
  # compute N, the total population size
  N = sum(initial_model_state[1,2:ncol(initial_model_state)])
  
  ###############################################################################################################
  ############################################### Run Simulations ############################################### 
  ###############################################################################################################
    
  # Set the model state to the initial state of the model. The model state shows the number of individuals in each compartment at each time step.
  model_state = initial_model_state
  # new shows the in-flow of each compartment for each time step. 
  new = initial_new_individuals
    
  #new_V1 = NA
  #new_V2 = NA
    
  for(t in 1:(time_steps)){
    # introduce COVID-19
    if(((t-1) * length_of_time_step) + 1 == introduction_time_1){
      model_state$E1_S1S2[t] = initial_infections_1
    }
    # introduce Influenza
    if(((t-1) * length_of_time_step) + 1 == introduction_time_2){
      model_state$E2_S1S2[t] = initial_infections_2
    }
      
    # update classes
    ### compute all in- and out- flows
    p_1 = beta_1a * (
      model_state$I1a_S1S2[t]
      + lambda_hat_1i * (model_state$I1a_S1R2[t] + model_state$I1a_S1V2[t] + model_state$I1a_S1VR2[t])
      + lambda_hat_1I * (model_state$I1a_V1S2[t] + model_state$I1a_V1R2[t] + model_state$I1a_V1V2[t] + model_state$I1a_V1VR2[t])) + beta_1s * (
        model_state$I1s_S1S2[t]
        + lambda_hat_1i * (model_state$I1s_S1R2[t] + model_state$I1s_S1V2[t] + model_state$I1s_S1VR2[t])
        + lambda_hat_1I * (model_state$I1s_V1S2[t] + model_state$I1s_V1R2[t] + model_state$I1s_V1V2[t] + model_state$I1s_V1VR2[t]))
    p_2 = beta_2a * (
      model_state$I2a_S1S2[t] 
      + lambda_hat_2i * (model_state$I2a_R1S2[t] + model_state$I2a_V1S2[t] + model_state$I2a_VR1S2[t])
      + lambda_hat_2I * (model_state$I2a_S1V2[t] + model_state$I2a_R1V2[t] + model_state$I2a_V1V2[t] + model_state$I2a_VR1V2[t])) + beta_2s * (
        model_state$I2s_S1S2[t] 
        + lambda_hat_2i * (model_state$I2s_R1S2[t] + model_state$I2s_V1S2[t] + model_state$I2s_VR1S2[t])
        + lambda_hat_2I * (model_state$I2s_S1V2[t] + model_state$I2s_R1V2[t] + model_state$I2s_V1V2[t] + model_state$I2s_VR1V2[t]))
    new$E1_S1S2_new[t+1] = rbinom(1, model_state$S12[t], 1 - exp(-length_of_time_step/N * p_1))
    I_NEW = rbinom(1, model_state$E1_S1S2[t], 1-exp(-length_of_time_step * gamma_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I1a_S1S2_new[t+1] = round(I_NEW * gamma_hat_1a, 0)
      new$I1s_S1S2_new[t+1] = I_NEW - new$I1a_S1S2_new[t+1]
    } else {
      new$I1s_S1S2_new[t+1] = round(I_NEW * (1 - gamma_hat_1a), 0)
      new$I1a_S1S2_new[t+1] = I_NEW - new$I1s_S1S2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I1s_S1S2[t], 1 - exp(-length_of_time_step * delta_1))  
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R1s_S1S2_new[t+1] = round(R_NEW * (1 - mu_1), 0)
      new$D1_S1S2_new[t+1] = R_NEW - new$R1s_S1S2_new[t+1]
    } else {
      new$D1_S1S2_new[t+1] = round(R_NEW * mu_1, 0)
      new$R1s_S1S2_new[t+1] = R_NEW - new$D1_S1S2_new[t+1]
    }
    new$R1a_S1S2_new[t+1] = rbinom(1, model_state$I1a_S1S2[t], 1 - exp(-length_of_time_step * delta_1))                                          
    new$E1_S1R2_new[t+1] = rbinom(1, model_state$R2_S1S2[t], 1 - exp(-length_of_time_step/N * Lambda_hat_1i * p_1))
    I_NEW = rbinom(1, model_state$E1_S1R2[t], 1 - exp(-length_of_time_step * gamma_1))        
    u = runif(1, 0, 1)
    if(u>0.5){
      new$I1a_S1R2_new[t+1] = round(I_NEW * gamma_hat_1a, 0)
      new$I1s_S1R2_new[t+1] = I_NEW - new$I1a_S1R2_new[t+1]
    } else {
      new$I1s_S1R2_new[t+1] = round(I_NEW * (1 - gamma_hat_1a), 0)
      new$I1a_S1R2_new[t+1] = I_NEW - new$I1s_S1R2_new[t+1]
    }
    new$R1a2_S1S2_new[t+1] = rbinom(1, model_state$I1a_S1R2[t], 1 - exp(-length_of_time_step * delta_1))                         
    R_NEW = rbinom(1, model_state$I1s_S1R2[t], 1 - exp(-length_of_time_step * delta_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R1s2_S1S2_new[t+1] = round(R_NEW * (1 - mu_1), 0)
      new$D1_S1R2_new[t+1] = R_NEW - new$R1s2_S1S2_new[t+1]
    } else {
      new$D1_S1R2_new[t+1] = round(R_NEW * mu_1,0)
      new$R1s2_S1S2_new[t+1] = R_NEW - new$D1_S1R2_new[t+1]
    }
    new$E2_S1S2_new[t+1] = rbinom(1, model_state$S12[t], 1 - exp(-length_of_time_step/N * p_2))
    I_NEW = rbinom(1, model_state$E2_S1S2[t], 1 - exp(-length_of_time_step * gamma_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I2a_S1S2_new[t+1] = round(I_NEW * gamma_hat_2a, 0)
      new$I2s_S1S2_new[t+1] = I_NEW - new$I2a_S1S2_new[t+1]
    } else {
      new$I2s_S1S2_new[t+1] = round(I_NEW * (1 - gamma_hat_2a), 0)
      new$I2a_S1S2_new[t+1] = I_NEW - new$I2s_S1S2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I2s_S1S2[t], 1 - exp(-length_of_time_step * delta_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R2s_S1S2_new[t+1] = round(R_NEW * (1-mu_2), 0)
      new$D2_S1S2_new[t+1] = R_NEW - new$R2s_S1S2_new[t+1]
    } else {
      new$D2_S1S2_new[t+1] = round(R_NEW * mu_2, 0)
      new$R2s_S1S2_new[t+1] = R_NEW - new$D2_S1S2_new[t+1]
    }
    new$R2a_S1S2_new[t+1] = rbinom(1, model_state$I2a_S1S2[t], 1 - exp(-length_of_time_step * delta_2))                           
    new$E2_R1S2_new[t+1] = rbinom(1, model_state$R1_S1S2[t], 1 - exp(-length_of_time_step/N * Lambda_hat_2i * p_2))  
    I_NEW = rbinom(1, model_state$E2_R1S2[t], 1 - exp(-length_of_time_step * gamma_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I2a_R1S2_new[t+1] = round(I_NEW * gamma_hat_2a, 0)
      new$I2s_R1S2_new[t+1] = I_NEW - new$I2a_R1S2_new[t+1]
    } else {
      new$I2s_R1S2_new[t+1] = round(I_NEW * (1 - gamma_hat_2a), 0)
      new$I2a_R1S2_new[t+1] = I_NEW - new$I2s_R1S2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I2s_R1S2[t], 1-exp(-length_of_time_step * delta_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R12s_S1S2_new[t+1] = round(R_NEW * (1 - mu_2), 0)
      new$D2_R1S2_new[t+1] = R_NEW - new$R12s_S1S2_new[t+1]
    } else {
      new$D2_R1S2_new[t+1] = round(R_NEW * mu_2, 0)
      new$R12s_S1S2_new[t+1] = R_NEW - new$D2_R1S2_new[t+1]
    }
    new$R12a_S1S2_new[t+1] = rbinom(1, model_state$I2a_R1S2[t], 1-exp(-length_of_time_step * delta_2))           
    if((model_state$S12[t] + model_state$V2[t]) > 0){
      NEW_V1 = rbinom(1, (model_state$S12[t] + model_state$V2[t]), 1 - exp(-length_of_time_step * omega_1))
      new$V12_new_1[t+1] = round(NEW_V1 * (model_state$V2[t]/(model_state$S12[t] + model_state$V2[t])), 0)
      new$V1_new[t+1] = NEW_V1 - new$V12_new_1[t+1]  
    }else{
      new$V12_new_1[t+1] = 0
      new$V1_new[t+1] = 0
    }
    new_V1 = c(new_V1, NEW_V1)
    new$E1_V1S2_new[t+1] = rbinom(1, model_state$V1[t], 1 - exp(-length_of_time_step/N * Lambda_hat_1I * p_1))
    I_NEW = rbinom(1, model_state$E1_V1S2[t], 1 - exp(-length_of_time_step * gamma_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I1a_V1S2_new[t+1] = round(I_NEW * gamma_hat_1a, 0)
      new$I1s_V1S2_new[t+1] = I_NEW - new$I1a_V1S2_new[t+1]
    } else {
      new$I1s_V1S2_new[t+1] = round(I_NEW * (1 - gamma_hat_1a), 0)
      new$I1a_V1S2_new[t+1] = I_NEW - new$I1s_V1S2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I1s_V1S2[t], 1-exp(-length_of_time_step * delta_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R1s_V1S2_new[t+1] = round(R_NEW * (1 - mu_1), 0)
      new$D1_V1S2_new[t+1] = R_NEW - new$R1s_V1S2_new[t+1]
    } else {
      new$D1_V1S2_new[t+1] = round(R_NEW * mu_1, 0)
      new$R1s_V1S2_new[t+1] = R_NEW - new$D1_V1S2_new[t+1]
    }
    new$R1a_V1S2_new[t+1] = rbinom(1, model_state$I1a_V1S2[t], 1 - exp(-length_of_time_step * delta_1))                                      
    new$E1_V1R2_new[t+1] = rbinom(1, model_state$R2_V1S2[t], 1 - exp(-length_of_time_step/N * Lambda_hat_1I * p_1)) 
    I_NEW = rbinom(1, model_state$E1_V1R2[t], 1 - exp(-length_of_time_step * gamma_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I1a_V1R2_new[t+1] = round(I_NEW * gamma_hat_1a, 0)
      new$I1s_V1R2_new[t+1] = I_NEW - new$I1a_V1R2_new[t+1]
    } else {
      new$I1s_V1R2_new[t+1] = round(I_NEW * (1 - gamma_hat_1a), 0)
      new$I1a_V1R2_new[t+1] = I_NEW - new$I1s_V1R2_new[t+1]
    }
    new$R1a2_V1S2_new[t+1] = rbinom(1, model_state$I1a_V1R2[t], 1-exp(-length_of_time_step * delta_1))                                     
    R_NEW = rbinom(1, model_state$I1s_V1R2[t], 1 - exp(-length_of_time_step * delta_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R1s2_V1S2_new[t+1] = round(R_NEW * (1 - mu_1), 0)
      new$D1_V1R2_new[t+1] = R_NEW - new$R1s2_V1S2_new[t+1]
    } else {
      new$D1_V1R2_new[t+1] = round(R_NEW * mu_1, 0)
      new$R1s2_V1S2_new[t+1] = R_NEW - new$D1_V1R2_new[t+1]
    }
    new$E2_V1S2_new[t+1] = rbinom(1, model_state$V1[t], 1 - exp(-length_of_time_step/N * Lambda_hat_2i * p_2))    
    I_NEW = rbinom(1, model_state$E2_V1S2[t], 1 - exp(-length_of_time_step * gamma_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I2a_V1S2_new[t+1] = round(I_NEW * gamma_hat_2a, 0)
      new$I2s_V1S2_new[t+1] = I_NEW - new$I2a_V1S2_new[t+1]
    } else {
      new$I2s_V1S2_new[t+1] = round(I_NEW * (1 - gamma_hat_2a), 0)
      new$I2a_V1S2_new[t+1] = I_NEW - new$I2s_V1S2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I2s_V1S2[t], 1 - exp(-length_of_time_step * delta_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R2s_V1S2_new[t+1] = round(R_NEW * (1 - mu_2), 0)
      new$D2_V1S2_new[t+1] = R_NEW - new$R2s_V1S2_new[t+1]
    } else {
      new$D2_V1S2_new[t+1] = round(R_NEW * mu_2, 0)
      new$R2s_V1S2_new[t+1] = R_NEW - new$D2_V1S2_new[t+1]
    }
    new$R2a_V1S2_new[t+1] = rbinom(1, model_state$I2a_V1S2[t], 1 - exp(-length_of_time_step * delta_2))                                      
    new$E2_VR1S2_new[t+1] = rbinom(1, model_state$R1_V1S2[t], 1 - exp(-length_of_time_step/N * Lambda_hat_2i * p_2))                          
    I_NEW = rbinom(1, model_state$E2_VR1S2[t], 1 - exp(-length_of_time_step * gamma_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I2a_VR1S2_new[t+1] = round(I_NEW * gamma_hat_2a, 0)
      new$I2s_VR1S2_new[t+1] = I_NEW - new$I2a_VR1S2_new[t+1]
    } else {
      new$I2s_VR1S2_new[t+1] = round(I_NEW * (1 - gamma_hat_2a), 0)
      new$I2a_VR1S2_new[t+1] = I_NEW - new$I2s_VR1S2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I2s_VR1S2[t], 1 - exp(-length_of_time_step * delta_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R12s_V1S2_new[t+1] = round(R_NEW * (1 - mu_2), 0)
      new$D2_VR1S2_new[t+1] = R_NEW - new$R12s_V1S2_new[t+1]
    } else {
      new$D2_VR1S2_new[t+1] = round(R_NEW * mu_2, 0)
      new$R12s_V1S2_new[t+1] = R_NEW - new$D2_VR1S2_new[t+1]
    }
    new$R12a_V1S2_new[t+1] = rbinom(1, model_state$I2a_VR1S2[t], 1 - exp(-length_of_time_step * delta_2))                                    

    if(model_state$S12[t] + model_state$V1[t] > 0){
      NEW_V2 = rbinom(1, (model_state$S12[t] + model_state$V1[t]), 1 - exp(-length_of_time_step * omega_2))
      new$V12_new_2[t+1] = round(NEW_V2 * (model_state$V1[t]/(model_state$S12[t] + model_state$V1[t])), 0)
      new$V2_new[t+1] = NEW_V2 - new$V12_new_2[t+1] 
    }else{
      new$V12_new_2[t+1] = 0
      new$V2_new[t+1] = 0
    }
    new_V2 = c(new_V2, NEW_V2)
    new$E1_S1V2_new[t+1] = rbinom(1, model_state$V2[t], 1-exp(-length_of_time_step/N * Lambda_hat_1i * p_1))
    I_NEW = rbinom(1, model_state$E1_S1V2[t], 1-exp(-length_of_time_step * gamma_1))        
    u = runif(1,0,1)
    if(u > 0.5){
      new$I1a_S1V2_new[t+1] = round(I_NEW * gamma_hat_1a, 0)
      new$I1s_S1V2_new[t+1] = I_NEW - new$I1a_S1V2_new[t+1]
    } else {
      new$I1s_S1V2_new[t+1] = round(I_NEW * (1 - gamma_hat_1a), 0)
      new$I1a_S1V2_new[t+1] = I_NEW - new$I1s_S1V2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I1s_S1V2[t], 1 - exp(-length_of_time_step * delta_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R1s_S1V2_new[t+1] = round(R_NEW * (1 - mu_1), 0)
      new$D1_S1V2_new[t+1] = R_NEW - new$R1s_S1V2_new[t+1]
    } else {
      new$D1_S1V2_new[t+1] = round(R_NEW * mu_1, 0)
      new$R1s_S1V2_new[t+1] = R_NEW - new$D1_S1V2_new[t+1]
    }
    new$R1a_S1V2_new[t+1] = rbinom(1, model_state$I1a_S1V2[t], 1 - exp(-length_of_time_step * delta_1))                       
    new$E1_S1VR2_new[t+1] = rbinom(1, model_state$R2_S1V2[t], 1-exp(-length_of_time_step/N * Lambda_hat_1i * p_1))
    I_NEW = rbinom(1, model_state$E1_S1VR2[t], 1 - exp(-length_of_time_step * gamma_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I1a_S1VR2_new[t+1] = round(I_NEW * gamma_hat_1a, 0)
      new$I1s_S1VR2_new[t+1] = I_NEW - new$I1a_S1VR2_new[t+1]
    } else {
      new$I1s_S1VR2_new[t+1] = round(I_NEW * (1 - gamma_hat_1a), 0)
      new$I1a_S1VR2_new[t+1] = I_NEW - new$I1s_S1VR2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I1s_S1VR2[t], 1 - exp(-length_of_time_step * delta_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R1s2_S1V2_new[t+1] = round(R_NEW * (1 - mu_1), 0)
      new$D1_S1VR2_new[t+1] = R_NEW - new$R1s2_S1V2_new[t+1]
    } else {
      new$D1_S1VR2_new[t+1] = round(R_NEW * mu_1, 0)
      new$R1s2_S1V2_new[t+1] = R_NEW - new$D1_S1VR2_new[t+1]
    }
    new$R1a2_S1V2_new[t+1] = rbinom(1, model_state$I1a_S1VR2[t], 1 - exp(-length_of_time_step * delta_1))                     
    new$E2_S1V2_new[t+1] = rbinom(1, model_state$V2[t], 1 - exp(-length_of_time_step/N * Lambda_hat_2I * p_2))   
    I_NEW = rbinom(1, model_state$E2_S1V2[t], 1 - exp(-length_of_time_step * gamma_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I2a_S1V2_new[t+1] = round(I_NEW * gamma_hat_2a, 0)
      new$I2s_S1V2_new[t+1] = I_NEW - new$I2a_S1V2_new[t+1]
    } else {
      new$I2s_S1V2_new[t+1] = round(I_NEW * (1 - gamma_hat_2a), 0)
      new$I2a_S1V2_new[t+1] = I_NEW - new$I2s_S1V2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I2s_S1V2[t], 1-exp(-length_of_time_step * delta_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R2s_S1V2_new[t+1] = round(R_NEW * (1 - mu_2), 0)
      new$D2_S1V2_new[t+1] = R_NEW - new$R2s_S1V2_new[t+1]
    } else {
      new$D2_S1V2_new[t+1] = round(R_NEW * mu_2, 0)
      new$R2s_S1V2_new[t+1] = R_NEW - new$D2_S1V2_new[t+1]
    }
    new$R2a_S1V2_new[t+1] = rbinom(1, model_state$I2a_S1V2[t], 1 - exp(-length_of_time_step * delta_2))                       
    new$E2_R1V2_new[t+1] = rbinom(1, model_state$R1_S1V2[t], 1 - exp(-length_of_time_step/N * Lambda_hat_2I * p_2))           
    I_NEW = rbinom(1, model_state$E2_R1V2[t], 1 - exp(-length_of_time_step * gamma_2))        
    u = runif(1,0,1)
    if(u > 0.5){
      new$I2a_R1V2_new[t+1] = round(I_NEW * gamma_hat_2a, 0)
      new$I2s_R1V2_new[t+1] = I_NEW - new$I2a_R1V2_new[t+1]
    } else {
      new$I2s_R1V2_new[t+1] = round(I_NEW * (1 - gamma_hat_2a), 0)
      new$I2a_R1V2_new[t+1] = I_NEW - new$I2s_R1V2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I2s_R1V2[t], 1 - exp(-length_of_time_step * delta_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R12s_S1V2_new[t+1] = round(R_NEW * (1 - mu_2), 0)
      new$D2_R1V2_new[t+1] = R_NEW - new$R12s_S1V2_new[t+1]
    } else {
      new$D2_R1V2_new[t+1] = round(R_NEW * mu_2, 0)
      new$R12s_S1V2_new[t+1] = R_NEW - new$D2_R1V2_new[t+1]
    }
    new$R12a_S1V2_new[t+1] = rbinom(1, model_state$I2a_R1V2[t], 1 - exp(-length_of_time_step * delta_2))
    new$E1_V1V2_new[t+1] = rbinom(1, model_state$V12[t], 1 - exp(-length_of_time_step/N * Lambda_hat_1I * p_1)) 
    I_NEW = rbinom(1, model_state$E1_V1V2[t], 1 - exp(-length_of_time_step * gamma_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I1a_V1V2_new[t+1] = round(I_NEW * gamma_hat_1a, 0)
      new$I1s_V1V2_new[t+1] = I_NEW - new$I1a_V1V2_new[t+1]
    } else {
      new$I1s_V1V2_new[t+1] = round(I_NEW * (1 - gamma_hat_1a), 0)
      new$I1a_V1V2_new[t+1] = I_NEW - new$I1s_V1V2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I1s_V1V2[t], 1 - exp(-length_of_time_step * delta_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R1s_V1V2_new[t+1] = round(R_NEW * (1 - mu_1), 0)
      new$D1_V1V2_new[t+1] = R_NEW - new$R1s_V1V2_new[t+1]
    } else {
      new$D1_V1V2_new[t+1] = round(R_NEW * mu_1, 0)
      new$R1s_V1V2_new[t+1] = R_NEW - new$D1_V1V2_new[t+1]
    }
    new$R1a_V1V2_new[t+1] = rbinom(1, model_state$I1a_V1V2[t], 1 - exp(-length_of_time_step * delta_1))                                      
    new$E1_V1VR2_new[t+1] = rbinom(1, model_state$R2_V1V2[t], 1 - exp(-length_of_time_step/N * Lambda_hat_1I * p_1))              
    I_NEW = rbinom(1, model_state$E1_V1VR2[t], 1 - exp(-length_of_time_step * gamma_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I1a_V1VR2_new[t+1] = round(I_NEW * gamma_hat_1a, 0)
      new$I1s_V1VR2_new[t+1] = I_NEW - new$I1a_V1VR2_new[t+1]
    } else {
      new$I1s_V1VR2_new[t+1] = round(I_NEW * (1 - gamma_hat_1a), 0)
      new$I1a_V1VR2_new[t+1] = I_NEW - new$I1s_V1VR2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I1s_V1VR2[t], 1 - exp(-length_of_time_step * delta_1))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R1s2_V1V2_new[t+1] = round(R_NEW * (1 - mu_1), 0)
      new$D1_V1VR2_new[t+1] = R_NEW - new$R1s2_V1V2_new[t+1]
    } else {
      new$D1_V1VR2_new[t+1] = round(R_NEW * mu_1, 0)
      new$R1s2_V1V2_new[t+1] = R_NEW - new$D1_V1VR2_new[t+1]
    }
    new$R1a2_V1V2_new[t+1] = rbinom(1, model_state$I1a_V1VR2[t], 1 - exp(-length_of_time_step * delta_1))                                    
    new$E2_V1V2_new[t+1] = rbinom(1, model_state$V12[t], 1 - exp(-length_of_time_step/N * Lambda_hat_2I * p_2))    
    I_NEW = rbinom(1, model_state$E2_V1V2[t], 1 - exp(-length_of_time_step * gamma_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I2a_V1V2_new[t+1] = round(I_NEW * gamma_hat_2a, 0)
      new$I2s_V1V2_new[t+1] = I_NEW - new$I2a_V1V2_new[t+1]
    } else {
      new$I2s_V1V2_new[t+1] = round(I_NEW * (1 - gamma_hat_2a), 0)
      new$I2a_V1V2_new[t+1] = I_NEW - new$I2s_V1V2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I2s_V1V2[t], 1 - exp(-length_of_time_step * delta_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R2s_V1V2_new[t+1] = round(R_NEW * (1 - mu_2), 0)
      new$D2_V1V2_new[t+1] = R_NEW - new$R2s_V1V2_new[t+1]
    } else {
      new$D2_V1V2_new[t+1] = round(R_NEW * mu_2, 0)
      new$R2s_V1V2_new[t+1] = R_NEW - new$D2_V1V2_new[t+1]
    }
    new$R2a_V1V2_new[t+1] = rbinom(1, model_state$I2a_V1V2[t], 1 - exp(-length_of_time_step * delta_2))                                      
    new$E2_VR1V2_new[t+1] = rbinom(1, model_state$R1_V1V2[t], 1 - exp(-length_of_time_step/N * Lambda_hat_2I * p_2))
    I_NEW = rbinom(1, model_state$E2_VR1V2[t], 1 - exp(-length_of_time_step * gamma_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$I2a_VR1V2_new[t+1] = round(I_NEW * gamma_hat_2a, 0)
      new$I2s_VR1V2_new[t+1] = I_NEW - new$I2a_VR1V2_new[t+1]
    } else {
      new$I2s_VR1V2_new[t+1] = round(I_NEW * (1 - gamma_hat_2a), 0)
      new$I2a_VR1V2_new[t+1] = I_NEW - new$I2s_VR1V2_new[t+1]
    }
    R_NEW = rbinom(1, model_state$I2s_VR1V2[t], 1 - exp(-length_of_time_step * delta_2))        
    u = runif(1, 0, 1)
    if(u > 0.5){
      new$R12s_V1V2_new[t+1] = round(R_NEW * (1 - mu_2), 0)
      new$D2_VR1V2_new[t+1] = R_NEW - new$R12s_V1V2_new[t+1]
    } else {
      new$D2_VR1V2_new[t+1] = round(R_NEW * mu_2, 0)
      new$R12s_V1V2_new[t+1] = R_NEW - new$D2_VR1V2_new[t+1]
    }
    new$R12a_V1V2_new[t+1] = rbinom(1, model_state$I2a_VR1V2[t], 1-exp(-length_of_time_step * delta_2))                                    

    ### update model states
    model_state$S12[t+1] = model_state$S12[t] - new$E1_S1S2_new[t+1] - new$E2_S1S2_new[t+1] - new$V1_new[t+1] - new$V2_new[t+1]
    model_state$E1_S1S2[t+1] = model_state$E1_S1S2[t] + new$E1_S1S2_new[t+1] - new$I1a_S1S2_new[t+1] - new$I1s_S1S2_new[t+1]
    model_state$I1a_S1S2[t+1] = model_state$I1a_S1S2[t] + new$I1a_S1S2_new[t+1] - new$R1a_S1S2_new[t+1]                        
    model_state$I1s_S1S2[t+1] = model_state$I1s_S1S2[t] + new$I1s_S1S2_new[t+1] - new$R1s_S1S2_new[t+1] - new$D1_S1S2_new[t+1]
    model_state$R1_S1S2[t+1] = model_state$R1_S1S2[t] + new$R1a_S1S2_new[t+1] + new$R1s_S1S2_new[t+1] - new$E2_R1S2_new[t+1]   
    model_state$D1_S1S2[t+1] = model_state$D1_S1S2[t] + new$D1_S1S2_new[t+1] + new$D1_S1R2_new[t+1]                            
    model_state$E1_S1R2[t+1] = model_state$E1_S1R2[t] + new$E1_S1R2_new[t+1] - new$I1a_S1R2_new[t+1] - new$I1s_S1R2_new[t+1]
    model_state$I1a_S1R2[t+1] = model_state$I1a_S1R2[t] + new$I1a_S1R2_new[t+1] - new$R1a2_S1S2_new[t+1]                       
    model_state$I1s_S1R2[t+1] = model_state$I1s_S1R2[t] + new$I1s_S1R2_new[t+1] - new$R1s2_S1S2_new[t+1] - new$D1_S1R2_new[t+1]
    model_state$E2_S1S2[t+1] = model_state$E2_S1S2[t] + new$E2_S1S2_new[t+1] - new$I2a_S1S2_new[t+1] - new$I2s_S1S2_new[t+1]
    model_state$I2a_S1S2[t+1] = model_state$I2a_S1S2[t] + new$I2a_S1S2_new[t+1] - new$R2a_S1S2_new[t+1]                        
    model_state$I2s_S1S2[t+1] = model_state$I2s_S1S2[t] + new$I2s_S1S2_new[t+1] - new$R2s_S1S2_new[t+1] - new$D2_S1S2_new[t+1] 
    model_state$R2_S1S2[t+1] = model_state$R2_S1S2[t] + new$R2s_S1S2_new[t+1] + new$R2a_S1S2_new[t+1] - new$E1_S1R2_new[t+1]   
    model_state$D2_S1S2[t+1] = model_state$D2_S1S2[t] + new$D2_S1S2_new[t+1] + new$D2_R1S2_new[t+1]                            
    model_state$E2_R1S2[t+1] = model_state$E2_R1S2[t] + new$E2_R1S2_new[t+1] - new$I2a_R1S2_new[t+1] - new$I2s_R1S2_new[t+1]  
    model_state$I2a_R1S2[t+1] = model_state$I2a_R1S2[t] + new$I2a_R1S2_new[t+1] - new$R12a_S1S2_new[t+1]                       
    model_state$I2s_R1S2[t+1] = model_state$I2s_R1S2[t] + new$I2s_R1S2_new[t+1] - new$R12s_S1S2_new[t+1] - new$D2_R1S2_new[t+1]
    model_state$R12_S1S2[t+1] = model_state$R12_S1S2[t] + new$R12a_S1S2_new[t+1] + new$R12s_S1S2_new[t+1] + new$R1a2_S1S2_new[t+1] + new$R1s2_S1S2_new[t+1] 
    
    model_state$V1[t+1] = model_state$V1[t] + new$V1_new[t+1] - new$E1_V1S2_new[t+1] - new$E2_V1S2_new[t+1] - new$V12_new_2[t+1]
    model_state$E1_V1S2[t+1] = model_state$E1_V1S2[t] + new$E1_V1S2_new[t+1] - new$I1a_V1S2_new[t+1] - new$I1s_V1S2_new[t+1]
    model_state$I1a_V1S2[t+1] = model_state$I1a_V1S2[t] + new$I1a_V1S2_new[t+1] - new$R1a_V1S2_new[t+1]                                             
    model_state$I1s_V1S2[t+1] = model_state$I1s_V1S2[t] + new$I1s_V1S2_new[t+1] - new$R1s_V1S2_new[t+1] - new$D1_V1S2_new[t+1]  
    model_state$R1_V1S2[t+1] = model_state$R1_V1S2[t] + new$R1a_V1S2_new[t+1] + new$R1s_V1S2_new[t+1] - new$E2_VR1S2_new[t+1]                                
    model_state$D1_V1S2[t+1] = model_state$D1_V1S2[t] + new$D1_V1S2_new[t+1] + new$D1_V1R2_new[t+1]                                                 
    model_state$E1_V1R2[t+1] = model_state$E1_V1R2[t] + new$E1_V1R2_new[t+1] - new$I1a_V1R2_new[t+1] - new$I1s_V1R2_new[t+1]
    model_state$I1a_V1R2[t+1] = model_state$I1a_V1R2[t] + new$I1a_V1R2_new[t+1] - new$R1a2_V1S2_new[t+1]                                            
    model_state$I1s_V1R2[t+1] = model_state$I1s_V1R2[t] + new$I1s_V1R2_new[t+1] - new$R1s2_V1S2_new[t+1] - new$D1_V1R2_new[t+1]  
    model_state$E2_V1S2[t+1] = model_state$E2_V1S2[t] + new$E2_V1S2_new[t+1] - new$I2a_V1S2_new[t+1] - new$I2s_V1S2_new[t+1]
    model_state$I2a_V1S2[t+1] = model_state$I2a_V1S2[t] + new$I2a_V1S2_new[t+1] - new$R2a_V1S2_new[t+1]                                             
    model_state$I2s_V1S2[t+1] = model_state$I2s_V1S2[t] + new$I2s_V1S2_new[t+1] - new$R2s_V1S2_new[t+1] - new$D2_V1S2_new[t+1]                                
    model_state$R2_V1S2[t+1] = model_state$R2_V1S2[t] + new$R2s_V1S2_new[t+1] + new$R2a_V1S2_new[t+1] - new$E1_V1R2_new[t+1]                                 
    model_state$D2_V1S2[t+1] = model_state$D2_V1S2[t] + new$D2_V1S2_new[t+1] + new$D2_VR1S2_new[t+1]                                                
    model_state$E2_VR1S2[t+1] = model_state$E2_VR1S2[t] + new$E2_VR1S2_new[t+1] - new$I2a_VR1S2_new[t+1] - new$I2s_VR1S2_new[t+1]
    model_state$I2a_VR1S2[t+1] = model_state$I2a_VR1S2[t] + new$I2a_VR1S2_new[t+1] - new$R12a_V1S2_new[t+1]                                         
    model_state$I2s_VR1S2[t+1] = model_state$I2s_VR1S2[t] + new$I2s_VR1S2_new[t+1] - new$R12s_V1S2_new[t+1] - new$D2_VR1S2_new[t+1]   
    model_state$R12_V1S2[t+1] = model_state$R12_V1S2[t] + new$R12a_V1S2_new[t+1] + new$R12s_V1S2_new[t+1] + new$R1a2_V1S2_new[t+1] + new$R1s2_V1S2_new[t+1]           
    
    model_state$V2[t+1] = model_state$V2[t] + new$V2_new[t+1] - new$E1_S1V2_new[t+1] - new$E2_S1V2_new[t+1] - new$V12_new_1[t+1]        
    model_state$E1_S1V2[t+1] = model_state$E1_S1V2[t] + new$E1_S1V2_new[t+1] - new$I1a_S1V2_new[t+1] - new$I1s_S1V2_new[t+1]
    model_state$I1a_S1V2[t+1] = model_state$I1a_S1V2[t] + new$I1a_S1V2_new[t+1] - new$R1a_S1V2_new[t+1]                              
    model_state$I1s_S1V2[t+1] = model_state$I1s_S1V2[t] + new$I1s_S1V2_new[t+1] - new$R1s_S1V2_new[t+1] - new$D1_S1V2_new[t+1]  
    model_state$R1_S1V2[t+1] = model_state$R1_S1V2[t] + new$R1a_S1V2_new[t+1] + new$R1s_S1V2_new[t+1] - new$E2_R1V2_new[t+1]                  
    model_state$D1_S1V2[t+1] = model_state$D1_S1V2[t] + new$D1_S1V2_new[t+1] + new$D1_S1VR2_new[t+1]                                 
    model_state$E1_S1VR2[t+1] = model_state$E1_S1VR2[t] + new$E1_S1VR2_new[t+1] - new$I1a_S1VR2_new[t+1] - new$I1s_S1VR2_new[t+1]
    model_state$I1a_S1VR2[t+1] = model_state$I1a_S1VR2[t] + new$I1a_S1VR2_new[t+1] - new$R1a2_S1V2_new[t+1]                          
    model_state$I1s_S1VR2[t+1] = model_state$I1s_S1VR2[t] + new$I1s_S1VR2_new[t+1] - new$R1s2_S1V2_new[t+1] - new$D1_S1VR2_new[t+1] 
    model_state$E2_S1V2[t+1] = model_state$E2_S1V2[t] + new$E2_S1V2_new[t+1] - new$I2a_S1V2_new[t+1] - new$I2s_S1V2_new[t+1]  
    model_state$I2a_S1V2[t+1] = model_state$I2a_S1V2[t] + new$I2a_S1V2_new[t+1] - new$R2a_S1V2_new[t+1]
    model_state$I2s_S1V2[t+1] = model_state$I2s_S1V2[t] + new$I2s_S1V2_new[t+1] - new$R2s_S1V2_new[t+1] - new$D2_S1V2_new[t+1]      
    model_state$R2_S1V2[t+1] = model_state$R2_S1V2[t] + new$R2s_S1V2_new[t+1] + new$R2a_S1V2_new[t+1] - new$E1_S1VR2_new[t+1]                 
    model_state$D2_S1V2[t+1] = model_state$D2_S1V2[t] + new$D2_S1V2_new[t+1] + new$D2_R1V2_new[t+1]                                  
    model_state$E2_R1V2[t+1] = model_state$E2_R1V2[t] + new$E2_R1V2_new[t+1] - new$I2a_R1V2_new[t+1] - new$I2s_R1V2_new[t+1]
    model_state$I2a_R1V2[t+1] = model_state$I2a_R1V2[t] + new$I2a_R1V2_new[t+1] - new$R12a_S1V2_new[t+1]                             
    model_state$I2s_R1V2[t+1] = model_state$I2s_R1V2[t] + new$I2s_R1V2_new[t+1] - new$R12s_S1V2_new[t+1] - new$D2_R1V2_new[t+1]  
    model_state$R12_S1V2[t+1] = model_state$R12_S1V2[t] + new$R12a_S1V2_new[t+1] + new$R12s_S1V2_new[t+1] + new$R1a2_S1V2_new[t+1] + new$R1s2_S1V2_new[t+1]
    
    model_state$V12[t+1] = model_state$V12[t] + new$V12_new_1[t+1] + new$V12_new_2[t+1] - new$E1_V1V2_new[t+1] - new$E2_V1V2_new[t+1]
    model_state$E1_V1V2[t+1] = model_state$E1_V1V2[t] + new$E1_V1V2_new[t+1] - new$I1a_V1V2_new[t+1] - new$I1s_V1V2_new[t+1] 
    model_state$I1a_V1V2[t+1] = model_state$I1a_V1V2[t] + new$I1a_V1V2_new[t+1] - new$R1a_V1V2_new[t+1]                                             
    model_state$I1s_V1V2[t+1] = model_state$I1s_V1V2[t] + new$I1s_V1V2_new[t+1] - new$R1s_V1V2_new[t+1] - new$D1_V1V2_new[t+1]                               
    model_state$R1_V1V2[t+1] = model_state$R1_V1V2[t] + new$R1a_V1V2_new[t+1] + new$R1s_V1V2_new[t+1] - new$E2_VR1V2_new[t+1]                                
    model_state$D1_V1V2[t+1] = model_state$D1_V1V2[t] + new$D1_V1V2_new[t+1] + new$D1_V1VR2_new[t+1]                                                
    model_state$E1_V1VR2[t+1] = model_state$E1_V1VR2[t] + new$E1_V1VR2_new[t+1] - new$I1a_V1VR2_new[t+1] - new$I1s_V1VR2_new[t+1] 
    model_state$I1a_V1VR2[t+1] = model_state$I1a_V1VR2[t] + new$I1a_V1VR2_new[t+1] - new$R1a2_V1V2_new[t+1]                                          
    model_state$I1s_V1VR2[t+1] = model_state$I1s_V1VR2[t] + new$I1s_V1VR2_new[t+1] - new$R1s2_V1V2_new[t+1] - new$D1_V1VR2_new[t+1]                         
    model_state$E2_V1V2[t+1] = model_state$E2_V1V2[t] + new$E2_V1V2_new[t+1] - new$I2a_V1V2_new[t+1] - new$I2s_V1V2_new[t+1]                                
    model_state$I2a_V1V2[t+1] = model_state$I2a_V1V2[t] + new$I2a_V1V2_new[t+1] - new$R2a_V1V2_new[t+1]                                             
    model_state$I2s_V1V2[t+1] = model_state$I2s_V1V2[t] + new$I2s_V1V2_new[t+1] - new$R2s_V1V2_new[t+1] - new$D2_V1V2_new[t+1]                              
    model_state$R2_V1V2[t+1] = model_state$R2_V1V2[t] + new$R2s_V1V2_new[t+1] + new$R2a_V1V2_new[t+1] - new$E1_V1VR2_new[t+1]                               
    model_state$D2_V1V2[t+1] = model_state$D2_V1V2[t] + new$D2_V1V2_new[t+1] + new$D2_VR1V2_new[t+1]                                               
    model_state$E2_VR1V2[t+1] = model_state$E2_VR1V2[t] + new$E2_VR1V2_new[t+1] - new$I2a_VR1V2_new[t+1] - new$I2s_VR1V2_new[t+1]
    model_state$I2a_VR1V2[t+1] = model_state$I2a_VR1V2[t] + new$I2a_VR1V2_new[t+1] - new$R12a_V1V2_new[t+1]                                        
    model_state$I2s_VR1V2[t+1] = model_state$I2s_VR1V2[t] + new$I2s_VR1V2_new[t+1] - new$R12s_V1V2_new[t+1] - new$D2_VR1V2_new[t+1]
    model_state$R12_V1V2[t+1] = model_state$R12_V1V2[t] + new$R12a_V1V2_new[t+1] + new$R12s_V1V2_new[t+1] + new$R1a2_V1V2_new[t+1] + new$R1s2_V1V2_new[t+1]          
  }
    
  model_state_daily = model_state[rep(c(TRUE, rep(FALSE, (1/length_of_time_step) - 1)), number_of_days),]
  model_state_daily = model_state_daily[-1]
  new_daily = new %>% group_by(day = day) %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
  new_daily = new_daily[-2]
  simulation_results = list(model_state_daily, new_daily, number_of_days, N)
  system(paste0("echo 'simulation ", i, " ended.'"))
  return(simulation_results)
}
