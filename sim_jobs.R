# Code for running on Cluster #####

# source sim function
source("sim_with_strategies.R")

library(parallel)

sim_ub_arg_list <- function(arg_list) {
  return(sim_ub(arg_list[1], arg_list[2], arg_list[3], arg_list[4], arg_list[5], arg_list[6], arg_list[7], arg_list[8], arg_list[9], arg_list[10], arg_list[11], arg_list[12])) # arg_list needs to be manually updated if sim_ub function inputs changed (i.e. param added/removed)
}

# here set to defaults ####
tmax <- 10
jobs_default <- expand.grid(tmax=10, 
                    N_plots=100, 
                    N_migrants=20, 
                    N_fams=20,
                    use_strat= 1,
                    perc_sq=0.2, 
                    cap_thres_st2=0, 
                    cap_thres_st13=2, 
                    cap_thres_build=0,
                    cap_shock_mean=1,
                    cap_shock_dev=0.25,
                    res_log=0
) # enter parameter ranges

# convert to a list of parameter vectors
run_sim <- 20
jobs_default_list <- list(1:run_sim) 
jobs_default_list[1:run_sim] <- as.list( as.data.frame(t(jobs_default)) )

# farm out to cores ####
results_default <- mclapply( jobs_default_list , sim_ub_arg_list, mc.cores = 2)  


####

# testing multiple sims of most likely values, these are to be updated with pilot data ####
# run this run_sim times
jobs_realistic <- expand.grid(tmax=30, 
                              N_plots=5000, 
                              N_migrants=1000, 
                              N_fams=20000,
                              use_strat= 1,
                              perc_sq=0.2, 
                              cap_thres_st2=0, 
                              cap_thres_st13=2, 
                              cap_thres_build=0,
                              cap_shock_mean=2,
                              cap_shock_dev=0.5,
                              res_log=0
) # enter parameter ranges

# convert to a list of parameter vectors
run_sim <- 20
jobs_realistic_list <- list(1:run_sim)    
jobs_realistic_list[1:run_sim] <- as.list( as.data.frame(t(jobs_realistic)) )

# farm out to cores ####
results_realistic <- mclapply( jobs_realistic_list , sim_ub_arg_list, mc.cores = 50) 
####

# example of settling process  - realistic but save each timestep ####

jobs_realistic_seq <- expand.grid(tmax=30, 
                              N_plots=5000, 
                              N_migrants=1000, 
                              N_fams=20000,
                              use_strat= 1,
                              perc_sq=0.2, 
                              cap_thres_st2=0, 
                              cap_thres_st13=2, 
                              cap_thres_build=0,
                              cap_shock_mean=2,
                              cap_shock_dev=0.5,
                              res_log=1
) # enter parameter ranges

# convert to a list of parameter vectors
run_sim <- 10
jobs_realistic_seq_list <- list(1:run_sim) 
jobs_realistic_seq_list <- as.list( as.data.frame(t(jobs_realistic_seq)) )

# farm out to cores ####
results_realistic_seq <- mclapply( jobs_realistic_seq_list , sim_ub_arg_list, mc.cores = 50)


# testing min, middle, and max values ####
jobs_test <- expand.grid(tmax= 30,                        # tmax should roughly correspond to years, in the study site this is 30 
                            N_plots= c(10, 100, 3000),            # based on khoroo, but should be in the 1000s
                            N_migrants=c(10, 100, 1000),          # n of people coming into each khoroo every timestep, should be quite high also but less then max plots
                            N_fams= c(1, 100, 10000),             # everyone has the same fam, resonable chance of having fam in each timestep, low chance of having any fam in env
                            use_strat= c(1, 0),                   # strat used in sim decision making or not
                            perc_sq=c(0, 0.5, 1),                 # no squatters, half squatters move, all squatters move
                            cap_thres_st2=c(-1, 0, 1),            # just went for 1 sd either direction
                            cap_thres_st13=c(-2, 0, 2),           # went for 2 sd either direction
                            cap_thres_build=c(-1, 0, 1),          # just went for 1 sd either direction
                            cap_shock_mean=c(-1, 0, 1),
                            cap_shock_dev=c(0.1, 0.5, 1),
                            res_log=0
) # enter parameter ranges

# convert to a list of parameter vectors
jobs_test_list <- as.list( as.data.frame(t(jobs_test)) )

# farm out to cores ####
results_test <- mclapply( jobs_test_list , sim_ub_arg_list, mc.cores = 50) 
####

# testing changing parameter values ####
# values increase, values decrease, values are stochastic
tmax <- 30
jobs_change <- expand.grid(tmax=tmax, 
                              N_plots=1000, 
                              N_migrants=200, 
                              N_fams=10000, 
                              use_strat= 1,
                              perc_sq= c((seq(from = 0.1, to = 0.9, length.out = tmax)), (seq(from = 0.9, to = 0.1, length.out = tmax)), (sample((seq(from = 0.9, to = 0.1, length.out = tmax)), tmax)) ),  
                              cap_thres_st2=c((seq(from = -2, to = 2, length.out = tmax)), (seq(from = 2, to = -2, length.out = tmax)), (sample((seq(from = 2, to = -2, length.out = tmax)), tmax)) ), 
                              cap_thres_st13=c((seq(from = -2, to = 2, length.out = tmax)), (seq(from = 2, to = -2, length.out = tmax)), (sample((seq(from = 2, to = -2, length.out = tmax)), tmax)) ), 
                              cap_thres_build=c((seq(from = -2, to = 2, length.out = tmax)), (seq(from = 2, to = -2, length.out = tmax)), (sample((seq(from = 2, to = -2, length.out = tmax)), tmax)) ),
                              cap_shock_mean=1,
                              cap_shock_dev=0.25,
                              res_log=0    #logging all would kill it 
) # enter parameter ranges

# convert to a list of parameter vectors
jobs_change_list[1:run_sim] <- as.list( as.data.frame(t(jobs_change)) )

# farm out to cores ####
results_change <- mclapply( jobs_change_list , sim_ub_arg_list, mc.cores = 50) 
####


#variation in capital thresholds ####
jobs_capvar <- expand.grid(tmax= 30,                  
                         N_plots= 5000,            
                         N_migrants= 1000,          
                         N_fams= 10000,
                         use_strat= 1,
                         perc_sq= 0.2,                 
                         cap_thres_st2=c(-2, -1, 0, 1, 2),          
                         cap_thres_st13=c(-2, -1, 0, 1, 2),          
                         cap_thres_build=c(-2, -1, 0, 1, 2), 
                         cap_shock_mean=c(-2, -1, 0, 1, 2),
                         cap_shock_dev=c(0, 0.5, 1),
                         res_log=0
) # enter parameter ranges

# convert to a list of parameter vectors
jobs_capvar_list <- as.list( as.data.frame(t(jobs_capvar)) )

# farm out to cores ####
results_capvar <- mclapply( jobs_capvar_list , sim_ub_arg_list, mc.cores = 50) 
####


# variation in strategy prob assignment ####
jobs_strat <- expand.grid(tmax= 30,                  
                           N_plots= 5000,            
                           N_migrants= 1000,          
                           N_fams= 10000,
                           use_strat= c(1, 0), 
                           perc_sq= 0.2,                 
                           cap_thres_st2= 0 ,          
                           cap_thres_st13= 2,          
                           cap_thres_build= 0, 
                           cap_shock_mean=1,
                           cap_shock_dev=0.25,
                           res_log=0
) # enter parameter ranges

# convert to a list of parameter vectors
jobs_strat_list <- as.list( as.data.frame(t(jobs_strat)) )

# farm out to cores ####
results_strat <- mclapply( jobs_strat_list , sim_ub_arg_list, mc.cores = 50) 

# write results

save(c(results_default, results_realistic, results_test, results_change, results_capvar, results_strat), file = "output.Rdata")

####

# variation in capital shock ####
jobs_capshock <- expand.grid(tmax= 30,                  
                          N_plots= 5000,            
                          N_migrants= 1000,          
                          N_fams= 10000,
                          use_strat= 1, 
                          perc_sq= 0.2,                 
                          cap_thres_st2= 0 ,          
                          cap_thres_st13= 2,          
                          cap_thres_build= 0, 
                          cap_shock_mean=c(-2, -1, 0, 1, 2),
                          cap_shock_dev=c(0, 0.5, 1),
                          res_log=0
) # enter parameter ranges

# convert to a list of parameter vectors
jobs_capshock_list <- as.list( as.data.frame(t(jobs_capshock)) )

# farm out to cores ####
results_capshock <- mclapply( jobs_capshock_list , sim_ub_arg_list, mc.cores = 50) 







