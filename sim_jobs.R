# Code for running on Cluster #####

# source sim function
source("sim_with_strategies")


# make a grid of parameter combinations
# here set to defaults
jobs_default <- expand.grid(tmax=10, 
                    N_plots=100, 
                    N_migrants=20, 
                    N_fams=20, 
                    perc_sq=0.2, 
                    cap_thres_st2=0, 
                    cap_thres_st13=2, 
                    cap_thres_build=0
) # enter parameter ranges

# convert to a list of parameter vectors
jobs_default_list <- as.list( as.data.frame(t(jobs_default)) )


# testing min, middle, and max values
jobs_test <- expand.grid(tmax= c(1, 30, 100),                  # tmax should roughly correspond to years, in the study site this is 30 
                            N_plots= c(10, 100, 3000),            # based on khoroo, but should be in teh 1000s
                            N_migrants=c(10, 100, 1000),          # n of people coming into each khoroo every timestep, should be quite high also but less then max plots
                            N_fams= c(1, 100, 10000),             # everyone has the same fam, resonable chance of having fam in each timestep, low chance of having any fam in env
                            perc_sq=c(0, 0.5, 1),                 # no squatters, half squatters move, all squatters move
                            cap_thres_st2=c(-1, 0, 1),            # just went for 1 sd either direction
                            cap_thres_st13=c(-2, 0, 2),           # went for 2 sd either direction
                            cap_thres_build=c(-1, 0, 1)           # just went for 1 sd either direction
) # enter parameter ranges

# convert to a list of parameter vectors
jobs_test_list <- as.list( as.data.frame(t(jobs_test)) )

# testing multiple sims of most likely values, these are to be updated with pilot data
# run this 1000 times
jobs_realistic <- expand.grid(tmax=30, 
                            N_plots=5000, 
                            N_migrants=1000, 
                            N_fams=20000, 
                            perc_sq=0.2, 
                            cap_thres_st2=0, 
                            cap_thres_st13=2, 
                            cap_thres_build=0
) # enter parameter ranges

# convert to a list of parameter vectors
jobs_realistic_list <- as.list( as.data.frame(t(jobs_realistic)) )






# farm out to cores
library(parallel)

sim_ub_arg_list <- function(arg_list) {
  return(sim_ub(arg_list[1], arg_list[2], arg_list[3], arg_list[4], arg_list[5], arg_list[6], arg_list[7], arg_list[8])) # arg_list needs to be manually updated if sim_ub function inputs changed (i.e. param added/removed)
}


results <- mclapply( jobs_list , sim_ub_arg_list, mc.cores = 3)