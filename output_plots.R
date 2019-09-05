# script for plotting ABM outputs

# load outputs

load("output.Rdata")

# TODO should one spend time on reorganizing the results into a more reasonable format?


# plot n of houses against n of gers (i.e. agents who built house against agents who didn't)
# relevant data is hh_id, plot_house (and can do this longitudinal too to show setting process)
# make this a function because we want to do the same for different parameter combos
# house ownership also encoded in hh_df - in how much is invested, so only need to have a column for land buying added

data_structuring <- function(output){
  # this function joins plot info with agent info for ease of plotting 
  
  in_env <- hh_df_1[hh_df_1$in_env == 1, ]
  
  for(i in 1: nrow(in_env)){
    ind <-  match(in_env$hh_id[i],  plot_ids_output)
    if(ind > 100){
      ind <- ind - 100  # second place in matrix
    }
    
    if(plot_own_output[ind] == 1){
      
      in_env$owns_land[i] <- 1
      
    }else{
      
      in_env$owns_land[i] <- 0
      
    }
  }
  
  in_env$owns_house <- ifelse(is.na(in_env$house_invest), 0, 1)
  
  for(i in 1: nrow(in_env)){
    if (in_env$owns_land[i] == 1 & in_env$owns_house[i] == 1){
      in_env$cat[i] <- "house_owner"
    }
    
    if (in_env$owns_land[i] == 1 & in_env$owns_house[i] == 0){
      in_env$cat[i] <- "plot_owner"
    }
    
    if (in_env$owns_land[i] == 0 & in_env$owns_house[i] == 0){
      in_env$cat[i] <- "ger_squatter"
    }
  }
}

plotprop_plot_single <- function(output, plot_ids_output, plot_own_output, plot_house_output, hh_df){
  # this function plots house vs. ger, and land ownership, for different strategy categories, for single sim run or single timestep
  # param output: data considered in plotting (i.e. simulation run)
  # param plot_ids_output: matrix of plots with agent IDs
  # param plot_own_output: vector containing 0 for land not owned and 1 for land owned
  # param plot_house_output: vector containing 0 for no permanent house and 1 for built house
  # param hh_df: agent dataframe
  # output: plot
  
  
  
  counts <- table(in_env$strategy, in_env$cat)
  
  barplot(t(counts), beside = TRUE)
  
}



