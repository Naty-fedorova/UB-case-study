library(rethinking)


get_preference_for_migrant <- function(capital, intend_stay) {
  # This function decided a migrants preference between staying with family, or squatting a empty plot 
  # based on their capital and intended stay
  # preferences are symmetrical - 2 options are very likely, and in 2 options preferences are weak
  # param capital: is both wealth and general ability of agents to get things done
  # param intend_stay: is how long agents intend to stay in environment
  # return: either "family" or "squat"
  
  if (capital > 0 && intend_stay > 0) {
    if (rbinom(1, 1, prob = 0.99) == 1) {
      return("squat")
    }
    else {
      return("family")
    }
  }
  if (capital > 0 && intend_stay <= 0) {
    if (rbinom(1, 1, prob = 0.50) == 1) {
      return("squat")
    }
    else {
      return("family")
    }
  }
  if (capital <= 0 && intend_stay > 0) {
    if (rbinom(1, 1, prob = 0.50) == 1) {
      return("squat")
    }
    else {
      return("family")
    }
  }
  if (capital <= 0 && intend_stay <= 0) {
    if (rbinom(1, 1, prob = 0.01) == 1) {
      return("squat")
    }
    else {
      return("family")
    }
  }
}

get_destination_family <- function(my_fam_id, plot_ids, plot_pop, plot_capacity, hh_df){
  # This function gets the destination (index of plot) of agent's family member where the plot is not at full capacity
  # param my_fam_id: agents family id, taken from hh_df
  # param plot_ids: matrix of plots containing hh ids
  # param plot_pop: vector containing plot population size
  # param plot_capacity: capacity of plot, max value of agents allowed
  # param hh_df: dataframe containing agent info
  # return: index of plot, or 0 if no plots free or no family available
  
  # get hh_ids of all those that are in my family
  my_fam <- subset(hh_df$hh_id, hh_df$fam_id == my_fam_id)
  
  # get index of plots where these family members live
  fam_destinations <- which(plot_ids %in% my_fam)

  # check if family plot is under capacity, if yes, move there, otherwise destination = 0 
  if ( length(fam_destinations)>0 ) {
    fam_dest_indices <- which( plot_pop[fam_destinations] < plot_capacity )
    if (length(fam_dest_indices) > 0) {
      if (length(fam_dest_indices) == 1)
        return(fam_destinations[fam_dest_indices])
      else
        return(sample(fam_destinations[fam_dest_indices], size=1))
    }
  }
  return(0)
}
  
get_destination_squat <- function(plot_pop) {
  # This function gets the destination (index of plot) of empty plot
  # param plot_pop: vector containing plot population size
  # return: index of plot, or 0 if no plots free
  
  empty_destinations <- which(plot_pop == 0)
  if (length(empty_destinations) > 0) {
    if (length(empty_destinations) == 1)
      return(empty_destinations)
    else
      return(sample(empty_destinations, size=1))
  }
  return(0)
}

sim_ub <- function( tmax=10 , N_plots=100 , N_migrants=20 , plot_capacity=2 , plot_cost=1 , N_fams=20) {
  # Master function for ABM
  # param tmax: number of runs
  # param N_plots: number of plots in environment
  # param N_migrants: number of agents entering environment at each timestep
  # param plot_capacity: number of agents that can stay at each plot
  # param plot_cost: price of plot
  # param N_fams: number of families in environment
  # return: details of the environment after simulation runs
  
  # init agents
  N_hh <- tmax*N_migrants
  
  hh_id <- 1:N_hh
  fam_id <- sample( 1:N_fams , size=N_hh, replace = TRUE)
  capital <- rnorm(N_hh)
  intend_stay <- rnorm(N_hh)
  residence_length_plot <- rep(0, times = N_hh)
  residence_length_total <- rep(0, times = N_hh)
  in_env <- rep(0, time = N_hh)
  house_invest <- rep(0, time = N_hh)
  
  hh_df <- data.frame(hh_id, fam_id, capital, intend_stay, residence_length_plot, residence_length_total, in_env, house_invest)
  
  # init plots
  plot_pop <- rep( 0 , N_plots ) #TODO remove this, can just use length of plot_ids row
  plot_own <- rep( 0 , N_plots )
  plot_house <- rep( 0 , N_plots )
  plot_ids <- matrix( 0 , nrow=N_plots , ncol=plot_capacity ) #matrix of plots to be filled with hh ids
  
  # loop
  for ( t in 1:tmax ) {
    print(c("Iteration ", t))
    
    #TODO 
    # stochastic change to capital at start of each time step
    # remove squatters that did not buy land
    
    # finding land
    for ( i in 1:N_migrants ) {
      #calculate index so household data.frame can be indexed
      hh_index = (t-1)*N_migrants + i
      destination <- 0
      
      # preference dictates order of choice between empty or family plot
      preference_of_stay <- get_preference_for_migrant(hh_df$capital[hh_index], hh_df$intend_stay[hh_index])
      
      if (preference_of_stay == "family") {
        destination <- get_destination_family( hh_df$fam_id[hh_index], plot_ids, plot_pop, plot_capacity, hh_df)
        if ( destination==0 ) {
          destination <- get_destination_squat(plot_pop)
        }
      }
      else { # (preference_of_stay == "squat") 
        destination <- get_destination_squat(plot_pop)
        if(destination==0){
          destination <- get_destination_family(hh_df$fam_id[hh_index], plot_ids, plot_pop, plot_capacity, hh_df)
        }
      }
      
      # occupy!
      if ( destination > 0 ) {
        plot_pop[destination] <- plot_pop[destination] + 1
        if ( plot_pop[destination] > plot_capacity ) {
          print("##### THIS SHOULD NEVER HAPPEN! #####")
          print((plot_ids))
          print(destination)
          print(hh_df$hh_id[hh_index])
          print(plot_pop)
          print(hh_df)
        }
        plot_ids[destination , plot_pop[destination]] <- hh_df$hh_id[hh_index]
        
        #update hh_df to show which agents are now in the environment/occupying plots
        hh_df$in_env[hh_index] <- 1
      }
      else {
        print(c("The possible plots are full for id: ", hh_df$hh_id[hh_index]))
      }
    }# finding land
    
    # buying land
    # if live on available land from previous round, and have sufficient capital - buy land
    # if live on family land already for *some time*, look for new land
    
    #go through plots
    for(i in 1:nrow(plot_ids)){
      # get own occupied patches
      if(plot_ids[i,1] > 0){
        # if their capital is more than 0, they can buy the land (stochastic)
        if(hh_df$capital[plot_ids[i,1]] > 0 & plot_own[i] == 0){
          plot_own[i] <- rbinom(1, 1, 0.7) #stochastic
        }
      }
      
    }
    
    # building home
    # if live on family land stay in ger, so ignore
    # if live on own land, can build
    # investment proportional to capital and intended stay
    # min capital needed for building house:0 
    for(i in 1:length(plot_own)){
      if(plot_own[i] == 1){
        
        if(plot_house[i] == 0){
          plot_house[i] <- rbinom(1, 1, 0.7)
          
          if(plot_house[i] == 1){
            hh_df$house_invest[plot_ids[i,1]] <- rnorm(1, hh_df$capital[plot_ids[i,1]] + hh_df$intend_stay[plot_ids[i,1]] )
          }
        }
      }
    }
    

    

    
    
    
    #TODO end of each timestep - update total residence
    #TODO can only squat for one timestep
    
  }#t
  
  return(
    list(
      plot_own = plot_own,
      plot_house = plot_house, 
      ids = plot_ids,
      hh_df = hh_df
    )
  )
  
}

s <- sim_ub(tmax=10,N_plots=100)
