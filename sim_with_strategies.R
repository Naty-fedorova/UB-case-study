# Functions ####

strategy_assignment <- function(possessions, HC_at_move, intend_stay) {
  # This functions assigns one of three strategies to each agent (household)
  # they can either be assigned urban (1), suburban (2), or temporary (3)
  # strategy is a product of internat preference , possesions outside UB, and household composition at move
  # param possessions: ownership of objects outside environment, i.e. an alternative (0 = none, 1 = some)
  # param HC_at_move: household composition at move, in what composition does hh enter environment (1:single, 2:couple 3:fam with young kids, 4:fam with old kids, 5: fam with res. adult kids, 6:retired couple/single)
  # param intend_stay: intended stay of hh in environment, proxy for internal preference (0 = short(no), 1 = long (yes,sometime))
  # return: 1,2, or 3 representing urban, suburban, and temporary strategy respectively
  
  # this function will change in response to the optimality model
  
  # for urban strategy
  if ((HC_at_move == 1 | HC_at_move == 2 | HC_at_move == 3) & possessions == 0 & intend_stay == 1) {
    sample(1:3, 1, prob = c(.60, .20, .20) )
  }
  
  # for suburban strategy
  if ((HC_at_move == 4 | HC_at_move == 5) & possessions == 0 & intend_stay == 1) {
    sample(1:3, 1, prob = c(.20, .60, .20) )
  }
  
  # for temporary strategy
  if ((HC_at_move == 1 | HC_at_move == 2 | HC_at_move == 5 | HC_at_move == 6 ) & possessions == 1 & intend_stay == 0) {
    sample(1:3, 1, prob = c(.20, .20, .60) )
  } else{
    # if none of these combinations are fulfilled, just random sample
    sample(1:3, 1)
  }
}

get_preference_for_migrant <- function(strategy, capital) {
  # This function decides a migrant's preference between staying with family, or squatting an empty plot 
  # based on their strategy, and capital (could also include all the other factors)
  # preferences are stochastic, preserving nuance (what if you have a really sucky family?)
  # capital only affects the suburban strategy, as those are the only ones prefering squatting 
  # param strategy: whether the agents strategy is urban(1), suburban(2), or temporary(3)
  # param capital: wealth (broadly)
  # return: either "family" or "squat"
  
  # atm I don't have strong reasons for why these options have different likelihoods, discuss
  
  
  # for the suburban strategy
  if (strategy == 2 && capital > 0) {
    if (rbinom(1, 1, prob = 0.99) == 1) {
      return("squat")
    }
    else {
      return("family")
    }
  }
  
  # for the suburban strategy (poor)
  if (strategy == 2 && capital <= 0) {
    if (rbinom(1, 1, prob = 0.60) == 1) {
      return("family")
    }
    else {
      return("squat")
    }
  }
  
  # for urban strategy
  if (strategy == 1) {
    if (rbinom(1, 1, prob = 0.80) == 1) {
      return("family")
    }
    else {
      return("squat")
    }
  }
  
  # for temporary strategy
  if (strategy == 3) {
    if (rbinom(1, 1, prob = 0.80) == 1) {
      return("family")
    }
    else {
      return("squat")
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
    fam_dest_indices <- which(plot_pop[fam_destinations] < plot_capacity )
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
  # squatting is a stochastic process; even if land is free, you might not find it
  # param plot_pop: vector containing plot population size
  # return: index of plot, or 0 if no plots free
  
  empty_destinations <- which(plot_pop == 0)
  if (length(empty_destinations) > 0) {
    
    # chance of finding land, even when it is available
    if(rbinom(1, 1, 0.9) == 1){
      if (length(empty_destinations) == 1)
        return(empty_destinations)
      else
        return(sample(empty_destinations, size=1))
    }
  }
  return(0)
}

finding_land <- function(hh_index, hh_df, plot_ids, plot_pop, plot_capacity){
  # This function carries out the land finding part of the simulation
  # based on preferences derived from strategy and capital
  # agents either first try family, or first try to squat
  # param hh_index: index of the hh_ids under question
  # param hh_df: dataframe of household info
  # param plot_ids: matrix of plots
  # param plot_pop: vector of plot populations
  # param plot_capacity: max plot capacity
  # return: hh_df, plot_ids, plot_pop
  
  destination <- 0
  # preference dictates order of choice between empty or family plot
  preference_of_stay <- get_preference_for_migrant(hh_df$strategy[hh_index], hh_df$capital[hh_index])
  
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
    if ( (plot_pop[destination] > plot_capacity) | (plot_pop[destination] < 0)) {
      print("##### THIS SHOULD NEVER HAPPEN! #####")
      print((plot_ids))
      print(destination)
      print(hh_df$hh_id[hh_index])
      print(plot_pop)
      print(hh_df)
    }
    plot_ids[destination , plot_pop[destination]] <- hh_df$hh_id[hh_index]
    
    # update total mig
    hh_df$total_mig[hh_index] <- hh_df$total_mig[hh_index] + 1
    
    # update hh_df to show which agents are now in the environment/occupying plots
    hh_df$in_env[hh_index] <- 1
  }
  else {
    
    if (hh_df$in_env[hh_index] == 1) { # already in env previously, we have to kill
      
      # remove them from environment, can never come back, they ded
      hh_df$in_env[hh_index] <- 0
      
      # update plot pop and plot ids
      plot_index <- which(plot_ids == hh_df$hh_id[hh_index], arr.ind = TRUE)
      if (length(plot_index) > 0) {
        print(c('#################### problem in finding land', hh_index, plot_index))
      }
    }
    
    # update total mig
    hh_df$total_mig[hh_index] <- hh_df$total_mig[hh_index] + 1
    
    #print(c("The possible plots are full for id: ", hh_df$hh_id[hh_index]))
  }
  return(
    list(
      hh_df = hh_df,
      plot_ids = plot_ids,
      plot_pop = plot_pop
    )
  )
}

leaving_land <- function(strategy, capital_acc, residence_length_total){
  # function that determines whether you should leave the environment or not
  # thresholds in this function can respond to optimal model
  # based on strategy, capital_acc, and residence_length_total
  # param strategy: strategy of agent, atm leaving only assigned for urban and temporary agents
  # param capital_acc: parameter that tracks how much capital has been accumulated (difference from first assignment) over t
  # param residence_length_total: total t spent in environment
  # return: either "leave" or "stay" 
  
  
  # if your strategy is urban and you get above 2 in capital_acc, leave environment (return "leave")
  if(strategy == 1 & capital_acc > 2){
    return("leave")
  } 
  
  # if your strategy is temporary and you get above 5 (or?) in residence_length_total, leave environment (return "leave")
  if(strategy == 3 & residence_length_total > 5){
    return("leave")
  }
  
  # everything else is "stay"
  return("stay")
}

error_check <- function(t, hh_df, plot_ids, plot_pop, message){
  # Function to print out if unexpected stuff happens
  # param t: timestep
  # param hh_df: dataframe containing info about agents
  # param plot_ids: matrix of plots, n of columns = plot_capacity
  # param plot_pop: vector of plot population 
  # param message: informative message about location in code, to be entered
  
  for (i in 1:length(plot_pop)){
    
    # check if seconds plots are never occupied if first plot is empty
    if (plot_ids[i,1] == 0 & plot_ids[i,2] > 0) {
      cat('\n####### ERROR ####### Second plot occupied when first is empty', message, t, i, plot_ids[i], '\n')
    }
    
    # check if plot_pop is tracking plot_ids
    n_plot_ids <- min(plot_ids[i,1], 1) + min(plot_ids[i,2], 1)
    if (plot_pop[i] != n_plot_ids) {
      cat('\n####### ERROR ####### plot_ids not synced with plot_pop', message, t, i, plot_ids[i], plot_pop[i], '\n')
    }
  }
  
  # check if in_env is tracking plot_ids occupation
  hh_present <- which(hh_df$in_env == 1)
  if(length(hh_present) > 0){
    for (i in 1:length(hh_present)){
      plot_row_col <- which(plot_ids == hh_present[i], arr.ind = TRUE)
      if (length(plot_row_col) == 0) {
        cat('\n####### ERROR ####### hh_df$in_env not in sync with plot_ids', message, t, hh_present[i], plot_row_col, '\n')
      }
    }
  }
}

# Simulation #########

sim_ub <- function( tmax=10, N_plots=100, N_migrants=20, N_fams=20, perc_sq=rep(0.2, tmax), cap_thres_st2=rep(0, tmax), cap_thres_st13=rep(2, tmax), cap_thres_build=rep(0, tmax)) {
  # Master function for ABM
  # param tmax: number of runs
  # param N_plots: number of plots in environment
  # param N_migrants: number of agents entering environment at each timestep
  # param N_fams: number of families in environment
  # param perc_sq: percent of squatters that have to move after each timestep
  # param cap_thres_st2: capital threshold needed for strategy 2 agents to buy land 
  # param cap_thres_st13: capital threshold needed for strategy 1 and 3 agents to buy land
  # param cap_thres_build: capital threshold needed for house building
  # return: details of the environment after simulation runs: plot_own, plot_house, plot_ids,hh_df (dataframe tracking agents)
  
  # init ####
  N_hh <- tmax*N_migrants    # init agents (number of households)
  hh_id <- 1:N_hh
  fam_id <- sample( 1:N_fams , size=N_hh, replace = TRUE)
  strategy <- rep(0, times=N_hh)
  capital <- rnorm(N_hh, mean = 0, sd = 2)                # issue here, in real data capital might not be a normally distributed continuous variable
  capital_tminusone <- rep(0, times = N_hh)   # capital at t-1
  capital_acc <- rnorm(N_hh, mean = 0, sd = 1)      # capital accumulation over timesteps spent in env, 
  intend_stay <- sample(0:1, size = N_hh, replace = TRUE)
  HC_at_move <- sample(1:6, size = N_hh, replace = TRUE)
  possessions <- sample(0:1, size = N_hh, replace = TRUE)
  residence_length_plot <- rep(0, times = N_hh)
  residence_length_total <- rep(0, times = N_hh)
  in_env <- rep(0, times = N_hh)
  fam_in_env <- rep(0, times = N_hh)    # is family present in env? 
  house_invest <- rep(NA, times = N_hh)
  total_mig <- rep(0, times = N_hh)
  env_left <- rep(0, times = N_hh) # t at which env was left 
  
  hh_df <- data.frame(hh_id, fam_id, strategy, capital, capital_tminusone, capital_acc, intend_stay, HC_at_move, possessions, residence_length_plot, residence_length_total, total_mig, in_env, fam_in_env, house_invest, env_left)
  
  # strategy assignment
  for (i in 1:nrow(hh_df)){
    hh_df$strategy[i] <- strategy_assignment(possessions = hh_df$possessions[i], HC_at_move = hh_df$HC_at_move[i], intend_stay = hh_df$intend_stay[i])
  }
  
  # init plots
  plot_capacity <- 2 # hard-coded for now
  plot_pop <- rep( 0 , N_plots )
  plot_own <- rep( 0 , N_plots )
  plot_house <- rep( 0 , N_plots )
  plot_ids <- matrix( 0 , nrow=N_plots , ncol=plot_capacity ) #matrix of plots to be filled with hh ids, plot_id index works as plot id
  
  # initialize for loop output
  # lists for hh_df & plot_ids
  hh_df_output <- list(1:tmax)
  plot_ids_output <- list(1:tmax) 
  
  # matrices for plot_pop, plot_house, plot_ids
  plot_pop_output <- matrix(0, nrow = tmax, ncol = N_plots)
  plot_own_output <- matrix(0, nrow = tmax, ncol = N_plots)
  plot_house_output <- matrix(0, nrow = tmax, ncol = N_plots)
  
  
  #####
  
  # Timestep loop ####
  for (t in 1:tmax){
    #print(c("Iteration ", t))
    
    #index of agents that are in the environment
    hh_present <- which(hh_df$in_env == 1)
    
    error_check(t, hh_df, plot_ids, plot_pop, message = "start")
    
    # for each hh, are there any relatives in_env?
    if(length(hh_present) > 0){
      for (i in 1:length(hh_present)){
        hh_fam_id <- hh_df$fam_id[hh_present][i]
        hh_present_other <- hh_present[-i]    # remove agent from set of hh_present 
        all_fam_ids <- hh_df$fam_id[hh_present_other] 
        
        if(hh_fam_id %in% all_fam_ids) {
          hh_df$fam_in_env[hh_present][i] <- 1
        }
      }
    }
    
    # internal migration ####
    
    # stochastic, a few squatters have to relocate, atm this is random 
    # find squatters (and fam on their plot), relocate 
    # find plots of squatters (agents that settled on empty land but don't own it)
    squatter_plot_index <- which((plot_ids[,1] > 0) & (plot_own == 0))
    
    # get 20%  of them
    squatter_plot_index <- sample(squatter_plot_index, (perc_sq[t]*length(squatter_plot_index)), replace = FALSE)
    
    if(length(squatter_plot_index) > 0){
      # get squatter and fam ID so they can move later
      squatter_id <- c(plot_ids[squatter_plot_index], plot_ids[,2][squatter_plot_index])
      
      #remove 0s from squatter_ids
      squatter_id <- squatter_id[squatter_id != 0]
      
      # remove squatters from plots and update plot ids, remove their family, update plot_pop, and residence length plot
      plot_ids[squatter_plot_index] <- 0
      plot_ids[,2][squatter_plot_index] <- 0
      plot_pop[squatter_plot_index] <- 0
      hh_df$residence_length_plot[squatter_id] <- 0
      
      # internal migration for newly displaced squatters 
      # internal migrants get first choice on migration given their premium knowledge of the environment
      for (i in 1:length(squatter_id)){
        hh_index = squatter_id[i] # one agent
        
        l <- finding_land(hh_index, hh_df, plot_ids, plot_pop, plot_capacity)   
        hh_df <- l[["hh_df"]]
        plot_ids <- l[["plot_ids"]]
        plot_pop <- l[["plot_pop"]]
      }
    }
    ##### 
    
    error_check(t, hh_df, plot_ids, plot_pop, message = "after internal mig")
    
    # Suburbans try to find empty land ####
    
    # agents with suburban strategy living with family try to find own land, based on if they have capital to move 
    # if capital is above 0, try to find empty plot
    # agents with urban or suburban strategy want to live with family so don't move them
    
    # get people living with family and with strategy = 2
    visitor_plots <- which(plot_ids[,2] != 0)
    visitor_ids <- plot_ids[,2][visitor_plots]
    visitors_2 <- visitor_ids[which(hh_df$strategy[visitor_ids] == 2)]
    
    if(length(visitors_2) > 0 ){
      for(i in 1:length(visitors_2)){
        # get plot
        plot <- which(plot_ids[,2] == visitors_2[i])
        
        if(hh_df$capital[visitors_2[i]] > 0){
          destination <- get_destination_squat(plot_pop)
          if ( destination > 0 ) {
            
            # if they have found a new plot, remove them from all prior 
            plot_ids[,2][plot] <- 0
            plot_pop[plot] <- plot_pop[plot] - 1
            hh_df$residence_length_plot[visitors_2[i]] <- 0
            
            #put them in new plot & update total mig
            plot_pop[destination] <- plot_pop[destination] + 1
            plot_ids[destination] <- visitors_2[i]
            hh_df$total_mig[visitors_2[i]] <-  hh_df$total_mig[visitors_2[i]] + 1
          }
        }
      }
    }
    #####
    
    error_check(t, hh_df, plot_ids, plot_pop, message = "after visitors")
    
    # inmigration of new migrants ####
    
    # finding land
    for (i in 1:N_migrants){
      #calculate index so household data.frame can be indexed
      hh_index <- (t-1)*N_migrants + i #one agent
      
      l <- finding_land(hh_index, hh_df, plot_ids, plot_pop, plot_capacity)
      
      hh_df <- l[["hh_df"]]
      plot_ids <- l[["plot_ids"]]
      plot_pop <- l[["plot_pop"]]
    }
    #####
    
    error_check(t, hh_df, plot_ids, plot_pop, message = "after new mig")
    
    # buying land ####
    
    # buying land reflects strategy: 
    # if you live on open land, have enough capital accumulated, and have suburban strategy, buy land
    # however, if you have urban or temporary strategy and a lot of capital accumulated, you also buy land (as an investment)
    
    for (i in 1:nrow(plot_ids)){
      # get own occupied patches
      if(plot_ids[i,1] > 0){
        # if strategy is suburban (2), their capital is more than 0, and they are living on their own plot, they can buy the land (stochastic)
        if(hh_df$capital_acc[plot_ids[i,1]] > cap_thres_st2[t] & plot_own[i] == 0 & hh_df$strategy[plot_ids[i,1]] == 2){  
          plot_own[i] <- rbinom(1, 1, 0.7) #stochastic
        }
        
        #if strategy is urban (1) or temporary (3), their capital more than 2, and they are living on their own plot, they can buy the land (stochastic)
        if(hh_df$capital_acc[plot_ids[i,1]] > cap_thres_st13[t] & plot_own[i] == 0 & (hh_df$strategy[plot_ids[i,1]] == 1 | hh_df$strategy[plot_ids[i,1]] == 3)){ 
          plot_own[i] <- rbinom(1, 1, 0.7) #stochastic
        }
      }
    }
    ##### 
    
    # building home ####
    # if live on family land stay in ger, so ignore
    # if live on own land, can build, provided you have enough capital OR you have family in the env able to help(check against pilot data)
    # investment proportional to capital
    for (i in 1:length(plot_own)){
      if(plot_ids[i,1] > 0){ # if the agent is still present
        if(plot_own[i] == 1){
          if((plot_house[i] == 0 & hh_df$capital[plot_ids[i,1]] >= cap_thres_build[t]) | (plot_house[i] == 0 & hh_df$fam_in_env[plot_ids[i,1]] ==1)){
            plot_house[i] <- rbinom(1, 1, 0.7)
            
            if(plot_house[i] == 1){
              hh_df$house_invest[plot_ids[i,1]] <- rnorm(1, hh_df$capital[plot_ids[i,1]])
            }
          }
        }
      }
    }
    ##### 
    
    #update hh_present
    hh_present <- which(hh_df$in_env == 1)
    
    # update residence time ####
    
    # for agents that are in the environment, residence +1
    for (i in 1:length(hh_present)){
      hh_df$residence_length_total[hh_present][i] <- hh_df$residence_length_total[hh_present][i] + 1
      
      # update plot residence length
      hh_df$residence_length_plot[hh_present][i] <- hh_df$residence_length_plot[hh_present][i] + 1
    }
    #####
    
    # stochastic shock to capital ####
    
    # city life is a lottery
    # for agents in env, capital gets shocked, this difference is then accumulated over timesteps
    for (i in 1:length(hh_present)){
      hh_df$capital_tminusone[hh_present][i] <- hh_df$capital[hh_present][i]
      
      # TODO level of capital shock (improvement or not) can be determined from pilot data
      hh_df$capital[hh_present][i] <- hh_df$capital[hh_present][i] + rnorm(1, 0, 0.25) 
      hh_df$capital_acc[hh_present][i] <- hh_df$capital_acc[hh_present][i] + (hh_df$capital[hh_present][i] - hh_df$capital_tminusone[hh_present][i]) 
      
    }
    #####
    
    error_check(t, hh_df, plot_ids, plot_pop, message = "before leaving")
    
    # leaving land ####
    
    # individuals that have reached leaving threshold must be removed from env
    for (i in 1:length(hh_present)){
      
      threshold_status <- leaving_land(strategy = hh_df$strategy[hh_present[i]], capital_acc = hh_df$capital_acc[hh_present[i]], residence_length_total = hh_df$residence_length_total[hh_present[i]])
      
      if(threshold_status == "leave"){ 
        
        plot_row_col <- which(plot_ids == hh_df$hh_id[hh_present[i]], arr.ind = TRUE)
        plot_row <- plot_row_col[1]
        
        # if agent is leaving first space on plot, and second place is occupied, move occupee of second place to first, otherwise remove agent      
        if((plot_row_col[2] == 1) && (plot_ids[plot_row, 2] != 0)){
          plot_ids[plot_row, 1] <- plot_ids[plot_row, 2]
          plot_ids[plot_row, 2] <- 0
          plot_pop[plot_row] <- plot_pop[plot_row] - 1
        } else{
          plot_ids[plot_row_col] <- 0
          plot_pop[plot_row] <- plot_pop[plot_row] - 1
        }
        
        # need to update hh_df for agent: in_env and left_env
        hh_df$in_env[hh_present[i]] <- 0
        hh_df$env_left[hh_present[i]] <- t
      }
    }
    ####
    
    error_check(t, hh_df, plot_ids, plot_pop, message = "after leaving ")
    
    # add output to list and matrix
    hh_df_output[[t]] <- hh_df
    plot_ids_output[[t]] <- plot_ids
    plot_pop_output[t, ] <- plot_pop
    plot_own_output[t, ] <- plot_own
    plot_house_output[t, ] <- plot_house
    
  }#t
  
  return(
    list( 
      hh_df_output = hh_df_output ,
      plot_ids_output = plot_ids_output ,
      plot_pop_output = plot_pop_output ,
      plot_own_output = plot_own_output ,
      plot_house_output = plot_house_output
    )
  )
}



# Notes

# 1: urban strategy: goal is to live in the central city, centrality maximization
# 2: suburban strategy: goal is to live in the ger districts, space maximization
# 3: temporary strategy: goal is to extract capital from UB, capital maximization

# questions
# how to include household composition in here (without making this a crazy model) atm just used for strategy and recorded at environment entry
# how does experience with urb. env affect people differentially (in the capital shocks for example?)
# btw shouldn't strategy also depend on where you are in the decision making process? i.e. if you're a land owner, does this change your strategy? most importantly household composition
# stochastic leaving for strategy 2?











