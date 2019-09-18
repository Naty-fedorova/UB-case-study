# script for plotting ABM outputs

install.packages("viridis")
library(viridis)

# load outputs

load("outputs.Rdata")
load("output_capshock.Rdata")

# TODO should one spend time on reorganizing the results into a more reasonable format?


# plot n of houses against n of gers (i.e. agents who built house against agents who didn't)
# relevant data is hh_id, plot_house (and can do this longitudinal too to show setting process)
# make this a function because we want to do the same for different parameter combos
# house ownership also encoded in hh_df - in how much is invested, so only need to have a column for land buying added

data_structuring <- function(output){
  # this function joins plot info with agent info for ease of plotting 
  # TODO add description here
  
  in_env <- hh_df[hh_df$in_env == 1, ]
  
  for(i in 1: nrow(in_env)){
    ind <-  match(in_env$hh_id[i],  plot_ids_output)
    if(ind > (length(plot_ids_output)/2)){
      ind <- ind - (length(plot_ids_output)/2)   # second place in matrix
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
  return(in_env)
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


# plotting settling process from realistic parameter set ####

hh_df_rel <- list()

for(i in 1:30){
  hh_df <- results_realistic_seq$V1$hh_df_output[i]
  hh_df <- hh_df[[1]]
  plot_ids_output <- results_realistic_seq$V1$plot_ids_output[i]
  plot_ids_output <- plot_ids_output[[1]]
  plot_own_output <- results_realistic_seq$V1$plot_own_output[i, ]
  
  in_env <- data_structuring(hh_df)
  
  hh_df_rel[[i]] <- in_env
  
}

freq <- list()

for(i in 1:30){
  freq[[i]] <- table(hh_df_rel[[i]][["cat"]])
}

# plotting


plot(0, ylim = c(0, 3000), xlim= c(0, 30), type = "n", xlab = "Timestep", ylab = "Frequency", family = "mono", axes = FALSE)
axis(1, at = seq(0,30, 5), lwd = 0.5, col = "gray40", col.axis = "gray40")
axis(2, at = seq(0,3000, 500), lwd = 0.5, col = "gray40", col.axis = "gray40")


point_col <- viridis(4)

for(i in 1:30){
  n_ger_squatter <- 0
  if("ger_squatter" %in% names(freq[[i]])) {
    n_ger_squatter <-freq[[i]][["ger_squatter"]]
  }
  points(x = i, y = n_ger_squatter, pch = 1, col = point_col[1], lwd = 2)
  points(x = i, y = freq[[i]][["plot_owner"]], pch = 0, col = point_col[2], lwd = 2)
  points(x = i, y = freq[[i]][["house_owner"]], pch = 7, col = point_col[3], lwd = 2)
}

legend(x = 18, y = 1000, c("ger squatter", "ger plot owner", "house&plot owner"), pch = c(1, 0, 7), col = c(point_col[1], point_col[2], point_col[3]), cex = 0.6, bty = "n")
####

# plotting outcome for different levels of strategy strength

hh_df_strat <- list()

for(i in 1:4){
  hh_df <- results_strat[[i]][["hh_df_output"]]
  plot_ids_output <- results_strat[[i]][["plot_ids_output"]]
  plot_own_output <- results_strat[[i]][["plot_own_output"]]
  
  in_env <- data_structuring(hh_df)
  
  hh_df_strat[[i]] <- in_env
}


strat_1 <- table(hh_df_strat[[1]][["strategy"]], hh_df_strat[[1]][["cat"]])
strat_2 <- table(hh_df_strat[[2]][["strategy"]], hh_df_strat[[2]][["cat"]])
strat_3 <- table(hh_df_strat[[3]][["strategy"]], hh_df_strat[[3]][["cat"]])
strat_4 <- table(hh_df_strat[[4]][["strategy"]], hh_df_strat[[4]][["cat"]])

# plotting

bar_col <- viridis(3)

par(mfrow=c(1,4))
barplot(start_1, col= c(bar_col[1], bar_col[2], bar_col[3]), xlab = "0.1", ylim = c(0,3500), xlim = c(0, 20), xaxt='n', space = 1, width = 10 )
barplot(start_2, col= c(bar_col[1], bar_col[2], bar_col[3]), xlab = "0.5", ylim = c(0,3500), xlim = c(0, 20), xaxt='n', space = 1, width = 10 )
barplot(start_3, col= c(bar_col[1], bar_col[2], bar_col[3]), xlab = "0.7", ylim = c(0,3500), xlim = c(0, 20), xaxt='n', space = 1, width = 10 )
barplot(start_4, col= c(bar_col[1], bar_col[2], bar_col[3]), xlab = "1", ylim = c(0,3500), xlim = c(0, 20), xaxt='n', space = 1, width = 10 )

####

# plotting for different levels of capital shock ####

hh_df_capshock <- list()
capshock_table <- list()

for(i in 1:15){
  hh_df <- results_capshock[[i]][["hh_df_output"]]
  plot_ids_output <- results_capshock[[i]][["plot_ids_output"]]
  plot_own_output <- results_capshock[[i]][["plot_own_output"]]
  
  in_env <- data_structuring(hh_df)
  
  hh_df_capshock[[i]] <- in_env
}

for(i in 1:15){ 
  capshock_table[[i]] <- table(hh_df_capshock[[i]][["strategy"]], hh_df_capshock[[i]][["cat"]])
}

# plotting

bar_col <- viridis(3)

par(mfrow=c(3,5))
for(i in 1:4){
  
  # TODO this doesn't work atm because figure margins are too large
  barplot(capshock_table[[i]])
}




