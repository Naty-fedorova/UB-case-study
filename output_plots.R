# script for plotting ABM outputs

install.packages("viridis")
library(viridis)

# load outputs

load("outputs.Rdata")
load("output_capshock.Rdata")
load("output_strat.Rdata")
load("output_realseq.Rdata")



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



# plotting settling process from realistic parameter set ####

hh_df_rel <- rep(list(list()), 10)

for(j in 1:10){
  for(i in 1:30){
    hh_df <- results_realistic_seq[[j]]$hh_df_output[i]
    hh_df <- hh_df[[1]]
    plot_ids_output <- results_realistic_seq[[j]]$plot_ids_output[i]
    plot_ids_output <- plot_ids_output[[1]]
    plot_own_output <- results_realistic_seq[[j]]$plot_own_output[i, ]
    
    in_env <- data_structuring(hh_df)
    
    hh_df_rel[[j]][[i]] <- in_env
  }
}


freq <- rep(list(list()), 10)

for(j in 1:10){
  for(i in 1:30){
    freq[[j]][[i]] <- table(hh_df_rel[[j]][[i]][["cat"]])
  }
}

# plotting
png(filename = "Rplot_process.png", res = 300, height = 10, width = 10, units = "cm")
plot.new()
plot(0, ylim = c(0, 4000), xlim= c(0, 30), type = "n", xlab = "Timestep", ylab = "Frequency", line = 2, axes = FALSE)
axis(1, at = seq(0,30, 5), lwd = 0.5)
axis(2, at = seq(0,5000, 500), lwd = 0.5)
point_col <- cividis(4)

for(i in 1:30){
  n_ger_squatter <- 0
  if("ger_squatter" %in% names(freq[[1]][[i]])) {
    n_ger_squatter <-freq[[1]][[i]][["ger_squatter"]]
  }
  points(x = i, y = n_ger_squatter, pch = 1, col = point_col[1], lwd = 2)
  points(x = i, y = freq[[1]][[i]][["plot_owner"]], pch = 0, col = point_col[2], lwd = 2)
  points(x = i, y = freq[[1]][[i]][["house_owner"]], pch = 7, col = point_col[3], lwd = 2)
}

for(j in 2:10){
  for(i in 1:30){
    n_ger_squatter <- 0
    if("ger_squatter" %in% names(freq[[j]][[i]])) {
      n_ger_squatter <-freq[[j]][[i]][["ger_squatter"]]
    }
    points(x = i, y = n_ger_squatter, pch = 1, col = point_col[1])
    points(x = i, y = freq[[j]][[i]][["plot_owner"]], pch = 0, col = point_col[2])
    points(x = i, y = freq[[j]][[i]][["house_owner"]], pch = 7, col = point_col[3])
  }
}


legend(x = 18, y = 1000, c("ger squatter", "ger plot owner", "house&plot owner"), pch = c(1, 0, 7), col = c(point_col[1], point_col[2], point_col[3]), cex = 0.6, bty = "n")

dev.off()
####

# plotting outcome for different levels of strategy strength

hh_df_strat <- list()

for(i in 1:2){
  hh_df <- results_strat[[i]][["hh_df_output"]]
  plot_ids_output <- results_strat[[i]][["plot_ids_output"]]
  plot_own_output <- results_strat[[i]][["plot_own_output"]]
  
  in_env <- data_structuring(hh_df)
  
  hh_df_strat[[i]] <- in_env
}

strat_1 <- table(hh_df_strat[[1]][["strategy"]], hh_df_strat[[1]][["cat"]])
strat_2 <- table(hh_df_strat[[2]][["strategy"]], hh_df_strat[[2]][["cat"]])

# add empty ger_squatters to strat_1
ger_squatters <- c(0,0,0)
strat_1 <- cbind(ger_squatters, strat_1)

# plotting
png(filename = "Rplot_strat.png", res = 300, height = 10, width = 10, units = "cm")

bar_col <- cividis(4)
par(mfrow=c(1,2), mar = c(2, 3, 1, 1))
barplot(strat_1, col= c(bar_col[1], bar_col[2], bar_col[3]), xlab = "Strategy present", ylim = c(0,4000), xaxt='n', space = 1, width = 20, line = 1 )
text(x = c(30,70,110), y = c(3900,3900,3900), labels = c("gs", "gpo", "hpo"), cex = 0.7)

barplot(strat_2, col= c(bar_col[1], bar_col[2], bar_col[3]), xlab = "Strategy absent", ylim = c(0,4000), xaxt='n', space = 1, width = 20, yaxt = "n", line = 1)
text(x = c(30,70,110), y = c(2500,2500,2500), labels = c("gs", "gpo", "hpo"), cex = 0.7)

legend("topright", c("Urban strategy", "Suburban strategy", "Temporary strategy"), pch = c(15,15,15), col = c(bar_col[1], bar_col[2], bar_col[3], "black"), cex = 0.6, bty = "n")

dev.off()
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
  capshock_table[[i]] <- table(hh_df_capshock[[i]][["cat"]])
}

# plotting
# version names
#load("capshock_v.Rdata")

png(filename = "Rplot_capshock.png", res = 300, height = 10, width = 10, units = "cm")

bar_col <- cividis(4)
plot.new()
par(mfrow=c(3,5), mar = c(0.5, 3, 1, 0), oma = c(1,1,1,1))
axis(2, at = seq(0, 7000, 2000), lwd = 0.5, col = "gray40", col.axis = "gray40"  )
lab <- c("μ=-2", "μ=-1", "μ=0", "μ=1", "μ=2")    # should extract this directly from table names
lab_y <- c("σ=0","","","","","σ=0.5","", "", "", "", "σ=1")


for(i in 1:15){
  # TODO this doesn't work atm because figure margins are too large
  yaxt_on = "n"
  if (i == 1 | i == 6 | i == 11) {
    yaxt_on = "s"
  }
  
  if(length(capshock_table[[i]]) == 3){
    data_to_plot <- capshock_table[[i]]
  } else{
    data_to_plot <- c(0, capshock_table[[i]][1], capshock_table[[i]][2])
  }
  barplot(data_to_plot, col = c(bar_col[1], bar_col[2], bar_col[3]), xaxt = "n", yaxt = yaxt_on, ylim = c(0, 6000), space = 0.5, width = 20)
  
  if(i == 1 | i == 2 | i == 3 | i == 4 | i == 5){
    mtext(text = lab[i], side = 3)
  }
  if (i == 1 | i == 6 | i == 11) {
    mtext(text = lab_y[i], side = 2, line = 2)
  }
}

dev.off()


