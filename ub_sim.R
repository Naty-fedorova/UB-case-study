library(rethinking)

sim_ub <- function( tmax=10 , N_plots=100 , N_migrants=20 , plot_capacity=2 , plot_cost=1 , N_fams=20 ) {
  
  # init plots
  
  plot_pop <- rep( 0 , N_plots )
  plot_house <- rep( 0 , N_plots )
  plot_ids <- matrix( 0 , nrow=N_plots , ncol=plot_capacity ) # family ids
  
  # loop
  
  for ( t in 1:tmax ) {
    for ( i in 1:N_migrants ) {
      # look for a plot
      # do I have family here?
      # look in all plots_ids for my ID
      destination <- 0
      my_id <- sample( 1:N_fams , size=1 )
      fam_hits <- which( plot_ids==my_id , arr.ind=TRUE )[,1]
      if ( length(fam_hits)>0 ) {
        # we have family, so move in with them, as long as they are not full
        j <- which( plot_pop[fam_hits] < plot_capacity )
        if (length(j)>0) {
          if ( length(fam_hits[j])==1 )
            destination <- fam_hits[j]
          else
            destination <- sample( fam_hits[j] , size=1 )
        }
      }
      if ( destination==0 ) {
        # still not located, so try to find an empty plot
        empty <- which( plot_pop == 0 )
        if (length(empty)>0) {
          if ( length(empty)==1 )
            destination <- empty
          else
            destination <- sample(empty,size=1)
        }
      }
      # occupy!
      if ( destination > 0 ) {
        plot_pop[destination] <- plot_pop[destination] + 1
        #print( plot_pop[destination] )
        if ( plot_pop[destination] > plot_capacity ) {
          print((plot_ids))
          print(destination)
          print(my_id)
          print(fam_hits)
        }
        plot_ids[ destination , plot_pop[destination] ] <- my_id
        #print( plot_ids[ destination , ] )
      }
    }#i
  }#t
  
  return(
    list(
      plot_pop = plot_pop,
      plot_house = plot_house,
      ids = plot_ids
    )
  )
  
}

s <- sim_ub(tmax=100,N_plots=50)
