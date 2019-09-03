#script for plotting ABM outputs

#load outputs

results_default_hhdf <- read.csv("results_default_hhdf")
results_dafault_plots <- read.csv()

# TODO should one spend time on reorganizing the results into a more reasonable format?


# plot n of houses against n of gers (i.e. agents who built house against agents who didn't)
# relevant data is hh_id, plot_house (and can do this longitudinal too to show setting process)

d <- data.frame(c(results_default$plot_ids_output.1, results_default$plot_house_output))


