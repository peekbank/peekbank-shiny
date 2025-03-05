library(peekbankr)
library(tidyverse)
library(tictoc)

input <- list(dataset_name = c("pomper_saffran_2016"), analysis_window_range = c(250,2250),
              plot_window_range = c(-1000,4000), age_range = c(8, 84))

datasets <- get_datasets()
administrations <- get_administrations(dataset_name = input$dataset_name)  %>%
  mutate(age_binned = "all ages")
tic()
aoi_timepoints <- get_aoi_timepoints(dataset_name = input$dataset_name, age = input$age_range)
toc()
stimuli <- get_stimuli(dataset_name = input$dataset_name)
trials <- get_trials(dataset_name = input$dataset_name)
trial_types <- get_trial_types(dataset_name = input$dataset_name)

aoi_data_joined <- function() {
aoi_timepoints %>%
  right_join(administrations) %>%
  right_join(trials) %>%
  right_join(trial_types) %>%
  right_join(datasets) %>%
  mutate(stimulus_id = target_id) %>%
  right_join(stimuli) %>%
  filter(t_norm > input$plot_window_range[1],
         t_norm < input$plot_window_range[2]) 
}
  
aoi_data_joined()


# default_admins <- saveRDS(administrations, "cached_data/administrations.Rds")
# aoi_timepoints <- saveRDS(aoi_timepoints, "cached_data/aoi_timepoints.Rds")
