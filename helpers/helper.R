# write_to_subjects
write_to_subjects <- function(df) {
  # insert validations here
  # each column must exist
  # each column's contents must match the appropriate datatype
  
  
  if (file.exists(here::here("data/processed-data/subjects.csv"))) {
    subjects <- read_csv(here::here("data/processed-data/subjects.csv"))
    subjects <- bind_rows(subjects, df)
  } else {
    subjects <- df 
  }
  
  write_csv(subjects, here::here("data/processed-data/subjects.csv"))
}

# write_to_datasets
write_to_datasets <- function(df) {
  # insert validations here
  # each column must exist
  # each column's contents must match the appropriate datatype
  
  
  if (file.exists(here::here("data/processed-data/datasets.csv"))) {
    datasets <- read_csv(here::here("data/processed-data/datasets.csv"))
    datasets <- bind_rows(datasets, df)
  } else {
    datasets <- df 
  }
  
  write_csv(datasets, here::here("data/processed-data/datasets.csv"))
}


# write_to_trials
write_to_trials <- function(df) {
  # insert validations here
  # each column must exist
  # each column's contents must match the appropriate datatype
  
  
  if (file.exists(here::here("data/processed-data/trials.csv"))) {
    trials <- read_csv(here::here("data/processed-data/trials.csv"))
    trials <- bind_rows(trials, df)
  } else {
    trials <- df 
  }
  
  write_csv(trials, here::here("data/processed-data/trials.csv"))
}

## function for trial_id
create_zero_index <- function(data, id_column_name="Subject") {
  data <- data %>%
    mutate(query_lag = lag(Query), 
           temp = ifelse(Query != query_lag, 1, 0), 
           temp_id = cumsum(c(0, temp[!is.na(temp)])), 
           trial_id = temp_id)
}

#combining trial level and kl level data 
combine_data <- function(df1, df2) {
  ##strip KL data down 
  kls <- kl_only_data %>%
    dplyr::select(Experiment, Subject, KL)
  
  ##join matching rows
  ##so this df has the trial-level data, and trial-level data that has KLs
  ##but otherwise it does not have the KL data
  ##so we need to pull that in
  trial_with_kl <- left_join(trial_level_data, kls, by = c("Experiment", "Subject"))
  
  ##so now we need the kl data that does not have a match in trial_level data
  kls_without_trial <- anti_join(kl_only_data, trial_level_data, by = c("Experiment", "Subject"))
  
  ##now we can put these together
  all_data <- full_join(trial_with_kl, kls_without_trial)
  
  return(all_data)
}
