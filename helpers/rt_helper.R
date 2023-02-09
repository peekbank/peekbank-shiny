## RT function tested in peekbank_rts.Rmd
# takes rle_data dataframe (already rle'd)
get_rt <- function (rle_data, SAMPLING_RATE = 40) {
  
  # end if no data
  if (is.null(rle_data$values) | is.null(rle_data$lengths)) {
    return(tibble(rt = NA, 
                  shift_type = NA))
  }
  
  onset_aoi <- rle_data$values[1] # zero point AOI
  
  # end if missing for start
  if (!(onset_aoi %in% c("target","distractor"))) {
    return(tibble(rt = NA, 
                  shift_type = "other"))
  }
  
  first_landing <- rle_data$values[rle_data$values != onset_aoi &
                                     rle_data$values %in% c("target","distractor")][1]
  
  # end if no shift
  if (is.na(first_landing)) {
    return(tibble(rt = NA, 
                  shift_type = "no shift"))
  }
  
  shift_type <- case_when(onset_aoi == "distractor" &
                            first_landing == "target" ~ "D-T",
                          onset_aoi == "target" &
                            first_landing == "distractor" ~ "T-D",
                          TRUE ~ "other")
  
  first_landing_idx <- which(rle_data$values == first_landing)[1]
  
  values_before_first_landing <- rle_data$lengths[1:(first_landing_idx-1)]
  value_before_first_shift <- rle_data$lengths[1]
  
  # if (first_landing_idx>2 & length(rle_data$lengths)>1) {
  #   shift_values <- rle_data$lengths[2:(first_landing_idx-1)]
  # } else {
  #   shift_values <- c()
  # }
  
  # rt is the number of samples happening before arrival + 1 
  # (first sample of arrival)
  # times the length of a sample
  landing_time_rt <- (sum(values_before_first_landing) + 1) * (1000/SAMPLING_RATE)
  
  # rt is the number of samples happening before a shift is initiated + 1
  # first sample off of onset AOI prior to moving to the next location
  # times the length of a sample
  shift_start_rt <- (value_before_first_shift + 1) * (1000/SAMPLING_RATE)
  
  # shift length: how long is the time between shift initiation and first landing
  shift_length <- landing_time_rt - shift_start_rt
    
  
  return(tibble(rt = landing_time_rt, # define rt as landing time RT for now to avoid breaking things downstream
                shift_start_rt = shift_start_rt,
                shift_type = shift_type,
                shift_length = shift_length))
}
