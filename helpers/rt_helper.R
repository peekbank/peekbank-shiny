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
  
  # rt is the number of samples happening before arrival + 1 
  # (first sample of arrival)
  # times the length of a sample
  rt <- (sum(values_before_first_landing) + 1) * (1000/SAMPLING_RATE)
  
  return(tibble(rt = rt, 
                shift_type = shift_type))
}