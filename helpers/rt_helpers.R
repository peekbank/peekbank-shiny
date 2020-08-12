compute_rt <- function(trial, sampling_rate = NULL) {
  d_f0 <- trial %>% 
    filter(t >= 0) 
  
  fst_shift_rle <- rle(d_f0$aoi)
  
  tibble(
    crit_onset_aoi = trial %>% 
      filter(t == 0) %>% 
      pull(aoi),
    fst_shift_land_aoi = fst_shift_rle$values[3],
    shift_type = case_when(
      crit_onset_aoi == "distractor" & fst_shift_land_aoi == "target" ~ "D-T",
      crit_onset_aoi == "distractor" & fst_shift_land_aoi == "other" ~ "D-O",
      crit_onset_aoi == "target" & fst_shift_land_aoi == "distractor" ~ "T-D",
      crit_onset_aoi == "target" & fst_shift_land_aoi == "other" ~ "T-O",
      TRUE ~ "other shift"
    ),
    rt_value = fst_shift_rle$lengths[1] * sampling_rate,
    fst_shift_gap_ms = fst_shift_rle$lengths[2] * sampling_rate,
  )
}