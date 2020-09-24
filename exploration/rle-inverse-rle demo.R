foo <- select(aoi_data_joined, administration_id, trial_id, t_norm, aoi) 
rled <- foo %>%
  group_by(administration_id, trial_id) %>%
  summarise(lengths = rle(aoi)$lengths, 
            values = rle(aoi)$values) 

tic()
unrled <- rled %>%
  group_by(administration_id, trial_id) %>%
  nest() %>%
  mutate(rle_vector = map(data,
                           ~ `class<-`(list(lengths = .$lengths, values = .$values), "rle")), 
            inverse_vector = map(rle_vector,inverse.rle)) %>%
  select(-data, -rle_vector) %>%
  unnest(inverse_vector)
toc()