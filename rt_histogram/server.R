# Define server logic required to draw RT histograms ----
rt_result <- d %>% 
  group_by(sub_id, trial_id) %>% 
  nest() %>% 
  mutate(rt = purrr::map(data, .f = compute_rt, sampling_rate = 33)) %>% 
  unnest(rt, .drop = T) %>% 
  filter(shift_type %in% c("D-T", "T-D")) %>% 
  left_join(d_participants, by = c("sub_id")) %>% 
  mutate(age_binned = cut_number(age, n = 3))

rt_ylim_buffer <- 100

server <- function(input, output) {
  # filter rts
  observeEvent( input$filter, {
    rt_filt <- rt_result %>% 
      filter(rt_value >= as.numeric(input$rt_window[1]),
             rt_value <= as.numeric(input$rt_window[2]),
             fst_shift_gap_ms <= as.numeric(input$max_fst_gap))
    
    # compute median rts for different shift types
    rt_median <- rt_filt %>% 
      group_by(shift_type) %>% 
      summarise(rt_median = median(rt_value))
    
    # compute mean rt for each participant based on filtering
    ss_rt <- rt_filt %>% 
      filter(shift_type == "D-T", !is.na(rt_value)) %>% 
      group_by(sub_id, age_binned, age) %>% 
      summarise(m_rt = mean(rt_value)) %>% 
      ungroup() %>% 
      mutate(min_rt = min(m_rt),
             max_rt = max(m_rt))
    
    # Histogram of RTs in peekbank data ----
    # with requested number of bins and RT filters
    output$plot <- renderPlot({
      
      rt_hist <- rt_filt %>% 
        ggplot(aes(x = rt_value)) +
        geom_histogram(bins = input$bins, color = "black", alpha = 0.4) +
        labs(x = "RT (msec)", y = "Count") +
        facet_wrap(~shift_type, ncol = 1) +
        geom_vline(data = rt_median, aes(xintercept = rt_median), 
                   color = "darkorange", size = 1.5)
      
      age_scatter <- ss_rt %>% 
        ggplot(aes(x = age, y = m_rt)) +
        geom_point(shape = 21, size = 3, color = "black", 
                   fill = "grey") +
        geom_smooth(method = "lm") +
        lims(y = c(0, ss_rt$max_rt[1] + rt_ylim_buffer)) +
        labs(x = "Age (months)", y = "Average RT (msec)")
      
      
      cowplot::plot_grid(rt_hist, age_scatter, scale = c(1, 0.9))
    })
  })
}