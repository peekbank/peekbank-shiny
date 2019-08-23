library(tidyverse)
library(eyetrackingR)
library(ggthemes)
library(langcog)
source(here::here("rt_histogram/rt_helpers.R"))

ci.95 <- function(x) {
  n <- sum(!is.na(x))
  sem <- sd(x, na.rm = TRUE) / sqrt(n)
  return(c(qnorm(0.025)*sem, qnorm(0.975)*sem))
}

aoi_data_sample <- read_csv(here::here("demo_data/aoi_data.csv"))
dataset_sample <- read_csv(here::here("demo_data/dataset.csv"))
subjects_sample <- read_csv(here::here("demo_data/subjects.csv"))
trials_sample <- read_csv(here::here("demo_data/trials.csv"))

# MAIN SHINY SERVER
server <- function(input, output, session) {
  ## ----------------------- DATA -----------------------
  
  # all_ is the incoming data where we get ranges etc. 
  # this is inefficient and should be revisited
  
  all_aoi_data <- reactive({
    aoi_data_sample
  })
  
  all_subjects_data <- reactive({
    subjects_sample
  })
  
  all_trials_data <- reactive({
    trials_sample
  })
  
  all_datasets <- reactive({
    dataset_sample
  })
  
  # ---- reactive parameters 
  
  age_min <- reactive({
    req(all_subjects_data())
    min(all_subjects_data()$age)
  })
  
  age_max <- reactive({
    req(all_subjects_data())
    max(all_subjects_data()$age)
  })
  
  window_min <- reactive({
    req(all_aoi_data())
    min(all_aoi_data()$t)
  })
  
  window_max <- reactive({
    req(all_aoi_data())
    max(all_aoi_data()$t)
  })
  
  target_words <- reactive({
    req(all_trials_data())
    
    c("All", unique(all_trials_data()$target_label))
  })

  datasets_list <- reactive({
    req(all_datasets())

    print(unique(all_datasets()$dataset))

    c("All", unique(all_datasets()$dataset))
  })
  
  # ---- actual restricted data
  subjects_data <- reactive({
    subjects_data <- all_subjects_data() %>%
      filter(age >= input$age_range[1] & 
               age <= input$age_range[2]) 
    
    if (input$age_nbins > 1) {
      subjects_data %>%
        mutate(age_binned = cut(age, input$age_nbins))
    } else {
      subjects_data %>%
        mutate(age_binned = "all ages")
    }
  })
  
  trials_data <- reactive({
    if (input$word == "All") {
      all_trials_data() %>%
        mutate(target_label = "All")
    } else {
      all_trials_data() %>%
        filter(target_label %in% input$word)
    }
  })
  
  datasets <- reactive({
    if (input$dataset == "All") {
      all_datasets()
    } else {
      all_datasets() %>%
        filter(dataset %in% input$dataset)
    }
  })
  
  aoi_data <- reactive({
    all_aoi_data() %>%
      right_join(subjects_data()) %>%
      right_join(trials_data()) %>%
      right_join(datasets()) %>%
      filter(t > input$plot_window_range[1],
             t < input$plot_window_range[2])
  })
  
  ## ----------------------- SELECTORS -----------------------

  # SELECTOR FOR AGE
  output$age_range_selector <- renderUI({
    sliderInput("age_range",
                label = "Ages to include (months)",
                value = c(age_min(), age_max()),
                step = 1, 
                min = floor(age_min()), max = ceiling(age_max()))
  })
  
  # SLIDER FOR AGE BINWIDTH
  # output$age_binwidth_selector <- renderUI({
  #   sliderInput("age_binwidth",
  #               label = "Bin size (months)",
  #               value = 2, step = 2,
  #               min = 0, max = 24)
  # })
  output$age_nbins_selector <- renderUI({
    sliderInput("age_nbins",
                label = "Number of age groups",
                value = 1, step = 1,
                min = 1, max = 6)
  })
  
  # SWITCH FOR AGE DISPLAY
  output$age_facet_selector <- renderUI({
    prettySwitch("age_facet",
                label = "Age plotted as colors",
                value = TRUE)
  })
  
  # TEXT INPUT FOR WORDS
  output$word_selector <- renderUI({
    selectizeInput(inputId = "word",
                   label = "Word",
                   selected = "All",   
                   choices = target_words(),
                   multiple = TRUE)
  })
  
  # SELECTORS FOR WINDOW
  output$plot_selector <- renderUI({
    sliderInput("plot_window_range",
                label = "Plotting Window",
                value = c(-500, 4000),
                step = 100, 
                min = window_min(), 
                max = window_max())
  })
  
  output$window_selector <- renderUI({
    sliderInput("analysis_window_range",
                label = "Analysis Window",
                value = c(250, 2250),
                step = 100, 
                min = 0, 
                max = window_max())
  })
  
  # SELECTOR FOR DATASET
  output$dataset_selector <- renderUI({
    selectizeInput(inputId = "dataset",
                   label = "Dataset",
                   choices = datasets_list(),
                   selected = "All",
                   multiple = TRUE)
  })

  
  ## ----------------------- PLOTS -----------------------
  
  output$profile_plot <- renderPlot({
    req(aoi_data())
    
    means <- aoi_data() %>%
      group_by(t, age_binned, target_label) %>%
      summarise(n = sum(!is.na(aoi)), 
                p = sum(aoi == "target", na.rm = TRUE),
                prop_looking = mean(aoi == "target", na.rm = TRUE), 
                ci_lower = binom::binom.confint(p, n, method = "bayes")$lower,
                ci_upper = binom::binom.confint(p, n, method = "bayes")$upper) 

    if (input$age_facet) {
      p <- ggplot(means, 
                  aes(x = t, y = prop_looking)) + 
        geom_rect(xmin = input$analysis_window_range[1],
                  xmax = input$analysis_window_range[2],
                  ymin = 0,
                  ymax = 1, fill = "gray", alpha = .1) +
        geom_line(aes(col = age_binned)) + 
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                        fill = age_binned), alpha = .5) +
        facet_wrap(.~target_label) 
    } else {
      p <- ggplot(means, 
                  aes(x = t, y = prop_looking)) + 
        geom_rect(xmin = input$analysis_window_range[1],
                  xmax = input$analysis_window_range[2],
                  ymin = 0,
                  ymax = 1, fill = "gray", alpha = .1) +
        geom_line(aes(col = target_label)) + 
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                        fill = target_label), alpha = .5) +
        facet_wrap(.~age_binned) 
    }
    
    p + 
      geom_hline(yintercept = .5, lty = 2) + 
      geom_vline(xintercept = 0, lty = 2) +
      ylab("Proportion Target Looking") +
      xlab("Time (msec)") +
      theme_classic() +
      scale_color_solarized() +
      scale_fill_solarized() 
  })
  
  output$onset_plot <- renderPlot({
    onset_means <- aoi_data() %>%
      group_by(sub_id, trial_id) %>%
      mutate(t0 = aoi[t == 0]) %>%
      group_by(t, t0, age_binned, target_label) %>%
      filter(!is.na(t0), t0 != "other") %>%
      summarise(prop_looking = mean(aoi != t0 & aoi != "other", na.rm = TRUE))

    if (input$age_facet) {
      p <- ggplot(onset_means, 
                  aes(x = t, y = prop_looking, lty = t0)) + 
        geom_rect(xmin = input$analysis_window_range[1],
                  xmax = input$analysis_window_range[2],
                  ymin = 0,
                  ymax = 1, fill = "gray", alpha = .1) +
        geom_line(aes(col = age_binned)) 
        facet_wrap(.~target_label)
    } else {
      p <- ggplot(onset_means, 
                  aes(x = t, y = prop_looking)) + 
        geom_rect(xmin = input$analysis_window_range[1],
                  xmax = input$analysis_window_range[2],
                  ymin = 0,
                  ymax = 1, fill = "gray", alpha = .1) +
        geom_line(aes(col = target_label)) +
        facet_wrap(.~age_binned)
    }
    
    p + 
      geom_hline(yintercept = .5, lty = 2) + 
      geom_vline(xintercept = 0, lty = 2) +
      xlim(0, max(onset_means$t)) +
      ylab("Proportion Target Looking") +
      xlab("Time (msec)") +
      theme_classic() +
      scale_color_solarized() +
      scale_fill_solarized()
  })
  
  output$accuracy_plot <- renderPlot({
    acc_means <- aoi_data() %>%
    # acc_means <- foo %>%
      group_by(sub_id, trial_id, age_binned, target_label) %>%
      filter(t >= input$analysis_window_range[1],
             t <= input$analysis_window_range[2]) %>%
      summarise(prop_looking = mean(aoi == "target", na.rm = TRUE)) %>%
      group_by(age_binned, target_label) %>%
      summarise(mean = mean(prop_looking, na.rm = TRUE),
                ci_lower = mean + ci.95(prop_looking)[1],
                ci_upper = mean + ci.95(prop_looking)[2],
                n = n())
             
    if (input$age_facet) {
      p <- ggplot(acc_means, 
                  aes(x = age_binned, y = mean, fill = age_binned)) + 
        geom_bar(stat="identity") +
        facet_wrap(~target_label)
    } else {
      p <- ggplot(acc_means, 
                  aes(x = target_label, y = mean, fill = target_label)) + 
        geom_bar(stat="identity") +
        facet_wrap(~age_binned)
    }
    p + 
      geom_linerange(aes(ymin = ci_lower, ymax = ci_upper)) +
      geom_hline(yintercept = .5, lty = 2) + 
      ylim(0,1) + 
      ylab("Proportion Target Looking") +
      xlab("Age (binned)") +
      theme_classic() +
      scale_color_solarized() +
      scale_fill_solarized()
      
  })
  
  output$rt_plot <- renderPlot({

    rt_result <- aoi_data() %>%
    # rt_result <- aoi_data %>%
        group_by(sub_id, trial_id) %>%
        nest() %>%
        mutate(rt = purrr::map(data, .f = compute_rt, sampling_rate = 33)) %>%
        unnest(rt, .drop = T) %>%
        filter(shift_type %in% c("D-T", "T-D")) %>%
        left_join(subjects_data(), by = c("sub_id"))
        # mutate(age_binned = cut(age, 2))

        # Histogram of RTs in peekbank data ----
        # with requested number of bins and RT filters
        if (input$age_facet) {
          p <- rt_result %>%
            ggplot(aes(x = rt_value, color=age_binned)) +
            geom_histogram(
              fill="white",
              alpha = 0.5,
              position = "identity") +
            labs(x = "RT (msec)", y = "Count")
        } else {
          p <- ggplot(rt_result,
                      aes(x = rt_value)) +
            geom_histogram(
              fill="white", alpha = 0.4, position ="identity")
            labs(x = "RT (msec)", y = "Court")
        }
    p
  })
}

# 
# # Define server logic required to draw RT histograms ----
# rt_result <- d %>% 
#   group_by(sub_id, trial_id) %>% 
#   nest() %>% 
#   mutate(rt = purrr::map(data, .f = compute_rt, sampling_rate = 33)) %>% 
#   unnest(rt, .drop = T) %>% 
#   filter(shift_type %in% c("D-T", "T-D")) %>% 
#   left_join(d_participants, by = c("sub_id")) %>% 
#   mutate(age_binned = cut_number(age, n = 3))
# 
# rt_ylim_buffer <- 100
# 
# server <- function(input, output) {
#   # filter rts
#   observeEvent( input$filter, {
#     rt_filt <- rt_result %>% 
#       filter(rt_value >= as.numeric(input$rt_window[1]),
#              rt_value <= as.numeric(input$rt_window[2]),
#              fst_shift_gap_ms <= as.numeric(input$max_fst_gap))
#     
#     # compute median rts for different shift types
#     rt_median <- rt_filt %>% 
#       group_by(shift_type) %>% 
#       summarise(rt_median = median(rt_value))
#     
#     # compute mean rt for each participant based on filtering
#     ss_rt <- rt_filt %>% 
#       filter(shift_type == "D-T", !is.na(rt_value)) %>% 
#       group_by(sub_id, age_binned, age) %>% 
#       summarise(m_rt = mean(rt_value)) %>% 
#       ungroup() %>% 
#       mutate(min_rt = min(m_rt),
#              max_rt = max(m_rt))
#     
#     # Histogram of RTs in peekbank data ----
#     # with requested number of bins and RT filters
#     output$plot <- renderPlot({
#       
#       rt_hist <- rt_filt %>% 
#         ggplot(aes(x = rt_value)) +
#         geom_histogram(bins = input$bins, color = "black", alpha = 0.4) +
#         labs(x = "RT (msec)", y = "Count") +
#         facet_wrap(~shift_type, ncol = 1) +
#         geom_vline(data = rt_median, aes(xintercept = rt_median), 
#                    color = "darkorange", size = 1.5)
#       
#       age_scatter <- ss_rt %>% 
#         ggplot(aes(x = age, y = m_rt)) +
#         geom_point(shape = 21, size = 3, color = "black", 
#                    fill = "grey") +
#         geom_smooth(method = "lm") +
#         lims(y = c(0, ss_rt$max_rt[1] + rt_ylim_buffer)) +
#         labs(x = "Age (months)", y = "Average RT (msec)")
#       
#       
#       cowplot::plot_grid(rt_hist, age_scatter, scale = c(1, 0.9))