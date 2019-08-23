library(tidyverse)
# library(eyetrackingR)
library(ggthemes)
library(langcog)
library(peekbankr)
source(here::here("helpers/rt_helpers.R"))

ci.95 <- function(x) {
  n <- sum(!is.na(x))
  sem <- sd(x, na.rm = TRUE) / sqrt(n)
  return(c(qnorm(0.025)*sem, qnorm(0.975)*sem))
}

debug <- FALSE
debug_local <- FALSE

if (debug) {
  aoi_data <- get_aoi_data() %>%
    mutate(age_binned = 1, 
           target_label = "all")
  subjects <- get_subjects()
  trials <- get_trials()
  datasets <- get_datasets()
  
  aoi_data_joined <- aoi_data %>%
    left_join(subjects, by = "subject_id") %>%
    left_join(trials, by = "trial_id") %>%
    left_join(datasets, by = "dataset_id")
  
  input <- list()
  input$analysis_window_range <- c(250,2250)
} else if (debug_local) {
  aoi_data_sample <- read_csv(here::here("demo_data/aoi_data.csv"))
  dataset_sample <- read_csv(here::here("demo_data/dataset.csv"))
  subjects_sample <- read_csv(here::here("demo_data/subjects.csv"))
  trials_sample <- read_csv(here::here("demo_data/trials.csv"))
  
}

# MAIN SHINY SERVER
server <- function(input, output, session) {
  ## ----------------------- DATA -----------------------
  
  # all_ is the incoming data where we get ranges etc. 
  # this is inefficient and should be revisited
  
  all_aoi_data <- reactive({
    get_aoi_data()
  })
  
  all_subjects_data <- reactive({
    get_subjects()
  })
  
  all_trials_data <- reactive({
    get_trials()
  })
  
  all_datasets <- reactive({
    get_datasets()
  })
  
  # ---- reactive parameters 
  
  age_min <- reactive({
    req(all_subjects_data())
    
    min(all_subjects_data()$age, na.rm = TRUE)
  })
  
  age_max <- reactive({
    req(all_subjects_data())
    max(all_subjects_data()$age, na.rm = TRUE)
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

    print(unique(all_datasets()$lab_dataset_id))

    c("All", unique(all_datasets()$lab_dataset_id))
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
  
  aoi_data_joined <- reactive({
    all_aoi_data() %>%
      right_join(subjects_data(), by = "subject_id") %>%
      right_join(trials_data(), by = "trial_id") %>%
      right_join(datasets(), by = "dataset_id") %>%
      filter(t > input$plot_window_range[1],
             t < input$plot_window_range[2])
  })
  
  rts <- reactive({
    aoi_data_joined() %>%
      # rts <- aoi_data %>%
      group_by(subject_id, trial_id, age_binned, target_label) %>%
      nest() %>%
      mutate(rt = purrr::map(data, .f = compute_rt, sampling_rate = 33)) %>%
      unnest(rt, .drop = T) %>%
      filter(shift_type %in% c("D-T", "T-D"))
  })
  
  subinfo <- reactive({
    aoi_data_joined() %>%
      # aoi_data_joined %>%
      group_by(subject_id, dataset_id, lab_dataset_id, age) %>%
      summarise(trials = length(unique(trial_id)))
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
  
  ## ---------- PROFILE 
  output$profile_plot <- renderPlot({
    req(aoi_data_joined())
    
    means <- aoi_data_joined() %>%
    # means <- aoi_data_joined %>%
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
  
  ## ---------- ONSET 
  output$onset_plot <- renderPlot({
    req(aoi_data_joined()) 
    req(rts())
    
    onset_means <- left_join(rts(), aoi_data_joined()) %>%
    # onset_means <- left_join(rts, aoi_data_joined) %>%
      group_by(t, shift_type, age_binned, target_label) %>%
      summarise(prop_looking = mean(aoi != crit_onset_aoi & aoi != "other", na.rm = TRUE))
    
    ggplot(onset_means, 
           aes(x = t, y = prop_looking, lty = shift_type)) + 
      geom_line(aes(col = target_label)) +
      facet_grid(target_label~age_binned) +
      geom_hline(yintercept = .5, lty = 2) +
      geom_vline(xintercept = 0, lty = 2) +
      xlim(0, max(onset_means$t)) +
      ylab("Proportion Target Looking") +
      xlab("Time (msec)") +
      theme_mikabr() +
      scale_color_solarized() +
      scale_fill_solarized()
    # TODO: SHADE REGIONS
    
  })
  
  
  ## ---------- ACCURACY BAR
  output$accuracy_plot <- renderPlot({
    acc_means <- aoi_data_joined() %>%
    # acc_means <- foo %>%
      group_by(subject_id, trial_id, age_binned, target_label) %>%
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
      theme_mikabr() +
      scale_color_solarized() +
      scale_fill_solarized()
  })
  
  ## ---------- RT BAR
  output$rt_hist <- renderPlot({
    req(rts())
    
    rt_means <- rts() %>%
      filter(shift_type == "D-T") %>%
      group_by(age_binned, target_label) %>%
      summarise(mean = mean(rt_value, na.rm = TRUE),
                ci_lower = mean + ci.95(rt_value)[1],
                ci_upper = mean + ci.95(rt_value)[2],
                n = n())
    
    if (input$age_facet) {
      p <- ggplot(rt_means,
                  aes(x = age_binned, y = mean, fill = age_binned)) +
        geom_bar(stat="identity") +
        facet_wrap(~target_label)
    } else {
      p <- ggplot(rt_means,
                  aes(x = target_label, y = mean, fill = target_label)) +
        geom_bar(stat="identity") +
        facet_wrap(~age_binned)
    }
    p +
      geom_linerange(aes(ymin = ci_lower, ymax = ci_upper)) +
      geom_hline(yintercept = .5, lty = 2) +
      ylab("Reaction time (msec)") +
      xlab("Age (binned)") +
      theme_mikabr() +
      scale_color_solarized() +
      scale_fill_solarized()
  })
  
  
  ## ---------- RT HISTOGRAM
  output$rt_hist <- renderPlot({
    req(rts())

        # Histogram of RTs in peekbank data ----
        # with requested number of bins and RT filters
        if (input$age_facet) {
          p <- rts() %>%
            ggplot(aes(x = rt_value, color = age_binned))
        } else {
          p <- ggplot(rts(),
                      aes(x = rt_value)) 
        }
    p +     
      geom_histogram(fill = "white", alpha = 0.4, 
                     position = "identity") + 
      labs(x = "RT (msec)", y = "Count") + 
      theme_mikabr() +
      scale_color_solarized() +
      scale_fill_solarized()
  })
  
  
  ## ---------- AGE BAR
  output$age_hist <- renderPlot({
    req(subinfo()) 
    
    subinfo() %>% 
      ggplot(aes(x = age, fill = lab_dataset_id)) +
      geom_histogram() + 
      theme_mikabr() +
      scale_fill_solarized(name = "Dataset") +
      xlab("Age (months)") + 
  })
  
}
