library(tidyverse)
library(ggthemes)
library(langcog)
library(peekbankr)
source(here::here("helpers/general_helpers.R"))

DEBUG_LOCAL <- FALSE
SAMPLING_RATE <- 40

# MAIN SHINY SERVER
server <- function(input, output, session) {
  ## ----------------------- DATA -----------------------
  
  # datasets
  datasets <- reactive({
    
    print("datasets") 
    
    if (DEBUG_LOCAL) {
      read_csv(here::here("demo_data/datasets.csv"), col_types = cols())
    } else {
      get_datasets()
    }
  })
  
  # administration data with the age rebinning happening
  administrations <- reactive({
    req(input$dataset)
    
    print("administrations")
  
    if (DEBUG_LOCAL) {
      administrations <- read_csv(here::here("demo_data/administrations.csv"), col_types = cols())
    } else {
      administrations <- get_administrations(dataset_name = input$dataset_name)
    }
    
    # administrations <- administrations %>%
    #   mutate(age = age / (365.25/12)) # months conversion
    
    if (input$age_nbins > 1) {
      administrations %>%
        mutate(age_binned = cut(age, input$age_nbins))
    } else {
      administrations %>%
        mutate(age_binned = "all ages")
    }
  })
  
  # aoi data
  aoi_timepoints <- reactive({
    req(input$dataset)
    req(input$age_range)
    
    print("aoi_timepoints")
    
    if (DEBUG_LOCAL) {
      read_csv(here::here("demo_data/aoi_timepoints.csv"), col_types = cols())
    } else {
      get_aoi_timepoints(dataset_name = input$dataset_name, age = input$age_range)
    }
  })
  
  # trials
  trials <- reactive({
    req(input$dataset)
    req(input$age_range)
    
    print("trials")
    
    if (DEBUG_LOCAL) {
      read_csv(here::here("demo_data/trials.csv"), col_types = cols())
    } else {
      get_trials(dataset_name = input$dataset)
    }
  })
  
  # all stimuli in the current datasets
  # - need to get all the stimuli to retrieve the words
  # - then stimuli can be filtered/mutated for the trials of interest
  dataset_stimuli <- reactive({
    req(input$dataset)
    
    print("dataset stimuli")
    
    if (DEBUG_LOCAL) {
      stimuli <- read_csv(here::here("demo_data/stimuli.csv"), col_types = cols())
    } else {
      stimuli <- get_stimuli(dataset_name = input$dataset)
    }
  })
  
  # those stimuli being used in the current analysis
  stimuli <- reactive({
    req(dataset_stimuli())
    req(input$word)

    print("stimuli")
    
    if (input$word == "All") {
      dataset_stimuli() %>%
        mutate(stimulus_label = "All")
    } else {
      dataset_stimuli() %>%
        filter(stimulus_label %in% input$word)
    }
  })

  
  
  # ---------------- REACTIVE PARAMETERS FOR SELECTORS -----------------
  # these are just parameters that get used in selectors and plotting
    
  age_min <- reactive({
    req(administrations())
    min(administrations()$age, na.rm = TRUE)
  })
  
  age_max <- reactive({
    req(administrations())
    max(administrations()$age, na.rm = TRUE)
  })
   
  window_min <- reactive({
    req(aoi_timepoints())
    min(aoi_timepoints()$t_norm)
  })

  window_max <- reactive({
    req(aoi_timepoints())
    max(aoi_timepoints()$t_norm)
  })
  
  target_words <- reactive({
    req(dataset_stimuli())
    c("All", unique(dataset_stimuli()$stimulus_label))
  })
  
  datasets_list <- reactive({
    req(datasets())
    c("All", unique(datasets()$dataset_name))
  })
  
  ## ----------------------- SELECTORS -----------------------
  
  # SELECTOR FOR AGE
  output$age_range_selector <- renderUI({
    # print("age_range_selector")
    sliderInput("age_range",
                label = "Ages to include (months)",
                value = c(age_min(), age_max()),
                step = 1, 
                min = floor(age_min()), max = ceiling(age_max()))
  })
  
  # SELECTOR FOR AGE BINNING
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
  
  # SELECTOR FOR ANALYSIS WINDOW
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
                   selected = "pomper_saffran_2016",
                   multiple = TRUE)
  })
  
  
  # -------------------------- JOINS -------------------------
  
  # JOIN TABLES TO AOI DATA - CREATE MAIN DATAFRAME FOR ANALYSIS
  aoi_data_joined <- reactive({
    req(aoi_timepoints())
    req(trials())
    req(datasets())
    req(administrations())
    req(stimuli())
    
    print("aoi_data_joined")
    aoi_timepoints() %>%
      right_join(administrations()) %>%
      right_join(trials()) %>%
      right_join(datasets()) %>%
      right_join(stimuli(), by = c("stimulus_id" = "target_id")) %>%
      filter(t_norm > input$plot_window_range[1],
             t_norm < input$plot_window_range[2]) 
  })
  
  # COMPUTE REACTION TIMES A LA FERNALD
  rts <- reactive({
    req(aoi_data_joined())

    print("rts")
    # write_csv(aoi_data_joined(), "aoi_data_joined.csv")

    # vectorized version 20x faster
    # aoi_data_joined() %>%
    aoi_data_joined %>%
      group_by(subject_id, trial_id, age_binned, stimulus_label) %>%
      mutate(crit_onset_aoi = aoi[t_norm == 0], 
             fst_shift_land_aoi = rle(aoi[t_norm >= 0])$values[3],
             shift_type = case_when(
               crit_onset_aoi == "distractor" & fst_shift_land_aoi == "target" ~ "D-T",
               crit_onset_aoi == "distractor" & fst_shift_land_aoi == "other" ~ "D-O",
               crit_onset_aoi == "target" & fst_shift_land_aoi == "distractor" ~ "T-D",
               crit_onset_aoi == "target" & fst_shift_land_aoi == "other" ~ "T-O",
               TRUE ~ "other shift"
             ), 
             rt_value = rle(aoi[t_norm >= 0])$lengths[1] * SAMPLING_RATE,
             fst_shift_gap_ms = rle(aoi[t_norm >= 0])$lengths[2] * SAMPLING_RATE)
  })
  
  # GET SUBJECT INFO FOR DESCRIPTIVE HISTOGRAMS
  subinfo <- reactive({
    req(aoi_data_joined())
    
    print("subinfo")
    
    aoi_data_joined() %>%
      group_by(subject_id, dataset_id, lab_dataset_id, age) %>%
      summarise(trials = length(unique(trial_id)))
  })
  
  
  
  ## ----------------------- PLOTS -----------------------
  
  ## ---------- PROFILE 
  output$profile_plot <- renderPlot({
    req(aoi_data_joined())
    
    print("profile_plot")
    
    means <- aoi_data_joined() %>%
      group_by(t_norm, age_binned, stimulus_label) %>%
      summarise(n = sum(!is.na(aoi)), 
                p = sum(aoi == "target", na.rm = TRUE),
                prop_looking = mean(aoi == "target", na.rm = TRUE), 
                ci_lower = binom::binom.confint(p, n, method = "bayes")$lower,
                ci_upper = binom::binom.confint(p, n, method = "bayes")$upper) 
    
    
    # print(head(means))
    
    if (input$age_facet) {
      p <- ggplot(means, 
                  aes(x = t_norm, y = prop_looking)) + 
        geom_rect(xmin = input$analysis_window_range[1],
                  xmax = input$analysis_window_range[2],
                  ymin = 0,
                  ymax = 1, fill = "gray", alpha = .1) +
        geom_line(aes(col = age_binned)) + 
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                        fill = age_binned), alpha = .5) +
        facet_wrap(.~stimulus_label) 
    } else {
      p <- ggplot(means, 
                  aes(x = t_norm, y = prop_looking)) + 
        geom_rect(xmin = input$analysis_window_range[1],
                  xmax = input$analysis_window_range[2],
                  ymin = 0,
                  ymax = 1, fill = "gray", alpha = .1) +
        geom_line(aes(col = stimulus_label)) + 
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                        fill = stimulus_label), alpha = .5) +
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
  
  
  ## ---------- ACCURACY BAR
  output$accuracy_plot <- renderPlot({
    acc_means <- aoi_data_joined() %>%
      # acc_means <- foo %>%
      group_by(subject_id, trial_id, age_binned) %>%
      filter(t_norm >= input$analysis_window_range[1],
             t_norm <= input$analysis_window_range[2]) %>%
      summarise(prop_looking = mean(aoi == "target", na.rm = TRUE)) %>%
      group_by(age_binned) %>%
      summarise(mean = mean(prop_looking, na.rm = TRUE),
                ci_lower = mean + ci.95(prop_looking)[1],
                ci_upper = mean + ci.95(prop_looking)[2],
                n = n())

    if (input$age_facet) {
      p <- ggplot(acc_means,
                  aes(x = age_binned, y = mean, fill = age_binned)) +
        geom_bar(stat = "identity") +
        facet_wrap(~stimulus_label)
    } else {
      p <- ggplot(acc_means,
                  aes(x = stimulus_label, y = mean, fill = stimulus_label)) +
        geom_bar(stat = "identity") +
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
  
  
  ## ---------- ONSET 
  output$onset_plot <- renderPlot({
    req(aoi_data_joined())
    req(rts())

    onset_means <- left_join(rts(), aoi_data_joined()) %>%
      group_by(t_norm, shift_type, age_binned, stimulus_label) %>%
      summarise(prop_looking = mean(aoi != crit_onset_aoi & aoi != "other", na.rm = TRUE))

    ggplot(onset_means,
           aes(x = t_norm, y = prop_looking, lty = shift_type)) +
      geom_line(aes(col = stimulus_label)) +
      facet_grid(stimulus_label~age_binned) +
      geom_hline(yintercept = .5, lty = 2) +
      geom_vline(xintercept = 0, lty = 2) +
      xlim(0, max(onset_means$t_norm)) +
      ylab("Proportion Target Looking") +
      xlab("Time (msec)") +
      theme_mikabr() +
      scale_color_solarized() +
      scale_fill_solarized()
    # TODO: SHADE REGIONS
    
  })
  
  ## ---------- RT BAR
  output$rt_plot <- renderPlot({
    req(rts())

    rt_means <- rts() %>%
      filter(shift_type == "D-T") %>%
      group_by(age_binned, stimulus_label) %>%
      summarise(mean = mean(rt_value, na.rm = TRUE),
                ci_lower = mean + ci.95(rt_value)[1],
                ci_upper = mean + ci.95(rt_value)[2],
                n = n())

    if (input$age_facet) {
      p <- ggplot(rt_means,
                  aes(x = age_binned, y = mean, fill = age_binned)) +
        geom_bar(stat="identity") +
        facet_wrap(~stimulus_label)
    } else {
      p <- ggplot(rt_means,
                  aes(x = stimulus_label, y = mean, fill = stimulus_label)) +
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
      geom_histogram(binwidth = 3) +
      theme_mikabr() +
      scale_fill_solarized(name = "Dataset") +
      xlab("Age (months)")
  })
  
}
