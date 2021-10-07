library(tidyverse)
library(ggthemes)
library(langcog)
library(peekbankr)
library(tictoc)
source(here::here("helpers/general_helpers.R"))
source(here::here("helpers/rt_helper.R"))
# renv::deactivate()

# load administrations summary for slider max/mins
default_admins <- readRDS("cached_data/administrations.Rds")
aoi_timepoints <- readRDS("cached_data/aoi_timepoints.Rds")
DEFAULT_AGE_MAX <- 84 #max(default_admins$age, na.rm=T) # 1430.7 (days?)
DEFAULT_AGE_MIN <- 0 #min(default_admins$age, na.rm=T) # 1248
DEFAULT_PLOT_WINDOW_MIN <- -1000 #min(aoi_timepoints$t_norm, na.rm=T) # -990
DEFAULT_PLOT_WINDOW_MAX <- 4000 #max(aoi_timepoints$t_norm, na.rm=T) # 6867
DEFAULT_ANALYSIS_WINDOW_MIN <- 0 #min(aoi_timepoints$t_norm, na.rm=T) # -990
DEFAULT_ANALYSIS_WINDOW_MAX <- 4000 #max(aoi_timepoints$t_norm, na.rm=T) # 6867

DEBUG_LOCAL <- FALSE
SAMPLING_RATE <- 40
DEFAULT_DATASET <- "pomper_saffran_2016" 
# set true once and click "Re-load Data" to re-save cached_data for initial app loading
CACHE_DATA <- FALSE


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
    if(input$goButton) {
      req(input$dataset)
      req(input$age_range)
      
      print("administrations")
      
      if (DEBUG_LOCAL) {
        administrations <- read_csv(here::here("demo_data/administrations.csv"), col_types = cols())
      } else {
        administrations <- get_administrations(dataset_name = input$dataset)
      }
    }
    
    # print(input$age_nbins)
    if (input$age_nbins > 1) {
      administrations <- administrations %>%
        filter(age >= input$age_range[1],
               age <= input$age_range[2]) %>%
        mutate(age_binned = cut(age, input$age_nbins))
    } else {
      administrations <- administrations %>%
        filter(age >= input$age_range[1],
               age <= input$age_range[2]) %>%
        mutate(age_binned = "all ages")
    }
    
    administrations
  })
  
  # aoi data
  aoi_timepoints <- reactive({
    # first time? load cached data
    if(input$goButton) {
        req(input$dataset)
        req(input$age_range)
        
        print("aoi_timepoints")
        
        if (DEBUG_LOCAL) {
          aoi_timepoints_data <- read_csv(here::here("demo_data/aoi_timepoints.csv"), col_types = cols())
        } else {
          tictoc::tic()
          aoi_timepoints_data <- get_aoi_timepoints(dataset_name = input$dataset, age = input$age_range) 
          tictoc::toc()
        }
        tictoc::tic()

    }
    
    aoi_timepoints_data
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
  
  # trial types
  trial_types <- reactive({
    req(input$dataset)
    req(input$age_range)
    
    print("trial types")
    
    if (DEBUG_LOCAL) {
      read_csv(here::here("demo_data/trial_types.csv"), col_types = cols())
    } else {
      get_trial_types(dataset_name = input$dataset)
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
    
    if (any("All" %in% input$word)) {
      dataset_stimuli() %>%
        mutate(english_stimulus_label = "All")
    } else {
      dataset_stimuli() %>%
        filter(english_stimulus_label %in% input$word)
    }
  })
  
  # ---------------- REACTIVE PARAMETERS FOR SELECTORS -----------------
  # these are just parameters that get used in selectors and plotting
  
  age_min <- reactive({
    if(input$goButton==0) {
      DEFAULT_AGE_MIN
    } else {
      req(administrations())
      min(administrations()$age, na.rm = TRUE)
    }
  })
  
  age_max <- reactive({
    if(input$goButton==0) {
      DEFAULT_AGE_MAX
    } else {
      req(administrations())
      max(administrations()$age, na.rm = TRUE)
    }
  })
  
  plot_window_min <- reactive({
    #if(input$goButton==0) {
    #  DEFAULT_WINDOW_MIN
    #} else {
    #  req(aoi_timepoints())
    #  min(aoi_timepoints()$t_norm)
    #}
    DEFAULT_PLOT_WINDOW_MIN
  })
  
  plot_window_max <- reactive({
    #if(input$goButton==0) {
    #  DEFAULT_WINDOW_MAX
    #} else {
    #  req(aoi_timepoints())
    #  max(aoi_timepoints()$t_norm)
    #}
    DEFAULT_PLOT_WINDOW_MAX
  })
  
  analysis_window_min <- reactive({
    #if(input$goButton==0) {
    #  DEFAULT_WINDOW_MIN
    #} else {
    #  req(aoi_timepoints())
    #  min(aoi_timepoints()$t_norm)
    #}
    DEFAULT_ANALYSIS_WINDOW_MIN
  })
  
  analysis_window_max <- reactive({
    #if(input$goButton==0) {
    #  DEFAULT_WINDOW_MAX
    #} else {
    #  req(aoi_timepoints())
    #  max(aoi_timepoints()$t_norm)
    #}
    DEFAULT_ANALYSIS_WINDOW_MAX
  })
  
  target_words <- reactive({
    req(dataset_stimuli())
    c("All", unique(dataset_stimuli()$english_stimulus_label))
  })
  
  datasets_list <- reactive({
    req(datasets())
    unique(datasets()$dataset_name)
  })
  
  ## ----------------------- SELECTORS -----------------------
  
  # SELECTOR FOR AGE
  output$age_range_selector <- renderUI({
    # print("age_range_selector")
    # GK: doing this makes the age slider reset to defaults when re-load button is pushed
    #sliderInput("age_range",
    # s           label = "Ages to include (months)",
    #            value = c(age_min(), age_max()),
    #            step = 1, 
    #            min = floor(age_min()), max = ceiling(age_max()))
    # GK: so instead we use global defaults (0 - 84)
    sliderInput("age_range",
                label = "Ages to include (months)",
                value = c(DEFAULT_AGE_MIN, DEFAULT_AGE_MAX),
                step = 1, 
                min = DEFAULT_AGE_MIN, max = DEFAULT_AGE_MAX)
  })
  
  # SELECTOR FOR AGE BINNING
  output$age_nbins_selector <- renderUI({
    sliderInput("age_nbins",
                label = "Number of age groups",
                value = 2, step = 1,
                min = 1, max = 6)
  })
  
  # SWITCH FOR AGE DISPLAY
  output$age_facet_selector <- renderUI({
    prettySwitch("age_facet",
                 label = "Plot age as color (vs. age as facet)",
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
  output$plotting_window_selector <- renderUI({
    sliderInput("plot_window_range",
                label = "Plotting window for profile plot (msec)",
                value = c(-500, 4000),
                step = 100, 
                min = plot_window_min(), 
                max = plot_window_max())
  })
  
  # SELECTOR FOR ANALYSIS WINDOW
  output$analysis_window_selector <- renderUI({
    sliderInput("analysis_window_range",
                label = "Analysis window for accuracy plot (msec)",
                value = c(250, 2250),
                step = 100, 
                min = analysis_window_min(), 
                max = analysis_window_max())
  })
  
  # SELECTOR FOR DATASET
  output$dataset_selector <- renderUI({
    selectizeInput(inputId = "dataset",
                   label = "Dataset",
                   choices = datasets_list(),
                   selected = DEFAULT_DATASET,
                   multiple = TRUE)
  })
  
  
  # -------------------------- JOINS -------------------------
  
  # JOIN TABLES TO AOI DATA - CREATE MAIN DATAFRAME FOR ANALYSIS
  aoi_data_joined <- reactive({
    
    # first time? load cached data
    if(input$goButton==0) {
      # load pre-joined data..
      print("loading cached aoi_data_joined")
      aoi_data_joined <- readRDS("cached_data/aoi_data_joined.Rds")
    } else {
      isolate({
        
        print("aoi_data_joined")
        aoi_data_joined <- aoi_timepoints() %>%
          right_join(administrations()) %>%
          right_join(trials()) %>%
          right_join(trial_types()) %>%
          right_join(datasets()) %>%
          mutate(stimulus_id = target_id) %>%
          right_join(stimuli()) 
      })
    }
    
    if(CACHE_DATA) saveRDS(aoi_data_joined, file="cached_data/aoi_data_joined.Rds")
    aoi_data_joined
  })
  
  # COMPUTE REACTION TIMES A LA FERNALD
  rts <- reactive({
    req(aoi_data_joined())
    
    print("rts")
    rt_data <- aoi_data_joined() %>%
      filter(any(t_norm == 0), # must have data at 0
             t_norm >= 0) %>% # only pass data after 0
      group_by(administration_id, trial_id) %>%
      summarise(lengths = rle(aoi)$lengths, 
                values = rle(aoi)$values) 
    
    rt_data %>%
      group_by(administration_id, trial_id) %>%
      nest() %>%
      mutate(data = lapply(data, get_rt)) %>%
      unnest(cols = c(data)) %>%
      left_join(aoi_data_joined() %>%
                  select(administration_id, trial_id, 
                         age, age_binned, dataset_name, 
                         english_stimulus_label, 
                         stimulus_novelty, trial_order) %>%
                  distinct())
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
    req(input$plot_window_range)
    # req(input$age_facet)
    
    print("profile_plot")
    
    means <- aoi_data_joined() %>%
      group_by(t_norm, age_binned, english_stimulus_label) %>%
      filter(t_norm > input$plot_window_range[1], 
             t_norm < input$plot_window_range[2]) %>%
      summarise(n = sum(aoi %in% c("target", "distractor"), na.rm=TRUE), # don't include 'other' / 'missing' / etc
                p = sum(aoi == "target", na.rm = TRUE),
                prop_looking = p / n,
                ci_lower = binom::binom.confint(p, n, method = "bayes")$lower,
                ci_upper = binom::binom.confint(p, n, method = "bayes")$upper) 
    
    
    # print(head(means))
    p <- ggplot(means, 
                aes(x = t_norm, y = prop_looking)) + 
      geom_rect(xmin = isolate(input$analysis_window_range[1]),
                xmax = isolate(input$analysis_window_range[2]),
                ymin = 0,
                ymax = 1, fill = "gray", alpha = .1)
    
    if (input$age_facet) {
      p <- p +
        geom_line(aes(col = age_binned)) + 
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                        fill = age_binned), alpha = .5) +
        facet_wrap(.~english_stimulus_label) 
    } else {
      p <- p +
        geom_line(aes(col = english_stimulus_label)) + 
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, 
                        fill = english_stimulus_label), alpha = .5) +
        facet_wrap(.~age_binned) 
    }
    tictoc::toc()
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
      group_by(subject_id, trial_id, age_binned, english_stimulus_label) %>%
      filter(t_norm >= input$analysis_window_range[1],
             t_norm <= input$analysis_window_range[2]) %>%
      summarise(prop_looking = mean(aoi == "target", na.rm = TRUE)) %>%
      group_by(age_binned, english_stimulus_label) %>%
      summarise(mean = mean(prop_looking, na.rm = TRUE),
                ci_lower = mean + ci.95(prop_looking)[1],
                ci_upper = mean + ci.95(prop_looking)[2],
                n = n())
    
    if (input$age_facet) {
      p <- ggplot(acc_means,
                  aes(x = age_binned, y = mean, fill = age_binned)) +
        geom_bar(stat = "identity") +
        facet_wrap(~english_stimulus_label)
    } else {
      p <- ggplot(acc_means,
                  aes(x = english_stimulus_label, y = mean, fill = english_stimulus_label)) +
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
    
    onset_means <- left_join(select(rts(), 
                                    administration_id, trial_id, rt, shift_type),
                             aoi_data_joined()) %>%
      filter(shift_type != "other", 
             shift_type != "no shift") %>%
      filter(t_norm > input$plot_window_range[1],
             t_norm < input$plot_window_range[2]) %>%
      group_by(t_norm, shift_type, age_binned, english_stimulus_label) %>%
      summarise(prop_looking = mean(ifelse(shift_type == "T-D", 
                                           aoi == "distractor" & aoi != "other",
                                           aoi == "target" & aoi != "other"), 
                                    na.rm = TRUE))
    
    print("onset means")
    
    ggplot(onset_means,
           aes(x = t_norm, y = prop_looking, lty = shift_type)) +
      geom_line(aes(col = english_stimulus_label)) +
      facet_grid(english_stimulus_label~age_binned) +
      geom_hline(yintercept = .5, lty = 2) +
      geom_vline(xintercept = 0, lty = 2) +
      xlim(0, max(onset_means$t_norm)) +
      ylab("Proportion Shifting from Onset Image") +
      xlab("Time (msec)") +
      theme_mikabr() +
      scale_color_solarized() +
      scale_fill_solarized()
    # TODO: SHADE REGIONS
    
  })
  
  ## ---------- RT BAR
  output$rt_plot <- renderPlot({
    req(rts())
    req(input$plot_window_range)
    # req(input$age_facet)
    
    rt_means <- rts() %>%
      filter(shift_type == "D-T") %>% # shift_type not found?
      group_by(age_binned, english_stimulus_label) %>%
      summarise(mean = mean(rt, na.rm = TRUE),
                ci_lower = mean + ci.95(rt)[1],
                ci_upper = mean + ci.95(rt)[2],
                n = n())
    
    if (input$age_facet) { 
      p <- ggplot(rt_means,
                  aes(x = age_binned, y = mean, fill = age_binned)) +
        geom_bar(stat="identity") +
        facet_wrap(~english_stimulus_label) +
        scale_fill_solarized(name = "Age group")
    } else {
      p <- ggplot(rt_means,
                  aes(x = english_stimulus_label, y = mean, fill = english_stimulus_label)) +
        geom_bar(stat="identity") +
        facet_wrap(~age_binned) + 
        scale_fill_solarized(name = "Label")
    }
    p +
      geom_linerange(aes(ymin = ci_lower, ymax = ci_upper)) +
      ylab("Reaction time (msec)") +
      xlab("Age (binned)") +
      theme_mikabr()
  })
  
  
  ## ---------- RT HISTOGRAM
  output$rt_hist <- renderPlot({
    req(rts())
    
    rts() %>%
      ggplot(aes(x = rt, fill = age_binned)) +
      geom_histogram(alpha = 0.4,
                     position = "identity", binwidth = 200) +
      labs(x = "RT (msec)", y = "Count") +
      theme_mikabr() +
      scale_color_solarized(name = "Age group")
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
  