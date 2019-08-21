library(tidyverse)
library(eyetrackingR)
library(ggthemes)

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
    ggplot()
  })
  
  output$rt_plot <- renderPlot({
    ggplot()
  })
}