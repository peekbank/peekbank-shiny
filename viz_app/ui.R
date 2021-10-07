library(tidyverse)
library(shinyBS)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  bsCollapse(id = "doc", open = "title",
             bsCollapsePanel(title = h3("PeekBank Visualization Tool"),
                             includeMarkdown("../docs/peekbank.md"),
                             value = "title",
                             style = "default")
  ),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("age_range_selector"),
      uiOutput("age_nbins_selector"),
      uiOutput("word_selector"),
      uiOutput("dataset_selector"),
      actionButton("goButton", "Re-load Data"), 
      br(),
      br(),
      uiOutput("plotting_window_selector"),
      uiOutput("analysis_window_selector"),
      uiOutput("age_facet_selector"),
      # Note that uiOutputs are necessary for bookmark to work
      # bookmarkButton(label = "Share Analysis"),
      # br()
      # uiOutput("db_version_number")
    ),
    
    mainPanel(
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(selected = "Profile", 
                  tabPanel("Profile",
                           plotOutput("profile_plot") %>%
                             shinycssloaders::withSpinner(hide.ui = FALSE)
                  ),
                  tabPanel("Accuracy",
                           plotOutput("accuracy_plot") %>%
                             shinycssloaders::withSpinner(hide.ui = FALSE)
                  )
      ),
      tabsetPanel(selected = "Reaction Time", 
                  tabPanel("Reaction Time",
                           plotOutput("rt_plot") %>%
                             shinycssloaders::withSpinner(hide.ui = FALSE)
                  ),
                  tabPanel("Onset",
                           plotOutput("onset_plot") %>%
                             shinycssloaders::withSpinner(hide.ui = FALSE)
                  ),
                  tabPanel("RT Histogram",
                           plotOutput("rt_hist") %>%
                             shinycssloaders::withSpinner(hide.ui = FALSE)
                  )
      ),
      tabsetPanel(selected = "Age Histogram", 
                  tabPanel("Age Histogram",
                           plotOutput("age_hist") %>%
                             shinycssloaders::withSpinner(hide.ui = FALSE)
                  )
      )
    ) 
  )
)
