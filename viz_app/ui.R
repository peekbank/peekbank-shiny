library(shinyBS)
library(shinythemes)
library(shinyWidgets)

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
      uiOutput("age_facet_selector"),
      uiOutput("word_selector"),
      uiOutput("plot_selector"),
      uiOutput("window_selector"),
      uiOutput("dataset_selector"),

      # Note that uiOutputs are necessary for bookmark to work
      bookmarkButton(label = "Share Analysis"),
      br(), br()
      # uiOutput("db_version_number")
    ),
    
    mainPanel(
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabsetPanel(selected = "Profile", 
                  tabPanel("Profile",
                           plotOutput("profile_plot")
                  ),
                  tabPanel("Accuracy",
                           plotOutput("accuracy_plot")
                  )
      ),
      tabsetPanel(selected = "Reaction Time", 
                  tabPanel("Reaction Time",
                           plotOutput("rt_plot")
                  ),
                  tabPanel("Onset",
                           plotOutput("onset_plot")
                  ),
                  tabPanel("RT Histogram",
                           plotOutput("rt_hist")
                  )
      ),
      tabsetPanel(selected = "Age Histogram", 
                  tabPanel("Age Histogram",
                           plotOutput("age_hist")
                  )
      )
    )
  )
)
