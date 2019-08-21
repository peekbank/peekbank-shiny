library(shinyBS)
library(shinythemes)
library(shinyWidgets)

ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  bsCollapse(id = "doc", open = "title",
             bsCollapsePanel(title = h3("PeekBank Analyses"),
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
                  tabPanel("Onset",
                           plotOutput("onset_plot")
                  )
      ),
      tabsetPanel(selected = "Accuracy", 
                  tabPanel("Accuracy",
                           plotOutput("accuracy_plot")
                  ),
                  tabPanel("Reaction Time",
                           plotOutput("rt_plot")
                  )
      )
                  # ), 
                  # tabPanel("Table",
                  #          br(),
                  #          downloadButton("download_table", "Download Table",
                  #                         class = "btn-default btn-xs"),
                  #          br(), br(),
                  #          dataTableOutput("trajectory_table")
    )
  )
)
