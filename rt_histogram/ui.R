# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  theme = shinytheme("spacelab"),
  
  # App title ----
  bsCollapse(id = "doc", open = "title",
             bsCollapsePanel(title = h3("Reaction Time Browser"),
                             includeMarkdown(here::here("docs/rt-hist-doc.md")),
                             value = "title",
                             style = "default")
  ),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins in histogram:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput(inputId = "rt_window",
                  label = "RT Analysis Window:",
                  min = 0,
                  max = 5000,
                  step = 100,
                  value = c(0, 1800)),
      sliderInput(inputId = "max_fst_gap",
                  label = "Max Gap for First Shift:",
                  min = 100,
                  max = 1000,
                  step = 50,
                  value = 500),
      actionButton("filter", "Generate Plot")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plot ----
      plotOutput(outputId = "plot")
      
    )
  )
)