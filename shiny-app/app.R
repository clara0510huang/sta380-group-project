library(shiny)
library(ggplot2)
library(MASS)

#UI design
ui <- fluidPage(
  titlePanel("STA380: Bootstrap Estimation of SLR for Boston Housing Data"),
  sidebarLayout(
    sidebarPanel(
      my_inputs
    ),
    mainPanel(
      h4("LR vs Bootstrap Scatter Plot"),
      plotOutput("scatter_plot"),
      h4("Bootstrap Summary Statistics"),
      verbatimTextOutput("summary_table"),
      h4("Bootstrap correlation histogram and Pearson correlation"),
      plotOutput("boot_cor_hist"),
      verbatimTextOutput("pearson_cor"),
      h4("IQR Boxplot of Predictor and Response"),
      plotOutput("iqr_boxplot"),
      verbatimTextOutput("iqr_text")
    )
  )
)

server <- function(input, output, session) {
  
  source(file.path("server_plots.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
