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
      conditionalPanel(
        condition = "input.info_present.includes('Scatter Plot')",
        h4("LR vs Bootstrap Scatter Plot"),
        plotOutput("scatter_plot")
      ),
      conditionalPanel(
        condition = "input.info_present.includes('Summary')",
      h4("Bootstrap Summary Statistics"),
      verbatimTextOutput("summary_table")),
      conditionalPanel(
      condition = "input.info_present.includes('Correlation Histogram')",
      h4("Bootstrap correlation histogram and Pearson correlation"),
      plotOutput("boot_cor_hist"),
      verbatimTextOutput("pearson_cor")),
      conditionalPanel(
        condition = "input.info_present.includes('Bootstrap Histogram')",
        h4("Bootstrap Distribution (Intercept/Slope)"),
        selectInput("boot_hist_term", "Select Term", choices = c("intercept", "slope"), selected = "slope"),
        plotOutput("boot_hist_plot")
      ),
      conditionalPanel(
      condition = "input.info_present.includes('IQR')",
      h4("IQR Boxplot of Predictor and Response"),
      plotOutput("iqr_boxplot"),
      verbatimTextOutput("iqr_text")
    ),
    conditionalPanel(
      condition = "input.info_present.includes('CI')",
      h4("95% CI: Bootstrap vs OLS"),
      selectInput("ci_term", "Select Term", choices = c("intercept", "slope"), selected = "slope"),
      plotOutput("ci_compare_plot")
    ),
  )))







server <- function(input, output, session) {

  source(file.path("server_plots.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
