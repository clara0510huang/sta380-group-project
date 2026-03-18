library(shiny)
library(ggplot2)
library(MASS)
library(kableExtra)
library(dplyr)

source(file.path("intro_content.R"), local = TRUE)
# UI design
ui <- fluidPage(
  titlePanel("STA380: Bootstrap Estimation of SLR for Boston Housing Data"),
  tabsetPanel(  tabPanel(
    value = "intro_tab",
    title = "Intro to the Boston Housing Data",
    intro_text
  ),
    tabPanel(
      value = "model_tab",
      title = "Model analysis",

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
          uiOutput("summary_table")
        ),
        conditionalPanel(
          condition = "input.info_present.includes('Correlation Histogram')",
          h4("Bootstrap correlation histogram and Pearson correlation"),
          plotOutput("boot_cor_hist"),
          uiOutput("pearson_cor")
        ),
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
          uiOutput("iqr_values")
        ),
        conditionalPanel(
          condition = "input.info_present.includes('CI')",
          h4("95% CI: Bootstrap vs OLS"),
          selectInput("ci_term", "Select Term", choices = c("intercept", "slope"), selected = "slope"),
          plotOutput("ci_compare_plot")
        )
  ))))
)

server <- function(input, output, session) {
  source(file.path("server_plots.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
