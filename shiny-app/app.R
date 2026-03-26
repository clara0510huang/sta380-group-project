library(shiny)
library(ggplot2)
library(MASS)
library(DT)
library(dplyr)
library(bslib)

source("R/main.R")
source("R/ui_inputs.R")
source(file.path("intro_content.R"), local = TRUE)

# UI design
ui <- page_navbar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "STA380: Bootstrap Estimation of SLR for Boston Housing Data",
  
  nav_spacer(),
  
  nav_panel(
      title = "Introduction",
      div(style = "padding: 20px;", intro_text)
  ),
  
  nav_panel(
    title = "Model analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        open = "open",
        my_inputs
      ),
      
      tabsetPanel(
        #Overview
        tabPanel("Overview",
                br(),
                h4("LR vs Bootstrap Scatter Plot"),
                plotOutput("scatter_plot"),
                hr(),
                h4("Bootstrap Summary Statistics"),
                DTOutput("summary_table")
        ),
        
        #()
        tabPanel("Correlation Analysis",
                h4("Bootstrap correlation histogram"),
                plotOutput("boot_cor_hist"),
                br(),
                h4("Pearson Correlation Test"),
                DTOutput("pearson_cor")),
        
        tabPanel("Coefficient Distributions",
                h4("Bootstrap Distribution (Intercept/Slope)"),
                selectInput("boot_hist_term", "Select Term", choices = c("intercept", "slope"), selected = "slope", width = "200px"),
                plotOutput("boot_hist_plot")),
        
        tabPanel("Confidence Intervals",
                h4("95% CI: Bootstrap vs OLS"),
                selectInput("ci_term", "Select Term", choices = c("intercept", "slope"), selected = "slope", width = "200px"),
                plotOutput("ci_compare_plot")),

        tabPanel("Variable IQR Boxplot",
                h4("IQR Boxplot of Predictor and Response"),
                plotOutput("iqr_boxplot"),
                br(),
                DTOutput("iqr_values"))
      )
    )
  )
)

server <- function(input, output, session) {
  source(file.path("server_plots.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
