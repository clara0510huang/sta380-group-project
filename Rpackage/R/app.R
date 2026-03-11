library(shiny)
library(ggplot2)
library(MASS)

source("../Rpachage/R/main.R")

#Error-proofing
all_vals <- names(MASS::Boston)
forbidden <- c("chas", "rad", "b")
allowed <- setdiff(all_vals, forbidden)

#UI design
ui <- fluidPage(
  titlePanel("STA380: Bootstrap Estimation of SLR for Boston Housing Data"),
  sidebarLayout(
    sidebarPanel(
      #1
      selectInput("respond", "Select Response (Y):", choices = allowed, selected = "medv"),
      #2
      selectInput("predictor", "Select Response (X):", choices = allowed, selected = "lstat"),
      #3
      sliderInput("r_val", "Bootstrap Replications (R):", min = 100, max = 10000, value = 500, step = 100),
      #4
      numericInput("seed_val", "Seed:", value = 2000),
      #5
      actionButton("run_btn", "Run Analysis!:", class = "btn-primary", style = "width: 100%; margin-top: 15px;")
    ),
    mainPanel(
      h4("LR vs Bootstrap Scatter Plot"),
      plotOutput("scatter_plot"),
      h4("Bootstrap Summary Statistics"),
      verbatimTextOutput("summary_table")
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$run_btn)
}
