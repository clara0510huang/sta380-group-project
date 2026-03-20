load("../Rpackage/data/BostonHousing.rda")
my_data <- BostonHousing

#Error-proofing
all_vals <- names(my_data)
forbidden <- c("chas", "rad", "b")
allowed <- setdiff(all_vals, forbidden)

#Color selection
color_choices <- c("Blue" = "steelblue",
                   "Red" = "tomato",
                   "Green" = "seagreen",
                   "Gray" = "slategray",
                   "Purple" = "orchid")

my_inputs <- tagList(
  #1
  selectInput("respond", "Select Response (Y):", choices = allowed, selected = "medv"),
  #2
  selectInput("predictor", "Select Response (X):", choices = allowed, selected = "lstat"),
  #3
  sliderInput("r_val", "Bootstrap Replications (R):", min = 100, max = 10000, value = 500, step = 100),
  #4
  numericInput("seed_val", "Seed:", value = 2000),
  #5
  hr(),
  h4("Plot Settings"),
  selectInput("color_scatter", "Scatter Plot & Boxplot Color:", choices = color_choices, selected = "steelblue"),
  selectInput("color_hist", "Histogram Color:", choices = color_choices, selected = "seagreen"),
  hr(),
  
  actionButton("run_btn", "Run Analysis...", class = "btn-primary", style = "width: 100%;")
)

