library(boot)
library(readr)
library(ggplot)

# Assign the data from the csv file
data <- read_csv("submission/Basic Implementation/BostonHousing.csv")

# Model setup

#' Simple linear regression
#'@description A simple linear regression model generate by using Boston Housing
#' dataset.
#' @data Boston Housing dataset.
#' @param respond A string contains selecting variable name.
#' @param predictor A string contains selecting variable name.
#' @return return the linear regression model.
#' @importFrom stats lm
#' @examples 
#' respond <- "MEDV"
#' predictor <-"LSTAT"
#' @export
setup_linear_regression_function<-function(data,respond, predictor){
  #check the data if run correctly
  head(data)
  # Setup the linear regression model
  lr_model <- lm(reformulate(predictor, respond), data = data)
  # View the COEF table
  return(lr_model) 
}

#'Calculate OLS Estimates 
#' @description Help to calculate the OLS estimator 
#' @param model A linear regression model.
#' @return A list containing: beta0, beta1, se and ols confidence interval.
#' @export
ols_estimators<- function(model = lr_model){
  model_summary <- summary(model)
  # Summary beta0 beta1
  beta0 <- coef(model)[1]
  beta1 <- coef(model)[2]
  std_error = model_summary$coefficients[1,"Std. Error"]
  # Calculate confidence intervals
  ols_conf_int <- confint(model, level = 0.95)
  return(list(
    beta0,
    beta1,
    std_error,
    ols_conf_int))
}


#' Bootstrap analysis for Linear Regression
#' @description
#' @param data Boston Housoing dataset
#' @param model A linear regression model.
#' @param R Number of bootstrap samples (set as 1000)
#' @param seed Input seed for reproducibility 
#' @return A boot object
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @export
bootstrap_analysis <- function(data, model= lrmodel, R = 1000,seed = NULL){  
  if (!is.null(seed)) {
    set.seed(seed)
  }else{
    set.seed(100)
  }
  boot_model<- boot(data, lrmodel, R=1000, stype="f")#show boot can not found
  return(boot_model)
}#incomplete

#' Bootstrap Summary Statistics
#' @description calculates summary statistics from a boot object
#' @param boot_model A boot object from bootstrap_analysis()
#' @return A list containing: Bootstrap bias estimates,standard errors, etc
#' @export
bootstrap_summary<-function(boot_model){
  
}


# Output Implementation

#' Calculates the descriptive statistics selected by the user
#'@description Calculates descriptive statistics for the selected 
#'numerical variables, including mean, median, standard deviation,etc.
#' @param coef_boot Bootstrap coefficients
#' @param coef_ols OLS's coefficients
#' @param stats A character vector specifying the type of statistics.
#' Optional values include: "mean", "median", "sd", and "IQR".
#' Calculates all the statistics value as default.
#' @return Returns a data frame, with one variable per row and
#'  one statistic per column.
#' @examples
#' calculate_descriptive_stats(stats = c("mean", "sd"))
#' @export


#' Calculate the correlation coefficient
#' @description Calculate the correlation coefficient between two variables and 
#' perform a significance test.
#' @return Returns a list containing the followings: 
#' correlation coefficient, p-value
#' @importFrom stats cor.test
#' @export


# Visualization

#'Plot a histogram of the Bootstrap distribution of the slope (beta1)
#'
#'
#'

#'Draw a histogram of the Bootstrap distribution at the intercept (beta0)
#'
#'
#'
#'

#' Plot a box plot comparing confidence intervals
#' 
#' 


#'Draw a bias-variance plot(???)
#'
#'
#'