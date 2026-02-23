library(boot)
library(readr)
library(mvtnorm)


#' Bootstrap estimation for simple linear regression
#'@description Bootstrap estimation on a simple linear regression model
#' and returns Bootstrap estimates of the intercept, slope.
#' @data A data frame containing the dataset.
#' @param R Number of bootstrap repetitions.
#' @param seed Optional random seed for reproducibility.
#' @return A Bootstrap Statisticsc Table.
#' @importFrom boot boot
#' @importFrom stats lm
#' @export




#draft
# Assign the data from the csv file
data <- read_csv("submission/Basic Implementation/BostonHousing.csv")
# Setup the linear regression model
head(data)
model <- lm(medv ~ lstat, data = data)
# View the COEF table
summary(model)
# Summary beta0 beta1
beta0 <- coef(fit)[1]
beta1 <- coef(fit)[2]
# set seed 100 as default example
set.seed(100) 
# indices
indices <- 505
# Bootstrap function setup
boot_fn <- function(data, indices) {
  
  d <- data[indices, ]  
  
  fit <- model
  
  beta0 <- beta0
  beta1 <- beta1
  
  return(c(beta0, beta1))
}
#Bootstrapping with 1000 replications
boot_results <- boot(data = data,
                     statistic = boot_fn,
                     R = 1000)
# check the bootsrap result
boot_results

