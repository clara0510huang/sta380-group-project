library(boot)
library(readr)
library(ggplot2)
library(testthat)

# Core functions for STA380 checkpoint: bootstrap estimation for simple linear regression

# Assign the data from the csv file
if (interactive()) {
  data <- read_csv("submission/Basic Implementation/BostonHousing.csv")
}


# Model setup

#' Generate simple linear regression model
#' @description Help to calculate the OLS estimators generate by using 
#' Boston Housing dataset.
#' @data Boston Housing dataset.
#' @param respond A string contains selecting variable name.
#' @param predictor A string contains selecting variable name.
#' @return A list containing: beta0, beta1, ols_conf_int 
#' (95% confidence interval).
#' @examples 
#' respond <- "medv"
#' predictor <-"lstat"
#' ols_slr <- ols_estimators(data,respond, predictor)
#' @importFrom stats lm
#' @importFrom stats coef
#' @importFrom stats confint
#' @export
ols_estimators<-function(data,respond, predictor){
  #check the data if run correctly
  head(data)
  # Setup the linear regression model
  lr_model <- lm(reformulate(predictor, respond), data = data)
  model_summary <- summary(lr_model)
  ols_slr <- list(
    beta0 = coef(lr_model)[1],
    beta1 = coef(lr_model)[2],
    ols_conf_int = confint(lr_model, level = 0.95))
  return(ols_slr)
}

#' Bootstrap analysis
#' @description
#' @param data Boston Housoing dataset
#' @param R Number of bootstrap samples (set as 1000)
#' @param seed Input seed for reproducibility 
#' @param respond A string contains selecting variable name.
#' @param predictor A string contains selecting variable name.
#' @return A list containing: Bootstrap bias estimates,standard errors, etc
#' @example 
#' respond <- "medv"
#' predictor <-"lstat"
#' ols_slr <- ols_estimators(data,respond, predictor)
#' boot_slr<-bootstrap_slr_summary(data, R = 1000,seed = NULL,predictor,respond)
#' @importFrom boot boot
#' @importForm boot boot.ci
#' @export
bootstrap_slr_summary <- function(data, R = 1000,seed = NULL,
                                  predictor,respond){  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  boot_model <- boot(data = data,
                     statistic = function(data, indices) {
                       d <- data[indices, ]
                       model <- lm(reformulate(predictor, respond), data = d)
                       return(coef(model))
                     },
                     R = 1000)
  boot_slr <- list(
    boot_b0_star = boot_model$t[,1],
    boot_b1_star = boot_model$t[,2],
    boot_b0_sd = sd(boot_model$t[,1]),
    boot_b1_sd = sd(boot_model$t[,2]),
    boot_b0_bias = boot_model$bias[1],
    boot_b1_bias = boot_model$bias[2],
    boot_b0_ci = boot.ci(boot_model, index = 1, type = "perc"),
    boot_b1_ci = boot.ci(boot_model, index = 2, type = "perc")
  )
  return(boot_slr)
}


# Output Implementation

#' Bootstrap Summary Table
#'@description Calculates descriptive statistics for the selected 
#'numerical variables, including mean, median, standard deviation,etc.
#' @param boot_slr Bootstrap coefficients generate by bootstrap_slr_summary()
#' @param ols_slr OLS's coefficients generate by ols_estimators()
#' @return A data.frame with rows for intercept and slope and columns.
#' @examples
#' respond <- "medv"
#' predictor <-"lstat"
#' ols_slr <- ols_estimators(data,respond, predictor)
#' boot_slr<-bootstrap_slr_summary(data, R = 1000,seed = NULL,predictor,respond)
#' @export
bootstrap_slr <-function(boot_slr,ols_slr){
  ols_b0 <- ols_slr$beta0
  ols_b1 <- ols_slr$beta1
  boot_b0 <- boot_slr$boot_b0_star
  boot_b1 <- boot_slr$boot_b1_star
  boot_mean <- c(intercept = mean(boot_b0), slope = mean(boot_b1))
  boot_se   <- c(intercept = boot_slr$boot_b0_sd, slope = boot_slr$boot_b1_sd)
  bias      <- c(intercept = boot_slr$boot_b0_bias, slope = boot_slr$boot_b1_bias)
  variance  <- boot_se^2
  mse       <- variance + bias^2
  ols_coef<-c(ols_b0, ols_b1)
  data.frame(
    term = c("intercept", "slope"),
    ols = unname(ols_coef),
    boot_mean = unname(boot_mean),
    boot_se = unname(boot_se),
    bias = unname(bias),
    variance = unname(variance),
    mse = unname(mse),
    row.names = NULL
  )
  
}

#' Calculate the correlation coefficient
#' @description Calculate the correlation coefficient between two variables and 
#' perform a significance test.
#' @param data A data frame containing the variables
#' @param respond A string contains selecting variable name.
#' @param predictor A string contains selecting variable name.
#' @return Returns a list containing the followings: 
#' correlation coefficient, p-value
#' @importFrom stats cor.test
#' @examples 
#' respond <- "medv"
#' predictor <-"lstat"
#' res <- calculate_correlation(data, predictor, respond)
#' print(res)
#' @export
#' 
calculate_correlation <- function(data, predictor, respond) {
  str <- data[[predictor]]
  obj <- data[[respond]]
  
  cor_res <- cor.test(str, obj)
  
  res <- list(
    correlation = as.numeric(cor_res$estimate),
    p_value = cor_res$p.value
  )
  return(res)
}


# Visualization

#' Plot bootstrap histogram with OLS reference line
#' @param boot_slr Bootstrap coefficients generated by bootstrap_slr_summary()
#' @param ols_slr OLS coefficients generated by ols_estimators()
#' @param term One of "intercept" or "slope"
#' @param breaks Histogram breaks passed to hist(). Default "Sturges"
#' @param main Optional title. If NULL, generates automatic title
#' @param xlab Optional x-axis label. If NULL, generates automatic label
#' @param col_hist Color of histogram bars. Set "lightblue" as default.
#' @param col_ols Color of OLS reference line. Set "red" as default.
#' @return Invisibly returns the histogram object
#' @example 
#' respond <- "medv"
#' predictor <-"lstat"
#' ols_slr <- ols_estimators(data, predictor, respond)
#' boot_slr<-bootstrap_slr_summary(data, R = 1000,seed = NULL,predictor,respond)
#' plot_boot_hist(boot_slr, ols_slr, term = "intercept")
#' plot_boot_hist(boot_slr, ols_slr, term = "slope")
#' @export

plot_boot_hist <- function(boot_slr, ols_slr, 
                           term = c("intercept", "slope"),
                           breaks = "Sturges", 
                           main = NULL, 
                           xlab = NULL,
                           col_hist = "lightblue",
                           col_ols = "red") {
  
  # vaild argument
  term <- match.arg(term)
  
  
  if (term == "intercept") {
    if (is.null(boot_slr$boot_b0_star)) {
      stop("boot_slr must contain boot_b0_star")
    }
    if (is.null(ols_slr$beta0)) {
      stop("ols_slr must contain beta0")
    }
    
    vals <- unname(boot_slr$boot_b0_star)
    ols_val <- unname(ols_slr$beta0)
    param_name <- "Intercept"
    
  } else { # slope
    if (is.null(boot_slr$boot_b1_star)) {
      stop("boot_slr must contain boot_b1_star")
    }
    if (is.null(ols_slr$beta1)) {
      stop("ols_slr must contain beta1")
    }
    
    vals <- unname(boot_slr$boot_b1_star)
    ols_val <- unname(ols_slr$beta1)
    param_name <- "Slope"
  }
  
  # set topic
  if (is.null(main)) {
    main <- paste0("Bootstrap Distribution of ", param_name, 
                   "\n(OLS estimate = ", round(ols_val, 4), ")")
  }
  
  if (is.null(xlab)) {
    xlab <- paste0(param_name, " Estimate")
  }
  
  h <- graphics::hist(vals, 
                      breaks = breaks, 
                      main = main, 
                      xlab = xlab,
                      col = col_hist,
                      border = "white",
                      ...)
  
  graphics::abline(v = ols_val, 
                   col = col_ols, 
                   lwd = 2, 
                   lty = 2)
  
  graphics::legend("topright",
                   legend = paste0("OLS = ", round(ols_val, 4)),
                   col = col_ols,
                   lwd = 2,
                   lty = 2,
                   bty = "n",
                   cex = 0.9)
  
  invisible(h)
}

#' Confidence intervals for SLR coefficients (bootstrap percentile + OLS)
#' @param boot_slr Output list from bootstrap_slr().
#' @param level Confidence level (default 0.95).
#' @return A list with boot_percentile and ols_wald, each a 2x2 matrix
#'   (rows = intercept, slope; cols = lower, upper).
#' @examples
#' respond <- "medv"
#' predictor <-"lstat"
#' ols_slr <- ols_estimators(data,respond, predictor)
#' boot_slr<-bootstrap_slr_summary(data, R = 1000,seed = NULL,predictor,respond)
#' ci_list<- bootstrap_slr_ci(boot_slr, ols_slr, level = 0.95)
#' @export
bootstrap_slr_ci <- function(boot_slr, ols_slr, level = 0.95) {
  boot_ci <- rbind(
    intercept = boot_slr$boot_b0_ci$percent[4:5],
    slope     = boot_slr$boot_b1_ci$percent[4:5])
  
  colnames(boot_ci) <- c("lower", "upper")
  
  ols_ci <- ols_slr$ols_conf_int
  # Map to intercept/slope rows in a stable way
  ols_ci2 <- rbind(
    intercept = as.numeric(ols_ci[1, ]),
    slope     = as.numeric(ols_ci[2, ])
  )
  colnames(ols_ci2) <- c("lower", "upper")
  
  list(boot_perc = boot_ci, ols_wald = ols_ci2)
}
#' Plot confidence interval comparison between bootstrap and OLS
#' @param ci_list List containing boot_perc and ols_wald matrices
#' @param term One of "intercept" or "slope"
#' @param main Optional title
#' @param col_hist Color of bootstrap graph. Set "lightblue" as default.
#' @param col_ols Color of OLS ci graph. Set "red" as default.
#' @return Invisibly returns the data used for plotting
#' @examples
#' respond <- "medv"
#' predictor <-"lstat"
#' ols_slr <- ols_estimators(data, predictor, respond)
#' ci_list<- bootstrap_slr_ci(boot_slr, ols_slr, level = 0.95)
#' plot_ci_box(ci_list, "intercept")
plot_ci_box <- function(ci_list, term = c("intercept", "slope"), main = NULL,
                        col_boots = "lightblue",
                        col_ols = "red") {
  # Validate term argument
  term <- match.arg(term)
  
  # Select row index based on term
  if (term == "intercept") {
    boot_ci <- ci_list$boot_perc[1, ]  # First row is intercept
    ols_ci  <- ci_list$ols_wald[1, ]   # First row is intercept
    param_name <- "Intercept"
  } else {
    boot_ci <- ci_list$boot_perc[2, ]  # Second row is slope
    ols_ci  <- ci_list$ols_wald[2, ]   # Second row is slope
    param_name <- "Slope"
  }
  
  # Set default title if not provided
  if (is.null(main)) {
    main <- paste(param_name, "95% CI: Bootstrap vs OLS")
  }
  
  # Prepare data for plotting
  dat <- list(
    method = c("Bootstrap (percentile)", "OLS (Wald)"),
    lower = c(boot_ci[1], ols_ci[1]),  # First column is lower bound
    upper = c(boot_ci[2], ols_ci[2])   # Second column is upper bound
  )
  
  # Calculate y-axis range with some padding
  y_range <- range(c(dat$lower, dat$upper))
  y_padding <- diff(y_range) * 0.1
  y_range <- c(y_range[1] - y_padding, y_range[2] + y_padding)
  
  # Create empty plot
  graphics::plot(NA, 
                 xlim = c(0.5, 2.5),
                 ylim = y_range,
                 xaxt = "n", 
                 xlab = "", 
                 ylab = "Coefficient value", 
                 main = main,
                 cex.main = 1.2)
  
  # Add x-axis labels
  graphics::axis(1, at = 1:2, labels = dat$method, cex.axis = 0.9)
  
  # Add light grid for better readability
  graphics::grid(nx = NA, ny = NULL, lty = 3, col = "lightgray")
  
  # Draw confidence intervals
  for (i in 1:2) {
    # Main vertical line (the interval)
    graphics::segments(i, dat$lower[i], i, dat$upper[i], 
                       lwd = 3, col = c(col_boots, col_ols)[i])
    
    # Lower horizontal cap
    graphics::segments(i - 0.08, dat$lower[i], i + 0.08, dat$lower[i], 
                       lwd = 3, col = c(col_boots, col_ols)[i])
    
    # Upper horizontal cap
    graphics::segments(i - 0.08, dat$upper[i], i + 0.08, dat$upper[i], 
                       lwd = 3, col = c(col_boots, col_ols)[i])
    
    # Add numeric label at the midpoint (optional)
    mid_point <- (dat$lower[i] + dat$upper[i]) / 2
    graphics::text(i, mid_point, 
                   sprintf("[%.3f, %.3f]", dat$lower[i], dat$upper[i]),
                   pos = 4, cex = 0.8, offset = 0.5)
  }
  
  # Add legend
  graphics::legend("topright",
                   legend = c("Bootstrap", "OLS"),
                   col = c(col_boots, col_ols),
                   lwd = 3,
                   bty = "n",
                   cex = 0.9)
  
  # Return data invisibly
  invisible(dat)
}

