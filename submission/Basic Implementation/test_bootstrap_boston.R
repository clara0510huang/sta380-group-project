library(testthat)

# Source the function file (adjust the path if you move files around)
source("main.R")

# Test data setup for consistent testing
test_data <- data.frame(
  MEDV = 1:30,
  LSTAT = 1:30,
  RM = runif(30, 3, 8),
  CRIM = runif(30, 0, 10)
)

test_that("ols_estimators returns correct structure and values", {
  result <- ols_estimators(test_data, "LSTAT", "MEDV")
  
  # Check structure
  expect_true(is.list(result))
  expect_true(all(c("beta0", "beta1", "ols_conf_int") %in% names(result)))
  
  # Check values (perfect linear relationship y = x should give beta0=0, beta1=1)
  expect_equal(result$beta0, 0, tolerance = 0.05)
  expect_equal(result$beta1, 1, tolerance = 0.05)
  
  # Check confidence interval structure
  expect_equal(dim(result$ols_conf_int), c(2, 2))
  expect_true(result$ols_conf_int[1, 1] <= result$beta0)  # lower bound <= estimate
  expect_true(result$ols_conf_int[1, 2] >= result$beta0)  # upper bound >= estimate
})

test_that("boot_fn works correctly with indices", {
  # Create indices for bootstrap sample (with replacement)
  set.seed(123)
  indices <- sample(1:nrow(test_data), nrow(test_data), replace = TRUE)
  
  result <- boot_fn(test_data, indices, "LSTAT", "MEDV")
  
  # Check that result is a named numeric vector of length 2
  expect_true(is.numeric(result))
  expect_equal(length(result), 2)
  expect_equal(names(result), c("(Intercept)", "LSTAT"))
  
  # Compare with direct lm on the bootstrap sample
  direct_result <- lm(MEDV ~ LSTAT, data = test_data[indices, ])
  expect_equal(unname(result), unname(coef(direct_result)))
})

test_that("bootstrap_slr_summary runs with default parameters", {
  # Create a simple model first
  model <- lm(MEDV ~ LSTAT, data = test_data)
  
  # Run bootstrap
  result <- bootstrap_slr_summary(
    data = test_data,
    model = model,
    R = 100,
    seed = 123,
    boot_fn = boot_fn
  )
  
  # Check structure
  expect_true(is.list(result))
  expect_true(all(c("boot_b0_star", "boot_b1_star", "boot_b0_sd", 
                    "boot_b1_sd", "boot_b0_bias", "boot_b1_bias") %in% names(result)))
  
  # Check dimensions
  expect_equal(length(result$boot_b0_star), 100)
  expect_equal(length(result$boot_b1_star), 100)
  
  # Check that standard errors are positive
  expect_true(result$boot_b0_sd > 0)
  expect_true(result$boot_b1_sd > 0)
})

test_that("bootstrap_slr_summary respects seed for reproducibility", {
  model <- lm(MEDV ~ LSTAT, data = test_data)
  
  # Run with same seed
  result1 <- bootstrap_slr_summary(test_data, model, R = 50, seed = 42, boot_fn)
  result2 <- bootstrap_slr_summary(test_data, model, R = 50, seed = 42, boot_fn)
  
  # Results should be identical with same seed
  expect_equal(result1$boot_b0_star, result2$boot_b0_star)
  expect_equal(result1$boot_b1_star, result2$boot_b1_star)
  
  # Run with different seed
  result3 <- bootstrap_slr_summary(test_data, model, R = 50, seed = 99, boot_fn)
  
  # Results should be different with different seed
  expect_false(identical(result1$boot_b0_star, result3$boot_b0_star))
})

test_that("bootstrap_slr creates correct summary table", {
  model <- lm(MEDV ~ LSTAT, data = test_data)
  ols_result <- ols_estimators(test_data, "MEDV", "LSTAT")
  boot_result <- bootstrap_slr_summary(test_data, model, R = 100, seed = 123, boot_fn)
  
  summary_table <- bootstrap_slr(boot_result, ols_result)
  
  # Check structure
  expect_true(is.data.frame(summary_table))
  expect_equal(nrow(summary_table), 2)
  expect_equal(summary_table$term, c("intercept", "slope"))
  
  # Check all required columns exist
  expected_cols <- c("term", "ols", "boot_mean", "boot_se", "bias", "variance", "mse")
  expect_true(all(expected_cols %in% names(summary_table)))
  
  # Check mathematical relationships
  expect_equal(summary_table$bias, 
               summary_table$boot_mean - summary_table$ols, 
               tolerance = 0.05)
  expect_equal(summary_table$variance, 
               summary_table$boot_se^2, 
               tolerance = 0.05)
  expect_equal(summary_table$mse, 
               summary_table$variance + summary_table$bias^2, 
               tolerance = 0.05)
})

test_that("bootstrap_slr_ci returns correctly structured confidence intervals", {
  model <- lm(MEDV ~ LSTAT, data = test_data)
  ols_result <- ols_estimators(test_data, "MEDV", "LSTAT")
  boot_result <- bootstrap_slr_summary(test_data, model, R = 200, seed = 123, boot_fn)
  
  # Fix the boot_slr object to include the confidence intervals
  library(boot)
  boot_model <- boot(data = test_data, 
                     statistic = function(d, i) boot_fn(d, i, "LSTAT", "MEDV"),
                     R = 200)
  boot_result$boot_b0_ci <- boot.ci(boot_model, index = 1, type = "perc")
  boot_result$boot_b1_ci <- boot.ci(boot_model, index = 2, type = "perc")
  ols_result$ols_conf_int <- confint(model)
  
  ci_result <- bootstrap_slr_ci(boot_result, ols_result, level = 0.95)
  
  # Check structure
  expect_true(is.list(ci_result))
  expect_true(all(c("boot_perc", "ols_wald") %in% names(ci_result)))
  
  # Check dimensions
  expect_equal(dim(ci_result$boot_perc), c(2, 2))
  expect_equal(dim(ci_result$ols_wald), c(2, 2))
  
  # Check that lower <= upper for all intervals
  expect_true(all(ci_result$boot_perc[, "lower"] <= ci_result$boot_perc[, "upper"]))
  expect_true(all(ci_result$ols_wald[, "lower"] <= ci_result$ols_wald[, "upper"]))
})

test_that("plot_boot_hist runs without errors", {
  model <- lm(MEDV ~ LSTAT, data = test_data)
  ols_result <- ols_estimators(test_data, "MEDV", "LSTAT")
  boot_result <- bootstrap_slr_summary(test_data, model, R = 100, seed = 123, boot_fn)
  
  # Test that plot function runs without errors
  expect_error(
    plot_boot_hist(boot_result, ols_result, term = "intercept"), 
    NA
  )
  expect_error(
    plot_boot_hist(boot_result, ols_result, term = "slope", 
                   breaks = 20, main = "Custom Title"), 
    NA
  )
})

test_that("plot_ci_box runs without errors", {
  model <- lm(MEDV ~ LSTAT, data = test_data)
  ols_result <- ols_estimators(test_data, "MEDV", "LSTAT")
  boot_result <- bootstrap_slr_summary(test_data, model, R = 200, seed = 123, boot_fn)
  
  # Create CI list
  library(boot)
  boot_model <- boot(data = test_data, 
                     statistic = function(d, i) boot_fn(d, i, "LSTAT", "MEDV"),
                     R = 200)
  boot_result$boot_b0_ci <- boot.ci(boot_model, index = 1, type = "perc")
  boot_result$boot_b1_ci <- boot.ci(boot_model, index = 2, type = "perc")
  ols_result$ols_conf_int <- confint(model)
  ci_list <- bootstrap_slr_ci(boot_result, ols_result)
  
  # Test plot functions
  expect_error(
    plot_ci_box(ci_list, term = "intercept"),
    NA
  )
  expect_error(
    plot_ci_box(ci_list, term = "slope", main = "Slope CI Comparison"),
    NA
  )
})

test_that("Functions handle edge cases appropriately", {
  # Test with minimal data (n=3)
  tiny_data <- data.frame(
    MEDV = 1:3,
    LSTAT = 1:3
  )
  
  # ols_estimators should still work
  result <- ols_estimators(tiny_data, "MEDV", "LSTAT")
  expect_equal(result$beta1, 1, tolerance = 1e-10)
  
  # bootstrap with small R
  model <- lm(MEDV ~ LSTAT, data = tiny_data)
  boot_result <- bootstrap_slr_summary(tiny_data, model, R = 10, seed = 123, boot_fn)
  expect_equal(length(boot_result$boot_b0_star), 10)
  
  # Test with different variable names
  renamed_data <- data.frame(
    y = 1:10,
    x = 1:10
  )
  result_renamed <- ols_estimators(renamed_data, "y", "x")
  expect_equal(result_renamed$beta1, 1, tolerance = 1e-10)
})

# The following test will fail until you fix the bug in bootstrap_slr_summary
# There's a typo: "boot_slr < - list(" should be "boot_slr <- list("
test_that("bootstrap_slr_summary syntax is correct", {
  # This is a meta-test to check for syntax errors
  source_file <- readLines("bootstrap_boston.R")
  
  # Check for the typo
  typo_line <- grep("boot_slr < - list", source_file)
  expect_equal(length(typo_line), 0, 
               info = "Found 'boot_slr < - list' typo in bootstrap_slr_summary function")
})