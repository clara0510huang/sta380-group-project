library(testthat)

# Test data setup for consistent testing
test_data <- data.frame(
  MEDV = 1:30 + rep(c(0.01, -0.01), 15),
  LSTAT = 1:30,
  RM = runif(30, 3, 8),
  CRIM = runif(30, 0, 10)
)

test_that("ols_estimators returns correct structure and values", {
  result <- ols_estimators(test_data, predictor = "LSTAT", respond = "MEDV")
  
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

test_that("bootstrap_slr_summary runs with default parameters", {
  # Create a simple model first
  
  # Run bootstrap
  result <- bootstrap_slr_summary(
    data = test_data,
    predictor = "LSTAT",
    respond = "MEDV",
    R = 100,
    seed = 123
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
  
  # Run with same seed
  result1 <- bootstrap_slr_summary(test_data, predictor = "LSTAT", respond = "MEDV", R = 50, seed = 42)
  result2 <- bootstrap_slr_summary(test_data, predictor = "LSTAT", respond = "MEDV", R = 50, seed = 42)
  
  # Results should be identical with same seed
  expect_equal(result1$boot_b0_star, result2$boot_b0_star)
  expect_equal(result1$boot_b1_star, result2$boot_b1_star)
  
  # Run with different seed
  result3 <- bootstrap_slr_summary(test_data, predictor = "LSTAT", respond = "MEDV", R = 50, seed = 99)
  
  # Results should be different with different seed
  expect_false(identical(result1$boot_b0_star, result3$boot_b0_star))
})

test_that("bootstrap_slr creates correct summary table", {
  ols_result <- ols_estimators(test_data, predictor = "LSTAT", respond = "MEDV")
  boot_result <- bootstrap_slr_summary(test_data, predictor = "LSTAT", respond = "MEDV", R = 100, seed = 123)
  
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
  ols_result <- ols_estimators(test_data, predictor = "LSTAT", respond = "MEDV")
  boot_result <- bootstrap_slr_summary(test_data, predictor = "LSTAT", respond = "MEDV", R = 200, seed = 123)
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
  ols_result <- ols_estimators(test_data, predictor = "LSTAT", respond = "MEDV")
  boot_result <- bootstrap_slr_summary(test_data, predictor = "LSTAT", respond = "MEDV", R = 100, seed = 123)
  
  # Test that plot function runs without errors
  expect_no_error(
    plot_boot_hist(boot_result, ols_result, term = "intercept")
  )
  expect_no_error(
    plot_boot_hist(boot_result, ols_result, term = "slope", 
                   breaks = 20, main = "Custom Title")
  )
})

test_that("plot_ci_box runs without errors", {
  ols_result <- ols_estimators(test_data, predictor = "LSTAT", respond = "MEDV")
  boot_result <- bootstrap_slr_summary(test_data, predictor = "LSTAT", respond = "MEDV", R = 200, seed = 1230)
  ci_list <- bootstrap_slr_ci(boot_result, ols_result)
  
  # Test plot functions
  expect_no_error(
    plot_ci_box(ci_list, term = "intercept")
  )
  expect_no_error(
    plot_ci_box(ci_list, term = "slope", main = "Slope CI Comparison")
  )
})

test_that("Functions handle edge cases appropriately", {
  # Test with minimal data (n=3)
  tiny_data <- data.frame(
    MEDV = c(1, 2.01, 2.99),
    LSTAT = c(1, 2, 3)
  )
  
  # ols_estimators should still work
  result <- ols_estimators(tiny_data, predictor = "LSTAT", respond = "MEDV")
  expect_equal(result$beta1, 1, tolerance = 0.05)
  
  # bootstrap with small R
  boot_result <- bootstrap_slr_summary(tiny_data, predictor = "LSTAT", respond = "MEDV", R = 10, seed = 123)
  expect_equal(length(boot_result$boot_b0_star), 10)
  
  # Test with different variable names
  renamed_data <- data.frame(
    y = c(1, 2, 3),
    x = c(-2, 2, 0)
  )
  result_renamed <- ols_estimators(renamed_data, "y", "x")
  expect_equal(result_renamed$beta1, 1, tolerance = 1e-10)
})

test_that("calculate_correlation returns correct structure and values", {
  
  res <- calculate_correlation(test_data, "LSTAT", "MEDV")
  # check whether returns a list
  expect_true(is.list(res))
  expect_true(all(c("correlation", "p_value") %in% names(res)))
  
  # Check whether returns a number
  expect_true(is.numeric(res$correlation))
  expect_true(is.numeric(res$p_value))
  
  # Corr between -1 to 1
  expect_true(res$correlation >= -1 && res$correlation <= 1)
})

test_that("plot_lr_bootstrap_scatter runs without errors", {
  
  ols_result <- ols_estimators(test_data, "LSTAT", "MEDV")
  boot_result <- bootstrap_slr_summary(data = test_data, predictor = "LSTAT", respond = "MEDV", R = 100, seed = 123)
  
  expect_error(plot_lr_bootstrap_scatter(ols_result, boot_result), NA)
})
