observeEvent(input$run_btn, {

  y_var <- input$respond
  x_var <- input$predictor
  r_num <- input$r_val
  seed_num <- input$seed_val

  #Color Selection
  col_scatter <- input$color_scatter
  col_hist <- input$color_hist

  #Error-proofing
  if (y_var == x_var) {
    showNotification("Error, Respond and Predictor cannot be same",
                     type = "error")
    return()
  }
  showNotification("Loading Bootstrap...", id = "loading", duration = NULL,
                   type = "message")

  #Calculations
  ols_res <- ols_estimators(data = my_data, predictor = x_var, respond = y_var)
  boot_res <- bootstrap_slr_summary(data = my_data, R = r_num, seed = seed_num,
                                    predictor = x_var, respond = y_var)

  #Generating Plots
  output$scatter_plot <- renderPlot({
    plot_lr_bootstrap_scatter(ols_slr = ols_res,
                              boot_slr = boot_res,
                              color_boot = col_scatter)
  })

  #Bootstrap histogram
  output$boot_hist_plot <- renderPlot({
    plot_boot_hist(
      boot_slr = boot_res,
      ols_slr = ols_res,
      term = input$boot_hist_term,
      col_hist = col_hist
    )
  })

  #CI comparison plot
  output$ci_compare_plot <- renderPlot({
    ci_list <- bootstrap_slr_ci(
      boot_slr = boot_res,
      ols_slr = ols_res,
      level = 0.95
    )

    plot_ci_box(
      ci_list = ci_list,
      term = input$ci_term,
      col_boots = col_scatter
    )
  })

  #Bootstrap correlation histogram and Pearson correlation
  output$boot_cor_hist <- renderPlot({
    plot_bootstrap_correlation(
      data      = my_data,
      predictor = x_var,
      respond   = y_var,
      R         = r_num,
      seed      = seed_num,
      col_hist = col_hist
    )
  })

  output$pearson_cor <- renderDT({
    res <- calculate_correlation(my_data, x_var, y_var)
    cor_df <- data.frame(
      Statistic = c("Pearson r", "p-value"),
      Value = c(sprintf("%.4f", res$correlation), sprintf("%.3e", res$p_value))
    )
    datatable(
      cor_df,
      class = 'cell-border stripe',
      options = list(dom = 't', ordering = FALSE, autoWidth = TRUE)
    )
  })

  #IQR boxplot and values
  output$iqr_boxplot <- renderPlot({
    plot_iqr_boxplot(
      data = my_data,
      predictor = x_var,
      respond = y_var
    )
  })
  
  output$iqr_values <- renderDT({
    iqr_x <- IQR(my_data[[x_var]], na.rm = TRUE)
    iqr_y <- IQR(my_data[[y_var]], na.rm = TRUE)
    iqr_df <- data.frame(
      Variable = c(paste("Predictor (", x_var, ")", sep = ""),
                   paste("Response (", y_var, ")", sep = "")),
      IQR = c(sprintf("%.4f", iqr_x), sprintf("%.4f", iqr_y))
    )
    
    datatable(
      iqr_df,
      rownames = FALSE,
      class = 'cell-border stripe',
      options = list(dom = 't', ordering = FALSE, autoWidth = TRUE)
    )
  })

  output$summary_table <- renderDT({
    boot_tbl <- bootstrap_slr(boot_slr = boot_res, ols_slr = ols_res)
    
    datatable(
      boot_tbl,
      rownames = FALSE,
      colnames = c("Term", "OLS Estimate", "Bootstrap Mean", "Bootstrap SE", "Bias", "Variance", "MSE"),
      options = list(
        dom = 't',
        ordering = FALSE,
        autoWidth = TRUE
      )
    ) %>%
      formatRound(columns = c("ols", "boot_mean", "boot_se"), digits = 3) %>%
      formatSignif(columns = c("bias", "variance", "mse"), digits = 3) %>%
      formatStyle('term', fontWeight = 'bold')
  })

  removeNotification(id = "loading")
})
