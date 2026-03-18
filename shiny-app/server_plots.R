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

  output$pearson_cor <- renderUI({
    res <- calculate_correlation(my_data, x_var, y_var)
    output$pearson_cor <- renderUI({
      res <- calculate_correlation(my_data, x_var, y_var)
      HTML(sprintf(
        "<pre style='font-size:14px;'>Pearson r(%s, %s) = %.4f<br>p-value= %.3e</pre>",
        x_var, y_var, res$correlation, res$p_value
      ))
    })
  })

  #IQR boxplot and values
  output$iqr_boxplot <- renderPlot({
    plot_iqr_boxplot(
      data      = my_data,
      predictor = x_var,
      respond   = y_var
    )
    output$iqr_values <- renderUI({
      iqr_x <- IQR(my_data[[x_var]], na.rm = TRUE)
      iqr_y <- IQR(my_data[[y_var]], na.rm = TRUE)
      HTML(sprintf("<pre style='font-size:14px;'>IQR(%s) = %.4f<br>IQR(%s) = %.4f</pre>",
                   x_var, iqr_x, y_var, iqr_y))
    })  
  })

  output$summary_table <- renderUI({
    boot_tbl <- bootstrap_slr(boot_slr = boot_res, ols_slr = ols_res)
    
    boot_tbl <- boot_tbl %>% 
      mutate(
        across(c(ols, boot_mean, boot_se), ~ sprintf("%.6f", .)),
        across(c(bias, variance), ~ sprintf("%.2e", .))
      )
    
    kbl(boot_tbl, 
        caption = "Bootstrap Summary Statistics",
        digits = 6,
        format.args = list(big.mark = ",", scientific = FALSE)) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                    full_width = FALSE,
                    position = "left") %>%

      column_spec(1, bold = TRUE, border_right = TRUE) %>%  
      row_spec(0, background = "#D3D3D3", bold = TRUE) %>%  
      footnote(general = "Based on OLS and Bootstrap (R = {r_num})") %>%
      HTML() 
  })


  removeNotification(id = "loading")
})
