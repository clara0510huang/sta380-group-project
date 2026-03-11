observeEvent(input$run_btn, {
  
  y_var <- input$respond
  x_var <- input$predictor
  r_num <- input$r_val
  seed_num <- input$seed_val
  
  #Error-proofing
  if (y_var == x_var) {
    showNotification("Error, Respond and Predictor cannot be same", type == "error")
    return()
  }
  showNotification("Loading Bootstrap...", id = "loading", duration = NULL, type = "message")
  
  #Calculations
  ols_res <- ols_estimators(data = my_data, predictor = x_var, respond = y_var)
  boot_res <- bootstrap_slr_summary(data = my_data, R = r_num, seed = seed_num, predictor = x_var, respond = y_var)
  
  #Generating Plots
  output$scatter_plot <- renderPlot({
    plot_lr_bootstrap_scatter(ols_slr = ols_res, boot_slr = boot_res)
  })
  
  output$summary_table <- renderPrint({
    boot_tbl <- bootstrap_slr(boot_slr = boot_res, ols_slr = ols_res)
    print(boot_tbl, row.names = FALSE)
  })
  
  removeNotification(id = "loading")
  }
)

