library(dplyr)
library(data.table)
library(ggplot2)
library(car)

# function for full regression analysis
reg_analysis <- function(data_table, endo_var, exo_vars, 
                         test_var1, test_var2, 
                         resid_by_var, resid_sample){
  
  reg_output <- regression(data_table, endo_var, exo_vars)
  
  data_table$residuals <- reg_output$residuals
  
  ttest_output <- ttest(reg_output$reg, test_var1, test_var2)
  
  plot_output <- plot_resid(data_table, endo_var, "residuals", 
                            resid_by_var, resid_sample)
  
  print("printing regression results")
  print(summary(reg_output$reg))
  
  print("printing ttest results")
  print(ttest_output)

  print("printing residual plot")
  print(plot_output)
}

# function to run regression
regression <- function(data_table, endo_var, exo_vars){
  reg_formula <- as.formula(paste0(endo_var, "~", paste(exo_vars, collapse = "+")))
  reg <- lm(reg_formula, data_table)
  
  resid <- residuals(reg)
  pred <- predict(reg)
  
  output <- list("reg" = reg, "residuals" = resid, "pred" = pred)
  return(output)
}

# function to report the ttest of two coefficients
ttest <- function(model, var1, var2){
  linearHypothesis(model, paste(var1, "=", var2))
}

# function to plot residuals
plot_resid <- function(data_table, x_var, y_var, by_var, sample_size){
  ggplot(data_table[sample(.N, sample_size)], 
         aes_string(x=x_var, y=y_var)) +
    geom_point(alpha = 0.1) +
    facet_grid(as.formula(paste0("~", by_var))) +
    theme_minimal()
}