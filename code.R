#' Cross Validation Function
#' Evaluates linear model using cross validation, 
#' group data by specifc units (months, weekend/weekdays)
#' compute performance metrics for each group
#' 
#' @param lm A linear model
#' @param data dataset containing all the variables used in the model
#' 
cross_validation <-function(lm, data,
                            date_var = "Date",
                            grouping = "Monthly",
                            metrics = c("rmse", "mae", "mape", "r2")){
  ## Validation of inputs
  if (!inherits(lm, "lm")){
    stop("Model must be a linear model (lm) object")
  }
  if (!date_var %in% names(data)) {
    stop(paste("Date variable", date_var, "not found in data"))
  }
  valid_metrics <- c("rmse", "mae", "mape", "r2")
  if (!all(metrics %in% valid_metrics)) {
    stop(paste("Invalid metrics. Valid options are:", paste(valid_metrics, collapse = ", ")))
  }
  
  ## Check date variable is as a date format
  if (!inherits(data[[date_var]], "Date")){
    data[[date_var]] <- as.Date(data[[date_var]])
  }
  
  ## Group by month
  if (grouping == "Month"){
    data$group <- factor(month(data[[date_var]]), levels = 1:12,
                         labels = month.name)
  } else if (grouping == "Weekend"){
    data$group <- factor(
      ifelse(wday(data[[date_var]]) %in% c(1, 7), "Weekend", "Weekday"),
      levels = c("Weekday", "Weekend")
    )
  } else{
    stop(paste("Invalid grouping. Valid options are: Month, Weekend"))
  }
  
  
  # Get model formula and variables
  model_formula <- formula(lm)
  model_vars <- all.vars(model_formula)
  
  
  calculate_metrics <- function(actual, predicted){
    results <- list()
    
    if ("rmse" %in% metrics) {
      results$rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    }
    
    if ("mae" %in% metrics) {
      results$mae <- mean(abs(actual - predicted), na.rm = TRUE)
    }
    
    if ("mape" %in% metrics) {
      results$mape <- 100 * mean(abs((actual - predicted) / actual), na.rm = TRUE)
    }
    
    if ("r2" %in% metrics) {
      if (var(actual, na.rm = TRUE) > 0) {
        ss_total <- sum((actual - mean(actual))^2)
        ss_residual <- sum((actual - predicted)^2)
        results$r2 <- 1 - (ss_residual / ss_total)
      } else {
        results$r2 <- NA
      }
    }
    
    results$n <- sum(!is.na(actual) & !is.na(predicted))
    
    return(results)
  }
  
  #Initialize results
  results <- list()
  
  # Get unique groups
  unique_groups <- unique(data$group)
  
  # Cross-validation by group
  group_results <- list()
  overall_actuals <- c()
  overall_predictions <- c()
  
  for (grp in unique_groups) {
    # Split data into training (all other groups) and testing (current group)
    train_data <- data[data$group != grp, ]
    test_data <- data[data$group == grp, ]
    
    # Check if we have enough data in both sets
    if (nrow(train_data) < 10 || nrow(test_data) < 5) {
      warning(paste("Insufficient data for group:", grp))
      next
    }
    
    # Refit model on training data
    tryCatch({
      cv_model <- update(lm, data = train_data)
      
      # Predict on test data
      predictions <- predict(cv_model, newdata = test_data)
      actuals <- test_data[["Y"]]
      
      # Store predictions for overall metrics
      overall_actuals <- c(overall_actuals, actuals)
      overall_predictions <- c(overall_predictions, predictions)
      
      # Calculate metrics for this group
      group_metrics <- calculate_metrics(actuals, predictions)
      group_results[[as.character(grp)]] <- group_metrics
      
    }, error = function(e) {
      warning(paste("Error in cross-validation for group:", grp, "-", e$message))
    })
  }
  # Calculate overall metrics
  overall_metrics <- calculate_metrics(overall_actuals, overall_predictions)
  
  # Prepare final results
  results$group_metrics <- group_results
  results$overall_metrics <- overall_metrics
  results$group_by <- group_by
  
  # Create summary data frame for easy viewing
  metrics_df <- data.frame(
    Group = names(group_results),
    N = sapply(group_results, function(x) x$n)
  )
  
  for (m in metrics) {
    metrics_df[[toupper(m)]] <- sapply(group_results, function(x) x[[m]])
  }
  
  
  return(metrics_df)
}