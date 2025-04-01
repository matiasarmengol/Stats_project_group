#' Cross Validation Function
#' Evaluates linear model using cross validation, 
#' group data by specifc units (months, weekend/weekdays)
#' compute performance metrics for each group
#' 
#' @param lm A linear model
#' @param data dataset containing all the variables used in the model
#' 
cross_validation <-function(lm, data,
                            date_var = "Date"){
  ## Validation of inputs
  if (!inherits(lm, "lm")){
    stop("Model must be a linear model (lm) object")
  }
  if (!date_var %in% names(data)) {
    stop(paste("Date variable", date_var, "not found in data"))
  }
  
  ## Check date variable is as a date format
  if (!inherits(data[[date_var]], "Date")){
    data[[date_var]] <- as.Date(data[[date_var]])
  }
  
  ## Group by month
  data$group <- factor(month(data[[date_var]]), levels = 1:12,
                       labels = month.name)
  
  # Get model formula and variables
  model_formula <- formula(lm)
  model_vars <- all.vars(model_formula)
  
  
  calculate_metrics <- function(actual, predicted){
    results <- list()
    
    valid_indices <- !is.na(actual) & !is.na(predicted) & actual != 0
    actual_valid <- actual[valid_indices]
    predicted_valid <- predicted[valid_indices]
    
    results$rmse <- sqrt(mean((actual-predicted)^2, na.rm= TRUE))
    
    results$n <- sum(!is.na(actual) & !is.na(predicted))
    
    return(results)
  }
  
  
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
      cv_model <- update(model, data = train_data)
      
      # Predict on test data
      predictions <- predict(cv_model, newdata = test_data)
      actuals <- test_data[[target_var]]
      
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
  
  return(results)
}