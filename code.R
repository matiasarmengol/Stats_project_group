#' Cross Validation Function
#' Evaluates linear model using cross validation, 
#' group data by specifc units (months, weekend/weekdays)
#' compute performance metrics for each group
#' 
#' @param lm A linear model
#' @param data dataset containing all the variables used in the model
#' 
cross_validation <- function(lm, data,
                             date_var = "Date",
                             grouping = "Monthly",
                             metrics = c("rmse", "mae", "mape", "r2", "adj_r2")){
  ## Validation of inputs
  if (!inherits(lm, "lm")){
    stop("Model must be a linear model (lm) object")
  }
  if (!date_var %in% names(data)) {
    stop(paste("Date variable", date_var, "not found in data"))
  }
  valid_metrics <- c("rmse", "mae", "mape", "r2", "adj_r2")
  if (!all(metrics %in% valid_metrics)) {
    stop(paste("Invalid metrics. Valid options are:", paste(valid_metrics, collapse = ", ")))
  }
  
  ## Check date variable is as a date format
  if (!inherits(data[[date_var]], "Date")){
    data[[date_var]] <- as.Date(data[[date_var]])
  }
  
  ## Group by month or weekend
  if (grouping == "Month"){
    data$group <- factor(month(data[[date_var]]), levels = 1:12,
                         labels = month.name)
  } else if (grouping == "Weekend"){
    data$group <- factor(
      ifelse(wday(data[[date_var]]) %in% c(1, 7), "Weekend", "Weekday"),
      levels = c("Weekday", "Weekend")
    )
  } else{
    stop(paste("Invalid grouping. Valid options are: Month, Weekend, K-fold"))
  }
  
  
  # Get model formula and variables
  model_formula <- formula(lm)
  model_vars <- all.vars(model_formula)
  
  # Helper function to get number of predictors from a model
  get_num_predictors <- function(model) {
    # Get total number of coefficients (including intercept if present)
    total_coeffs <- length(coef(model))
    
    # Check if model has intercept
    has_intercept <- any(names(coef(model)) == "(Intercept)")
    
    # Subtract 1 if there's an intercept to get predictors count
    num_predictors <- if (has_intercept) total_coeffs - 1 else total_coeffs
    
    return(num_predictors)
  }
  
  # Function to calculate performance metrics
  calculate_metrics <- function(actual, predicted, model = NULL){
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
    
    # Get sample size (excluding NA values)
    n <- sum(!is.na(actual) & !is.na(predicted))
    results$n <- n
    
    if ("r2" %in% metrics || "adj_r2" %in% metrics) {
      # Calculate R-squared
      if (var(actual, na.rm = TRUE) > 0) {
        mean_actual <- mean(actual, na.rm = TRUE)
        ss_total <- sum((actual - mean_actual)^2, na.rm = TRUE)
        ss_residual <- sum((actual - predicted)^2, na.rm = TRUE)
        r2 <- 1 - (ss_residual / ss_total)
        
        if ("r2" %in% metrics) {
          results$r2 <- r2
        }
        
        # Calculate adjusted R-squared if needed and if model is provided
        if ("adj_r2" %in% metrics && !is.null(model)) {
          num_predictors <- get_num_predictors(model)
          if (n > num_predictors + 1) {  # Ensure we have enough data points
            adj_r2 <- 1 - ((1 - r2) * (n - 1) / (n - num_predictors - 1))
            results$adj_r2 <- adj_r2
          } else {
            results$adj_r2 <- NA
            warning("Not enough data points to calculate adjusted R-squared")
          }
        } else if ("adj_r2" %in% metrics) {
          results$adj_r2 <- NA  # Cannot calculate without model
        }
      } else {
        if ("r2" %in% metrics) results$r2 <- NA
        if ("adj_r2" %in% metrics) results$adj_r2 <- NA
      }
    }
    
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
      
      # Extract the response variable from the test data
      # Get the response variable name from the model formula
      response_var <- all.vars(formula(lm))[1]
      actuals <- test_data[[response_var]]
      
      # Store predictions for overall metrics
      overall_actuals <- c(overall_actuals, actuals)
      overall_predictions <- c(overall_predictions, predictions)
      
      # Calculate metrics for this group
      group_metrics <- calculate_metrics(actuals, predictions, model = cv_model)
      group_results[[as.character(grp)]] <- group_metrics
      
    }, error = function(e) {
      warning(paste("Error in cross-validation for group:", grp, "-", e$message))
    })
  }
  
  # Calculate overall metrics
  # For overall metrics we use the original model for adjusted R-squared calculation
  overall_metrics <- calculate_metrics(overall_actuals, overall_predictions, model = lm)
  
  # Prepare final results
  results$group_metrics <- group_results
  results$overall_metrics <- overall_metrics
  results$group_by <- grouping  # Fixed variable name from group_by to grouping
  
  # Create summary data frame for easy viewing
  metrics_df <- data.frame(
    Group = names(group_results),
    N = sapply(group_results, function(x) x$n)
  )
  
  for (m in metrics) {
    metrics_df[[toupper(m)]] <- sapply(group_results, function(x) x[[m]])
  }
  
  # Add overall metrics as the last row
  overall_row <- data.frame(
    Group = "Overall",
    N = overall_metrics$n
  )
  
  for (m in metrics) {
    overall_row[[toupper(m)]] <- overall_metrics[[m]]
  }
  
  metrics_df <- rbind(metrics_df, overall_row)
  
  return(metrics_df)
}






library(dplyr)
library(caret)  # For R2 and adjusted R2

# Function for simple cross-validation
simple_cross_validation <- function(lm_formula, data, 
                                    metrics = c("rmse", "mae", "mape", "r2", "adj_r2"),
                                    fold_n = 5) {
  
  # Shuffle data and create folds
  set.seed(123)  # Ensure reproducibility
  data <- data[sample(nrow(data)), ]
  folds <- cut(seq(1, nrow(data)), breaks = fold_n, labels = FALSE)
  
  # Initialize metric storage
  results <- list(rmse = c(), mae = c(), mape = c(), r2 = c(), adj_r2 = c())
  
  for (i in 1:fold_n) {
    # Split into training and validation sets
    test_idx <- which(folds == i, arr.ind = TRUE)
    test_data <- data[test_idx, ]
    train_data <- data[-test_idx, ]
    
    # Train model
    model <- lm(lm_formula, data = train_data)
    
    # Predictions
    preds <- predict(model, newdata = test_data)
    actuals <- test_data[[all.vars(lm_formula)[1]]]  # Get response variable
    
    # Compute metrics
    if ("rmse" %in% metrics) {
      results$rmse <- c(results$rmse, sqrt(mean((preds - actuals)^2)))
    }
    if ("mae" %in% metrics) {
      results$mae <- c(results$mae, mean(abs(preds - actuals)))
    }
    if ("mape" %in% metrics) {
      results$mape <- c(results$mape, mean(abs((preds - actuals) / actuals)) * 100)
    }
    if ("r2" %in% metrics) {
      results$r2 <- c(results$r2, cor(preds, actuals)^2)
    }
    if ("adj_r2" %in% metrics) {
      results$adj_r2 <- c(results$adj_r2, summary(model)$adj.r.squared)
    }
  }
  
  # Compute the average for each metric
  avg_results <- sapply(results, mean)
  
  return(avg_results)
}