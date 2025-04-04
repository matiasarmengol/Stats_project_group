cross_validation_fixed <- function(lm, data,
                                   date_var = "Date",
                                   grouping = "Month",
                                   metrics = c("rmse", "mae", "mape", "r2", "adj_r2"),
                                   k = FALSE,
                                   validation_type = FALSE){
  
  require(lubridate)
  require(caret)
  
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
  
  ## Extract response variable from model formula
  model_formula <- formula(lm)
  model_vars <- all.vars(model_formula)
  response_var <- model_vars[1]  # First variable is the response
  
  ## Identify factor variables in the dataset
  factor_vars <- names(data)[sapply(data, is.factor)]
  
  ## Check date variable is as a date format
  if (!inherits(data[[date_var]], "Date")){
    data[[date_var]] <- as.Date(data[[date_var]])
  }
  
  ## Group by month or weekend
  if (grouping == "Month"){
    data$group <- factor(lubridate::month(data[[date_var]]), levels = 1:12,
                         labels = month.name)
  } else if (grouping == "Weekend"){
    data$group <- factor(
      ifelse(lubridate::wday(data[[date_var]]) %in% c(1, 7), "Weekend", "Weekday"),
      levels = c("Weekday", "Weekend")
    )
  } else if (grouping == "k-fold"){
    tryCatch({
      reindex_data <- data[sample(nrow(data)), ]
      data$group <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)
    }, error = function(e){
      warning(paste("Error in k-fold cross validation. Select a k:", e$message))
    })
  } else{
    stop(paste("Invalid grouping. Valid options are: Month, Weekend, k-fold"))
  }
  
  if (!validation_type == FALSE){
    if (validation_type == "Month"){
      
    } else if (validation_type == "Weekend") {
      
    } else if (validation_type == "Year"){
      
    } else {
      stop(paste("Error in validation type: Valid options are: Month, Weekend, Year"))
    }
  }
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
    # Guard against NULL or empty vectors
    if (is.null(actual) || length(actual) == 0 || 
        is.null(predicted) || length(predicted) == 0) {
      warning("Empty or NULL data passed to calculate_metrics")
      return(list(rmse = NA, mae = NA, mape = NA, r2 = NA, adj_r2 = NA, n = 0))
    }
    
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
    # Split data
    train_data <- data[data$group != grp, ]
    test_data <- data[data$group == grp, ]
    
    # Check data size
    if (nrow(train_data) < 10 || nrow(test_data) < 5) {
      warning(paste("Insufficient data for group:", grp))
      next
    }
    
    tryCatch({
      # Create a new model that drops problematic factor variables
      modified_formula <- model_formula
      
      # Remove factor variables that might cause issues
      vars_to_remove <- c()
      if (grouping == "Month") {
        # If cross-validating by month, remove season (highly collinear with month)
        if ("season" %in% factor_vars) {
          vars_to_remove <- c(vars_to_remove, "season")
        }
      } else if (grouping == "Weekend") {
        # If cross-validating by weekend, remove is_weekend
        if ("is_weekend" %in% factor_vars) {
          vars_to_remove <- c(vars_to_remove, "week_type")
        }
      }
      
      # Remove problematic variables from formula
      if (length(vars_to_remove) > 0) {
        for (var in vars_to_remove) {
          modified_formula <- update(modified_formula, paste0(". ~ . - ", var))
        }
      }
      
      # Fit model on training data with modified formula
      cv_model <- lm(modified_formula, data = train_data)
      
      # Predict on test data
      predictions <- predict(cv_model, newdata = test_data)
      actuals <- test_data[[response_var]]
      
      # Verify we have valid data
      if (length(actuals) == 0 || all(is.na(actuals)) || 
          length(predictions) == 0 || all(is.na(predictions))) {
        warning(paste("No valid data for group:", grp))
        next
      }
      
      # Store results
      overall_actuals <- c(overall_actuals, actuals)
      overall_predictions <- c(overall_predictions, predictions)
      
      # Calculate metrics
      group_metrics <- calculate_metrics(actuals, predictions, model = cv_model)
      group_results[[as.character(grp)]] <- group_metrics
      
    }, error = function(e) {
      warning(paste("Error in cross-validation for group:", grp, "-", e$message))
    })
  }
  
  # Check if we have any valid results
  if (length(overall_actuals) == 0 || length(overall_predictions) == 0) {
    warning("No valid predictions or actuals generated during cross-validation")
    return(NULL)
  }
  
  # Calculate overall metrics
  # For overall metrics we use the original model for adjusted R-squared calculation
  overall_metrics <- calculate_metrics(overall_actuals, overall_predictions, model = lm)
  
  # Prepare final results
  results$group_metrics <- group_results
  results$overall_metrics <- overall_metrics
  results$group_by <- grouping
  
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



cross_validation_simple <- function(lm_formula, data,
                                    grouping = "Month",
                                    metrics = c("rmse", "mae", "mape", "r2", "adj_r2"),
                                    k = 5){
  
  # Shuffle data
  data <- data[sample(nrow(data)), ]
  
  # Create k folds
  folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)
  
  # Choose grouping variable
  group_var <- switch(grouping,
                      "Month" = "Month_Index",
                      "Day" = "Day_Index",
                      "Year" = "year",
                      stop("Invalid grouping option"))
  
  # Initialize list to store results
  results_list <- list()
  
  for (i in 1:k) {
    # Split into training and validation sets
    test_idx <- which(folds == i, arr.ind = TRUE)
    train_data <- data[test_idx, ]
    validation_data <- data[-test_idx, ]
    
    # Train model
    model <- lm(lm_formula, data = train_data)
    
    # Compute predictions BEFORE summarizing
    validation_data <- validation_data %>%
      mutate(pred = predict(model, newdata = validation_data))
    
    # Group and compute metrics
    grouped_results <- validation_data %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        rmse = if ("rmse" %in% metrics) sqrt(mean((pred - Y)^2)) else NA,
        mae = if ("mae" %in% metrics) mean(abs(pred - Y)) else NA,
        mape = if ("mape" %in% metrics) mean(abs((pred - Y) / Y)) * 100 else NA,
        r2 = if ("r2" %in% metrics) ifelse(n() > 1, cor(pred, Y)^2, NA) else NA,
        adj_r2 = if ("adj_r2" %in% metrics) summary(model)$adj.r.squared else NA
      )
    
    # Store results
    results_list[[i]] <- grouped_results
  }
  
  # Combine results from all folds and compute average metrics
  final_results <- bind_rows(results_list) %>%
    group_by(!!sym(group_var)) %>%
    summarise(across(everything(), mean, na.rm = TRUE))
  
  return(final_results)
}