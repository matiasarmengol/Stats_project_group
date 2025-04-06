###Using docstring for documentation

compare_winter <- function(model, data, other_year) {
  #' @title Compare maximum demand in 2013/14 winter with projections based on different weather conditions
  #' @description Use weather data from another year to generate a prediction for the maximum demand
  #' @param model A linear model object
  #' @param data A dataframe
  #' @param other_year The year whose weather data we wish to use
  
  #obtain the actual maximum gross demand from the 2013/14 winter
  control_set <- data |> 
    filter((year == "2013" & Month_Index %in% c(10, 11)) | (year == "2014" & Month_Index %in% c(0, 1, 2)))
  control_max <- max(control_set$Y)
  
  #create our specified set to predict on
  other_set <- data |> 
    filter((year == as.character(other_year) & Month_Index %in% c(10, 11)) | (year == as.character(as.numeric(other_year) + 1) & Month_Index %in% c(0, 1, 2))) |> 
    mutate(
      year = case_when(
        year == as.character(other_year) ~ 2013,
        year == as.character(as.numeric(other_year) + 1) ~ 2014,
        TRUE ~ year #don't break if it's not one of the above
      )
    ) |> 
    mutate(year_d = abs(as.numeric(year) - 2005)) |> 
    mutate(week_type = factor(ifelse(Day_Index %in% c(0, 6), 0, 1), 
                              levels = c(0, 1), 
                              labels = c("Weekend", "Weekday")))
  
  #predict the demand based on our newly defined set and extract the maximum
  #print(head(other_set)) # debugging
  prediction <- predict(model, newdata = other_set)
  max_predicted <- max(prediction, na.rm = TRUE)
  
  return(list(control_max = control_max, max_predicted = max_predicted))
}

get_response_var <- function(model) {
  #' @title Extract response variable from a linear model
  #' @description Gets the name of the variable being predicted by the model
  #' @param model A linear model object
  
  #' @return Character string with the name of the response variable
  return(all.vars(formula(model))[1])
}


get_degrees_freedom <- function(model) {
  #' @title Get number of predictors in a model
  #' @description Calculates the number of predictive variables in a linear model
  #' @param model A linear model object
  #' @return Integer count of predictors (excluding intercept)

  # Get total number of coefficients (including intercept if present)
  total_coeffs <- length(coef(model))
  
  has_intercept <- any(names(coef(model)) == "(Intercept)")
  
  degrees <- if (has_intercept) total_coeffs - 1 else total_coeffs
  
  return(degrees)
}


calculate_metrics <- function(actual, predicted, model = NULL, 
                              metrics = c("rmse", "mae", "mape", "r2", "adj_r2")) {
  #' @title Calculate model performance metrics
  #' @description Computes various performance metrics comparing predicted vs actual values
  #' @param actual Vector of actual observed values
  #' @param predicted Vector of model predictions
  #' @param model Optional model object for adjusted R-squared calculation
  #' @param metrics Character vector of metrics to calculate
  #' @return List of calculated performance metrics

  # Make sure not NA
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
    # Avoid division by zero
    valid_indices <- which(actual != 0 & !is.na(actual) & !is.na(predicted))
    if (length(valid_indices) > 0) {
      results$mape <- 100 * mean(abs((actual[valid_indices] - predicted[valid_indices]) / actual[valid_indices]))
    } else {
      results$mape <- NA
      warning("Cannot calculate MAPE: division by zero or all NA values")
    }
  }
  
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
        num_predictors <- get_degrees_freedom(model)
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


cross_validate <- function(model, data,
                           date_var = "Date",
                           grouping = "Month",
                           metrics = c("rmse", "mae", "mape", "r2", "adj_r2"),
                           k = 5,
                           analyze_by = c("Month", "Weekend", "Year")) {
  #' @title Perform cross-validation with time-based analysis
  #' @description Evaluates model performance using cross-validation with analysis by time periods
  #' @param model A linear model object
  #' @param data DataFrame containing all variables used in the model
  #' @param date_var Name of the date column
  #' @param grouping Method for dividing data into folds ("Month", "Weekend", "Year", "k-fold")
  #' @param metrics Performance metrics to calculate
  #' @param k Number of folds for k-fold cross-validation
  #' @param analyze_by Time dimensions to analyze performance by ("Month", "Weekend", "Year")
  #' @return List containing cross-validation results and time-based performance metrics
  
  ## Validation of inputs
  if (!inherits(model, "lm")){
    stop("Model must be a linear model (lm) object")
  }
  if (!date_var %in% names(data)) {
    stop(paste("Date variable", date_var, "not found in data"))
  }
  valid_metrics <- c("rmse", "mae", "mape", "r2", "adj_r2")
  if (!all(metrics %in% valid_metrics)) {
    stop(paste("Invalid metrics. Valid options are:", paste(valid_metrics, collapse = ", ")))
  }
  valid_analyze_by <- c("Month", "Weekend", "Year", "None")
  if (!all(analyze_by %in% valid_analyze_by)) {
    stop(paste("Invalid analyze_by options. Valid options are:", paste(valid_analyze_by, collapse = ", ")))
  }
  
  cv_data <- data.frame(data)
  
  ## Extract response variable from model formula
  response_var <- get_response_var(model)
  
  ## Check date variable is in Date format
  if (!inherits(cv_data[[date_var]], "Date")){
    cv_data[[date_var]] <- as.Date(cv_data[[date_var]])
  }
  
  ## Create mapping functions to use existing columns where possible
  # Map for month data
  month_map <- function(d) {
    if ("Month_Index" %in% names(cv_data)) {
      return(factor(cv_data$Month_Index, levels = 1:12, labels = month.name))
    } else {
      return(factor(month(d), levels = 1:12, labels = month.name))
    }
  }
  
  # Map for weekend data
  weekend_map <- function(d) {
    if ("week_type" %in% names(cv_data)) {
      return(factor(cv_data$week_type, levels = c("Weekday", "Weekend")))
    } else {
      return(factor(ifelse(wday(d) %in% c(1, 7), "Weekend", "Weekday"), 
                    levels = c("Weekday", "Weekend")))
    }
  }
  
  # Map for year data
  year_map <- function(d) {
    if ("year" %in% names(cv_data)) {
      return(factor(cv_data$year))
    } else {
      return(factor(year(d)))
    }
  }
  
  ## Create primary grouping variable
  if (grouping == "Month"){
    cv_data$CV_GROUP <- month_map(cv_data[[date_var]])
  } else if (grouping == "Weekend"){
    cv_data$CV_GROUP <- weekend_map(cv_data[[date_var]])
  } else if (grouping == "Year"){
    cv_data$CV_GROUP <- year_map(cv_data[[date_var]])
  } else if (grouping == "k-fold"){
    set.seed(123) # For reproducibility
    folds <- createFolds(cv_data[[response_var]], k = k, list = FALSE)
    cv_data$CV_GROUP <- factor(folds)
  } else {
    stop(paste("Invalid grouping. Valid options are: Month, Weekend, Year, k-fold"))
  }
  
  unique_groups <- unique(cv_data$CV_GROUP)
  
  # For primary cross-validation
  group_results <- list()
  overall_actuals <- c()
  overall_predictions <- c()
  
  # For time-based analysis
  time_categories <- list()
  time_actuals <- list()
  time_predictions <- list()
  
  # Create storage for each time dimension
  for (dim in analyze_by) {
    if (dim != "None") {
      time_actuals[[dim]] <- list()
      time_predictions[[dim]] <- list()
      
      # Pre-allocate lists based on dimension
      if (dim == "Month") {
        for (m in month.name) {
          time_actuals[[dim]][[m]] <- c()
          time_predictions[[dim]][[m]] <- c()
        }
      } else if (dim == "Weekend") {
        time_actuals[[dim]][["Weekday"]] <- c()
        time_actuals[[dim]][["Weekend"]] <- c()
        time_predictions[[dim]][["Weekday"]] <- c()
        time_predictions[[dim]][["Weekend"]] <- c()
      } else if (dim == "Year") {
        # For years, create dynamically based on data
        years <- unique(year_map(cv_data[[date_var]]))
        for (y in levels(years)) {
          time_actuals[[dim]][[y]] <- c()
          time_predictions[[dim]][[y]] <- c()
        }
      }
    }
  }
  
  ## Perform cross-validation
  for (grp in unique_groups) {
    # Split data
    train_data <- cv_data[cv_data$CV_GROUP != grp, ]
    test_data <- cv_data[cv_data$CV_GROUP == grp, ]
    
    # Check data size
    if (nrow(train_data) < 10 || nrow(test_data) < 5) {
      warning(paste("Insufficient data for group:", grp))
      next
    }
    
    tryCatch({
      # Fit model
      cv_model <- update(model, data = train_data)
      
      # Predict on test data
      predictions <- predict(cv_model, newdata = test_data)
      actuals <- test_data[[response_var]]
      
      # Verify we have valid data
      if (length(actuals) == 0 || all(is.na(actuals)) || 
          length(predictions) == 0 || all(is.na(predictions))) {
        warning(paste("No valid data for group:", grp))
        next
      }
      
      # Store overall results
      overall_actuals <- c(overall_actuals, actuals)
      overall_predictions <- c(overall_predictions, predictions)
      
      # Calculate metrics for this group
      group_metrics <- calculate_metrics(actuals, predictions, model = cv_model, metrics = metrics)
      group_results[[as.character(grp)]] <- group_metrics
      
      # Store results by time dimensions for analysis
      for (dim in analyze_by) {
        if (dim != "None") {
          # Get appropriate categorization for this dimension
          if (dim == "Month") {
            categories <- month_map(test_data[[date_var]])
          } else if (dim == "Weekend") {
            categories <- weekend_map(test_data[[date_var]])
          } else if (dim == "Year") {
            categories <- year_map(test_data[[date_var]])
          }
          
          # Store predictions and actuals by category
          for (i in 1:length(actuals)) {
            if (!is.na(actuals[i]) && !is.na(predictions[i]) && !is.na(categories[i])) {
              cat_name <- as.character(categories[i])
              time_actuals[[dim]][[cat_name]] <- c(time_actuals[[dim]][[cat_name]], actuals[i])
              time_predictions[[dim]][[cat_name]] <- c(time_predictions[[dim]][[cat_name]], predictions[i])
            }
          }
        }
      }
      
    }, error = function(e) {
      warning(paste("Error in cross-validation for group:", grp, "-", e$message))
    })
  }
  
  # Check if we have any valid results
  if (length(overall_actuals) == 0 || length(overall_predictions) == 0) {
    warning("No valid predictions or actuals generated during cross-validation")
    return(NULL)
  }
  
  ## Calculate overall metrics
  overall_metrics <- calculate_metrics(overall_actuals, overall_predictions, model = model, metrics = metrics)
  
  ## Calculate metrics for each time dimension
  time_results <- list()
  
  for (dim in analyze_by) {
    if (dim != "None") {
      time_results[[dim]] <- list()
      
      for (cat in names(time_actuals[[dim]])) {
        cat_actuals <- time_actuals[[dim]][[cat]]
        cat_predictions <- time_predictions[[dim]][[cat]]
        
        if (length(cat_actuals) > 0 && length(cat_predictions) > 0) {
          cat_metrics <- calculate_metrics(cat_actuals, cat_predictions, metrics = metrics)
          time_results[[dim]][[cat]] <- cat_metrics
        } else {
          time_results[[dim]][[cat]] <- list(rmse = NA, mae = NA, mape = NA, r2 = NA, adj_r2 = NA, n = 0)
        }
      }
    }
  }
  
  ## Create summary data frames
  # Primary cross-validation results
  metrics_df <- data.frame(
    Group = names(group_results),
    N = sapply(group_results, function(x) x$n)
  )
  
  for (m in metrics) {
    metrics_df[[toupper(m)]] <- sapply(group_results, function(x) x[[m]])
  }
  
  # Add overall row
  overall_row <- data.frame(
    Group = "Overall",
    N = overall_metrics$n
  )
  
  for (m in metrics) {
    overall_row[[toupper(m)]] <- overall_metrics[[m]]
  }
  
  metrics_df <- rbind(metrics_df, overall_row)
  
  # Time dimension summary data frames
  time_dfs <- list()
  
  for (dim in analyze_by) {
    if (dim != "None") {
      dim_df <- data.frame(
        Category = names(time_results[[dim]]),
        N = sapply(time_results[[dim]], function(x) x$n)
      )
      
      for (m in metrics) {
        dim_df[[toupper(m)]] <- sapply(time_results[[dim]], function(x) x[[m]])
      }
      
      # Add overall row
      dim_overall <- data.frame(
        Category = "Overall",
        N = overall_metrics$n
      )
      
      for (m in metrics) {
        dim_overall[[toupper(m)]] <- overall_metrics[[m]]
      }
      
      dim_df <- rbind(dim_df, dim_overall)
      time_dfs[[dim]] <- dim_df
    }
  }
  
  return(list(
    cv_summary = metrics_df,
    time_analysis = time_dfs,
    details = list(
      group_results = group_results,
      overall_metrics = overall_metrics,
      time_results = time_results,
      actuals = overall_actuals,
      predictions = overall_predictions
    )
  ))
}


analyze_time_performance <- function(results, metric = "r2", 
                                     dimension = "Month", 
                                     threshold = 0.9) {
  #' @title Analyze model performance across time periods
  #' @description Evaluates relative model performance across different time periods
  #' @param results Results from cross_validate function
  #' @param metric Performance metric to analyze (e.g., "r2")
  #' @param dimension Time dimension to analyze ("Month", "Weekend", "Year")
  #' @param threshold Relative performance threshold for flagging underperformance
  #' @return Analysis of model performance across time periods
  
  if (!dimension %in% names(results$time_analysis)) {
    stop(paste("Time dimension", dimension, "not found in results"))
  }
  
  # Get metric column
  metric_col <- toupper(metric)
  
  # Extract metric values
  metrics_df <- results$time_analysis[[dimension]]
  
  # Get overall performance
  overall_value <- metrics_df[metrics_df$Category == "Overall", metric_col]
  
  # Get category-specific performance
  category_values <- metrics_df[metrics_df$Category != "Overall", ]
  
  # Calculate relative performance
  category_values$RelativePerformance <- category_values[[metric_col]] / overall_value
  category_values$AbsoluteDifference <- category_values[[metric_col]] - overall_value
  category_values$PercentDifference <- (category_values[[metric_col]] - overall_value) / overall_value * 100
  
  # Identify underperforming categories
  underperforming <- category_values[category_values$RelativePerformance < threshold, ]
  
  return(list(
    overall_performance = overall_value,
    category_performance = category_values,
    underperforming = underperforming,
    threshold = threshold,
    metric = metric,
    dimension = dimension
  ))
}



compare_models <- function(model_results, metric = "r2", dimension = "Month") {
  #' @title Compare multiple models across time periods
  #' @description Creates a comparison table of model performance across time periods
  #' @param model_results List of results from cross_validate for different models
  #' @param metric Performance metric to compare (e.g., "r2")
  #' @param dimension Time dimension to analyze ("Month", "Weekend", "Year")
  #' @return Data frame comparing models across time periods

  # Get all categories across all models
  all_categories <- c()
  for (model_name in names(model_results)) {
    if (!dimension %in% names(model_results[[model_name]]$time_analysis)) {
      warning(paste("Dimension", dimension, "not found in model", model_name))
      next
    }
    
    categories <- model_results[[model_name]]$time_analysis[[dimension]]$Category
    all_categories <- c(all_categories, as.character(categories))
  }
  
  # Ensure "Overall" is last
  categories <- unique(all_categories)
  categories <- categories[categories != "Overall"]
  categories <- c(categories, "Overall")
  
  # Create comparison data frame
  comparison <- data.frame(Category = categories)
  
  # Add metric for each model
  metric_col <- toupper(metric)
  
  for (model_name in names(model_results)) {
    if (!dimension %in% names(model_results[[model_name]]$time_analysis)) {
      next
    }
    
    # Get metrics for this model
    model_metrics <- model_results[[model_name]]$time_analysis[[dimension]]
    
    # Create a named vector for easy lookup
    values <- model_metrics[[metric_col]]
    names(values) <- model_metrics$Category
    
    # Add to comparison data frame
    comparison[[model_name]] <- NA
    for (cat in categories) {
      if (cat %in% names(values)) {
        comparison[comparison$Category == cat, model_name] <- values[cat]
      }
    }
  }
  
  return(comparison)
}


cross_validate_models <- function(models, data,
                                  date_var = "Date",
                                  grouping = "k-fold",
                                  metrics = c("rmse", "mae", "mape", "r2", "adj_r2"),
                                  k = 5,
                                  analyze_by = c("Month", "Weekend", "Year")) {
  #' @title Run cross-validation for multiple models
  #' @description Performs cross-validation on multiple models and returns results
  #' @param models Named list of linear models
  #' @param data Dataset containing all variables used in models
  #' @param date_var Name of the date column
  #' @param grouping Method for dividing data into folds
  #' @param metrics Performance metrics to calculate
  #' @param k Number of folds for k-fold cross-validation
  #' @param analyze_by Time dimensions to analyze performance by
  #' @return List of cross-validation results for each model
  # Initialize results
  require(lubridate)
  require(caret)
  
  results <- list()
  
  # Run cross-validation for each model
  for (model_name in names(models)) {
    cat("Running cross-validation for", model_name, "\n")
    
    results[[model_name]] <- cross_validate(
      model = models[[model_name]],
      data = data,
      date_var = date_var,
      grouping = grouping,
      metrics = metrics,
      k = k,
      analyze_by = analyze_by
    )
  }
  
  # Add a model comparison
  if (length(results) > 1) {
    comparisons <- list()
    
    for (dim in analyze_by) {
      if (dim != "None") {
        comparisons[[dim]] <- compare_models(results, metric = "r2", dimension = dim)
      }
    }
    
    results$comparisons <- comparisons
  }
  
  return(results)
}
