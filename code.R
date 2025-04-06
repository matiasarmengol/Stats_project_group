validate_data <- function(data){
  return (is.null(data)||length(data) == 0)
}

calculate_rmse <- function(true_vals, pred_vals){
  rmse <- sqrt(mean((true_vals - pred_vals)^2, na.rm = TRUE))  # Fixed RMSE calculation
  return(rmse)
}

calculate_r2 <- function(true_vals, pred_vals){
  if(var(true_vals, na.rm = TRUE) > 0){
    avg_val <- mean(true_vals, na.rm = TRUE)
    ss_total <- sum((true_vals - avg_val)^2, na.rm = TRUE)
    ss_res <- sum((true_vals - pred_vals)^2, na.rm = TRUE)
    r2 <- 1 - ss_res/ss_total
    return(r2)
  } else{
    warning("Variance of true values is not greater than 0")
    return(NULL)
  }
}

get_degrees_freedom <- function(model){
  if (is.null(model)){
    warning("Model is NULL")
    return(NULL)
  } else{
    total_coeffs <- length(coef(model))
    has_intercept <- any(names(coef(model)) == "(Intercept)")
    if (has_intercept){
      return(total_coeffs - 1)
    } else{
      return(total_coeffs)
    }
  }
}

calculate_adj_r2 <- function(true_vals, pred_vals, model){
  num_predictors <- get_degrees_freedom(model)
  n <- sum(!is.na(true_vals) & !is.na(pred_vals))

  if (n <= num_predictors + 1){
    warning("Not enough data points to calculate adj_r2")
    return(NULL)
  } else{
    r2 <- calculate_r2(true_vals, pred_vals)
    adj_r2 <- 1 - ((1-r2)*(n-1)/(n - num_predictors - 1))
    return(adj_r2)
  }
}

calculate_metrics <- function(actual, predicted, model = NULL, metrics = c("rmse", "r2", "adj_r2")){
  # Initialize result variables
  true_vals <- NULL
  pred_vals <- NULL
  
  # Check actual values
  if (!validate_data(actual)){
    true_vals <- actual
  } else {
    warning("Empty or NULL actual data passed to calculate metrics")
    return(list())  # Return empty list if data invalid
  } 
  
  # Check predicted values
  if (!validate_data(predicted)){
    pred_vals <- predicted
  } else {
    warning("Empty or NULL predicted data passed to calculate metrics")
    return(list())  # Return empty list if data invalid
  }
  
  # Ensure both are numeric vectors with same length
  if (length(true_vals) != length(pred_vals)) {
    warning("Actual and predicted values must have the same length")
    return(list())
  }
  
  results <- list()

  if ("rmse" %in% metrics){
    results$rmse <- calculate_rmse(true_vals, pred_vals)
  }

  if ("r2" %in% metrics) {
    results$r2 <- calculate_r2(true_vals, pred_vals)
  }
  
  if ("adj_r2" %in% metrics && !is.null(model)){
    results$adj_r2 <- calculate_adj_r2(true_vals, pred_vals, model)
  }

  return(results)
}

validate_inputs <- function(model, data, k){
  if (!inherits(model, "lm")) {
    stop("Model must be a linear model object")
  }
   if(validate_data(data)){
    stop("Empty or NULL data passed to k-fold cross validation")
  }
  if (k < 2 || k > nrow(data)){
    stop("Invalid number of folds")
  }


}

create_k_folds <- function(data, k = 5, strat_vars = NULL){
  #Shuffle and split data
  if(!is.null(strat_vars)){
    missing_vars <- strat_vars[!strat_vars %in% colnames(data)]
    if (length(missing_vars) > 0){
      warning(paste("Stratification variables not found in data", paste(missing_vars, collapse= ', ')))
      return(NULL)
    }

    #Create combined stratificted factor
    strata_combination <- do.call(paste, c(data[strat_vars], sep = "_"))
    
    # Split by strata
    fold_indices <- createFolds(strata_combination, k = k, list = FALSE)
  } else{
    # Split into k folds
    shuffled_indices <- sample(nrow(data))
    data <- data[shuffled_indices, ]
    fold_indices <- cut(0:nrow(data), breaks = k, labels = FALSE)
  }

  return(fold_indices)
}

k_fold_cross_validate <- function(model, data,
                                  metrics = c("rmse", "r2", "adj_r2"),
                                  k = 5,
                                  strat_vars = NULL,
                                  groupings = c("Year", "Month", "Weekday"),
                                  date_var = "Date"){
  validate_inputs(model, data, k)
  # Model formula
  model_formula <- formula(model)
  
  # Get response variable name from formula
  response_var <- all.vars(model_formula)[1]
  
  # Check if response variable exists in data
  if (!response_var %in% colnames(data)) {
    warning(paste("Response variable", response_var, "not found in data"))
    return(NULL)
  }
  # Create time grouping variables if they don't exist
  data_with_groups <- data
  
  #Get folds
  fold_indices = create_k_folds(data, k, strat_vars)
  # Set up for results
  fold_metrics <- list()
  for (metric in metrics){
    fold_metrics[[metric]] <- numeric(k)
  }
  
  all_actuals <- numeric(nrow(data))
  all_predictions <- numeric(nrow(data))
  
  # Initialize grouping metrics
  fold_year_metrics <- list()
  fold_month_metrics <- list()
  fold_weekday_metrics <- list()
  
  # Perform k-fold cross validation
  for (i in 1:k){
    # Split data
    val_indices <- which(fold_indices == i)
    train_data <- data[-val_indices, ]
    val_data <- data[val_indices, ]
    # Train model
    fold_model <- lm(model_formula, data = train_data)
    # Make predictions
    predictions <- predict(fold_model, newdata = val_data)
    #For future
    all_actuals[val_indices] <- val_data[[response_var]]
    all_predictions[val_indices] <- predictions
    # Calculate metrics for the fold
    fold_results <- calculate_metrics(
      actual = val_data[[response_var]], 
      predicted = predictions,
      model = fold_model,
      metrics = metrics
    )
    
    # Group predictions by year within this fold
    if ("Year" %in% groupings) {
      # Check for different possible column name cases
      year_col <- NULL
      if ("Year" %in% colnames(val_data)) {
        year_col <- "Year"
      } else if ("year" %in% colnames(val_data)) {
        year_col <- "year"
      } else if (date_var %in% colnames(val_data)) {
        # Extract year from date if date column exists
        val_data$temp_year <- format(val_data[[date_var]], "%Y")
        year_col <- "temp_year"
      }
      
      if (!is.null(year_col)) {
        # Calculate metrics for each year in this fold
        years_in_fold <- unique(val_data[[year_col]])
        for (yr in years_in_fold) {
          year_indices <- which(val_data[[year_col]] == yr)
          year_predictions <- predictions[year_indices]
          year_actuals <- val_data[[response_var]][year_indices]
          
          # Store year-specific metrics for this fold
          year_key <- paste0("year_", yr)
          if (!(year_key %in% names(fold_year_metrics))) {
            fold_year_metrics[[year_key]] <- list(
              predictions = list(),
              actuals = list()
            )
          }
          
          fold_year_metrics[[year_key]]$predictions[[i]] <- year_predictions
          fold_year_metrics[[year_key]]$actuals[[i]] <- year_actuals
        }
      }
    }
    
    # Group predictions by month within this fold
    if ("Month" %in% groupings) {
      # Similar implementation for months
      month_col <- NULL
      if ("Month" %in% colnames(val_data)) {
        month_col <- "Month"
      } else if ("month" %in% colnames(val_data)) {
        month_col <- "month"
      } else if (date_var %in% colnames(val_data)) {
        val_data$temp_month <- format(val_data[[date_var]], "%m")
        month_col <- "temp_month"
      }
      
      if (!is.null(month_col)) {
        months_in_fold <- unique(val_data[[month_col]])
        for (mnth in months_in_fold) {
          month_indices <- which(val_data[[month_col]] == mnth)
          month_predictions <- predictions[month_indices]
          month_actuals <- val_data[[response_var]][month_indices]
          
          month_key <- paste0("month_", mnth)
          if (!(month_key %in% names(fold_month_metrics))) {
            fold_month_metrics[[month_key]] <- list(
              predictions = list(),
              actuals = list()
            )
          }
          
          fold_month_metrics[[month_key]]$predictions[[i]] <- month_predictions
          fold_month_metrics[[month_key]]$actuals[[i]] <- month_actuals
        }
      }
    }
    
    # Group predictions by weekday within this fold
    if ("Weekday" %in% groupings) {
      # Similar implementation for weekdays
      weekday_col <- NULL
      if ("Weekday" %in% colnames(val_data)) {
        weekday_col <- "Weekday"
      } else if ("weekday" %in% colnames(val_data)) {
        weekday_col <- "weekday"
      } else if (date_var %in% colnames(val_data)) {
        val_data$temp_weekday <- weekdays(val_data[[date_var]])
        weekday_col <- "temp_weekday"
      }
      
      if (!is.null(weekday_col)) {
        weekdays_in_fold <- unique(val_data[[weekday_col]])
        for (wday in weekdays_in_fold) {
          weekday_indices <- which(val_data[[weekday_col]] == wday)
          weekday_predictions <- predictions[weekday_indices]
          weekday_actuals <- val_data[[response_var]][weekday_indices]
          
          weekday_key <- paste0("weekday_", wday)
          if (!(weekday_key %in% names(fold_weekday_metrics))) {
            fold_weekday_metrics[[weekday_key]] <- list(
              predictions = list(),
              actuals = list()
            )
          }
          
          fold_weekday_metrics[[weekday_key]]$predictions[[i]] <- weekday_predictions
          fold_weekday_metrics[[weekday_key]]$actuals[[i]] <- weekday_actuals
        }
      }
    }
    
    # Store the results for this fold
    for (metric in metrics) {
      if (metric %in% names(fold_results)) {
        fold_metrics[[metric]][i] <- fold_results[[metric]]
      }
    }
  }
  
  # Calculate overall metrics using the calculate_metrics function
  overall_results <- calculate_metrics(
    actual = all_actuals,
    predicted = all_predictions,
    model = model,  # Using the original model here
    metrics = metrics
  )
  
  # Calculate average fold metrics
  avg_fold_metrics <- lapply(metrics, function(metric) {
    if (metric %in% names(fold_metrics)) {
      return(mean(fold_metrics[[metric]], na.rm = TRUE))
    } else {
      return(NA)
    }
  })
  names(avg_fold_metrics) <- metrics
  
  # Finalise Results
  results <- list(
    fold_metrics = fold_metrics,
    overall_metrics = overall_results,
    avg_fold_metrics = avg_fold_metrics
  )
  
  # Calculate overall metrics by year
  if (length(fold_year_metrics) > 0) {
    year_performance <- list()
    
    for (year_key in names(fold_year_metrics)) {
      # Combine predictions and actuals across all folds for this year
      all_year_predictions <- unlist(fold_year_metrics[[year_key]]$predictions)
      all_year_actuals <- unlist(fold_year_metrics[[year_key]]$actuals)
      
      # Calculate metrics for this year
      year_performance[[year_key]] <- calculate_metrics(
        actual = all_year_actuals,
        predicted = all_year_predictions,
        model = model,  # Using the original model here
        metrics = metrics
      )
    }
    
    # Add to results
    results$year_performance <- year_performance
  }
  
  # Calculate overall metrics by month
  if (length(fold_month_metrics) > 0) {
    month_performance <- list()
    
    for (month_key in names(fold_month_metrics)) {
      # Combine predictions and actuals across all folds for this month
      all_month_predictions <- unlist(fold_month_metrics[[month_key]]$predictions)
      all_month_actuals <- unlist(fold_month_metrics[[month_key]]$actuals)
      
      # Calculate metrics for this month
      month_performance[[month_key]] <- calculate_metrics(
        actual = all_month_actuals,
        predicted = all_month_predictions,
        model = model,
        metrics = metrics
      )
    }
    
    # Add to results
    results$month_performance <- month_performance
  }
  
  # Calculate overall metrics by weekday
  if (length(fold_weekday_metrics) > 0) {
    weekday_performance <- list()
    
    for (weekday_key in names(fold_weekday_metrics)) {
      # Combine predictions and actuals across all folds for this weekday
      all_weekday_predictions <- unlist(fold_weekday_metrics[[weekday_key]]$predictions)
      all_weekday_actuals <- unlist(fold_weekday_metrics[[weekday_key]]$actuals)
      
      # Calculate metrics for this weekday
      weekday_performance[[weekday_key]] <- calculate_metrics(
        actual = all_weekday_actuals,
        predicted = all_weekday_predictions,
        model = model,
        metrics = metrics
      )
    }
    
    # Add to results
    results$weekday_performance <- weekday_performance
  }
  
  return(results)
}
