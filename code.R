#' Validate Input data
#' @descripton Make sure that input data has at least 1 column and is not NULL
#' @param data dataframe.
#' @return logical TRUE if data is invalid
validate_data <- function(data){
  return (is.null(data)||length(data) == 0)
}

#' Calculate Root Mean Squared Error
#' @descripton Calculate rsme. ignore na values
#' @param true_vals y
#' @param pred_vals y hat
#' @return numeric
calculate_rmse <- function(true_vals, pred_vals){
  rmse <- sqrt(mean((true_vals - pred_vals)^2, na.rm = TRUE))
  return(rmse)
}

#' Calculate R Squared
#' @descripton Make sure variance is above 0, ignore na values
#' @param true_vals y
#' @param pred_vals y hat
#' @return numeric
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

#' Helper function to get model degress of freedom
#' @descripton has validation to ensure a model was passed. Accounts for intercept (if present)
#' @param model.
#' @return numeric.
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

#' Calculate Adjusted R Squared
#' @descripton alse checks that there are enough datapoints to use
#' @param model. To pass to calculate degrees of freedom
#' @param true_vals y
#' @param pred_vals y hat
#' @return numeric.
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

#' Calculate Evaluation metrics
#' @descripton Calculate metrics:
#'  -Root Mean Squared Error (RMSE)
#'  -R Squared (r2)
#'  -Adjusted R Squared (adj_r2)
#'  Validate data to make sure metrics can be calculated.
#' @param actual y
#' @param predicted y hat
#' @param model. If calculating adjusted r squared
#' @param metrics. character vector of metrics to calculate (see above for options)
#' @return df of results or empty list if invalid argument is passed
calculate_metrics <- function(actual, predicted, model = NULL, metrics = c("rmse", "r2", "adj_r2")){
  # Initialize variables
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

#' Validate inputs that will be entered into cross validation
#' @description First stop points to stop execution before stratified k-fold runs
#' @param model.
#' @param data. dataframe that the model was trained on
#' @param k. number of folds to cross validate with.
#' @return Nothing. Just stops execution if an error is thrown.
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

#' Split data into k (stratified) folds.
#' @descripton Split the data into folds. 
#' If stratification variables are given then it will ensure proportional representation using the caret (package in R).
#' Otherwise it will use random sampling to reindex the data and make the folds
#' @param data. Dataframe of data to be split
#' @param k. Defaults to 5.
#' @param strat_vars. Optional. Pass a character vector of column names to stratify by.
#' @ results. data split into k folds
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

#' Perform k fold cross validation
#' @descripton. K fold cross validation on set of metrics.
#' - Option to stratify by few variables
#' - After performing one fold, group by date periods to evaluate the model on each sub period (e.g. each year)
#' @param model lm model object specifying the model to evaluate
#' @param data dataframe containing the variables in the model
#' @param metrics character vector of performance metrics to calculate
#' @param k integer number of folds for cross-validation
#' @param strat_vars character vector of column names to use for stratified sampling
#' @param groupings character vector of time units to group results by
#' @param date_var character name of the date column in data
#' @return dataframe with all cross_validation results

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
      year_col <- NULL
      #Check incase year column already exists
      if (date_var %in% colnames(val_data)) {
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
      if (date_var %in% colnames(val_data)) {
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
      if (date_var %in% colnames(val_data)) {
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
