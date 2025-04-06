validate_data <- function(data){
  return (is.null(data)||length(data) == 0)
}

calculate_rmse <- function(true_vals, pred_vals){
  rmse <- mean(abs(true_vals - pred_vals)^2, na.rm = TRUE)
  return(rmse)
}

calculate_r2 <- function(true_vals, pred_vals){
  if(var(true_vals)>0){
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
    has_intercept <- any(names(coef(model))== "(Intercept)")
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

  if (n <= num_predictors +1){
    warning("Not enough data points to calculate adj_r2")
    return(NULL)
  } else{
    r2 <- calculate_r2(true_vals, pred_vals)
    adj_r2 <- 1 - ((1-r2)*(n-1)/(n - num_predictors - 1))
    return(adj_r2)
  }
}

calculate_metrics <- function(actual, predicted, model = NULL, metrics = c("rmse", "r2", "adj_r2")){
  if (validate_data(actual)){
    true_vals <- actual
  } else if (validate_data(predicted)){
    pred_vals <- predicted
  } else {
    warning("Empty or NULL data passed to calculate metrics")
  }
  results <- list()

  if ("rmse" %in% metrics){
    results$rmse <- calculate_rmse(true_vals, pred_vals)
  }

  if ("r2" %in% metrics) {
    results$r2 <- calculate_r2(true_vals, pred_vals)
  }
  if ("adj_r2" %in% metrics){
    results$adj_r2 <- calculate_adj_r2(true_vals, pred_vals, model)
  }

  return(results)
}

cross_validate <- function(model, data,
                            metrics = c("rmse", "r2", "adj_r2"),
                            k = 5, analyse_by = c("Year", "Month", "Day"),
                            date_var = "Date"){
  # Input validation
  if(is.null(data) || nrow(data)==0){
    stop("Empty or NULL data passed to cross validation")
  } else if (!inherits(model, "lm")){
    stop("Model must be a linear model")
  } else if (!date_var %in% colnames(data)){
    stop(paste("Date variable", date_var, "not found in data"))
  }

  # Make sure date column is a date
  if(!inherits(data[[date_var]], "Date")){
    data[[date_var]] <- as.Date(data[[date_var]])
  }

  #Make columns to split by Year, Month and Day
  data$Year <- as.numeric(format(data[[date_var]], "%Y"))
  data$Month <- as.numeric(format(data[[date_var]], "%m"))
  data$Day <- as.numeric(format(data[[date_var]], "%d"))

  cv_results <- list(
    overall = list(),
    by_strata = list()
  )

  # Create stratification variable based on analyse_by
  strata_vars <- paste(
    if ("Year" %in% analyse_by) "Year" else NULL,
    if ("Month" %in% analyse_by) "Month" else NULL,
    if ("Day" %in% analyse_by) "Day" else NULL
  )
  strata_vars <- strata_vars[strata_vars != ""]
  
  # Create a combined stratification factor
  if (length(strata_vars) > 0) {
    data$strata <- do.call(paste, c(data[strata_vars], sep = "-"))
  } else {
    data$strata <- "all"
  }
  
  # Get unique strata
  unique_strata <- unique(data$strata)
  
  # Create stratified folds
  folds <- vector("list", k)
  for (i in 1:k) {
    folds[[i]] <- data.frame()
  }
  
  # Distribute data into folds maintaining strata proportions
  for (s in unique_strata) {
    strata_data <- data[data$strata == s, ]
    strata_rows <- nrow(strata_data)
    
    # Handle case when strata has fewer rows than k
    if (strata_rows < k) {
      # Distribute rows equally and repeat if necessary
      for (i in 1:strata_rows) {
        fold_idx <- (i - 1) %% k + 1
        folds[[fold_idx]] <- rbind(folds[[fold_idx]], strata_data[i, ])
      }
    } else {
      # Shuffle the strata data
      strata_data <- strata_data[sample(strata_rows), ]
      
      # Distribute rows among folds
      for (i in 1:strata_rows) {
        fold_idx <- (i - 1) %% k + 1
        folds[[fold_idx]] <- rbind(folds[[fold_idx]], strata_data[i, ])
      }
    }
  }

  # Strata-specific metrics
  strata_metrics <- list()
  for (s in unique_strata) {
    strata_metrics[[s]] <- list()
    for (metric in metrics) {
      strata_metrics[[s]][[metric]] <- numeric(k)
    }
  }
  
  # Perform k-fold cross-validation
  for (i in 1:k) {
    # Training data (all folds except the current one)
    train_data <- do.call(rbind, folds[-i])
    
    # Validation data (current fold)
    val_data <- folds[[i]]
    
    # Train the model
    # Note: This assumes model is a formula-like specification
    # Adjust according to your modeling approach
    fitted_model <- update(model, data = train_data)
    
    # Generate predictions
    predictions <- predict(fitted_model, newdata = val_data)
    
    # Calculate overall metrics
    fold_metrics <- calculate_metrics(
      actual = val_data$Y, # Replace with actual response variable
      predicted = predictions,
      model = fitted_model,
      metrics = metrics
    )
    
    # Store overall fold metrics
    for (metric in metrics) {
      overall_metrics[[metric]][i] <- fold_metrics[[metric]]
    }
    
    # Calculate metrics by strata
    for (s in unique_strata) {
      strata_indices <- val_data$strata == s
      if (sum(strata_indices) > 0) {
        strata_actuals <- val_data$Y[strata_indices] # Replace with actual response variable
        strata_preds <- predictions[strata_indices]
        
        # Only calculate if we have enough data
        if (length(strata_actuals) > 1) {
          strata_fold_metrics <- calculate_metrics(
            actual = strata_actuals,
            predicted = strata_preds,
            model = fitted_model,
            metrics = metrics
          )
          
          # Store strata-specific fold metrics
          for (metric in metrics) {
            strata_metrics[[s]][[metric]][i] <- strata_fold_metrics[[metric]]
          }
        }
      }
    }
  }
  
  # Compute average metrics across folds
  cv_results$overall <- lapply(overall_metrics, mean, na.rm = TRUE)
  
  # Compute average metrics for each stratum
  for (s in unique_strata) {
    cv_results$by_strata[[s]] <- lapply(strata_metrics[[s]], mean, na.rm = TRUE)
  }

  return(cv_results)
}
