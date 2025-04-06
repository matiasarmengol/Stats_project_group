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

k_fold_cross_validate <- function(model, data,
                                  metrics = c("rmse", "r2", "adj_r2"),
                                  k = 5){
  if(validate_data(data)){
    warning("Empty or NULL data passed to k-fold cross validation")
    return(NULL)
  }
  if (k < 2 || k > nrow(data)){
    warning("Invalid number of folds")
    return(NULL)
  }

  # Model formula
  model_formula <- formula(model)
  
  # Get response variable name from formula
  response_var <- all.vars(model_formula)[1]
  
  # Check if response variable exists in data
  if (!response_var %in% colnames(data)) {
    warning(paste("Response variable", response_var, "not found in data"))
    return(NULL)
  }

  # Shuffle and split into k folds
  shuffled_indices <- sample(nrow(data))
  data <- data[shuffled_indices, ]
  fold_indices <- cut(1:nrow(data), breaks = k, labels = FALSE)

  # Set up for results
  fold_metrics <- list()
  for (metric in metrics){
    fold_metrics[[metric]] <- numeric(k)
  }
  
  all_predictions <- numeric(nrow(data))
  all_actuals <- numeric(nrow(data))

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

    # Store true and predicted
    all_predictions[val_indices] <- predictions
    all_actuals[val_indices] <- val_data[[response_var]]

    # Calculate metrics
    fold_results <- calculate_metrics(
      actual = val_data[[response_var]], 
      predicted = predictions,
      model = fold_model,
      metrics = metrics
    )
    
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
  
  # Return results
  results <- list(
    fold_metrics = fold_metrics,
    overall_metrics = overall_results,
    avg_fold_metrics = avg_fold_metrics,
    predictions = all_predictions,
    actuals = all_actuals
  )

  return(results)
}

cross_validate_model <- function(model, data,
                           metrics = c("rmse", "r2", "adj_r2"),
                           k = 5, 
                           groupings = c("Year", "Month", "Weekday"),
                           date_var = "Date") {
  
  # Validate inputs
  if (!inherits(model, "lm")) {
    stop("Model must be a linear model object")
  }
  
  if (validate_data(data)) {
    stop("Empty or NULL data passed to cross validation")
  }
  
  if (!date_var %in% colnames(data)) {
    stop(paste("Date variable", date_var, "not found in the data"))
  }
  
  
  
  # Create time grouping variables if they don't exist
  data_with_groups <- data
  
  if ("Year" %in% groupings && !"Year" %in% colnames(data)) {
    data_with_groups$Year <- as.factor(format(data_with_groups[[date_var]], "%Y"))
  }
  
  if ("Month" %in% groupings && !"Month" %in% colnames(data)) {
    data_with_groups$Month <- as.factor(format(data_with_groups[[date_var]], "%m"))
  }
  
  if ("Weekday" %in% groupings && !"Weekday" %in% colnames(data)) {
    data_with_groups$Weekday <- as.factor(format(data_with_groups[[date_var]], "%a"))
  }
  
  # Store results for each grouping
  all_results <- list()
  
  # Overall cross-validation (no grouping)
  all_results$Overall <- k_fold_cross_validate(
    model = model,
    data = data_with_groups,
    metrics = metrics,
    k = k
  )
  
  # Cross-validation for each grouping type
  for (group_var in groupings) {
    if (group_var %in% colnames(data_with_groups)) {
      message(paste("Performing cross-validation by", group_var, "..."))
      
      # Get unique groups
      unique_groups <- unique(data_with_groups[[group_var]])
      group_results <- list()
      
      # Cross-validate within each group
      for (group in unique_groups) {
        message(paste(" - Processing", group_var, "=", group))
        
        # Subset data for this group
        group_data <- data_with_groups[data_with_groups[[group_var]] == group, ]
        
        # Skip if not enough data points
        if (nrow(group_data) < 2 * k) {
          message(paste("   Skipping", group_var, "=", group, "- insufficient data points"))
          next
        }
        
        # Perform k-fold cross-validation
        cv_result <- k_fold_cross_validate(
          model = model,
          data = group_data,
          metrics = metrics,
          k = min(k, floor(nrow(group_data)/2))  # Ensure k isn't too large for the data
        )
        
        # Store result
        group_results[[as.character(group)]] <- cv_result
      }
      
      # Store results for this grouping
      all_results[[group_var]] <- group_results
      
      # Calculate aggregate metrics across all groups
      aggregate_metrics <- list()
      for (metric in metrics) {
        # Extract this metric from each group
        metric_values <- sapply(group_results, function(result) {
          if (!is.null(result$overall_metrics[[metric]])) {
            return(result$overall_metrics[[metric]])
          } else {
            return(NA)
          }
        })
        
        # Calculate weighted average (weighted by number of observations in each group)
        group_sizes <- sapply(names(group_results), function(group) {
          sum(!is.na(group_results[[group]]$actuals))
        })
        
        # Calculate weighted average if we have valid values
        if (any(!is.na(metric_values)) && sum(group_sizes) > 0) {
          weighted_avg <- sum(metric_values * group_sizes, na.rm = TRUE) / 
                          sum(group_sizes[!is.na(metric_values)])
          aggregate_metrics[[paste0("weighted_avg_", metric)]] <- weighted_avg
        }
      }
      
      # Store aggregate metrics
      all_results[[paste0(group_var, "_aggregate")]] <- aggregate_metrics
    } else {
      warning(paste("Grouping variable", group_var, "not found in data"))
    }
  }
  
  # Return all results
  return(all_results)
}

# Helper function to summarize cross-validation results
summarize_cv_results <- function(cv_results) {
  summary <- list()
  
  # Overall summary
  if (!is.null(cv_results$Overall)) {
    summary$Overall <- cv_results$Overall$overall_metrics
  }
  
  # Group summaries
  for (group in names(cv_results)) {
    if (group != "Overall" && !endsWith(group, "_aggregate")) {
      # Extract metrics for each subgroup
      group_summary <- data.frame(Group = character(), 
                                  N = integer(), 
                                  stringsAsFactors = FALSE)
      
      # Add columns for each metric
      metrics <- names(cv_results$Overall$overall_metrics)
      for (metric in metrics) {
        group_summary[[metric]] <- numeric()
      }
      
      # Add data for each subgroup
      for (subgroup in names(cv_results[[group]])) {
        row <- data.frame(
          Group = subgroup,
          N = length(cv_results[[group]][[subgroup]]$actuals),
          stringsAsFactors = FALSE
        )
        
        # Add metric values
        for (metric in metrics) {
          row[[metric]] <- cv_results[[group]][[subgroup]]$overall_metrics[[metric]]
        }
        
        # Add to summary
        group_summary <- rbind(group_summary, row)
      }
      
      # Sort by group name
      group_summary <- group_summary[order(group_summary$Group), ]
      
      # Add to overall summary
      summary[[group]] <- group_summary
    }
    
    # Include aggregate metrics
    if (endsWith(group, "_aggregate")) {
      summary[[group]] <- cv_results[[group]]
    }
  }
  
  return(summary)
}
