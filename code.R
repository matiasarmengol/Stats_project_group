val_data <- function(data){
  return (is.null(data)||length(data) == 0)
}

calculate_rmse <- function(true_vals, pred_vals){
  rmse <- mean(abs(actual - predicted)^2, na.rm = TRUE)
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
  if (val_data(actual)){
    true_vals <- actual
  } else if (val_data(predicted)){
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
                            k = 5, analyse_by = c("Year", "Month", "Day")
                            date_var = "Date"){

}
