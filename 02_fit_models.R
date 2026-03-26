# ============================================================
# 02_fit_models.R
# Purpose:
#   Split data, fit models, predict on test set
# ============================================================

x_names <- paste0("X", 1:20)

ols_base_formula <- as.formula(
  paste("Y ~", paste(x_names, collapse = " + "))
)

ols_true_interaction_formula <- as.formula(
  paste("Y ~", paste(x_names, collapse = " + "), "+ X1:X2")
)

split_data <- function(data, train_prop = 0.70) {
  n <- nrow(data)
  train_idx <- sample.int(n, size = floor(train_prop * n))
  
  list(
    train_data = data[train_idx, , drop = FALSE],
    test_data  = data[-train_idx, , drop = FALSE],
    train_idx = train_idx
  )
}

fit_ols_base <- function(train_data) {
  list(
    fit = lm(ols_base_formula, data = train_data),
    formula = ols_base_formula
  )
}

fit_ols_true_interaction <- function(train_data) {
  list(
    fit = lm(ols_true_interaction_formula, data = train_data),
    formula = ols_true_interaction_formula
  )
}

fit_xgb <- function(train_data,
                    nfold = 5,
                    seed = 123,
                    use_1se = FALSE) {
  
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("Package 'xgboost' is required.")
  }
  
  X_train <- as.matrix(train_data[, x_names, drop = FALSE])
  y_train <- train_data$Y
  
  dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  
  grid <- expand.grid(
    eta = c(0.03, 0.05, 0.10),
    max_depth = c(2, 3, 4),
    min_child_weight = c(1, 5),
    subsample = c(0.8),
    colsample_bytree = c(0.8),
    lambda = c(1),
    nrounds = c(100, 200, 300),
    stringsAsFactors = FALSE
  )
  
  cv_results <- vector("list", nrow(grid))
  
  for (i in seq_len(nrow(grid))) {
    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      eta = grid$eta[i],
      max_depth = grid$max_depth[i],
      min_child_weight = grid$min_child_weight[i],
      subsample = grid$subsample[i],
      colsample_bytree = grid$colsample_bytree[i],
      lambda = grid$lambda[i],
      nthread = 1
    )
    
    set.seed(seed)
    
    cv_fit <- xgboost::xgb.cv(
      params = params,
      data = dtrain,
      nrounds = grid$nrounds[i],
      nfold = nfold,
      verbose = 0,
      early_stopping_rounds = 20,
      maximize = FALSE
    )
    
    best_iter <- cv_fit$best_iteration
    best_rmse <- cv_fit$evaluation_log$test_rmse_mean[best_iter]
    best_rmse_sd <- cv_fit$evaluation_log$test_rmse_std[best_iter]
    
    cv_results[[i]] <- data.frame(
      eta = grid$eta[i],
      max_depth = grid$max_depth[i],
      min_child_weight = grid$min_child_weight[i],
      subsample = grid$subsample[i],
      colsample_bytree = grid$colsample_bytree[i],
      lambda = grid$lambda[i],
      nrounds = grid$nrounds[i],
      best_iteration = best_iter,
      cv_rmse = best_rmse,
      cv_rmse_sd = best_rmse_sd
    )
  }
  
  cv_results_df <- do.call(rbind, cv_results)
  
  if (!use_1se) {
    best_row <- which.min(cv_results_df$cv_rmse)
  } else {
    min_idx <- which.min(cv_results_df$cv_rmse)
    min_rmse <- cv_results_df$cv_rmse[min_idx]
    min_se <- cv_results_df$cv_rmse_sd[min_idx]
    threshold <- min_rmse + min_se
    
    candidate_idx <- which(cv_results_df$cv_rmse <= threshold)
    candidates <- cv_results_df[candidate_idx, , drop = FALSE]
    
    simplicity_order <- order(
      candidates$max_depth,
      -candidates$min_child_weight,
      candidates$best_iteration,
      candidates$eta
    )
    
    best_row <- candidate_idx[simplicity_order[1]]
  }
  
  best_params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = cv_results_df$eta[best_row],
    max_depth = cv_results_df$max_depth[best_row],
    min_child_weight = cv_results_df$min_child_weight[best_row],
    subsample = cv_results_df$subsample[best_row],
    colsample_bytree = cv_results_df$colsample_bytree[best_row],
    lambda = cv_results_df$lambda[best_row],
    nthread = 1
  )
  
  best_nrounds <- cv_results_df$best_iteration[best_row]
  
  final_fit <- xgboost::xgb.train(
    params = best_params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0
  )
  
  list(
    fit = final_fit,
    params = best_params,
    best_nrounds = best_nrounds,
    cv_results = cv_results_df
  )
}

predict_models <- function(test_data,
                           ols_base = NULL,
                           ols_true_interaction = NULL,
                           xgb_fit = NULL) {
  
  X_test <- as.matrix(test_data[, x_names, drop = FALSE])
  
  preds <- list()
  
  if (!is.null(ols_base)) {
    preds$ols_base <- as.numeric(predict(ols_base$fit, newdata = test_data))
  }
  
  if (!is.null(ols_true_interaction)) {
    preds$ols_true_interaction <- as.numeric(
      predict(ols_true_interaction$fit, newdata = test_data)
    )
  }
  
  if (!is.null(xgb_fit)) {
    preds$xgb <- as.numeric(predict(xgb_fit$fit, newdata = X_test))
  }
  
  preds
}
