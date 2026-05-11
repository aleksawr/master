# ============================================================
# 12_xgb_tuning_sensitivity_5fold_8cond.R
# Purpose:
#   Targeted sensitivity check for XGBoost tuning
#
#   Checks whether the main benchmark pattern changes when
#   XGBoost is tuned less conservatively.
#
#   Targeted conditions:
#     latent_R2    = .20 and .80
#     comp_linear  = .20
#     rho_X/rho_Y  = .60/.60 and 1.00/1.00
#     rho_betweenX = 0.00 and 0.50
#
#   XGBoost variants:
#     1. xgb_current_3fold_simple
#     2. xgb_minrmse_3fold
#     3. xgb_minrmse_5fold
# ============================================================

rm(list = ls(all.names = TRUE))

source("run_config.R")
source("00_design.R")
source("01_simulation.R")
source("02_fit_models.R")
source("03_evaluate.R")

library(parallel)
library(xgboost)

# ------------------------------------------------------------
# 1. Sensitivity settings
# ------------------------------------------------------------

# For a quick smoke test, set this to 20 first.
# For the real targeted sensitivity check, use 100 or 200.
sens_n_rep <- 200

sens_label <- paste0(
  "sensitivity_xgb_tuning_",
  "16maincond_",
  sens_n_rep, "rep_",
  "minrmse_5fold"
)

sensitivity_dir <- file.path(run_dir, sens_label)

dir.create(sensitivity_dir, recursive = TRUE, showWarnings = FALSE)

cat("\nSensitivity output folder:\n")
cat(sensitivity_dir, "\n\n")


# ------------------------------------------------------------
# 2. Select targeted sensitivity conditions: same 16 as main selected conditions
# ------------------------------------------------------------

cond_grid_sens <- cond_grid
cond_grid_sens$condition_row <- seq_len(nrow(cond_grid_sens))

sens_conditions <- subset(
  cond_grid_sens,
  rho_betweenX == 0.00 &
    rho_X %in% c(0.60, 1.00) &
    rho_Y %in% c(0.60, 1.00) &
    comp_linear %in% c(0.20, 0.80) &
    latent_R2 %in% c(0.20, 0.80)
)

sens_conditions <- sens_conditions[order(
  sens_conditions$rho_X,
  sens_conditions$rho_Y,
  sens_conditions$comp_linear,
  sens_conditions$latent_R2
), ]

sens_conditions$condition_label <- sprintf(
  "rhoX=%.2f | rhoY=%.2f | linear=%.2f | latentR2=%.2f | rhoBetweenX=%.2f",
  sens_conditions$rho_X,
  sens_conditions$rho_Y,
  sens_conditions$comp_linear,
  sens_conditions$latent_R2,
  sens_conditions$rho_betweenX
)

cat("Selected sensitivity conditions:\n")
print(sens_conditions)

cat("\nNumber of selected conditions:", nrow(sens_conditions), "\n\n")

if (nrow(sens_conditions) != 16) {
  stop("Expected 16 sensitivity conditions. Check condition selection.")
}

cat("Selected condition IDs:\n")
print(sens_conditions$condition_id)

expected_ids <- c(
  1, 3, 7, 9,
  19, 21, 25, 27,
  55, 57, 61, 63,
  73, 75, 79, 81
)

if (!setequal(sens_conditions$condition_id, expected_ids)) {
  stop("Selected condition IDs do not match the intended 16 conditions.")
}

write.csv(
  sens_conditions,
  file.path(sensitivity_dir, "selected_sensitivity_conditions.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 3. Build sensitivity job grid
# ------------------------------------------------------------

job_grid <- expand.grid(
  condition_row = sens_conditions$condition_row,
  rep = seq_len(sens_n_rep)
)

cat("Total sensitivity jobs:", nrow(job_grid), "\n\n")

job_list <- split(job_grid, seq_len(nrow(job_grid)))

# ------------------------------------------------------------
# 4. Helper: fixed CV folds
# ------------------------------------------------------------

make_cv_folds <- function(n, nfold, seed) {
  set.seed(seed)
  fold_id <- sample(rep(seq_len(nfold), length.out = n))
  split(seq_len(n), fold_id)
}


# ------------------------------------------------------------
# 5. Run XGBoost CV grid
# ------------------------------------------------------------

fit_xgb_cv_grid <- function(train_data,
                            nfold = 3,
                            cv_seed = 1) {
  
  X_train <- as.matrix(train_data[, x_names, drop = FALSE])
  y_train <- train_data$Y
  
  dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  
  grid <- expand.grid(
    eta = c(0.05, 0.10),
    max_depth = c(2, 3),
    min_child_weight = c(1, 5),
    subsample = c(0.8),
    colsample_bytree = c(0.8),
    lambda = c(1),
    stringsAsFactors = FALSE
  )
  
  folds <- make_cv_folds(
    n = nrow(train_data),
    nfold = nfold,
    seed = cv_seed
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
      nthread = 1,
      seed = cv_seed + i
    )
    
    set.seed(cv_seed + i)
    
    cv_fit <- xgboost::xgb.cv(
      params = params,
      data = dtrain,
      nrounds = 300,
      folds = folds,
      verbose = 0,
      early_stopping_rounds = 20,
      maximize = FALSE
    )
    
    best_iter <- cv_fit$best_iteration
    eval_log <- cv_fit$evaluation_log
    
    best_rmse <- eval_log$test_rmse_mean[best_iter]
    best_rmse_sd <- eval_log$test_rmse_std[best_iter]
    
    cv_results[[i]] <- data.frame(
      grid_row = i,
      eta = grid$eta[i],
      max_depth = grid$max_depth[i],
      min_child_weight = grid$min_child_weight[i],
      subsample = grid$subsample[i],
      colsample_bytree = grid$colsample_bytree[i],
      lambda = grid$lambda[i],
      best_iteration = best_iter,
      cv_rmse = best_rmse,
      cv_rmse_sd = best_rmse_sd,
      cv_rmse_se = best_rmse_sd / sqrt(nfold),
      nfold = nfold
    )
  }
  
  do.call(rbind, cv_results)
}


# ------------------------------------------------------------
# 6. Select tuning row
# ------------------------------------------------------------

select_xgb_row <- function(cv_results_df,
                           rule = c("current_simple_sd", "true_1se", "min_rmse")) {
  
  rule <- match.arg(rule)
  
  if (rule == "min_rmse") {
    return(which.min(cv_results_df$cv_rmse))
  }
  
  min_idx <- which.min(cv_results_df$cv_rmse)
  min_rmse <- cv_results_df$cv_rmse[min_idx]
  
  if (rule == "current_simple_sd") {
    threshold <- min_rmse + cv_results_df$cv_rmse_sd[min_idx]
  }
  
  if (rule == "true_1se") {
    threshold <- min_rmse + cv_results_df$cv_rmse_se[min_idx]
  }
  
  candidate_idx <- which(cv_results_df$cv_rmse <= threshold)
  candidates <- cv_results_df[candidate_idx, , drop = FALSE]
  
  simplicity_order <- order(
    candidates$max_depth,
    -candidates$min_child_weight,
    candidates$best_iteration,
    candidates$eta
  )
  
  candidate_idx[simplicity_order[1]]
}


# ------------------------------------------------------------
# 7. Train final XGBoost model from selected CV row
# ------------------------------------------------------------

train_xgb_from_cv_row <- function(train_data,
                                  selected_row,
                                  train_seed = 1) {
  
  X_train <- as.matrix(train_data[, x_names, drop = FALSE])
  y_train <- train_data$Y
  
  dtrain <- xgboost::xgb.DMatrix(data = X_train, label = y_train)
  
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = selected_row$eta,
    max_depth = selected_row$max_depth,
    min_child_weight = selected_row$min_child_weight,
    subsample = selected_row$subsample,
    colsample_bytree = selected_row$colsample_bytree,
    lambda = selected_row$lambda,
    nthread = 1,
    seed = train_seed
  )
  
  set.seed(train_seed)
  
  xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = selected_row$best_iteration,
    verbose = 0
  )
}


# ------------------------------------------------------------
# 8. Fit XGBoost tuning variants
# ------------------------------------------------------------

fit_xgb_sensitivity_variants <- function(train_data,
                                         base_seed = 1) {
  
  # 3-fold CV: current simple rule and minimum-RMSE rule
  cv3 <- fit_xgb_cv_grid(
    train_data = train_data,
    nfold = 3,
    cv_seed = base_seed + 100000
  )
  
  row_current_3fold <- select_xgb_row(
    cv_results_df = cv3,
    rule = "current_simple_sd"
  )
  
  row_minrmse_3fold <- select_xgb_row(
    cv_results_df = cv3,
    rule = "min_rmse"
  )
  
  fit_current_3fold <- train_xgb_from_cv_row(
    train_data = train_data,
    selected_row = cv3[row_current_3fold, , drop = FALSE],
    train_seed = base_seed + 200001
  )
  
  fit_minrmse_3fold <- train_xgb_from_cv_row(
    train_data = train_data,
    selected_row = cv3[row_minrmse_3fold, , drop = FALSE],
    train_seed = base_seed + 200002
  )
  
  selected_3fold <- rbind(
    data.frame(
      xgb_variant = "xgb_current_3fold_simple",
      selection_rule = "current_simple_sd",
      cv3[row_current_3fold, , drop = FALSE]
    ),
    data.frame(
      xgb_variant = "xgb_minrmse_3fold",
      selection_rule = "min_rmse",
      cv3[row_minrmse_3fold, , drop = FALSE]
    )
  )
  
  
  # 5-fold CV: minimum-RMSE rule
  cv5 <- fit_xgb_cv_grid(
    train_data = train_data,
    nfold = 5,
    cv_seed = base_seed + 300000
  )
  
  row_minrmse_5fold <- select_xgb_row(
    cv_results_df = cv5,
    rule = "min_rmse"
  )
  
  fit_minrmse_5fold <- train_xgb_from_cv_row(
    train_data = train_data,
    selected_row = cv5[row_minrmse_5fold, , drop = FALSE],
    train_seed = base_seed + 400001
  )
  
  selected_5fold <- data.frame(
    xgb_variant = "xgb_minrmse_5fold",
    selection_rule = "min_rmse",
    cv5[row_minrmse_5fold, , drop = FALSE]
  )
  
  list(
    fits = list(
      xgb_current_3fold_simple = fit_current_3fold,
      xgb_minrmse_3fold = fit_minrmse_3fold,
      xgb_minrmse_5fold = fit_minrmse_5fold
    ),
    selected_params = rbind(selected_3fold, selected_5fold)
  )
}


# ------------------------------------------------------------
# 9. Prediction helper for XGBoost
# ------------------------------------------------------------

predict_xgb_fit <- function(xgb_fit, test_data) {
  X_test <- as.matrix(test_data[, x_names, drop = FALSE])
  as.numeric(predict(xgb_fit, newdata = X_test))
}


# ------------------------------------------------------------
# 10. Metric row helper
# ------------------------------------------------------------

make_metric_rows <- function(y_test,
                             preds,
                             this_cond,
                             rep) {
  
  out <- do.call(
    rbind,
    lapply(names(preds), function(m) {
      data.frame(
        condition_id = this_cond$condition_id,
        latent_R2 = this_cond$latent_R2,
        rho_X = this_cond$rho_X,
        rho_Y = this_cond$rho_Y,
        comp_linear = this_cond$comp_linear,
        rho_betweenX = this_cond$rho_betweenX,
        rep = rep,
        model = m,
        R2 = r2_fun(y_test, preds[[m]]),
        RMSE = rmse_fun(y_test, preds[[m]])
      )
    })
  )
  
  rownames(out) <- NULL
  out
}


# ------------------------------------------------------------
# 11. One sensitivity job
# ------------------------------------------------------------

run_one_sensitivity_job <- function(this_job,
                                    cond_grid,
                                    sim_design,
                                    run_seed) {
  
  i <- this_job$condition_row
  r <- this_job$rep
  
  this_cond <- cond_grid[i, ]
  
  base_seed <- run_seed + i * 1000 + r
  
  set.seed(base_seed)
  
  sim_out <- generate_dataset(
    N = sim_design$N,
    p = sim_design$p,
    beta = sim_design$beta,
    latent_R2 = this_cond$latent_R2,
    rho_X = this_cond$rho_X,
    rho_Y = this_cond$rho_Y,
    comp_linear = this_cond$comp_linear,
    rho_betweenX = this_cond$rho_betweenX
  )
  
  split_out <- split_data(
    data = sim_out$data,
    train_prop = sim_design$train_prop
  )
  
  train_data <- split_out$train_data
  test_data  <- split_out$test_data
  
  # OLS benchmarks
  ols_base_fit <- fit_ols_base(train_data)
  ols_aligned_fit <- fit_ols_true_interaction(train_data)
  ols_oracle_fit <- fit_ols_oracle(train_data)
  
  # XGBoost tuning variants
  xgb_sens <- fit_xgb_sensitivity_variants(
    train_data = train_data,
    base_seed = base_seed
  )
  
  preds <- list(
    ols_base = as.numeric(predict(ols_base_fit$fit, newdata = test_data)),
    ols_aligned = as.numeric(predict(ols_aligned_fit$fit, newdata = test_data)),
    ols_oracle = as.numeric(predict(ols_oracle_fit$fit, newdata = test_data)),
    
    xgb_current_3fold_simple = predict_xgb_fit(
      xgb_sens$fits$xgb_current_3fold_simple,
      test_data
    ),
    
    xgb_minrmse_3fold = predict_xgb_fit(
      xgb_sens$fits$xgb_minrmse_3fold,
      test_data
    ),
    
    xgb_minrmse_5fold = predict_xgb_fit(
      xgb_sens$fits$xgb_minrmse_5fold,
      test_data
    )
  )
  
  metrics <- make_metric_rows(
    y_test = test_data$Y,
    preds = preds,
    this_cond = this_cond,
    rep = r
  )
  
  selected_params <- cbind(
    data.frame(
      condition_id = this_cond$condition_id,
      latent_R2 = this_cond$latent_R2,
      rho_X = this_cond$rho_X,
      rho_Y = this_cond$rho_Y,
      comp_linear = this_cond$comp_linear,
      rho_betweenX = this_cond$rho_betweenX,
      rep = r
    ),
    xgb_sens$selected_params
  )
  
  list(
    metrics = metrics,
    selected_params = selected_params
  )
}


# ------------------------------------------------------------
# 12. Run in parallel
# ------------------------------------------------------------

cat("Starting parallel sensitivity run with", n_cores, "cores...\n\n")

cl <- makeCluster(n_cores)

clusterExport(cl, varlist = c(
  "cond_grid",
  "sim_design",
  "run_seed",
  
  "generate_dataset",
  "generate_X_true",
  "generate_Y_true",
  "add_measurement_error",
  "generate_X_obs",
  "generate_Y_obs",
  "compute_realized_rho_X",
  "compute_realized_rho_Y",
  "compute_mean_cor_X",
  "make_sigma_equicorr",
  "split_data",
  
  "fit_ols_base",
  "fit_ols_true_interaction",
  "fit_ols_oracle",
  
  "make_cv_folds",
  "fit_xgb_cv_grid",
  "select_xgb_row",
  "train_xgb_from_cv_row",
  "fit_xgb_sensitivity_variants",
  "predict_xgb_fit",
  
  "make_metric_rows",
  "run_one_sensitivity_job",
  
  "rmse_fun",
  "r2_fun",
  
  "x_names",
  "oracle_x_names",
  "ols_base_formula",
  "ols_true_interaction_formula",
  "ols_oracle_formula"
), envir = environment())

clusterEvalQ(cl, {
  source("00_design.R")
  source("01_simulation.R")
  source("02_fit_models.R")
  source("03_evaluate.R")
  library(xgboost)
  library(MASS)
  NULL
})

start_time <- Sys.time()

sens_list <- parLapply(
  cl = cl,
  X = job_list,
  fun = run_one_sensitivity_job,
  cond_grid = cond_grid,
  sim_design = sim_design,
  run_seed = run_seed
)

stopCluster(cl)

runtime <- Sys.time() - start_time

cat("\nSensitivity runtime:\n")
print(runtime)


# ------------------------------------------------------------
# 13. Combine and save replication-level results
# ------------------------------------------------------------

sens_metrics <- do.call(rbind, lapply(sens_list, `[[`, "metrics"))
sens_selected_params <- do.call(rbind, lapply(sens_list, `[[`, "selected_params"))

write.csv(
  sens_metrics,
  file.path(sensitivity_dir, "sensitivity_metrics_replication_level.csv"),
  row.names = FALSE
)

write.csv(
  sens_selected_params,
  file.path(sensitivity_dir, "sensitivity_selected_xgb_params.csv"),
  row.names = FALSE
)

cat("\nSaved replication-level sensitivity metrics:\n")
cat(file.path(sensitivity_dir, "sensitivity_metrics_replication_level.csv"), "\n")

cat("\nSaved selected XGBoost tuning parameters:\n")
cat(file.path(sensitivity_dir, "sensitivity_selected_xgb_params.csv"), "\n")


# ------------------------------------------------------------
# 14. Condition-level means and Monte Carlo standard errors
# ------------------------------------------------------------

group_cols <- c(
  "condition_id",
  "latent_R2",
  "rho_X",
  "rho_Y",
  "comp_linear",
  "rho_betweenX",
  "model"
)

condition_means <- aggregate(
  cbind(R2, RMSE) ~ condition_id + latent_R2 + rho_X + rho_Y +
    comp_linear + rho_betweenX + model,
  data = sens_metrics,
  FUN = mean
)

condition_sds <- aggregate(
  cbind(R2, RMSE) ~ condition_id + latent_R2 + rho_X + rho_Y +
    comp_linear + rho_betweenX + model,
  data = sens_metrics,
  FUN = sd
)

condition_ns <- aggregate(
  R2 ~ condition_id + latent_R2 + rho_X + rho_Y +
    comp_linear + rho_betweenX + model,
  data = sens_metrics,
  FUN = length
)

names(condition_means)[names(condition_means) == "R2"] <- "mean_R2"
names(condition_means)[names(condition_means) == "RMSE"] <- "mean_RMSE"

names(condition_sds)[names(condition_sds) == "R2"] <- "sd_R2"
names(condition_sds)[names(condition_sds) == "RMSE"] <- "sd_RMSE"

names(condition_ns)[names(condition_ns) == "R2"] <- "n_rep"

condition_summary <- merge(
  condition_means,
  condition_sds,
  by = group_cols
)

condition_summary <- merge(
  condition_summary,
  condition_ns,
  by = group_cols
)

condition_summary$mcse_R2 <- condition_summary$sd_R2 / sqrt(condition_summary$n_rep)
condition_summary$mcse_RMSE <- condition_summary$sd_RMSE / sqrt(condition_summary$n_rep)

write.csv(
  condition_summary,
  file.path(sensitivity_dir, "sensitivity_condition_summary.csv"),
  row.names = FALSE
)

cat("\nSaved condition-level sensitivity summary:\n")
cat(file.path(sensitivity_dir, "sensitivity_condition_summary.csv"), "\n")


# ------------------------------------------------------------
# 15. Overall model summary
# ------------------------------------------------------------

overall_means <- aggregate(
  cbind(R2, RMSE) ~ model,
  data = sens_metrics,
  FUN = mean
)

overall_sds <- aggregate(
  cbind(R2, RMSE) ~ model,
  data = sens_metrics,
  FUN = sd
)

overall_ns <- aggregate(
  R2 ~ model,
  data = sens_metrics,
  FUN = length
)

names(overall_means)[names(overall_means) == "R2"] <- "mean_R2"
names(overall_means)[names(overall_means) == "RMSE"] <- "mean_RMSE"

names(overall_sds)[names(overall_sds) == "R2"] <- "sd_R2"
names(overall_sds)[names(overall_sds) == "RMSE"] <- "sd_RMSE"

names(overall_ns)[names(overall_ns) == "R2"] <- "n"

overall_summary <- merge(overall_means, overall_sds, by = "model")
overall_summary <- merge(overall_summary, overall_ns, by = "model")

overall_summary$mcse_R2 <- overall_summary$sd_R2 / sqrt(overall_summary$n)
overall_summary$mcse_RMSE <- overall_summary$sd_RMSE / sqrt(overall_summary$n)

write.csv(
  overall_summary,
  file.path(sensitivity_dir, "sensitivity_overall_model_summary.csv"),
  row.names = FALSE
)

cat("\nOverall model summary:\n")
print(overall_summary)


# ------------------------------------------------------------
# 16. Benchmark-pattern check: condition-level R2
# ------------------------------------------------------------

factor_cols <- c(
  "condition_id",
  "latent_R2",
  "rho_X",
  "rho_Y",
  "comp_linear",
  "rho_betweenX"
)

r2_condition_means <- condition_summary[
  ,
  c(factor_cols, "model", "mean_R2", "mcse_R2")
]

r2_wide <- reshape(
  r2_condition_means[, c(factor_cols, "model", "mean_R2")],
  idvar = factor_cols,
  timevar = "model",
  direction = "wide"
)

benchmark_check <- data.frame(
  r2_wide[, factor_cols],
  
  R2_ols_base = r2_wide$mean_R2.ols_base,
  R2_ols_aligned = r2_wide$mean_R2.ols_aligned,
  R2_ols_oracle = r2_wide$mean_R2.ols_oracle,
  
  R2_xgb_current_3fold_simple = r2_wide$mean_R2.xgb_current_3fold_simple,
  R2_xgb_minrmse_3fold = r2_wide$mean_R2.xgb_minrmse_3fold,
  R2_xgb_minrmse_5fold = r2_wide$mean_R2.xgb_minrmse_5fold
)

benchmark_check$delta_min5_vs_base <- benchmark_check$R2_xgb_minrmse_5fold -
  benchmark_check$R2_ols_base

benchmark_check$delta_min5_vs_aligned <- benchmark_check$R2_xgb_minrmse_5fold -
  benchmark_check$R2_ols_aligned

benchmark_check$delta_min5_vs_oracle <- benchmark_check$R2_xgb_minrmse_5fold -
  benchmark_check$R2_ols_oracle

benchmark_check$min5_beats_base <- benchmark_check$delta_min5_vs_base > 0
benchmark_check$min5_beats_aligned <- benchmark_check$delta_min5_vs_aligned > 0
benchmark_check$min5_beats_oracle <- benchmark_check$delta_min5_vs_oracle > 0

write.csv(
  benchmark_check,
  file.path(sensitivity_dir, "sensitivity_benchmark_check_R2.csv"),
  row.names = FALSE
)

cat("\nCondition-level benchmark check:\n")
print(benchmark_check)

cat("\nSummary of benchmark pattern for xgb_minrmse_5fold:\n")

cat(
  "Beats baseline OLS:",
  sum(benchmark_check$min5_beats_base),
  "of",
  nrow(benchmark_check),
  "conditions\n"
)

cat(
  "Beats aligned OLS:",
  sum(benchmark_check$min5_beats_aligned),
  "of",
  nrow(benchmark_check),
  "conditions\n"
)

cat(
  "Beats oracle OLS:",
  sum(benchmark_check$min5_beats_oracle),
  "of",
  nrow(benchmark_check),
  "conditions\n"
)


# ------------------------------------------------------------
# 17. Selected parameter summary
# ------------------------------------------------------------

param_summary <- aggregate(
  cbind(
    eta,
    max_depth,
    min_child_weight,
    best_iteration,
    cv_rmse
  ) ~ xgb_variant,
  data = sens_selected_params,
  FUN = mean
)

write.csv(
  param_summary,
  file.path(sensitivity_dir, "sensitivity_selected_param_summary.csv"),
  row.names = FALSE
)

cat("\nAverage selected XGBoost parameters:\n")
print(param_summary)


# ------------------------------------------------------------
# 18. Finish
# ------------------------------------------------------------

cat("\nFinished XGBoost tuning sensitivity check.\n")
cat("All outputs saved to:\n")
cat(sensitivity_dir, "\n")
