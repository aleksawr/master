# ============================================================
# 04_run_all.R
# Purpose:
#   Run full simulation pipeline in parallel
# ============================================================

rm(list = ls(all.names = TRUE))

source("00_design.R")
source("01_simulation.R")
source("02_fit_models.R")
source("03_evaluate.R")

library(parallel)

job_grid <- expand.grid(
  condition_row = seq_len(nrow(cond_grid)),
  rep = seq_len(n_rep)
)

cat("Total jobs:", nrow(job_grid), "\n")

job_list <- split(job_grid, seq_len(nrow(job_grid)))

run_one_job <- function(this_job, cond_grid, sim_design) {
  
  i <- this_job$condition_row
  r <- this_job$rep
  
  this_cond <- cond_grid[i, ]
  
  set.seed(100000 + i * 1000 + r)
  
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
  
  sim_data <- sim_out$data
  
  split_out <- split_data(
    data = sim_data,
    train_prop = sim_design$train_prop
  )
  
  train_data <- split_out$train_data
  test_data  <- split_out$test_data
  
  ols_base_fit <- fit_ols_base(train_data)
  ols_true_interaction_fit <- fit_ols_true_interaction(train_data)
  ols_oracle_fit <- fit_ols_oracle(train_data)
  xgb_fit <- fit_xgb(train_data)
  
  preds <- predict_models(
    test_data = test_data,
    ols_base = ols_base_fit,
    ols_true_interaction = ols_true_interaction_fit,
    ols_oracle = ols_oracle_fit,
    xgb_fit = xgb_fit
  )
  
  metrics_rep <- evaluate_predictions(
    y_test = test_data$Y,
    preds = preds,
    condition_id = this_cond$condition_id,
    latent_R2 = this_cond$latent_R2,
    rho_X = this_cond$rho_X,
    rho_Y = this_cond$rho_Y,
    comp_linear = this_cond$comp_linear,
    rho_betweenX = this_cond$rho_betweenX,
    rep = r,
    
    realized_latent_R2 = sim_out$realized_latent_R2,
    realized_rho_X = sim_out$realized_rho_X,
    realized_rho_X_min = sim_out$realized_rho_X_min,
    realized_rho_X_max = sim_out$realized_rho_X_max,
    realized_rho_Y = sim_out$realized_rho_Y,
    realized_linear_share = sim_out$realized_linear_share,
    realized_interaction_share = sim_out$realized_interaction_share,
    realized_lin_int_cor = sim_out$realized_lin_int_cor,
    realized_mean_cor_X = sim_out$realized_mean_cor_X
  )
  
  metrics_rep
}

# Start conservatively
n_cores <- 2
cl <- makeCluster(n_cores)

clusterExport(cl, varlist = c(
  "cond_grid",
  "sim_design",
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
  "fit_xgb",
  "predict_models",
  "evaluate_predictions",
  "rmse_fun",
  "r2_fun",
  "run_one_job",
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

results_list <- parLapply(
  cl = cl,
  X = job_list,
  fun = run_one_job,
  cond_grid = cond_grid,
  sim_design = sim_design
)

stopCluster(cl)

runtime <- Sys.time() - start_time
cat("\nTotal runtime:", runtime, "\n")

results_df <- do.call(rbind, results_list)

cat("Rows:", nrow(results_df), "\n")
cat("Unique conditions:", length(unique(results_df$condition_id)), "\n")

run_label <- "run_2026-04-07_00-523_nrep200"

dir.create(file.path("runs", run_label), recursive = TRUE, showWarnings = FALSE)

write.csv(
  results_df,
  file.path("runs", run_label, "results_replication_level.csv"),
  row.names = FALSE
)

cat("Finished. Results saved to:\n")
cat(file.path("runs", run_label, "results_replication_level.csv"), "\n")

cat("Finished. Results saved to results_replication_level.csv\n")
print(head(results_df))
