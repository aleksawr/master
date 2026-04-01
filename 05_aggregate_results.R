# ============================================================
# 05_aggregate_results.R
# Purpose:
#   Aggregate replication-level results within condition
# ============================================================

rm(list = ls(all.names = TRUE))
source("run_config.R")

results_df <- read.csv(file.path(run_dir, "results_replication_level.csv"))

str(results_df)
head(results_df)

# -----------------------------
# 1. Means within condition
# -----------------------------
mean_df <- aggregate(
  cbind(
    realized_latent_R2,
    realized_rho_X,
    realized_rho_X_min,
    realized_rho_X_max,
    realized_rho_Y,
    realized_linear_share,
    realized_interaction_share,
    realized_lin_int_cor,
    realized_mean_cor_X,
    
    rmse_ols_base,
    rmse_ols_true_interaction,
    rmse_ols_oracle,
    rmse_xgb,
    
    delta_rmse_true_vs_base,
    delta_rmse_oracle_vs_base,
    delta_rmse_oracle_vs_true,
    delta_rmse_xgb_vs_base,
    delta_rmse_xgb_vs_true,
    delta_rmse_xgb_vs_oracle,
    
    r2_ols_base,
    r2_ols_true_interaction,
    r2_ols_oracle,
    r2_xgb,
    
    delta_r2_true_vs_base,
    delta_r2_oracle_vs_base,
    delta_r2_oracle_vs_true,
    delta_r2_xgb_vs_base,
    delta_r2_xgb_vs_true,
    delta_r2_xgb_vs_oracle
  ) ~ condition_id + latent_R2 + rho_X + rho_Y + comp_linear + rho_betweenX,
  data = results_df,
  FUN = mean
)

names(mean_df) <- c(
  "condition_id", "latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX",
  
  "mean_realized_latent_R2",
  "mean_realized_rho_X",
  "mean_realized_rho_X_min",
  "mean_realized_rho_X_max",
  "mean_realized_rho_Y",
  "mean_realized_linear_share",
  "mean_realized_interaction_share",
  "mean_realized_lin_int_cor",
  "mean_realized_mean_cor_X",
  
  "mean_rmse_ols_base",
  "mean_rmse_ols_true_interaction",
  "mean_rmse_ols_oracle",
  "mean_rmse_xgb",
  
  "mean_delta_rmse_true_vs_base",
  "mean_delta_rmse_oracle_vs_base",
  "mean_delta_rmse_oracle_vs_true",
  "mean_delta_rmse_xgb_vs_base",
  "mean_delta_rmse_xgb_vs_true",
  "mean_delta_rmse_xgb_vs_oracle",
  
  "mean_r2_ols_base",
  "mean_r2_ols_true_interaction",
  "mean_r2_ols_oracle",
  "mean_r2_xgb",
  
  "mean_delta_r2_true_vs_base",
  "mean_delta_r2_oracle_vs_base",
  "mean_delta_r2_oracle_vs_true",
  "mean_delta_r2_xgb_vs_base",
  "mean_delta_r2_xgb_vs_true",
  "mean_delta_r2_xgb_vs_oracle"
)

# -----------------------------
# 2. SDs within condition
# -----------------------------
sd_df <- aggregate(
  cbind(
    realized_latent_R2,
    realized_rho_X,
    realized_rho_X_min,
    realized_rho_X_max,
    realized_rho_Y,
    realized_linear_share,
    realized_interaction_share,
    realized_lin_int_cor,
    realized_mean_cor_X,
    
    rmse_ols_base,
    rmse_ols_true_interaction,
    rmse_ols_oracle,
    rmse_xgb,
    
    delta_rmse_true_vs_base,
    delta_rmse_oracle_vs_base,
    delta_rmse_oracle_vs_true,
    delta_rmse_xgb_vs_base,
    delta_rmse_xgb_vs_true,
    delta_rmse_xgb_vs_oracle,
    
    r2_ols_base,
    r2_ols_true_interaction,
    r2_ols_oracle,
    r2_xgb,
    
    delta_r2_true_vs_base,
    delta_r2_oracle_vs_base,
    delta_r2_oracle_vs_true,
    delta_r2_xgb_vs_base,
    delta_r2_xgb_vs_true,
    delta_r2_xgb_vs_oracle
  ) ~ condition_id + latent_R2 + rho_X + rho_Y + comp_linear + rho_betweenX,
  data = results_df,
  FUN = sd
)

names(sd_df) <- c(
  "condition_id", "latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX",
  
  "sd_realized_latent_R2",
  "sd_realized_rho_X",
  "sd_realized_rho_X_min",
  "sd_realized_rho_X_max",
  "sd_realized_rho_Y",
  "sd_realized_linear_share",
  "sd_realized_interaction_share",
  "sd_realized_lin_int_cor",
  "sd_realized_mean_cor_X",
  
  "sd_rmse_ols_base",
  "sd_rmse_ols_true_interaction",
  "sd_rmse_ols_oracle",
  "sd_rmse_xgb",
  
  "sd_delta_rmse_true_vs_base",
  "sd_delta_rmse_oracle_vs_base",
  "sd_delta_rmse_oracle_vs_true",
  "sd_delta_rmse_xgb_vs_base",
  "sd_delta_rmse_xgb_vs_true",
  "sd_delta_rmse_xgb_vs_oracle",
  
  "sd_r2_ols_base",
  "sd_r2_ols_true_interaction",
  "sd_r2_ols_oracle",
  "sd_r2_xgb",
  
  "sd_delta_r2_true_vs_base",
  "sd_delta_r2_oracle_vs_base",
  "sd_delta_r2_oracle_vs_true",
  "sd_delta_r2_xgb_vs_base",
  "sd_delta_r2_xgb_vs_true",
  "sd_delta_r2_xgb_vs_oracle"
)

# -----------------------------
# 3. Merge means and SDs
# -----------------------------
agg_df <- merge(
  mean_df,
  sd_df,
  by = c("condition_id", "latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX")
)

# -----------------------------
# 4. Save summary
# -----------------------------
write.csv(
  agg_df,
  file.path(run_dir, "results_condition_summary.csv"),
  row.names = FALSE
)

cat("Expected summary rows:", length(unique(results_df$condition_id)), "\n")
cat("Observed summary rows:", nrow(agg_df), "\n")
cat("Finished. Saved results_condition_summary.csv\n")

print(head(agg_df))

