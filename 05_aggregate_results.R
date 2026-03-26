# ============================================================
# 05_aggregate_results.R
# Purpose:
#   Aggregate replication-level results within condition
# ============================================================

rm(list = ls(all.names = TRUE))

results_df <- read.csv("results_replication_level.csv")

str(results_df)
head(results_df)

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
    
    rmse_ols_base,
    rmse_ols_true_interaction,
    rmse_xgb,
    delta_rmse_true_vs_base,
    delta_rmse_xgb_vs_base,
    delta_rmse_xgb_vs_true,
    r2_ols_base,
    r2_ols_true_interaction,
    r2_xgb,
    delta_r2_true_vs_base,
    delta_r2_xgb_vs_base,
    delta_r2_xgb_vs_true
  ) ~ condition_id + latent_R2 + rho_X + rho_Y + comp_linear,
  data = results_df,
  FUN = mean
)

names(mean_df) <- c(
  "condition_id", "latent_R2", "rho_X", "rho_Y", "comp_linear",
  
  "mean_realized_latent_R2",
  "mean_realized_rho_X",
  "mean_realized_rho_X_min",
  "mean_realized_rho_X_max",
  "mean_realized_rho_Y",
  "mean_realized_linear_share",
  "mean_realized_interaction_share",
  "mean_realized_lin_int_cor",
  
  "mean_rmse_ols_base",
  "mean_rmse_ols_true_interaction",
  "mean_rmse_xgb",
  "mean_delta_rmse_true_vs_base",
  "mean_delta_rmse_xgb_vs_base",
  "mean_delta_rmse_xgb_vs_true",
  "mean_r2_ols_base",
  "mean_r2_ols_true_interaction",
  "mean_r2_xgb",
  "mean_delta_r2_true_vs_base",
  "mean_delta_r2_xgb_vs_base",
  "mean_delta_r2_xgb_vs_true"
)

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
    
    rmse_ols_base,
    rmse_ols_true_interaction,
    rmse_xgb,
    delta_rmse_true_vs_base,
    delta_rmse_xgb_vs_base,
    delta_rmse_xgb_vs_true,
    r2_ols_base,
    r2_ols_true_interaction,
    r2_xgb,
    delta_r2_true_vs_base,
    delta_r2_xgb_vs_base,
    delta_r2_xgb_vs_true
  ) ~ condition_id + latent_R2 + rho_X + rho_Y + comp_linear,
  data = results_df,
  FUN = sd
)

names(sd_df) <- c(
  "condition_id", "latent_R2", "rho_X", "rho_Y", "comp_linear",
  
  "sd_realized_latent_R2",
  "sd_realized_rho_X",
  "sd_realized_rho_X_min",
  "sd_realized_rho_X_max",
  "sd_realized_rho_Y",
  "sd_realized_linear_share",
  "sd_realized_interaction_share",
  "sd_realized_lin_int_cor",
  
  "sd_rmse_ols_base",
  "sd_rmse_ols_true_interaction",
  "sd_rmse_xgb",
  "sd_delta_rmse_true_vs_base",
  "sd_delta_rmse_xgb_vs_base",
  "sd_delta_rmse_xgb_vs_true",
  "sd_r2_ols_base",
  "sd_r2_ols_true_interaction",
  "sd_r2_xgb",
  "sd_delta_r2_true_vs_base",
  "sd_delta_r2_xgb_vs_base",
  "sd_delta_r2_xgb_vs_true"
)

agg_df <- merge(
  mean_df,
  sd_df,
  by = c("condition_id", "latent_R2", "rho_X", "rho_Y", "comp_linear")
)

write.csv(agg_df, "results_condition_summary.csv", row.names = FALSE)

cat("Expected summary rows:", length(unique(results_df$condition_id)), "\n")
cat("Observed summary rows:", nrow(agg_df), "\n")
cat("Finished. Saved results_condition_summary.csv\n")

print(head(agg_df))
