# ============================================================
# 06_diagnostics.R
# Purpose:
#   Diagnostics for both performance and realized condition values
# ============================================================

rm(list = ls(all.names = TRUE))

results_df <- read.csv("results_replication_level.csv")
agg_df <- read.csv("results_condition_summary.csv")

cat("\n==============================\n")
cat("BASIC STRUCTURE CHECK\n")
cat("==============================\n")

str(results_df)
cat("\nRows:", nrow(results_df), "\n")
cat("Unique conditions:", length(unique(results_df$condition_id)), "\n")

cat("\n==============================\n")
cat("REALIZED CONDITION CHECKS\n")
cat("==============================\n")

cat("\nTarget vs realized latent R2:\n")
print(aggregate(realized_latent_R2 ~ latent_R2, data = results_df, mean))

cat("\nTarget vs realized rho_X:\n")
print(aggregate(realized_rho_X ~ rho_X, data = results_df, mean))

cat("\nTarget vs realized rho_Y:\n")
print(aggregate(realized_rho_Y ~ rho_Y, data = results_df, mean))

cat("\nTarget vs realized linear share:\n")
print(aggregate(realized_linear_share ~ comp_linear, data = results_df, mean))

cat("\nTarget vs realized interaction share:\n")
print(aggregate(realized_interaction_share ~ comp_linear, data = results_df, mean))

cat("\nMean correlation between linear and interaction components:\n")
print(aggregate(realized_lin_int_cor ~ comp_linear, data = results_df, mean))

cat("\n==============================\n")
cat("PERFORMANCE PATTERNS\n")
cat("==============================\n")

print(aggregate(cbind(r2_ols_base, r2_ols_true_interaction, r2_xgb) ~ latent_R2,
                data = results_df, mean))

print(aggregate(cbind(r2_ols_base, r2_ols_true_interaction, r2_xgb) ~ rho_X,
                data = results_df, mean))

print(aggregate(cbind(r2_ols_base, r2_ols_true_interaction, r2_xgb) ~ rho_Y,
                data = results_df, mean))

print(aggregate(cbind(delta_r2_true_vs_base, delta_r2_xgb_vs_base, delta_r2_xgb_vs_true) ~ comp_linear,
                data = results_df, mean))

cat("\n==============================\n")
cat("RECOVERY CHECK: PERFECT RELIABILITY\n")
cat("==============================\n")

perfect_df <- subset(results_df, rho_X == 1 & rho_Y == 1)

if (nrow(perfect_df) > 0) {
  recovery_tab <- aggregate(
    cbind(realized_latent_R2, r2_ols_base, r2_ols_true_interaction, r2_xgb) ~ latent_R2 + comp_linear,
    data = perfect_df,
    mean
  )
  print(recovery_tab)
  
  recovery_tab$gap_true_model_vs_realized_latent <-
    recovery_tab$realized_latent_R2 - recovery_tab$r2_ols_true_interaction
  
  cat("\nGap between realized latent R2 and correctly specified model:\n")
  print(recovery_tab[, c("latent_R2", "comp_linear", "gap_true_model_vs_realized_latent")])
}

cat("\n==============================\n")
cat("PLOTS\n")
cat("==============================\n")

plot_df <- aggregate(
  cbind(realized_latent_R2, r2_ols_true_interaction) ~ latent_R2 + rho_X + rho_Y,
  data = results_df,
  mean
)

plot(plot_df$realized_latent_R2, plot_df$r2_ols_true_interaction,
     xlab = "Mean realized latent R2",
     ylab = "Mean observed test R2 (correctly specified model)",
     main = "Recovery of realized latent signal",
     pch = 19)
abline(0, 1, lty = 2)

boxplot(realized_linear_share ~ comp_linear,
        data = results_df,
        xlab = "Target comp_linear",
        ylab = "Realized linear share",
        main = "Target vs realized linear share")

boxplot(delta_r2_xgb_vs_base ~ comp_linear,
        data = results_df,
        xlab = "comp_linear",
        ylab = "delta_r2_xgb_vs_base",
        main = "XGB advantage over OLS base")

cat("\n==============================\n")
cat("DIAGNOSTICS COMPLETE\n")
cat("==============================\n")
