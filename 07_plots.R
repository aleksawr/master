# ============================================================
# 07_plots.R
# Purpose:
#   Create clean thesis-oriented plots for one selected run
#   and store additional recovery diagnostics in a more
#   organized structure.
# ============================================================

rm(list = ls(all.names = TRUE))
source("run_config.R")

# ------------------------------------------------------------
# 0. Load results and create output folders
# ------------------------------------------------------------
results_df <- read.csv(file.path(run_dir, "results_replication_level.csv"))

plot_dir <- file.path(run_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

diag_dir <- file.path(run_dir, "diagnostics")
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

cat("Loaded replication-level results from:\n", run_dir, "\n")

# ------------------------------------------------------------
# 1. Derived quantities used across plots and diagnostics
# ------------------------------------------------------------
results_df$upper_bound <- results_df$rho_Y * results_df$realized_latent_R2
results_df$ratio <- results_df$r2_ols_true_interaction / results_df$upper_bound

# ------------------------------------------------------------
# 2. Choose one clean slice of the design for thesis plots
# ------------------------------------------------------------
plot_df <- subset(
  results_df,
  latent_R2 == 0.5 & rho_betweenX == 0.0
)

if (nrow(plot_df) == 0) {
  stop("No rows found for latent_R2 = 0.5 and rho_betweenX = 0.0")
}

# ============================================================
# MAIN THESIS FIGURES
# ============================================================

# ------------------------------------------------------------
# 3. Figure 1:
#    Test R2 distribution by model
#    split by rho_X
#    (rho_Y fixed at 0.8 for clarity)
# ------------------------------------------------------------
fig1_df <- subset(plot_df, rho_Y == 0.8)

png(
  filename = file.path(plot_dir, "fig1_test_R2_by_comp_linear_rhoX.png"),
  width = 1800, height = 1400, res = 200
)

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

rhoX_vals <- sort(unique(fig1_df$rho_X))

for (rx in rhoX_vals) {
  sub <- subset(fig1_df, rho_X == rx)
  
  boxplot(
    sub$r2_ols_base,
    sub$r2_ols_true_interaction,
    sub$r2_xgb,
    names = c("OLS", "OLS + int.", "XGB"),
    xlab = "",
    ylab = expression(Test~R^2),
    main = bquote(rho[X] == .(rx)),
    outline = FALSE
  )
  
  ref_line <- mean(sub$realized_latent_R2)
  abline(h = ref_line, lty = 2)
}

mtext("Test R2 distribution by model", outer = FALSE, line = -1.5, cex = 1.1)
dev.off()

# ------------------------------------------------------------
# 4. Figure 2:
#    Delta R2 distribution: XGB - OLS base
#    by comp_linear, split by rho_X
#    (rho_Y fixed at 0.8)
# ------------------------------------------------------------
png(
  filename = file.path(plot_dir, "fig2_delta_R2_xgb_vs_ols_by_comp_linear_rhoX.png"),
  width = 1800, height = 1400, res = 200
)

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

for (rx in rhoX_vals) {
  sub <- subset(fig1_df, rho_X == rx)
  
  comp_vals <- sort(unique(sub$comp_linear))
  box_list <- lapply(comp_vals, function(cl) {
    subset(sub, comp_linear == cl)$delta_r2_xgb_vs_base
  })
  
  boxplot(
    box_list,
    names = comp_vals,
    xlab = "Linear share",
    ylab = expression(Delta*R^2~~"(XGB - OLS)"),
    main = bquote(rho[X] == .(rx)),
    outline = FALSE
  )
  
  abline(h = 0, lty = 2)
}

dev.off()

# ------------------------------------------------------------
# 5. Figure 3:
#    Perfect reliability recovery check
#    rho_X = 1, rho_Y = 1, rho_betweenX = 0
# ------------------------------------------------------------
fig3_df <- subset(
  results_df,
  rho_X == 1.0 & rho_Y == 1.0 & rho_betweenX == 0.0
)

png(
  filename = file.path(plot_dir, "fig3_recovery_perfect_reliability.png"),
  width = 1600, height = 1400, res = 200
)

par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

plot(
  fig3_df$realized_latent_R2,
  fig3_df$r2_ols_true_interaction,
  pch = 19,
  xlab = expression(Realized~latent~R^2),
  ylab = expression(Test~R^2~~"(OLS + int.)"),
  main = "Recovery under perfect reliability"
)

abline(0, 1, lty = 2)

dev.off()

# ------------------------------------------------------------
# 6. Figure 4:
#    Test R2 distribution by rho_Y, split by comp_linear
#    (latent_R2 = 0.5, rho_betweenX = 0.0, rho_X = 0.8)
# ------------------------------------------------------------
fig4_df <- subset(plot_df, rho_X == 0.8)

png(
  filename = file.path(plot_dir, "fig4_test_R2_by_rhoY_comp_linear.png"),
  width = 1800, height = 1400, res = 200
)

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))

comp_vals <- sort(unique(fig4_df$comp_linear))

for (cl in comp_vals) {
  sub <- subset(fig4_df, comp_linear == cl)
  
  box_list <- list(
    subset(sub, rho_Y == 0.6)$r2_ols_true_interaction,
    subset(sub, rho_Y == 0.8)$r2_ols_true_interaction,
    subset(sub, rho_Y == 1.0)$r2_ols_true_interaction
  )
  
  boxplot(
    box_list,
    names = c("0.6", "0.8", "1.0"),
    xlab = expression(rho[Y]),
    ylab = expression(Test~R^2~~"(OLS + int.)"),
    main = paste("Linear share =", cl),
    outline = FALSE
  )
}

dev.off()

# ============================================================
# RECOVERY DIAGNOSTIC PLOTS
# ============================================================

# ------------------------------------------------------------
# 7. Recovery ratio by rho_Y
# ------------------------------------------------------------
png(
  file.path(plot_dir, "upper_bound_ratio.png"),
  width = 800, height = 600
)

par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

boxplot(
  ratio ~ rho_Y,
  data = results_df,
  ylab = "Observed / theoretical maximum",
  xlab = expression(rho[Y]),
  main = "Recovery relative to theoretical bound"
)

abline(h = 1, lty = 2)

dev.off()

# ------------------------------------------------------------
# 8. Observed performance vs theoretical bound
# ------------------------------------------------------------
png(
  file.path(plot_dir, "observed_vs_upper_bound_scatter.png"),
  width = 800, height = 600
)

par(mfrow = c(1, 1), mar = c(4, 4, 2, 1))

plot(
  jitter(results_df$upper_bound, amount = 0.005),
  results_df$r2_ols_true_interaction,
  pch = 19,
  cex = 0.6,
  xlab = "Theoretical maximum",
  ylab = expression("Observed test " * R^2),
  main = "Observed performance vs theoretical bound"
)

abline(0, 1, lty = 2)

dev.off()

# ============================================================
# BASIC NUMERIC DIAGNOSTICS
# ============================================================

# ------------------------------------------------------------
# 9. Quick summaries
# ------------------------------------------------------------
summary(results_df$upper_bound)
summary(results_df$r2_ols_true_interaction)

aggregate(ratio ~ rho_X, data = results_df, mean)
aggregate(ratio ~ rho_Y, data = results_df, mean)
aggregate(ratio ~ comp_linear, data = results_df, mean)

# ============================================================
# REPLICATION-LEVEL RECOVERY CHECKS
# ============================================================

# ------------------------------------------------------------
# 10. Best and worst replications by recovery ratio
# ------------------------------------------------------------
top_rep <- results_df[order(-results_df$ratio), ]
bottom_rep <- results_df[order(results_df$ratio), ]

head(top_rep[, c(
  "condition_id", "rep",
  "latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX",
  "realized_latent_R2", "upper_bound", "r2_ols_true_interaction", "ratio"
)], 10)

head(bottom_rep[, c(
  "condition_id", "rep",
  "latent_R2", "rho_X", "rho_Y", "comp_linear", "rho_betweenX",
  "realized_latent_R2", "upper_bound", "r2_ols_true_interaction", "ratio"
)], 10)

# ============================================================
# CONDITION-LEVEL RECOVERY SUMMARIES
# ============================================================

# ------------------------------------------------------------
# 11. Mean recovery by condition
# ------------------------------------------------------------
cond_recovery <- aggregate(
  cbind(ratio, r2_ols_true_interaction, upper_bound) ~
    condition_id + latent_R2 + rho_X + rho_Y + comp_linear + rho_betweenX,
  data = results_df,
  FUN = mean
)

names(cond_recovery)[names(cond_recovery) == "ratio"] <- "mean_ratio"
names(cond_recovery)[names(cond_recovery) == "r2_ols_true_interaction"] <- "mean_r2_true"
names(cond_recovery)[names(cond_recovery) == "upper_bound"] <- "mean_upper_bound"

# ------------------------------------------------------------
# 12. Best and worst conditions by mean recovery
# ------------------------------------------------------------
top_cond <- cond_recovery[order(-cond_recovery$mean_ratio), ]
bottom_cond <- cond_recovery[order(cond_recovery$mean_ratio), ]

head(top_cond, 10)
head(bottom_cond, 10)

# ------------------------------------------------------------
# 13. Add SD of recovery ratio within condition
# ------------------------------------------------------------
cond_recovery_sd <- aggregate(
  ratio ~ condition_id,
  data = results_df,
  FUN = sd
)

names(cond_recovery_sd)[2] <- "sd_ratio"

cond_recovery_full <- merge(cond_recovery, cond_recovery_sd, by = "condition_id")

top_cond_full <- cond_recovery_full[order(-cond_recovery_full$mean_ratio), ]
bottom_cond_full <- cond_recovery_full[order(cond_recovery_full$mean_ratio), ]

head(top_cond_full, 10)
head(bottom_cond_full, 10)

# ------------------------------------------------------------
# 14. Marginal condition-level summaries
# ------------------------------------------------------------
aggregate(mean_ratio ~ rho_X, data = cond_recovery, mean)
aggregate(mean_ratio ~ rho_Y, data = cond_recovery, mean)
aggregate(mean_ratio ~ comp_linear, data = cond_recovery, mean)
aggregate(mean_ratio ~ rho_betweenX, data = cond_recovery, mean)
aggregate(mean_ratio ~ latent_R2, data = cond_recovery, mean)

# ============================================================
# EXPORT DIAGNOSTIC TABLES
# ============================================================

# ------------------------------------------------------------
# 15. Save recovery summaries
# ------------------------------------------------------------
write.csv(
  cond_recovery_full,
  file.path(diag_dir, "condition_recovery_summary.csv"),
  row.names = FALSE
)

write.csv(
  head(top_cond_full, 20),
  file.path(diag_dir, "top20_conditions_by_recovery.csv"),
  row.names = FALSE
)

write.csv(
  head(bottom_cond_full, 20),
  file.path(diag_dir, "bottom20_conditions_by_recovery.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 16. Final message
# ------------------------------------------------------------
cat("Plots saved to:\n", plot_dir, "\n")
cat("Diagnostics saved to:\n", diag_dir, "\n")

