# ============================================================
# 07_select_key_conditions.R
# Purpose:
#   Identify the most informative conditions for main-text boxplots
#   based on replication-level results.
# ============================================================

rm(list = ls(all.names = TRUE))
source("run_config.R")

library(dplyr)

# -----------------------------
# 1. Load data
# -----------------------------
results_df <- read.csv(file.path(run_dir, "results_replication_level.csv"))

out_dir <- file.path(run_dir, "selected_conditions")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 2. Summarize at condition level
# -----------------------------
cond_summary <- results_df %>%
  group_by(latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX) %>%
  summarise(
    mean_r2_base    = mean(r2_ols_base, na.rm = TRUE),
    mean_r2_aligned = mean(r2_ols_true_interaction, na.rm = TRUE),
    mean_r2_oracle  = mean(r2_ols_oracle, na.rm = TRUE),
    mean_r2_xgb     = mean(r2_xgb, na.rm = TRUE),
    
    mean_rmse_base    = mean(rmse_ols_base, na.rm = TRUE),
    mean_rmse_aligned = mean(rmse_ols_true_interaction, na.rm = TRUE),
    mean_rmse_oracle  = mean(rmse_ols_oracle, na.rm = TRUE),
    mean_rmse_xgb     = mean(rmse_xgb, na.rm = TRUE),
    
    mean_delta_r2_xgb_vs_base   = mean(delta_r2_xgb_vs_base, na.rm = TRUE),
    mean_delta_r2_xgb_vs_true   = mean(delta_r2_xgb_vs_true, na.rm = TRUE),
    mean_delta_r2_xgb_vs_oracle = mean(delta_r2_xgb_vs_oracle, na.rm = TRUE),
    
    mean_delta_rmse_xgb_vs_base   = mean(delta_rmse_xgb_vs_base, na.rm = TRUE),
    mean_delta_rmse_xgb_vs_true   = mean(delta_rmse_xgb_vs_true, na.rm = TRUE),
    mean_delta_rmse_xgb_vs_oracle = mean(delta_rmse_xgb_vs_oracle, na.rm = TRUE),
    
    sd_delta_r2_xgb_vs_base   = sd(delta_r2_xgb_vs_base, na.rm = TRUE),
    sd_delta_r2_xgb_vs_true   = sd(delta_r2_xgb_vs_true, na.rm = TRUE),
    sd_delta_r2_xgb_vs_oracle = sd(delta_r2_xgb_vs_oracle, na.rm = TRUE),
    
    model_spread_r2 =
      max(c(
        mean(r2_ols_base, na.rm = TRUE),
        mean(r2_ols_true_interaction, na.rm = TRUE),
        mean(r2_ols_oracle, na.rm = TRUE),
        mean(r2_xgb, na.rm = TRUE)
      )) -
      min(c(
        mean(r2_ols_base, na.rm = TRUE),
        mean(r2_ols_true_interaction, na.rm = TRUE),
        mean(r2_ols_oracle, na.rm = TRUE),
        mean(r2_xgb, na.rm = TRUE)
      )),
    
    .groups = "drop"
  )

# -----------------------------
# 3. Ranking scores
# -----------------------------
cond_summary <- cond_summary %>%
  mutate(
    score_practical_gain =
      mean_delta_r2_xgb_vs_base,
    
    score_aligned_beats_xgb =
      -mean_delta_r2_xgb_vs_true,
    
    score_compression =
      -model_spread_r2,
    
    score_structure_visible =
      mean_delta_r2_xgb_vs_base - abs(mean_delta_r2_xgb_vs_true),
    
    score_stable_gain =
      mean_delta_r2_xgb_vs_base / (sd_delta_r2_xgb_vs_base + 1e-6)
  )

# -----------------------------
# 4. Ranked tables
# -----------------------------
top_practical_gain <- cond_summary %>%
  arrange(desc(score_practical_gain)) %>%
  slice(1:10)

top_aligned_beats_xgb <- cond_summary %>%
  arrange(desc(score_aligned_beats_xgb)) %>%
  slice(1:10)

top_compression <- cond_summary %>%
  arrange(desc(score_compression)) %>%
  slice(1:10)

top_structure_visible <- cond_summary %>%
  arrange(desc(score_structure_visible)) %>%
  slice(1:10)

top_stable_gain <- cond_summary %>%
  arrange(desc(score_stable_gain)) %>%
  slice(1:10)

# -----------------------------
# 5. Save ranked tables
# -----------------------------
write.csv(cond_summary,
          file.path(out_dir, "condition_summary_ranked.csv"),
          row.names = FALSE)

write.csv(top_practical_gain,
          file.path(out_dir, "top_practical_gain.csv"),
          row.names = FALSE)

write.csv(top_aligned_beats_xgb,
          file.path(out_dir, "top_aligned_beats_xgb.csv"),
          row.names = FALSE)

write.csv(top_compression,
          file.path(out_dir, "top_compression.csv"),
          row.names = FALSE)

write.csv(top_structure_visible,
          file.path(out_dir, "top_structure_visible.csv"),
          row.names = FALSE)

write.csv(top_stable_gain,
          file.path(out_dir, "top_stable_gain.csv"),
          row.names = FALSE)

# -----------------------------
# 6. Print previews
# -----------------------------
cat("\nTop conditions: XGB improves most over Baseline OLS\n")
print(top_practical_gain)

cat("\nTop conditions: Aligned OLS beats / catches XGB most clearly\n")
print(top_aligned_beats_xgb)

cat("\nTop conditions: strongest model convergence / compression\n")
print(top_compression)

cat("\nTop conditions: structural dependence most visible\n")
print(top_structure_visible)

cat("\nTop conditions: stable XGB gain over baseline\n")
print(top_stable_gain)

# -----------------------------
# 7. Compact exports
# -----------------------------
selection_summary <- cond_summary %>%
  select(
    latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX,
    mean_r2_base, mean_r2_aligned, mean_r2_oracle, mean_r2_xgb,
    mean_delta_r2_xgb_vs_base,
    mean_delta_r2_xgb_vs_true,
    mean_delta_r2_xgb_vs_oracle,
    model_spread_r2,
    score_practical_gain,
    score_aligned_beats_xgb,
    score_compression,
    score_structure_visible,
    score_stable_gain
  )

write.csv(
  selection_summary,
  file.path(out_dir, "condition_summary_selection_compact.csv"),
  row.names = FALSE
)

candidate_pool <- bind_rows(
  top_practical_gain,
  top_aligned_beats_xgb,
  top_compression,
  top_structure_visible,
  top_stable_gain
) %>%
  distinct(latent_R2, rho_X, rho_Y, comp_linear, rho_betweenX, .keep_all = TRUE) %>%
  arrange(desc(score_practical_gain), desc(score_structure_visible))

write.csv(
  candidate_pool,
  file.path(out_dir, "candidate_pool_deduplicated.csv"),
  row.names = FALSE
)

cat("\nSaved ranked condition tables to:\n")
cat(out_dir, "\n")

cat("\nCompact selection summary saved to:\n")
cat(file.path(out_dir, "condition_summary_selection_compact.csv"), "\n")

cat("\nDeduplicated candidate pool saved to:\n")
cat(file.path(out_dir, "candidate_pool_deduplicated.csv"), "\n")
