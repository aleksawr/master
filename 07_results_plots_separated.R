# ============================================================
# 07_results_plots_separated.R
# Purpose:
#   Results plots with separated conditions
#   Base R only
#   ASCII-safe labels + improved readability
# ============================================================

rm(list = ls(all.names = TRUE))
source("run_config.R")

# -----------------------------
# 1. Load data
# -----------------------------
agg_df <- read.csv(file.path(run_dir, "results_condition_summary.csv"))
results_df <- read.csv(file.path(run_dir, "results_replication_level.csv"))

plot_dir <- file.path(run_dir, "results_plots_separated")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------
# 2. Levels
# -----------------------------
latent_levels <- sort(unique(agg_df$latent_R2))
rhoX_levels   <- sort(unique(agg_df$rho_X))
rhoY_levels   <- sort(unique(agg_df$rho_Y))
comp_levels   <- sort(unique(agg_df$comp_linear))
rhoB_levels   <- sort(unique(agg_df$rho_betweenX))

# -----------------------------
# 3. Manual y-limits
#    CHANGE THESE IF NEEDED
# -----------------------------
ylim_box_deltaR2    <- c(-0.10, 0.70)
ylim_box_deltaRMSE  <- c(-0.50, 0.10)

ylim_deltaR2_panel   <- c(-0.50, 0.50)
ylim_deltaRMSE_panel <- c(-0.30, 0.30)

ylim_R2_panel   <- c(0.00, 0.90)
ylim_RMSE_panel <- c(0.50, 3.00)

# -----------------------------
# 4. Plot styling
# -----------------------------
model_cols <- c("#000000", "#0072B2", "#D55E00", "#009E73")
model_pch  <- c(16, 17, 15, 18)
model_lty  <- c(1, 2, 3, 4)
model_lwd  <- c(3, 3, 3, 3)
model_cex  <- 1.2
# -----------------------------
# 5. Helper:
#    generic line + errorbar plot
# -----------------------------
draw_lines_with_errors <- function(x, y_mat, sd_mat, labels,
                                   ylim_use, xlab, ylab, main,
                                   add_zero_line = FALSE,
                                   legend_here = FALSE) {
  
  plot(
    x, y_mat[, 1],
    type = "b",
    pch = model_pch[1],
    lty = model_lty[1],
    lwd = model_lwd[1],
    col = model_cols[1],
    cex = model_cex,
    ylim = ylim_use,
    xlab = xlab,
    ylab = ylab,
    main = main
  )
  
  if (add_zero_line) abline(h = 0, lty = 2, col = "gray50")
  
  arrows(x, y_mat[, 1] - sd_mat[, 1], x, y_mat[, 1] + sd_mat[, 1],
         angle = 90, code = 3, length = 0.05, col = model_cols[1])
  
  if (ncol(y_mat) >= 2) {
    lines(x, y_mat[, 2], type = "b",
          pch = model_pch[2], lty = model_lty[2], lwd = model_lwd[2],
          col = model_cols[2], cex = model_cex)
    arrows(x, y_mat[, 2] - sd_mat[, 2], x, y_mat[, 2] + sd_mat[, 2],
           angle = 90, code = 3, length = 0.05, col = model_cols[2])
  }
  
  if (ncol(y_mat) >= 3) {
    lines(x, y_mat[, 3], type = "b",
          pch = model_pch[3], lty = model_lty[3], lwd = model_lwd[3],
          col = model_cols[3], cex = model_cex)
    arrows(x, y_mat[, 3] - sd_mat[, 3], x, y_mat[, 3] + sd_mat[, 3],
           angle = 90, code = 3, length = 0.05, col = model_cols[3])
  }
  
  if (ncol(y_mat) >= 4) {
    lines(x, y_mat[, 4], type = "b",
          pch = model_pch[4], lty = model_lty[4], lwd = model_lwd[4],
          col = model_cols[4], cex = model_cex)
    arrows(x, y_mat[, 4] - sd_mat[, 4], x, y_mat[, 4] + sd_mat[, 4],
           angle = 90, code = 3, length = 0.05, col = model_cols[4])
  }
  
  if (legend_here) {
    legend(
      "topleft",
      legend = labels,
      pch = model_pch[seq_along(labels)],
      lty = model_lty[seq_along(labels)],
      lwd = model_lwd[seq_along(labels)],
      col = model_cols[seq_along(labels)],
      bty = "n",
      cex = 0.85
    )
  }
}

# -----------------------------
# 6. Function 1:
#    panel R2 by rho_X
# -----------------------------
plot_panel_r2 <- function(sub, file_name, main_title) {
  
  png(file.path(plot_dir, file_name), width = 1400, height = 1000)
  
  par(mfrow = c(length(rhoY_levels), length(comp_levels)),
      mar = c(4, 4, 3, 1),
      oma = c(0, 0, 3, 0))
  
  for (ry in rhoY_levels) {
    for (cl in comp_levels) {
      
      d <- sub[sub$rho_Y == ry & sub$comp_linear == cl, ]
      d <- d[order(d$rho_X), ]
      
      if (nrow(d) == 0) {
        plot.new()
        title(main = paste("Outcome rel. =", ry, "| Linear share =", cl))
        next
      }
      
      x <- d$rho_X
      y_mat <- as.matrix(d[, c("mean_r2_ols_base",
                               "mean_r2_ols_true_interaction",
                               "mean_r2_ols_oracle",
                               "mean_r2_xgb"), drop = FALSE])
      sd_mat <- as.matrix(d[, c("sd_r2_ols_base",
                                "sd_r2_ols_true_interaction",
                                "sd_r2_ols_oracle",
                                "sd_r2_xgb"), drop = FALSE])
      
      draw_lines_with_errors(
        x = x,
        y_mat = y_mat,
        sd_mat = sd_mat,
        labels = c("Baseline OLS", "Aligned OLS", "Oracle OLS", "XGBoost"),
        ylim_use = ylim_R2_panel,
        xlab = "rho_X",
        ylab = "Mean test-set R2",
        main = paste0("rho_Y = ", ry, ", linear share = ", cl),
        add_zero_line = FALSE,
        legend_here = (ry == rhoY_levels[1] && cl == comp_levels[1])
      )
    }
  }
  
  mtext(main_title, outer = TRUE, side = 3, line = 1, cex = 1.2)
  dev.off()
}

# -----------------------------
# 7. Function 2:
#    panel RMSE by rho_X
# -----------------------------
plot_panel_rmse <- function(sub, file_name, main_title) {
  
  png(file.path(plot_dir, file_name), width = 1400, height = 1000)
  
  par(mfrow = c(length(rhoY_levels), length(comp_levels)),
      mar = c(4, 4, 3, 1),
      oma = c(0, 0, 3, 0))
  
  for (ry in rhoY_levels) {
    for (cl in comp_levels) {
      
      d <- sub[sub$rho_Y == ry & sub$comp_linear == cl, ]
      d <- d[order(d$rho_X), ]
      
      if (nrow(d) == 0) {
        plot.new()
        title(main = paste("Outcome rel. =", ry, "| Linear share =", cl))
        next
      }
      
      x <- d$rho_X
      y_mat <- as.matrix(d[, c("mean_rmse_ols_base",
                               "mean_rmse_ols_true_interaction",
                               "mean_rmse_ols_oracle",
                               "mean_rmse_xgb"), drop = FALSE])
      sd_mat <- as.matrix(d[, c("sd_rmse_ols_base",
                                "sd_rmse_ols_true_interaction",
                                "sd_rmse_ols_oracle",
                                "sd_rmse_xgb"), drop = FALSE])
      
      draw_lines_with_errors(
        x = x,
        y_mat = y_mat,
        sd_mat = sd_mat,
        labels = c("Baseline OLS", "Aligned OLS", "Oracle OLS", "XGBoost"),
        ylim_use = ylim_RMSE_panel,
        xlab = "rho_X",
        ylab = "Mean test-set RMSE",
        main = paste0("rho_Y = ", ry, ", linear share = ", cl),
        add_zero_line = FALSE,
        legend_here = (ry == rhoY_levels[1] && cl == comp_levels[1])
      )
    }
  }
  
  mtext(main_title, outer = TRUE, side = 3, line = 1, cex = 1.2)
  dev.off()
}

# -----------------------------
# 8. Function 3:
#    panel delta R2 by rho_X
# -----------------------------
plot_panel_delta_r2 <- function(sub, file_name, main_title) {
  
  png(file.path(plot_dir, file_name), width = 1400, height = 1000)
  
  par(mfrow = c(length(rhoY_levels), length(comp_levels)),
      mar = c(4, 4, 3, 1),
      oma = c(0, 0, 3, 0))
  
  contrast_cols <- c("#009E73", "#0072B2", "#D55E00")
  contrast_pch  <- c(16, 17, 15)
  contrast_lty  <- c(1, 2, 3)
  
  for (ry in rhoY_levels) {
    for (cl in comp_levels) {
      
      d <- sub[sub$rho_Y == ry & sub$comp_linear == cl, ]
      d <- d[order(d$rho_X), ]
      
      if (nrow(d) == 0) {
        plot.new()
        title(main = paste("Outcome rel. =", ry, "| Linear share =", cl))
        next
      }
      
      x <- d$rho_X
      y_mat <- as.matrix(d[, c("mean_delta_r2_xgb_vs_base",
                               "mean_delta_r2_xgb_vs_true",
                               "mean_delta_r2_xgb_vs_oracle"), drop = FALSE])
      sd_mat <- as.matrix(d[, c("sd_delta_r2_xgb_vs_base",
                                "sd_delta_r2_xgb_vs_true",
                                "sd_delta_r2_xgb_vs_oracle"), drop = FALSE])
      
      plot(
        x, y_mat[, 1],
        type = "b",
        pch = contrast_pch[1],
        lty = contrast_lty[1],
        lwd = 3,
        col = contrast_cols[1],
        cex = 1.1,
        ylim = ylim_deltaR2_panel,
        xlab = "rho_X",
        ylab = "Mean test-set delta R2",
        main = paste0("rho_Y = ", ry, ", linear share = ", cl)
      )
      abline(h = 0, lty = 2, col = "gray50")
      arrows(x, y_mat[, 1] - sd_mat[, 1], x, y_mat[, 1] + sd_mat[, 1],
             angle = 90, code = 3, length = 0.05, col = contrast_cols[1])
      
      lines(x, y_mat[, 2], type = "b",
            pch = contrast_pch[2], lty = contrast_lty[2], lwd = 3,
            col = contrast_cols[2], cex = 1.1)
      arrows(x, y_mat[, 2] - sd_mat[, 2], x, y_mat[, 2] + sd_mat[, 2],
             angle = 90, code = 3, length = 0.05, col = contrast_cols[2])
      
      lines(x, y_mat[, 3], type = "b",
            pch = contrast_pch[3], lty = contrast_lty[3], lwd = 3,
            col = contrast_cols[3], cex = 1.1)
      arrows(x, y_mat[, 3] - sd_mat[, 3], x, y_mat[, 3] + sd_mat[, 3],
             angle = 90, code = 3, length = 0.05, col = contrast_cols[3])
      
      if (ry == rhoY_levels[1] && cl == comp_levels[1]) {
        legend(
          "topleft",
          legend = c("XGBoost - Baseline OLS",
                     "XGBoost - Aligned OLS",
                     "XGBoost - Oracle OLS"),
          pch = contrast_pch,
          lty = contrast_lty,
          lwd = 3,
          col = contrast_cols,
          bty = "n",
          cex = 0.8
        )
      }
    }
  }
  
  mtext(main_title, outer = TRUE, side = 3, line = 1, cex = 1.2)
  dev.off()
}

# -----------------------------
# 9. Function 4:
#    panel delta RMSE by rho_X
# -----------------------------
plot_panel_delta_rmse <- function(sub, file_name, main_title) {
  
  png(file.path(plot_dir, file_name), width = 1400, height = 1000)
  
  par(mfrow = c(length(rhoY_levels), length(comp_levels)),
      mar = c(4, 4, 3, 1),
      oma = c(0, 0, 3, 0))
  
  contrast_cols <- c("#009E73", "#0072B2", "#D55E00")
  contrast_pch  <- c(16, 17, 15)
  contrast_lty  <- c(1, 2, 3)
  
  for (ry in rhoY_levels) {
    for (cl in comp_levels) {
      
      d <- sub[sub$rho_Y == ry & sub$comp_linear == cl, ]
      d <- d[order(d$rho_X), ]
      
      if (nrow(d) == 0) {
        plot.new()
        title(main = paste("Outcome rel. =", ry, "| Linear share =", cl))
        next
      }
      
      x <- d$rho_X
      y_mat <- as.matrix(d[, c("mean_delta_rmse_xgb_vs_base",
                               "mean_delta_rmse_xgb_vs_true",
                               "mean_delta_rmse_xgb_vs_oracle"), drop = FALSE])
      sd_mat <- as.matrix(d[, c("sd_delta_rmse_xgb_vs_base",
                                "sd_delta_rmse_xgb_vs_true",
                                "sd_delta_rmse_xgb_vs_oracle"), drop = FALSE])
      
      plot(
        x, y_mat[, 1],
        type = "b",
        pch = contrast_pch[1],
        lty = contrast_lty[1],
        lwd = 3,
        col = contrast_cols[1],
        cex = 1.1,
        ylim = ylim_deltaRMSE_panel,
        xlab = "rho_X",
        ylab = "Mean test-set delta RMSE",
        main = paste0("rho_Y = ", ry, ", linear share = ", cl)
      )
      abline(h = 0, lty = 2, col = "gray50")
      arrows(x, y_mat[, 1] - sd_mat[, 1], x, y_mat[, 1] + sd_mat[, 1],
             angle = 90, code = 3, length = 0.05, col = contrast_cols[1])
      
      lines(x, y_mat[, 2], type = "b",
            pch = contrast_pch[2], lty = contrast_lty[2], lwd = 3,
            col = contrast_cols[2], cex = 1.1)
      arrows(x, y_mat[, 2] - sd_mat[, 2], x, y_mat[, 2] + sd_mat[, 2],
             angle = 90, code = 3, length = 0.05, col = contrast_cols[2])
      
      lines(x, y_mat[, 3], type = "b",
            pch = contrast_pch[3], lty = contrast_lty[3], lwd = 3,
            col = contrast_cols[3], cex = 1.1)
      arrows(x, y_mat[, 3] - sd_mat[, 3], x, y_mat[, 3] + sd_mat[, 3],
             angle = 90, code = 3, length = 0.05, col = contrast_cols[3])
      
      if (ry == rhoY_levels[1] && cl == comp_levels[1]) {
        legend(
          "topleft",
          legend = c("XGBoost - Baseline OLS",
                     "XGBoost - Aligned OLS",
                     "XGBoost - Oracle OLS"),
          pch = contrast_pch,
          lty = contrast_lty,
          lwd = 3,
          col = contrast_cols,
          bty = "n",
          cex = 0.8
        )
      }
    }
  }
  
  mtext(main_title, outer = TRUE, side = 3, line = 1, cex = 1.2)
  dev.off()
}

# -----------------------------
# 10. Function 5:
#     replication-level boxplots delta R2
# -----------------------------
plot_box_delta_r2 <- function(sub, file_name, main_title) {
  
  png(file.path(plot_dir, file_name), width = 1400, height = 1000)
  
  par(mfrow = c(length(rhoY_levels), length(comp_levels)),
      mar = c(7, 4, 3, 1),
      oma = c(0, 0, 3, 0))
  
  for (ry in rhoY_levels) {
    for (cl in comp_levels) {
      
      d <- sub[sub$rho_Y == ry & sub$comp_linear == cl, ]
      
      if (nrow(d) == 0) {
        plot.new()
        title(main = paste("Outcome rel. =", ry, "| Linear share =", cl))
        next
      }
      
      grp <- factor(d$rho_X, levels = rhoX_levels)
      
      boxplot(
        d$delta_r2_xgb_vs_base ~ grp,
        outline = FALSE,
        las = 2,
        ylim = ylim_box_deltaR2,
        xlab = "rho_X",
        ylab = "Replication-level test-set delta R2",
        main = paste0("rho_Y = ", ry, ", linear share = ", cl),
        col = "gray85",
        border = "black"
      )
      abline(h = 0, lty = 2, col = "gray50")
    }
  }
  
  mtext(main_title, outer = TRUE, side = 3, line = 1, cex = 1.2)
  dev.off()
}

# -----------------------------
# 11. Function 6:
#     replication-level boxplots delta RMSE
# -----------------------------
plot_box_delta_rmse <- function(sub, file_name, main_title) {
  
  png(file.path(plot_dir, file_name), width = 1400, height = 1000)
  
  par(mfrow = c(length(rhoY_levels), length(comp_levels)),
      mar = c(7, 4, 3, 1),
      oma = c(0, 0, 3, 0))
  
  for (ry in rhoY_levels) {
    for (cl in comp_levels) {
      
      d <- sub[sub$rho_Y == ry & sub$comp_linear == cl, ]
      
      if (nrow(d) == 0) {
        plot.new()
        title(main = paste("Outcome rel. =", ry, "| Linear share =", cl))
        next
      }
      
      grp <- factor(d$rho_X, levels = rhoX_levels)
      
      boxplot(
        d$delta_rmse_xgb_vs_base ~ grp,
        outline = FALSE,
        las = 2,
        ylim = ylim_box_deltaRMSE,
        xlab = "rho_X",
        ylab = "Replication-level test-set delta RMSE",
        main = paste0("rho_Y = ", ry, ", linear share = ", cl),
        col = "gray85",
        border = "black"
      )
      abline(h = 0, lty = 2, col = "gray50")
    }
  }
  
  mtext(main_title, outer = TRUE, side = 3, line = 1, cex = 1.2)
  dev.off()
}

# -----------------------------
# 12. Make R2 panel plots
# -----------------------------
for (lr in latent_levels) {
  for (rb in rhoB_levels) {
    sub <- agg_df[agg_df$latent_R2 == lr & agg_df$rho_betweenX == rb, ]
    
    plot_panel_r2(
      sub = sub,
      file_name = paste0("panel_R2_by_rhoX_latent", lr, "_rhoB", rb, ".png"),
      main_title = paste0(
        "Mean test-set R2 by predictor reliability | latent_R2 = ",
        lr, ", predictor intercorrelation = ", rb
      )
    )
  }
}

# -----------------------------
# 13. Make RMSE panel plots
# -----------------------------
for (lr in latent_levels) {
  for (rb in rhoB_levels) {
    sub <- agg_df[agg_df$latent_R2 == lr & agg_df$rho_betweenX == rb, ]
    
    plot_panel_rmse(
      sub = sub,
      file_name = paste0("panel_RMSE_by_rhoX_latent", lr, "_rhoB", rb, ".png"),
      main_title = paste0(
        "Mean test-set RMSE by predictor reliability | latent_R2 = ",
        lr, ", predictor intercorrelation = ", rb
      )
    )
  }
}

# -----------------------------
# 14. Make delta R2 contrast plots
# -----------------------------
for (lr in latent_levels) {
  for (rb in rhoB_levels) {
    sub <- agg_df[agg_df$latent_R2 == lr & agg_df$rho_betweenX == rb, ]
    
    plot_panel_delta_r2(
      sub = sub,
      file_name = paste0("panel_deltaR2_xgb_by_rhoX_latent", lr, "_rhoB", rb, ".png"),
      main_title = paste0(
        "Mean test-set delta R2 for XGBoost comparisons | latent_R2 = ",
        lr, ", predictor intercorrelation = ", rb
      )
    )
  }
}

# -----------------------------
# 15. Make delta RMSE contrast plots
# -----------------------------
for (lr in latent_levels) {
  for (rb in rhoB_levels) {
    sub <- agg_df[agg_df$latent_R2 == lr & agg_df$rho_betweenX == rb, ]
    
    plot_panel_delta_rmse(
      sub = sub,
      file_name = paste0("panel_deltaRMSE_xgb_by_rhoX_latent", lr, "_rhoB", rb, ".png"),
      main_title = paste0(
        "Mean test-set delta RMSE for XGBoost comparisons | latent_R2 = ",
        lr, ", predictor intercorrelation = ", rb
      )
    )
  }
}

# -----------------------------
# 16. Make replication-level delta R2 boxplots
# -----------------------------
for (lr in latent_levels) {
  for (rb in rhoB_levels) {
    sub <- results_df[
      results_df$latent_R2 == lr &
        results_df$rho_betweenX == rb,
    ]
    
    plot_box_delta_r2(
      sub = sub,
      file_name = paste0("boxplot_replication_deltaR2_xgb_vs_base_latent", lr, "_rhoB", rb, ".png"),
      main_title = paste0(
        "Replication-level test-set delta R2: XGBoost vs baseline OLS | latent_R2 = ",
        lr, ", predictor intercorrelation = ", rb
      )
    )
  }
}

# -----------------------------
# 17. Make replication-level delta RMSE boxplots
# -----------------------------
for (lr in latent_levels) {
  for (rb in rhoB_levels) {
    sub <- results_df[
      results_df$latent_R2 == lr &
        results_df$rho_betweenX == rb,
    ]
    
    plot_box_delta_rmse(
      sub = sub,
      file_name = paste0("boxplot_replication_deltaRMSE_xgb_vs_base_latent", lr, "_rhoB", rb, ".png"),
      main_title = paste0(
        "Replication-level test-set delta RMSE: XGBoost vs baseline OLS | latent_R2 = ",
        lr, ", predictor intercorrelation = ", rb
      )
    )
  }
}

cat("Finished. Separated-condition plots saved to:\n")
cat(plot_dir, "\n")
