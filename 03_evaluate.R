# ============================================================
# 03_evaluate.R
# Purpose:
#   Compute predictive performance and save realized condition checks
# ============================================================

rmse_fun <- function(y, yhat) {
  sqrt(mean((y - yhat)^2))
}

r2_fun <- function(y, yhat) {
  ss_res <- sum((y - yhat)^2)
  ss_tot <- sum((y - mean(y))^2)
  1 - ss_res / ss_tot
}

evaluate_predictions <- function(
    y_test,
    preds,
    condition_id,
    latent_R2,
    rho_X,
    rho_Y,
    comp_linear,
    rep,
    realized_latent_R2,
    realized_rho_X,
    realized_rho_X_min,
    realized_rho_X_max,
    realized_rho_Y,
    realized_linear_share,
    realized_interaction_share,
    realized_lin_int_cor
) {
  
  if (!all(c("ols_base", "ols_true_interaction", "xgb") %in% names(preds))) {
    stop("preds must contain 'ols_base', 'ols_true_interaction', and 'xgb'")
  }
  
  yhat_ols_base <- preds[["ols_base"]]
  yhat_ols_true_interaction <- preds[["ols_true_interaction"]]
  yhat_xgb <- preds[["xgb"]]
  
  rmse_ols_base <- rmse_fun(y_test, yhat_ols_base)
  rmse_ols_true_interaction <- rmse_fun(y_test, yhat_ols_true_interaction)
  rmse_xgb <- rmse_fun(y_test, yhat_xgb)
  
  r2_ols_base <- r2_fun(y_test, yhat_ols_base)
  r2_ols_true_interaction <- r2_fun(y_test, yhat_ols_true_interaction)
  r2_xgb <- r2_fun(y_test, yhat_xgb)
  
  data.frame(
    condition_id = condition_id,
    latent_R2 = latent_R2,
    rho_X = rho_X,
    rho_Y = rho_Y,
    comp_linear = comp_linear,
    rep = rep,
    dataset = "test",
    
    realized_latent_R2 = realized_latent_R2,
    realized_rho_X = realized_rho_X,
    realized_rho_X_min = realized_rho_X_min,
    realized_rho_X_max = realized_rho_X_max,
    realized_rho_Y = realized_rho_Y,
    realized_linear_share = realized_linear_share,
    realized_interaction_share = realized_interaction_share,
    realized_lin_int_cor = realized_lin_int_cor,
    
    rmse_ols_base = rmse_ols_base,
    rmse_ols_true_interaction = rmse_ols_true_interaction,
    rmse_xgb = rmse_xgb,
    
    delta_rmse_true_vs_base = rmse_ols_true_interaction - rmse_ols_base,
    delta_rmse_xgb_vs_base = rmse_xgb - rmse_ols_base,
    delta_rmse_xgb_vs_true = rmse_xgb - rmse_ols_true_interaction,
    
    r2_ols_base = r2_ols_base,
    r2_ols_true_interaction = r2_ols_true_interaction,
    r2_xgb = r2_xgb,
    
    delta_r2_true_vs_base = r2_ols_true_interaction - r2_ols_base,
    delta_r2_xgb_vs_base = r2_xgb - r2_ols_base,
    delta_r2_xgb_vs_true = r2_xgb - r2_ols_true_interaction
  )
}
