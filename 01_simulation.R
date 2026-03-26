# ============================================================
# 01_simulation.R
# Purpose:
#   Generate one observed dataset for one simulation condition
# Outputs:
#   observed data + realized condition diagnostics
# ============================================================

# -----------------------------
# 1. Generate latent predictors X*
# -----------------------------
generate_X_true <- function(N, p) {
  X_true <- matrix(rnorm(N * p, mean = 0, sd = 1), nrow = N, ncol = p)
  colnames(X_true) <- paste0("X", seq_len(p))
  X_true
}

# -----------------------------
# 2. Add measurement error
# -----------------------------
add_measurement_error <- function(true_var, rho) {
  if (rho == 1) return(true_var)
  
  var_true <- var(true_var)
  var_error <- var_true * (1 - rho) / rho
  
  error <- rnorm(length(true_var), mean = 0, sd = sqrt(var_error))
  true_var + error
}

# -----------------------------
# 3. Generate latent outcome Y*
#    with target latent_R2 and target
#    linear variance share = comp_linear
# -----------------------------
generate_Y_true <- function(X_true, beta, latent_R2, comp_linear = 0.80) {
  
  if (comp_linear < 0 || comp_linear > 1) {
    stop("comp_linear must be between 0 and 1.")
  }
  
  # 3A. Raw components
  linear_signal_raw <- as.vector(X_true %*% beta)
  interaction_signal_raw <- X_true[, "X1"] * X_true[, "X2"]
  
  # 3B. Standardize components
  linear_signal_z <- as.numeric(scale(linear_signal_raw))
  interaction_signal_z <- as.numeric(scale(interaction_signal_raw))
  
  # 3C. Use sqrt weights so comp_linear approximates
  #     target variance share
  w_linear <- sqrt(comp_linear)
  w_interaction <- sqrt(1 - comp_linear)
  
  linear_part <- w_linear * linear_signal_z
  interaction_part <- w_interaction * interaction_signal_z
  
  signal_raw <- linear_part + interaction_part
  
  # standardize total signal for clean control of latent R2
  signal <- as.numeric(scale(signal_raw))
  
  # 3D. Residual variance to hit target latent R2
  var_signal <- var(signal)
  var_error <- var_signal * (1 - latent_R2) / latent_R2
  
  eps <- rnorm(nrow(X_true), mean = 0, sd = sqrt(var_error))
  Y_true <- signal + eps
  
  # 3E. Realized latent diagnostics
  realized_latent_R2 <- cor(signal, Y_true)^2
  
  total_signal_var_raw <- var(signal_raw)
  realized_linear_share <- var(linear_part) / total_signal_var_raw
  realized_interaction_share <- var(interaction_part) / total_signal_var_raw
  realized_lin_int_cor <- cor(linear_signal_z, interaction_signal_z)
  
  list(
    Y_true = Y_true,
    signal = signal,
    linear_signal = linear_signal_z,
    interaction_signal = interaction_signal_z,
    linear_part = linear_part,
    interaction_part = interaction_part,
    eps = eps,
    var_signal = var_signal,
    var_error = var_error,
    realized_latent_R2 = realized_latent_R2,
    realized_linear_share = realized_linear_share,
    realized_interaction_share = realized_interaction_share,
    realized_lin_int_cor = realized_lin_int_cor
  )
}

# -----------------------------
# 4. Observed predictors X
# -----------------------------
generate_X_obs <- function(X_true, rho_X) {
  X_obs <- X_true
  for (j in seq_len(ncol(X_true))) {
    X_obs[, j] <- add_measurement_error(X_true[, j], rho_X)
  }
  colnames(X_obs) <- colnames(X_true)
  X_obs
}

# -----------------------------
# 5. Observed outcome Y
# -----------------------------
generate_Y_obs <- function(Y_true, rho_Y) {
  add_measurement_error(Y_true, rho_Y)
}

# -----------------------------
# 6. Realized reliability checks
# -----------------------------
compute_realized_rho_X <- function(X_true, X_obs) {
  rho_vec <- sapply(seq_len(ncol(X_true)), function(j) {
    cor(X_true[, j], X_obs[, j])^2
  })
  list(
    mean_rho_X = mean(rho_vec),
    min_rho_X = min(rho_vec),
    max_rho_X = max(rho_vec),
    rho_X_by_var = rho_vec
  )
}

compute_realized_rho_Y <- function(Y_true, Y_obs) {
  cor(Y_true, Y_obs)^2
}

# -----------------------------
# 7. Main wrapper
# -----------------------------
generate_dataset <- function(N, p, beta, latent_R2, rho_X, rho_Y, comp_linear) {
  
  X_true <- generate_X_true(N, p)
  
  y_lat <- generate_Y_true(
    X_true = X_true,
    beta = beta,
    latent_R2 = latent_R2,
    comp_linear = comp_linear
  )
  
  Y_true <- y_lat$Y_true
  
  X_obs <- generate_X_obs(X_true, rho_X)
  Y_obs <- generate_Y_obs(Y_true, rho_Y)
  
  rhoX_info <- compute_realized_rho_X(X_true, X_obs)
  realized_rho_Y <- compute_realized_rho_Y(Y_true, Y_obs)
  
  data_obs <- data.frame(Y = Y_obs, X_obs, check.names = FALSE)
  
  list(
    data = data_obs,
    X_true = X_true,
    Y_true = Y_true,
    X_obs = X_obs,
    Y_obs = Y_obs,
    
    signal = y_lat$signal,
    linear_signal = y_lat$linear_signal,
    interaction_signal = y_lat$interaction_signal,
    linear_part = y_lat$linear_part,
    interaction_part = y_lat$interaction_part,
    eps = y_lat$eps,
    
    var_signal = y_lat$var_signal,
    var_error = y_lat$var_error,
    
    realized_latent_R2 = y_lat$realized_latent_R2,
    realized_linear_share = y_lat$realized_linear_share,
    realized_interaction_share = y_lat$realized_interaction_share,
    realized_lin_int_cor = y_lat$realized_lin_int_cor,
    
    realized_rho_X = rhoX_info$mean_rho_X,
    realized_rho_X_min = rhoX_info$min_rho_X,
    realized_rho_X_max = rhoX_info$max_rho_X,
    realized_rho_Y = realized_rho_Y
  )
}
