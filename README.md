# Simulation Study: Measurement Error and Structural Complexity in Prediction

This simulation study examines how **measurement error** and **signal structure** jointly influence predictive performance in **regression models** and **machine learning algorithms**.

The design explicitly separates:

-   the true underlying signal
-   random noise
-   measurement error in observed variables

This allows us to evaluate whether gains from flexible models reflect genuine structure or are limited by data quality.

------------------------------------------------------------------------

## Why This Matters

In the social and behavioural sciences, variables such as intelligence, motivation, or well-being are latent and measured with error. As a result, predictive models operate on imperfect representations of the underlying constructs.

Classical measurement theory shows that measurement error attenuates relationships and limits predictive performance. In particular, outcome reliability imposes an upper bound on the maximum achievable $R^2$.

This has direct implications for model comparison. Improvements in predictive performance are often interpreted as evidence that flexible models capture nonlinear or interaction effects. However, such gains may instead depend on measurement quality.

This simulation tests that distinction by manipulating:

-   signal strength ($R^2$)
-   predictor reliability ($\rho_X$)
-   outcome reliability ($\rho_Y$)
-   signal composition (linear vs. interaction effects)

Model performance is evaluated relative to a regression baseline to determine whether gains from flexible models persist under measurement error.

------------------------------------------------------------------------

## Data-Generating Process

The simulation is constructed in four conceptual steps:

1.  Generate true predictors
2.  Construct the signal ($\eta$)
3.  Add noise to form the true outcome
4.  Add measurement error to obtain observed data

This separates structural relationships from noise and measurement error.

------------------------------------------------------------------------

## Data-Generating Process (Step-by-step)

### Step 1: Generate latent predictors

True predictors are generated as independent standard normal variables:

$$
X_j^* \sim \mathcal{N}(0, 1), \quad j = 1, \dots, p
$$

``` r
X_true <- matrix(rnorm(N * p, 0, 1), nrow = N, ncol = p)
```

These represent the **true inputs** to the system.

------------------------------------------------------------------------

### Step 2: Construct raw signal components

The signal is built from a linear component and an interaction component:

$$
\eta_{\mathrm{linear,raw}} = X^* \beta
$$

$$
\eta_{\mathrm{interaction,raw}} = X_1^* \cdot X_2^*
$$

``` r
linear_signal_raw <- as.vector(X_true %*% beta)
interaction_signal_raw <- X_true[, "X1"] * X_true[, "X2"]
```

------------------------------------------------------------------------

### Step 3: Standardize components

Both components are standardized:

$$
A = \mathrm{scale}(\eta_{\mathrm{linear,raw}}), \quad
B = \mathrm{scale}(\eta_{\mathrm{interaction,raw}})
$$

``` r
linear_signal_z <- as.numeric(scale(linear_signal_raw))
interaction_signal_z <- as.numeric(scale(interaction_signal_raw))
```

This ensures both components have equal variance, so their contribution is not driven by scale differences.

------------------------------------------------------------------------

### Step 4: Form weighted signal (raw)

The components are combined using weights:

$$
\eta_{\mathrm{raw}} =
\sqrt{c_{\mathrm{linear}}} \cdot A +
\sqrt{1 - c_{\mathrm{linear}}} \cdot B
$$

``` r
w_linear <- sqrt(comp_linear)
w_interaction <- sqrt(1 - comp_linear)

signal_raw <- w_linear * linear_signal_z +
              w_interaction * interaction_signal_z
```

------------------------------------------------------------------------

### Signal composition and weighting

The parameter $c_{\mathrm{linear}}$ controls the relative contribution of linear and interaction effects in the signal.

The weights are defined as:

$$
w_{\mathrm{linear}} = \sqrt{c_{\mathrm{linear}}}, \quad
w_{\mathrm{interaction}} = \sqrt{1 - c_{\mathrm{linear}}}
$$

Using square-root weights ensures that, if the components were independent, their contributions to the variance of the signal would correspond to $c_{\mathrm{linear}}$ and $1 - c_{\mathrm{linear}}$.

In practice, the components are not independent, so the resulting variance contribution is approximate. This is evaluated using diagnostic checks.

------------------------------------------------------------------------

### Step 5: Standardize total signal

The combined signal is standardized:

$$
\eta = \mathrm{scale}(\eta_{\mathrm{raw}})
$$

``` r
signal <- as.numeric(scale(signal_raw))
```

This ensures that the variance of the signal is constant across conditions.\
As a result, differences between conditions reflect changes in structure rather than scale.

------------------------------------------------------------------------

### Step 6: Generate latent outcome

The true outcome is generated by adding noise:

$$
Y^* = \eta + \epsilon
$$

with noise variance chosen to achieve target $R^2$:

$$
\mathrm{Var}(\epsilon) =
\mathrm{Var}(\eta)\cdot \frac{1 - R^2}{R^2}
$$

``` r
var_signal <- var(signal)
var_error <- var_signal * (1 - latent_R2) / latent_R2

eps <- rnorm(N, 0, sqrt(var_error))
Y_true <- signal + eps
```

This ensures:

$$
\mathrm{corr}(\eta, Y^*)^2 \approx R^2
$$

------------------------------------------------------------------------

### Step 7: Add measurement error

Observed variables are generated as:

$$
X = X^* + u_X, \quad Y = Y^* + u_Y
$$

``` r
add_measurement_error <- function(true_var, rho) {
  var_true <- var(true_var)
  var_error <- var_true * (1 - rho) / rho
  error <- rnorm(length(true_var), 0, sqrt(var_error))
  true_var + error
}

X_obs <- apply(X_true, 2, add_measurement_error, rho = rho_X)
Y_obs <- add_measurement_error(Y_true, rho_Y)
```

This ensures that the reliability of the observed variables matches the target values:

$$
rho_X \approx cor(X, X^{\ast})^2
$$

$$
rho_Y \approx cor(Y, Y^{\ast})^2
$$

------------------------------------------------------------------------

### Observed Data

Models are fitted on $(X, Y)$, while the true data-generating process operates on $(X^*, Y^*)$.

Because both predictors and outcomes contain measurement error, models estimate attenuated relationships rather than the true signal.

------------------------------------------------------------------------

## Simulation Design

The simulation follows a factorial design in which key parameters are systematically manipulated.

### Manipulated factors

-   Latent signal strength ($R^2$): $\{0.20, 0.50, 0.80\}$\
-   Predictor reliability ($\rho_X$): $\{0.60, 0.80, 1.00\}$\
-   Outcome reliability ($\rho_Y$): $\{0.60, 0.80, 1.00\}$\
-   Signal composition ($c_{\mathrm{linear}}$): $\{0.20, 0.50, 0.80\}$

These factors define the conditions under which predictive performance is evaluated. Each combination of manipulated factors defines one simulation condition, allowing the effects of measurement error and structural complexity to be evaluated independently.

------------------------------------------------------------------------

### Fixed design parameters

The following aspects of the data-generating process are held constant across conditions:

-   Sample size: $N = 1000$
-   Number of predictors: $p = 20$
-   Number of true predictors: 4
-   True coefficients: $\beta = (0.6, 0.5, 0.4, 0.3)$
-   Remaining predictors: noise variables ($\beta = 0$)
-   Interaction structure: $X_1^* \cdot X_2^*$
-   Predictor correlation: independent predictors ($r = 0$)
-   Train/test split: 70/30
-   Number of replications per condition: (e.g., 100 or 200)

------------------------------------------------------------------------

## Models

### OLS (baseline)

$$
Y = \beta_0 + \sum_{j=1}^{p} \beta_j X_j + \epsilon
$$

OLS provides a reference with known behavior under measurement error.

------------------------------------------------------------------------

### OLS with interaction

$$
Y = \beta_0 + \sum_{j=1}^{p} \beta_j X_j + \gamma (X_1 X_2) + \epsilon
$$

------------------------------------------------------------------------

### XGBoost

$$
Y = f(X) + \epsilon
$$

XGBoost is a flexible model that can capture nonlinearities and interactions.

------------------------------------------------------------------------

## Evaluation

### RMSE

$$
\mathrm{RMSE} =
\sqrt{\frac{1}{n} \sum (Y_i - \hat{Y}_i)^2}
$$

### $R^2$

$$
R^2 =
1 - \frac{\sum (Y_i - \hat{Y}_i)^2}{\sum (Y_i - \bar{Y})^2}
$$

------------------------------------------------------------------------

### Relative performance

Models are compared relative to OLS:

$$
\Delta R^2 = R^2_{\mathrm{ML}} - R^2_{\mathrm{OLS}}, \quad
\Delta RMSE = RMSE_{\mathrm{ML}} - RMSE_{\mathrm{OLS}}
$$

These quantities measure whether flexible models provide gains beyond what is expected under attenuation.

------------------------------------------------------------------------

## Key Constraint

Predictive performance is limited by outcome reliability:

$$
R^2_{\mathrm{observed}} \leq \rho_Y
$$

No model can recover variance that is not present in the observed outcome.

------------------------------------------------------------------------

## Summary

The simulation separates:

-   signal
-   noise
-   measurement error

to evaluate when flexible models provide meaningful gains and when performance is limited by data quality.
