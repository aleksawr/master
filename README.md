# Simulation Study: Measurement Error and Structural Complexity in Prediction

This simulation study investigates how **measurement error** and **signal structure** jointly influence predictive performance in **regression models** and **machine learning algorithms**.

The design extends classical regression logic by introducing:

- latent (true) predictors and outcomes  
- controlled signal composition (linear vs interaction effects)  
- measurement error in both predictors and outcome  

The goal is to evaluate whether flexible models retain advantages when data quality is limited.

------------------------------------------------------------------------

## Data-Generating Process

### Latent predictors

Latent predictors \( X^* \) are generated as independent standard normal variables:

$$
X_j^* \sim \mathcal{N}(0, 1)
$$

These represent the **true, error-free variables**.

---

### Latent signal (η)

The outcome is generated from a **latent signal** \( \eta \), which combines:

- linear effects from a subset of predictors  
- an interaction between two predictors  

#### Linear component

Only the first four predictors contribute to the linear signal:

$$
\eta_{\text{linear}} =
0.6X_1^* + 0.5X_2^* + 0.4X_3^* + 0.3X_4^*
$$

---

#### Interaction component

A pairwise interaction is included:

$$
\eta_{\text{interaction}} = X_1^* \cdot X_2^*
$$

---

#### Signal composition

The total signal is a weighted combination of the two components:

$$
\eta = w_{\text{linear}} \cdot \tilde{\eta}_{\text{linear}} +
w_{\text{interaction}} \cdot \tilde{\eta}_{\text{interaction}}
$$

where

$$
w_{\text{linear}} = \sqrt{\mathrm{comp\_linear}}, \qquad
w_{\text{interaction}} = \sqrt{1 - \mathrm{comp\_linear}}
$$

- `comp_linear = 0.8` → mostly linear signal  
- `comp_linear = 0.2` → mostly interaction signal

---

### Latent outcome

The true outcome is generated as:

$$
Y^* = \eta + \varepsilon
$$

where:

$$
\varepsilon \sim \mathcal{N}(0, \sigma^2)
$$

The residual variance is chosen to achieve a target latent \( R^2 \):

$$
\text{Var}(\varepsilon) =
\text{Var}(\eta)\cdot \frac{1 - R^2}{R^2}
$$

---

## Measurement Error

Measurement error is added to both predictors and outcome.

---

### Observed predictors

$$
X = X^* + \delta
$$

where:

$$
\delta \sim \mathcal{N}(0, \sigma^2_\delta)
$$

The error variance is set to achieve target reliability:

$$
\rho_X =
\frac{\text{Var}(X^*)}{\text{Var}(X)}
$$

---

### Observed outcome

$$
Y = Y^* + u
$$

where:

$$
u \sim \mathcal{N}(0, \sigma^2_u)
$$

and:

$$
\rho_Y =
\frac{\text{Var}(Y^*)}{\text{Var}(Y)}
$$

---

## Models

Models are fitted on the **observed data (X, Y)**.

---

### Baseline OLS (misspecified)

$$
Y = \beta_0 + \sum_{j=1}^{p} \beta_j X_j + \varepsilon
$$

- Includes only main effects  
- Cannot capture interactions  

---

### Correctly specified OLS

$$
Y = \beta_0 + \sum_{j=1}^{p} \beta_j X_j + \gamma (X_1 \cdot X_2) + \varepsilon
$$

- Matches the data-generating process  

---

### Machine learning model (XGBoost)

$$
Y = f(X) + \varepsilon
$$

- Flexible, nonparametric function  
- Can capture nonlinear relationships automatically  

---

## Evaluation Metrics

Predictive performance is evaluated on a test set.

---

### Root Mean Squared Error (RMSE)

$$
RMSE =
\sqrt{\frac{1}{n} \sum (Y_i - \hat{Y}_i)^2}
$$

- Measures absolute prediction error  
- Lower is better  

---

### Coefficient of Determination (R²)

$$
R^2 =
1 - \frac{\sum (Y_i - \hat{Y}_i)^2}{\sum (Y_i - \bar{Y})^2}
$$

- Measures proportion of variance explained  
- Higher is better  

---

## Key Theoretical Idea

Predictive performance is constrained by both:

- **signal strength** (latent \( R^2 \))  
- **measurement quality** (reliability \( \rho_X, \rho_Y \))  

---

### Measurement error constraint

Even with perfect models:

$$
R^2_{\text{observed}} \leq \rho_Y
$$

---

### Structural mismatch

- Misspecified models cannot recover interaction effects  
- Flexible models can adapt, but depend on data quality  

---

## Summary

The simulation isolates three fundamental factors:

- signal strength  
- measurement error  
- model flexibility  

to evaluate when performance differences between models **persist or disappear**.