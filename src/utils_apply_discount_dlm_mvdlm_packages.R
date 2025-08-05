library(dlm)

library(dlm)

Y <- df$logit.s
X <- scale(df[, c("April", "September", "October")])

p <- 1 + ncol(X)  # intercept + 3 regressors

# Discounts -> variances ( (1 - δ) / δ )
discount_trend <- 0.90
discount_reg   <- 0.95
discount_var   <- 0.90

trend_var <- (1 - discount_trend) / discount_trend   # 0.1111111
reg_var   <- (1 - discount_reg)   / discount_reg     # 0.05263158
obs_var   <- (1 - discount_var)   / discount_var     # 0.1111111

W_diag <- c(trend_var, rep(reg_var, ncol(X)))  # length = 4

# Build a dynamic regression DLM (FF varies with X_t internally)
mod <- dlmModReg(X, addInt = TRUE, dV = obs_var, dW = W_diag)

# (optional but explicit)
GG(mod) <- diag(p)       # random-walk on states
m0(mod) <- rep(0, p)
C0(mod) <- diag(1e3, p)

# Fit
fit <- dlmFilter(Y, mod)

# One-step-ahead forecasts and pseudo-R^2
yhat <- drop(fit$f)
R2_pred <- 1 - sum((Y - yhat)^2) / sum((Y - mean(Y))^2)
R2_pred


library(mvdlm)


index_apr <- scale(df$April)[,1]
index_sep <- scale(df$September)[,1]
index_oct <- scale(df$October)[,1]

df<- data.frame(
  year = df$year,
  Y = df$logit.s,
  index_apr = index_apr,
  index_sep = index_sep,
  index_oct = index_oct
)

fit <- fit_dlm(
  time_varying = Y ~ 1 + index_apr + index_sep + index_oct,
  time = "year",
  data = df,
  chains = 2,
  iter = 1000
  # prior = list(
  #   sigma = 0.3,                        # intercept (trend)
  #   sigma_beta = rep(0.2, 3),           # regressions
  #   sigma_obs = 0.3                     # observation error
  # )
)
dlm_trends(fit)


summary(fit)
