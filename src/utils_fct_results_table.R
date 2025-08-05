# This R script compares SARs for CUI and CUTI using MARSS
# Applies models to Scheuerell, CJS, and DART SARs for April only
# Fits models with "diagonal and unequal" and with discount structure
# Adds base (no covariate) model and residual autocorrelation diagnostics

library(MARSS)
library(tidyverse)
library(broom)

results_table <- tibble()

# Function to run MARSS with optional covariate (April only)
eval_model <- function(df_sar, df_index = NULL, index_label = "None", source_label, discount = FALSE) {
  if (!is.null(df_index)) {
    df <- df_sar %>%
      left_join(df_index %>% filter(month == "April"), by = "year") %>%
      drop_na() %>%
      select(year, logit.s, value)
    index_apr <- scale(df$value)[,1]
    has_covariate <- TRUE
  } else {
    df <- df_sar %>% drop_na(logit.s) %>% select(year, logit.s)
    has_covariate <- FALSE
  }

  years <- df$year
  TT <- length(years)
  dat <- matrix(df$logit.s, nrow = 1)

  m <- ifelse(has_covariate, 2, 1)  # intercept + optional covariate

  B <- diag(m)
  U <- matrix(0, m, 1)
  Z <- array(NA, c(1, m, TT))
  Z[1,1,] <- 1
  if (has_covariate) Z[1,2,] <- index_apr

  A <- matrix(0)

  if (discount) {
    discount_trend <- 0.90
    discount_reg   <- 0.95
    discount_var   <- 0.90
    trend_var <- (1 - discount_trend) / discount_trend
    reg_var   <- ifelse(has_covariate, (1 - discount_reg) / discount_reg, 0)
    obs_var   <- (1 - discount_var) / discount_var

    Q <- matrix(list(0), m, m)
    diag(Q) <- if (has_covariate) c(trend_var, reg_var) else c(trend_var)
    R <- matrix(obs_var)
  } else {
    Q <- "diagonal and unequal"
    R <- matrix("r")
  }

  model_list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R)
  inits <- list(x0 = matrix(0, m, 1))

  fit <- tryCatch({
    MARSS(dat, model = model_list, inits = inits, silent = TRUE)
  }, error = function(e) NULL)

  if (!is.null(fit)) {
    observed_vals <- dat[1, ]

    # Predictive residuals (forecast model)
    resid_tt1 <- MARSSresiduals(fit, type = "tt1")$model.residuals[1, ]
    fitted_tt1 <- observed_vals - resid_tt1
    R2_pred <- 1 - sum((observed_vals - fitted_tt1)^2) / sum((observed_vals - mean(observed_vals))^2)

    # In-sample residuals (fitted model)
    resid_tt <- MARSSresiduals(fit, type = "tt")$model.residuals[1, ]
    fitted_tt <- observed_vals - resid_tt
    R2_fit <- 1 - sum((observed_vals - fitted_tt)^2) / sum((observed_vals - mean(observed_vals))^2)

    # Check autocorrelation
    acf_pval <- tryCatch({
      Box.test(resid_tt, lag = 10, type = "Ljung-Box")$p.value
    }, error = function(e) NA)

    tibble(
      Source = source_label,
      Index = index_label,
      Discount = discount,
      LL = fit$logLik,
      AICc = fit$AICc,
      R2_pred = R2_pred,
      R2_fit = R2_fit,
      Resid_ACF_pval = acf_pval
    )
  } else {
    tibble(Source = source_label, Index = index_label, Discount = discount,
           LL = NA, AICc = NA, R2_pred = NA, R2_fit = NA, Resid_ACF_pval = NA)
  }
}

# Define SAR data sets
sar_sets <- list(
  list(data = SalmonSurvCUI, label = "Scheuerell"),
  list(data = df.cbr %>% select(year, logit.s), label = "DART"),
  list(data = sar_cjs %>% select(year, logit.s), label = "CJS")
)

index_sets <- list(
  list(data = df_cui, label = "CUI"),
  list(data = df_cuti, label = "CUTI")
)

# Fit all models
for (sar in sar_sets) {
  # Add baseline model (no covariates)
  results_table <- bind_rows(
    results_table,
    eval_model(sar$data, df_index = NULL, index_label = "None", source_label = sar$label, discount = FALSE),
    eval_model(sar$data, df_index = NULL, index_label = "None", source_label = sar$label, discount = TRUE)
  )

  for (index in index_sets) {
    results_table <- bind_rows(
      results_table,
      eval_model(sar$data, index$data, index$label, sar$label, discount = FALSE),
      eval_model(sar$data, index$data, index$label, sar$label, discount = TRUE)
    )
  }
}

print(results_table)
write_csv(results_table, "results/model_comparison_table.csv")
