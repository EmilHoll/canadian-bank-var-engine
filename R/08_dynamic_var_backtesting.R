# ============================================================
# Canadian Bank Market Risk VaR Engine
# Rolling VaR Forecasts and Backtesting Framework
# ============================================================

# Load project setup if needed
if (!exists("required_packages")) {
  source(
    here::here(
      "R",
      "00_project_setup.R"
    )
  )
}

# Load portfolio definition if needed
if (!exists("portfolio_definition")) {
  source(
    here::here(
      "R",
      "01_define_portfolio.R"
    )
  )
}

# Load prepared returns if needed
if (!exists("portfolio_returns")) {
  source(
    here::here(
      "R",
      "03_prepare_returns.R"
    )
  )
}

# Check rugarch
if (
  !requireNamespace(
    "rugarch",
    quietly = TRUE
  )
) {
  stop(
    paste(
      "The rugarch package is required.",
      "Install it using renv::install('rugarch')."
    )
  )
}

library(rugarch)

# ============================================================
# Output paths
# ============================================================

dynamic_var_rds_path <- here::here(
  "data",
  "processed",
  "dynamic_var_forecasts.rds"
)

dynamic_var_csv_path <- here::here(
  "data",
  "processed",
  "dynamic_var_forecasts.csv"
)

backtest_summary_path <- here::here(
  "output",
  "tables",
  "var_backtest_summary.csv"
)

var_violations_path <- here::here(
  "output",
  "tables",
  "var_violations.csv"
)

latest_var_path <- here::here(
  "output",
  "tables",
  "latest_dynamic_var_estimates.csv"
)

# ============================================================
# Backtesting parameters
# ============================================================

backtest_window <- 500

ewma_lambda_backtest <- 0.94

garch_refit_every <- 20

# ============================================================
# Prepare return data
# ============================================================

backtest_data <- portfolio_returns |>
  dplyr::arrange(date) |>
  dplyr::select(
    date,
    portfolio_simple_return
  )

return_vector <-
  backtest_data$portfolio_simple_return

number_of_returns <-
  length(return_vector)

if (
  number_of_returns <= backtest_window
) {
  stop(
    "There are not enough observations for the selected backtest window."
  )
}

if (
  anyNA(return_vector)
) {
  stop(
    "Missing portfolio returns were detected."
  )
}

if (
  any(
    !is.finite(return_vector)
  )
) {
  stop(
    "Non-finite portfolio returns were detected."
  )
}

# Forecast observations begin after the initial
# 500-day estimation window.

forecast_indices <- seq.int(
  from = backtest_window + 1,
  to = number_of_returns
)

forecast_dates <-
  backtest_data$date[
    forecast_indices
  ]

number_of_forecasts <-
  length(forecast_indices)

# ============================================================
# Prepare EWMA one-day-ahead volatility
# ============================================================

ewma_forecast_variance <- rep(
  NA_real_,
  number_of_returns
)

# Initialize using only information from the
# first estimation window.

previous_ewma_variance <-
  stats::var(
    return_vector[
      1:backtest_window
    ]
  )

for (
  t in forecast_indices
) {

  current_ewma_variance <-
    ewma_lambda_backtest *
    previous_ewma_variance +
    (1 - ewma_lambda_backtest) *
    return_vector[t - 1]^2

  ewma_forecast_variance[t] <-
    current_ewma_variance

  previous_ewma_variance <-
    current_ewma_variance
}

ewma_forecast_sigma <-
  sqrt(
    ewma_forecast_variance
  )

# ============================================================
# Historical, Gaussian, and EWMA forecasts
# ============================================================

manual_var_forecasts <- purrr::map_dfr(
  forecast_indices,
  function(t) {

    estimation_indices <-
      (t - backtest_window):(t - 1)

    estimation_returns <-
      return_vector[
        estimation_indices
      ]

    rolling_mean <-
      mean(
        estimation_returns
      )

    rolling_sigma <-
      stats::sd(
        estimation_returns
      )

    purrr::map_dfr(
      confidence_levels,
      function(confidence) {

        alpha <-
          1 - confidence

        z_alpha <-
          stats::qnorm(
            alpha
          )

        # ----------------------------------------
        # Historical Simulation
        # ----------------------------------------

        historical_threshold <-
          as.numeric(
            stats::quantile(
              estimation_returns,
              probs = alpha,
              type = 1,
              na.rm = TRUE
            )
          )

        historical_var <-
          -historical_threshold

        # ----------------------------------------
        # Rolling Gaussian
        # ----------------------------------------

        gaussian_threshold <-
          rolling_mean +
          z_alpha *
          rolling_sigma

        gaussian_var <-
          -gaussian_threshold

        # ----------------------------------------
        # EWMA Gaussian
        # ----------------------------------------

        ewma_threshold <-
          rolling_mean +
          z_alpha *
          ewma_forecast_sigma[t]

        ewma_var <-
          -ewma_threshold

        tibble::tibble(
          date =
            backtest_data$date[t],

          confidence_level =
            confidence,

          realized_return =
            return_vector[t],

          rolling_mean =
            rolling_mean,

          rolling_sigma =
            rolling_sigma,

          ewma_sigma =
            ewma_forecast_sigma[t],

          historical_var =
            historical_var,

          rolling_gaussian_var =
            gaussian_var,

          ewma_var =
            ewma_var
        )
      }
    )
  }
)

# ============================================================
# GARCH(1,1) rolling forecast specification
# ============================================================

garch_backtest_specification <-
  rugarch::ugarchspec(

    variance.model = list(
      model = "sGARCH",
      garchOrder = c(1, 1)
    ),

    mean.model = list(
      armaOrder = c(0, 0),
      include.mean = TRUE
    ),

    distribution.model = "norm"
  )

# Use percentage returns for numerical stability.

garch_return_series <-
  xts::xts(
    return_vector * 100,
    order.by =
      backtest_data$date
  )

# ============================================================
# Rolling GARCH estimation
# ============================================================

message(
  "Running rolling GARCH forecasts. This may take a few minutes."
)

garch_roll <-
  rugarch::ugarchroll(

    spec =
      garch_backtest_specification,

    data =
      garch_return_series,

    n.ahead =
      1,

    forecast.length =
      number_of_forecasts,

    refit.every =
      garch_refit_every,

    refit.window =
      "moving",

    window.size =
      backtest_window,

    solver =
      "hybrid",

    calculate.VaR =
      FALSE,

    keep.coef =
      TRUE
  )

# ============================================================
# Check GARCH rolling convergence
# ============================================================

garch_roll_convergence <-
  convergence(
    garch_roll
  )

cat(
  "\nGARCH rolling convergence code:",
  garch_roll_convergence,
  "\n"
)

if (
  garch_roll_convergence != 0
) {
  warning(
    paste(
      "One or more rolling GARCH estimation windows",
      "did not converge."
    )
  )
}

# ============================================================
# Extract GARCH density forecasts
# ============================================================

if (
  is.null(
    garch_roll@forecast$density
  )
) {
  stop(
    paste(
      "GARCH density forecasts could not be extracted.",
      "Check whether all rolling estimation windows converged."
    )
  )
}

garch_density_forecasts <-
  as.data.frame(
    garch_roll@forecast$density
  )

if (
  !all(
    c(
      "Mu",
      "Sigma"
    ) %in%
    names(
      garch_density_forecasts
    )
  )
) {
  stop(
    paste(
      "The GARCH rolling forecast output",
      "does not contain Mu and Sigma."
    )
  )
}

if (
  nrow(
    garch_density_forecasts
  ) !=
  number_of_forecasts
) {
  stop(
    paste(
      "The number of GARCH forecasts",
      "does not match the backtesting sample."
    )
  )
}

garch_forecasts <- tibble::tibble(

  date =
    forecast_dates,

  garch_mean =
    garch_density_forecasts$Mu /
    100,

  garch_sigma =
    garch_density_forecasts$Sigma /
    100
)

if (
  anyNA(
    garch_forecasts$garch_mean
  ) ||
  anyNA(
    garch_forecasts$garch_sigma
  )
) {
  stop(
    paste(
      "Missing GARCH forecasts were detected.",
      "Inspect rolling-model convergence."
    )
  )
}

# ============================================================
# Calculate GARCH Gaussian VaR
# ============================================================

garch_var_forecasts <- tidyr::crossing(

  garch_forecasts,

  confidence_level =
    confidence_levels

) |>
  dplyr::mutate(

    tail_probability =
      1 -
      confidence_level,

    z_alpha =
      stats::qnorm(
        tail_probability
      ),

    garch_var =
      -(
        garch_mean +
        z_alpha *
        garch_sigma
      )
  ) |>
  dplyr::select(
    date,
    confidence_level,
    garch_mean,
    garch_sigma,
    garch_var
  )

# ============================================================
# Combine all forecast models
# ============================================================

combined_forecasts <- manual_var_forecasts |>
  dplyr::left_join(
    garch_var_forecasts,
    by = c(
      "date",
      "confidence_level"
    )
  )

# ============================================================
# Convert to long format
# ============================================================

dynamic_var_forecasts <- combined_forecasts |>
  dplyr::select(
    date,
    confidence_level,
    realized_return,
    historical_var,
    rolling_gaussian_var,
    ewma_var,
    garch_var
  ) |>
  tidyr::pivot_longer(

    cols = c(
      historical_var,
      rolling_gaussian_var,
      ewma_var,
      garch_var
    ),

    names_to =
      "model",

    values_to =
      "var_return"
  ) |>
  dplyr::mutate(

    model =
      dplyr::recode(
        model,

        historical_var =
          "Historical Simulation",

        rolling_gaussian_var =
          "Rolling Gaussian",

        ewma_var =
          "EWMA Gaussian",

        garch_var =
          "GARCH(1,1) Gaussian"
      ),

    tail_probability =
      1 -
      confidence_level,

    realized_loss =
      -realized_return,

    var_dollar =
      portfolio_value *
      var_return,

    realized_pnl =
      portfolio_value *
      realized_return,

    violation =
      realized_return <
      -var_return
  ) |>
  dplyr::arrange(
    model,
    confidence_level,
    date
  )

# ============================================================
# Validation checks
# ============================================================

if (
  anyNA(
    dynamic_var_forecasts$var_return
  )
) {
  stop(
    "Missing VaR forecasts were detected."
  )
}

if (
  any(
    !is.finite(
      dynamic_var_forecasts$var_return
    )
  )
) {
  stop(
    "Non-finite VaR forecasts were detected."
  )
}

if (
  any(
    dynamic_var_forecasts$var_return <= 0
  )
) {
  warning(
    paste(
      "At least one VaR estimate is non-positive.",
      "Inspect the corresponding forecasts."
    )
  )
}

# ============================================================
# Backtesting summary
# ============================================================

var_backtest_summary <- dynamic_var_forecasts |>
  dplyr::group_by(
    model,
    confidence_level
  ) |>
  dplyr::summarise(

    observations =
      dplyr::n(),

    expected_violation_rate =
      mean(
        tail_probability
      ),

    expected_violations =
      observations *
      expected_violation_rate,

    actual_violations =
      sum(
        violation
      ),

    actual_violation_rate =
      mean(
        violation
      ),

    violation_ratio =
      actual_violations /
      expected_violations,

    average_var =
      mean(
        var_return
      ),

    maximum_var =
      max(
        var_return
      ),

    .groups =
      "drop"
  ) |>
  dplyr::arrange(
    confidence_level,
    model
  )

# ============================================================
# Extract all VaR violations
# ============================================================

var_violations <- dynamic_var_forecasts |>
  dplyr::filter(
    violation
  ) |>
  dplyr::mutate(

    excess_loss =
      realized_loss -
      var_return,

    excess_loss_dollar =
      portfolio_value *
      excess_loss
  ) |>
  dplyr::arrange(
    confidence_level,
    model,
    date
  )

# ============================================================
# Latest VaR forecasts
# ============================================================

latest_forecast_date <-
  max(
    dynamic_var_forecasts$date
  )

latest_dynamic_var_estimates <-
  dynamic_var_forecasts |>
  dplyr::filter(
    date ==
      latest_forecast_date
  ) |>
  dplyr::select(
    date,
    model,
    confidence_level,
    var_return,
    var_dollar
  ) |>
  dplyr::arrange(
    confidence_level,
    model
  )

# ============================================================
# Save results
# ============================================================

saveRDS(
  dynamic_var_forecasts,
  dynamic_var_rds_path
)

readr::write_csv(
  dynamic_var_forecasts,
  dynamic_var_csv_path
)

readr::write_csv(
  var_backtest_summary,
  backtest_summary_path
)

readr::write_csv(
  var_violations,
  var_violations_path
)

readr::write_csv(
  latest_dynamic_var_estimates,
  latest_var_path
)

# ============================================================
# Display results
# ============================================================

print(
  var_backtest_summary |>
    dplyr::mutate(

      confidence_level =
        scales::percent(
          confidence_level,
          accuracy = 1
        ),

      expected_violation_rate =
        scales::percent(
          expected_violation_rate,
          accuracy = 0.01
        ),

      actual_violation_rate =
        scales::percent(
          actual_violation_rate,
          accuracy = 0.01
        ),

      average_var =
        scales::percent(
          average_var,
          accuracy = 0.01
        ),

      maximum_var =
        scales::percent(
          maximum_var,
          accuracy = 0.01
        )
    )
)

cat(
  "\nBacktesting window:",
  backtest_window,
  "trading days\n"
)

cat(
  "Out-of-sample forecasts:",
  number_of_forecasts,
  "\n"
)

cat(
  "First forecast date:",
  format(
    min(forecast_dates)
  ),
  "\n"
)

cat(
  "Last forecast date:",
  format(
    max(forecast_dates)
  ),
  "\n"
)

cat(
  "Total VaR violations across all model/confidence combinations:",
  nrow(var_violations),
  "\n"
)

message(
  "Dynamic VaR forecasting and backtesting framework completed successfully."
)