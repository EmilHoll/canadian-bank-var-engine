# ============================================================
# Canadian Bank Market Risk VaR Engine
# Expected Shortfall estimation
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

# Load VaR calculations if needed
if (!exists("var_results")) {
  source(
    here::here(
      "R",
      "05_var_models.R"
    )
  )
}

# ============================================================
# Output paths
# ============================================================

es_results_path <- here::here(
  "output",
  "tables",
  "expected_shortfall_estimates.csv"
)

tail_observations_path <- here::here(
  "output",
  "tables",
  "historical_es_tail_summary.csv"
)

var_es_comparison_path <- here::here(
  "output",
  "tables",
  "var_es_comparison.csv"
)

# ============================================================
# Basic inputs
# ============================================================

portfolio_return_series <-
  portfolio_returns$portfolio_simple_return

tail_probabilities <-
  1 - confidence_levels

# ============================================================
# Historical Expected Shortfall
# ============================================================

historical_es_calculation <- purrr::map_dfr(
  seq_along(confidence_levels),
  function(i) {
    
    confidence <-
      confidence_levels[i]
    
    alpha <-
      tail_probabilities[i]
    
    # Same empirical quantile convention used in VaR
    tail_threshold <- as.numeric(
      stats::quantile(
        portfolio_return_series,
        probs = alpha,
        na.rm = TRUE,
        type = 1
      )
    )
    
    # Returns lying beyond the VaR threshold
    tail_returns <-
      portfolio_return_series[
        portfolio_return_series <=
          tail_threshold
      ]
    
    if (length(tail_returns) == 0) {
      stop(
        paste(
          "No historical tail observations found",
          "for confidence level",
          confidence
        )
      )
    }
    
    tibble::tibble(
      model = "Historical Simulation",
      confidence_level = confidence,
      tail_probability = alpha,
      
      var_threshold_return =
        -tail_threshold,
      
      es_return =
        -mean(tail_returns),
      
      es_dollar =
        portfolio_value *
        (-mean(tail_returns)),
      
      tail_observations =
        length(tail_returns),
      
      worst_tail_return =
        min(tail_returns),
      
      average_tail_return =
        mean(tail_returns)
    )
  }
)

# ============================================================
# Parametric Gaussian Expected Shortfall
# ============================================================

parametric_es_calculation <- purrr::map_dfr(
  seq_along(confidence_levels),
  function(i) {
    
    confidence <-
      confidence_levels[i]
    
    alpha <-
      tail_probabilities[i]
    
    z_alpha <-
      stats::qnorm(alpha)
    
    normal_density <-
      stats::dnorm(z_alpha)
    
    gaussian_es <-
      -portfolio_mean_vcv +
      portfolio_volatility_vcv *
      normal_density /
      alpha
    
    gaussian_var <-
      -(
        portfolio_mean_vcv +
          z_alpha *
          portfolio_volatility_vcv
      )
    
    tibble::tibble(
      model = "Parametric Gaussian",
      confidence_level = confidence,
      tail_probability = alpha,
      
      var_threshold_return =
        gaussian_var,
      
      es_return =
        gaussian_es,
      
      es_dollar =
        portfolio_value *
        gaussian_es,
      
      tail_observations =
        NA_integer_,
      
      worst_tail_return =
        NA_real_,
      
      average_tail_return =
        NA_real_
    )
  }
)

# ============================================================
# Combine Expected Shortfall results
# ============================================================

es_results <- dplyr::bind_rows(
  historical_es_calculation,
  parametric_es_calculation
) |>
  dplyr::arrange(
    confidence_level,
    model
  )

# ============================================================
# Historical tail summary
# ============================================================

historical_tail_summary <-
  historical_es_calculation |>
  dplyr::select(
    confidence_level,
    tail_probability,
    tail_observations,
    var_threshold_return,
    average_tail_return,
    worst_tail_return,
    es_return,
    es_dollar
  )

# ============================================================
# Combine VaR and ES for comparison
# ============================================================

var_es_comparison <- var_results |>
  dplyr::select(
    model,
    confidence_level,
    var_return,
    var_dollar
  ) |>
  dplyr::left_join(
    es_results |>
      dplyr::select(
        model,
        confidence_level,
        es_return,
        es_dollar
      ),
    by = c(
      "model",
      "confidence_level"
    )
  ) |>
  dplyr::mutate(
    es_minus_var_return =
      es_return - var_return,
    
    es_to_var_ratio =
      es_return / var_return
  ) |>
  dplyr::arrange(
    confidence_level,
    model
  )

# ============================================================
# Validation checks
# ============================================================

if (
  any(
    !is.finite(
      es_results$es_return
    )
  )
) {
  stop(
    "Non-finite Expected Shortfall estimates were detected."
  )
}

if (
  any(
    !is.finite(
      es_results$es_dollar
    )
  )
) {
  stop(
    "Non-finite dollar Expected Shortfall estimates were detected."
  )
}

# ES should normally be at least as large as VaR
# for the loss distributions considered here.

if (
  any(
    var_es_comparison$es_return <
    var_es_comparison$var_return -
    1e-12
  )
) {
  warning(
    paste(
      "At least one Expected Shortfall estimate",
      "is below the corresponding VaR estimate.",
      "Review the tail calculation."
    )
  )
}

# 99% ES should normally exceed 95% ES
# because it averages a more extreme tail.

es_confidence_check <- es_results |>
  dplyr::select(
    model,
    confidence_level,
    es_return
  ) |>
  tidyr::pivot_wider(
    names_from = confidence_level,
    values_from = es_return,
    names_prefix = "confidence_"
  )

# ============================================================
# Save outputs
# ============================================================

readr::write_csv(
  es_results,
  es_results_path
)

readr::write_csv(
  historical_tail_summary,
  tail_observations_path
)

readr::write_csv(
  var_es_comparison,
  var_es_comparison_path
)

# ============================================================
# Display results
# ============================================================

print(
  es_results |>
    dplyr::select(
      model,
      confidence_level,
      es_return,
      es_dollar,
      tail_observations
    ) |>
    dplyr::mutate(
      confidence_level =
        scales::percent(
          confidence_level,
          accuracy = 1
        ),
      
      es_return =
        scales::percent(
          es_return,
          accuracy = 0.01
        ),
      
      es_dollar =
        scales::dollar(
          es_dollar,
          prefix = "CAD $"
        )
    )
)

cat(
  "\nHistorical tail observations:\n"
)

print(
  historical_tail_summary |>
    dplyr::select(
      confidence_level,
      tail_observations,
      var_threshold_return,
      es_return
    )
)

message(
  "Expected Shortfall estimation completed successfully."
)