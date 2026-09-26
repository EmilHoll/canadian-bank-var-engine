# ============================================================
# Canadian Bank Market Risk VaR Engine
# Model Comparison and Risk Dashboard
# ============================================================


# ============================================================
# Load project setup if needed
# ============================================================

if (!exists("required_packages")) {
  source(
    here::here(
      "R",
      "00_project_setup.R"
    )
  )
}


# ============================================================
# Load portfolio definition if needed
# ============================================================

if (!exists("portfolio_definition")) {
  source(
    here::here(
      "R",
      "01_define_portfolio.R"
    )
  )
}


# ============================================================
# Define input paths
# ============================================================

dynamic_var_path <- here::here(
  "data",
  "processed",
  "dynamic_var_forecasts.rds"
)

dynamic_volatility_path <- here::here(
  "data",
  "processed",
  "dynamic_volatility.rds"
)

backtest_summary_path <- here::here(
  "output",
  "tables",
  "var_backtest_summary.csv"
)

validation_summary_path <- here::here(
  "output",
  "tables",
  "var_model_validation_summary.csv"
)

es_results_path <- here::here(
  "output",
  "tables",
  "expected_shortfall_estimates.csv"
)

historical_stress_path <- here::here(
  "output",
  "tables",
  "historical_stress_results.csv"
)

hypothetical_stress_path <- here::here(
  "output",
  "tables",
  "hypothetical_stress_results.csv"
)

empirical_extreme_path <- here::here(
  "output",
  "tables",
  "empirical_extreme_events.csv"
)


# ============================================================
# Define output paths
# ============================================================

model_comparison_output_path <- here::here(
  "output",
  "tables",
  "model_comparison_dashboard.csv"
)

latest_var_output_path <- here::here(
  "output",
  "tables",
  "dashboard_latest_var.csv"
)

risk_snapshot_output_path <- here::here(
  "output",
  "tables",
  "current_risk_snapshot.csv"
)

stress_comparison_output_path <- here::here(
  "output",
  "tables",
  "dashboard_stress_comparison.csv"
)


# ============================================================
# Confirm required files exist
# ============================================================

required_files <- c(
  dynamic_var_path,
  dynamic_volatility_path,
  backtest_summary_path,
  validation_summary_path,
  es_results_path,
  historical_stress_path,
  hypothetical_stress_path,
  empirical_extreme_path
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  
  stop(
    paste(
      "The following required project outputs are missing:",
      paste(
        missing_files,
        collapse = "\n"
      ),
      "\nRun the corresponding earlier steps first."
    )
  )
}


# ============================================================
# Load saved model outputs
# ============================================================

dynamic_var_forecasts_dashboard <-
  readRDS(
    dynamic_var_path
  )

dynamic_volatility_dashboard <-
  readRDS(
    dynamic_volatility_path
  )

var_backtest_summary_dashboard <-
  readr::read_csv(
    backtest_summary_path,
    show_col_types = FALSE
  )

validation_summary_dashboard <-
  readr::read_csv(
    validation_summary_path,
    show_col_types = FALSE
  )

es_results_dashboard <-
  readr::read_csv(
    es_results_path,
    show_col_types = FALSE
  )

historical_stress_dashboard <-
  readr::read_csv(
    historical_stress_path,
    show_col_types = FALSE
  )

hypothetical_stress_dashboard <-
  readr::read_csv(
    hypothetical_stress_path,
    show_col_types = FALSE
  )

empirical_extreme_dashboard <-
  readr::read_csv(
    empirical_extreme_path,
    show_col_types = FALSE
  )


# ============================================================
# Latest dynamic VaR estimates
# ============================================================

latest_var_date <-
  max(
    dynamic_var_forecasts_dashboard$date
  )

latest_var_dashboard <-
  dynamic_var_forecasts_dashboard |>
  dplyr::filter(
    date == latest_var_date
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
# Latest dynamic volatility estimates
# ============================================================

latest_volatility <-
  dynamic_volatility_dashboard |>
  dplyr::arrange(
    date
  ) |>
  dplyr::slice_tail(
    n = 1
  )

latest_ewma_volatility <-
  latest_volatility$
  ewma_annualized_volatility

latest_garch_volatility <-
  latest_volatility$
  garch_annualized_volatility


# ============================================================
# Model-validation flags
# ============================================================

model_validation_flags <-
  validation_summary_dashboard |>
  dplyr::mutate(
    
    kupiec_pass =
      !is.na(kupiec_p_value) &
      kupiec_p_value >= 0.05,
    
    independence_pass =
      !is.na(independence_p_value) &
      independence_p_value >= 0.05,
    
    conditional_coverage_pass =
      !is.na(
        conditional_coverage_p_value
      ) &
      conditional_coverage_p_value >= 0.05,
    
    statistical_tests_passed =
      as.integer(kupiec_pass) +
      as.integer(independence_pass) +
      as.integer(
        conditional_coverage_pass
      ),
    
    validation_assessment =
      dplyr::case_when(
        
        statistical_tests_passed == 3 ~
          "Strong statistical support",
        
        statistical_tests_passed == 2 ~
          "Generally acceptable",
        
        statistical_tests_passed == 1 ~
          "Mixed evidence",
        
        TRUE ~
          "Weak statistical support"
      )
  )


# ============================================================
# Combine statistical validation with economic metrics
# ============================================================

model_comparison_dashboard <-
  var_backtest_summary_dashboard |>
  dplyr::left_join(
    
    model_validation_flags |>
      dplyr::select(
        
        model,
        confidence_level,
        
        kupiec_p_value,
        independence_p_value,
        conditional_coverage_p_value,
        
        statistical_tests_passed,
        validation_assessment
      ),
    
    by = c(
      "model",
      "confidence_level"
    )
  ) |>
  dplyr::mutate(
    
    violation_difference =
      actual_violations -
      expected_violations,
    
    violation_rate_difference =
      actual_violation_rate -
      expected_violation_rate,
    
    absolute_violation_ratio_distance =
      abs(
        violation_ratio - 1
      )
  ) |>
  dplyr::arrange(
    confidence_level,
    dplyr::desc(
      statistical_tests_passed
    ),
    absolute_violation_ratio_distance
  )


# ============================================================
# Stress-loss comparison
# ============================================================

historical_stress_losses <-
  historical_stress_dashboard |>
  dplyr::transmute(
    
    scenario =
      scenario,
    
    stress_type =
      "Historical",
    
    stress_loss =
      stress_loss
  )


hypothetical_stress_losses <-
  hypothetical_stress_dashboard |>
  dplyr::transmute(
    
    scenario =
      scenario,
    
    stress_type =
      "Hypothetical",
    
    stress_loss =
      stress_loss
  )


empirical_stress_losses <-
  empirical_extreme_dashboard |>
  dplyr::transmute(
    
    scenario =
      scenario,
    
    stress_type =
      "Empirical Extreme",
    
    stress_loss =
      stress_loss
  )


stress_comparison_dashboard <-
  dplyr::bind_rows(
    
    historical_stress_losses,
    hypothetical_stress_losses,
    empirical_stress_losses
    
  ) |>
  dplyr::arrange(
    dplyr::desc(
      stress_loss
    )
  )


# ============================================================
# Identify largest stress loss
# ============================================================

largest_stress_event <-
  stress_comparison_dashboard |>
  dplyr::slice_max(
    stress_loss,
    n = 1,
    with_ties = FALSE
  )


largest_stress_loss <-
  largest_stress_event$
  stress_loss


# ============================================================
# Expected Shortfall snapshot
# ============================================================

historical_es_99 <-
  es_results_dashboard |>
  dplyr::filter(
    model == "Historical Simulation",
    confidence_level == 0.99
  ) |>
  dplyr::pull(
    es_dollar
  )


gaussian_es_99 <-
  es_results_dashboard |>
  dplyr::filter(
    model == "Parametric Gaussian",
    confidence_level == 0.99
  ) |>
  dplyr::pull(
    es_dollar
  )


# ============================================================
# Latest 99% GARCH VaR
# ============================================================

latest_garch_var_99 <-
  latest_var_dashboard |>
  dplyr::filter(
    model == "GARCH(1,1) Gaussian",
    confidence_level == 0.99
  ) |>
  dplyr::pull(
    var_dollar
  )


# ============================================================
# Latest 99% Historical VaR
# ============================================================

latest_historical_var_99 <-
  latest_var_dashboard |>
  dplyr::filter(
    model == "Historical Simulation",
    confidence_level == 0.99
  ) |>
  dplyr::pull(
    var_dollar
  )


# ============================================================
# Validate dashboard values
# ============================================================

if (length(latest_garch_var_99) != 1) {
  stop(
    "Could not identify exactly one latest 99% GARCH VaR estimate."
  )
}

if (length(latest_historical_var_99) != 1) {
  stop(
    "Could not identify exactly one latest 99% Historical VaR estimate."
  )
}

if (length(historical_es_99) != 1) {
  stop(
    "Could not identify exactly one 99% Historical ES estimate."
  )
}

if (length(gaussian_es_99) != 1) {
  stop(
    "Could not identify exactly one 99% Gaussian ES estimate."
  )
}


# ============================================================
# Build current risk snapshot
# ============================================================

current_risk_snapshot <-
  tibble::tibble(
    
    risk_measure = c(
      
      "Portfolio Value",
      
      "Latest EWMA Annualized Volatility",
      
      "Latest GARCH Annualized Volatility",
      
      "Latest 99% Historical VaR",
      
      "Latest 99% GARCH VaR",
      
      "Full-Sample 99% Historical ES",
      
      "Full-Sample 99% Gaussian ES",
      
      "Largest Stress Loss"
    ),
    
    value = c(
      
      portfolio_value,
      
      latest_ewma_volatility,
      
      latest_garch_volatility,
      
      latest_historical_var_99,
      
      latest_garch_var_99,
      
      historical_es_99,
      
      gaussian_es_99,
      
      largest_stress_loss
    ),
    
    unit = c(
      
      "CAD",
      
      "Percent",
      
      "Percent",
      
      "CAD",
      
      "CAD",
      
      "CAD",
      
      "CAD",
      
      "CAD"
    )
  )


# ============================================================
# Model ranking within confidence level
#
# This is descriptive only.
# It is NOT intended to mechanically select a model.
# ============================================================

model_comparison_dashboard <-
  model_comparison_dashboard |>
  dplyr::group_by(
    confidence_level
  ) |>
  dplyr::arrange(
    
    dplyr::desc(
      statistical_tests_passed
    ),
    
    absolute_violation_ratio_distance,
    
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    
    descriptive_rank =
      dplyr::row_number()
    
  ) |>
  dplyr::ungroup()


# ============================================================
# Save dashboard outputs
# ============================================================

readr::write_csv(
  model_comparison_dashboard,
  model_comparison_output_path
)

readr::write_csv(
  latest_var_dashboard,
  latest_var_output_path
)

readr::write_csv(
  current_risk_snapshot,
  risk_snapshot_output_path
)

readr::write_csv(
  stress_comparison_dashboard,
  stress_comparison_output_path
)


# ============================================================
# Display dashboard results
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "CANADIAN BANK MARKET RISK DASHBOARD\n"
)

cat(
  "============================================================\n\n"
)

cat(
  "Latest VaR forecast date:",
  format(
    latest_var_date
  ),
  "\n\n"
)


cat(
  "Latest EWMA annualized volatility:",
  scales::percent(
    latest_ewma_volatility,
    accuracy = 0.01
  ),
  "\n"
)


cat(
  "Latest GARCH annualized volatility:",
  scales::percent(
    latest_garch_volatility,
    accuracy = 0.01
  ),
  "\n\n"
)


cat(
  "Latest 99% Historical VaR:",
  scales::dollar(
    latest_historical_var_99,
    prefix = "CAD $"
  ),
  "\n"
)


cat(
  "Latest 99% GARCH VaR:",
  scales::dollar(
    latest_garch_var_99,
    prefix = "CAD $"
  ),
  "\n\n"
)


cat(
  "Full-Sample 99% Historical ES:",
  scales::dollar(
    historical_es_99,
    prefix = "CAD $"
  ),
  "\n"
)


cat(
  "Largest stress loss:",
  scales::dollar(
    largest_stress_loss,
    prefix = "CAD $"
  ),
  "\n"
)


cat(
  "Largest stress scenario:",
  largest_stress_event$scenario,
  "\n\n"
)


cat(
  "Model Validation Summary\n"
)

print(
  model_comparison_dashboard |>
    dplyr::select(
      
      model,
      confidence_level,
      
      expected_violations,
      actual_violations,
      
      violation_ratio,
      
      statistical_tests_passed,
      
      validation_assessment,
      
      descriptive_rank
    )
)


message(
  "Risk dashboard and model comparison completed successfully."
)