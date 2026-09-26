# ============================================================
# Canadian Bank Market Risk VaR Engine
# Statistical VaR Backtesting Tests
#
# Tests:
# 1. Kupiec Unconditional Coverage
# 2. Christoffersen Independence
# 3. Christoffersen Conditional Coverage
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
# Load dynamic VaR forecasts if needed
# ============================================================

if (!exists("dynamic_var_forecasts")) {
  source(
    here::here(
      "R",
      "08_dynamic_var_backtesting.R"
    )
  )
}


# ============================================================
# Output paths
# ============================================================

backtesting_tests_path <- here::here(
  "output",
  "tables",
  "var_backtesting_tests.csv"
)

transition_counts_path <- here::here(
  "output",
  "tables",
  "var_violation_transition_counts.csv"
)

model_validation_summary_path <- here::here(
  "output",
  "tables",
  "var_model_validation_summary.csv"
)


# ============================================================
# Test configuration
# ============================================================

test_significance_level <- 0.05


# ============================================================
# Helper: numerically safe binomial log-likelihood
# ============================================================

binomial_log_likelihood <- function(
    successes,
    failures,
    probability
) {
  
  if (
    is.na(probability) ||
    probability < 0 ||
    probability > 1
  ) {
    return(NA_real_)
  }
  
  success_term <- if (
    successes == 0
  ) {
    
    0
    
  } else if (
    probability == 0
  ) {
    
    -Inf
    
  } else {
    
    successes *
      log(probability)
  }
  
  failure_term <- if (
    failures == 0
  ) {
    
    0
    
  } else if (
    probability == 1
  ) {
    
    -Inf
    
  } else {
    
    failures *
      log1p(
        -probability
      )
  }
  
  success_term +
    failure_term
}


# ============================================================
# Kupiec Proportion-of-Failures Test
# ============================================================

kupiec_test <- function(
    violations,
    expected_probability
) {
  
  violations <-
    as.integer(
      violations
    )
  
  observations <-
    length(
      violations
    )
  
  actual_violations <-
    sum(
      violations
    )
  
  non_violations <-
    observations -
    actual_violations
  
  observed_probability <-
    actual_violations /
    observations
  
  log_likelihood_null <-
    binomial_log_likelihood(
      successes =
        actual_violations,
      
      failures =
        non_violations,
      
      probability =
        expected_probability
    )
  
  log_likelihood_alternative <-
    binomial_log_likelihood(
      successes =
        actual_violations,
      
      failures =
        non_violations,
      
      probability =
        observed_probability
    )
  
  lr_uc <-
    -2 *
    (
      log_likelihood_null -
        log_likelihood_alternative
    )
  
  # Protect against tiny negative values
  # caused by floating-point rounding.
  
  lr_uc <-
    max(
      0,
      lr_uc
    )
  
  p_value_uc <-
    stats::pchisq(
      lr_uc,
      df = 1,
      lower.tail = FALSE
    )
  
  tibble::tibble(
    
    observations =
      observations,
    
    expected_violation_probability =
      expected_probability,
    
    expected_violations =
      observations *
      expected_probability,
    
    actual_violations =
      actual_violations,
    
    actual_violation_probability =
      observed_probability,
    
    kupiec_lr =
      lr_uc,
    
    kupiec_p_value =
      p_value_uc
  )
}


# ============================================================
# Christoffersen Independence Test
# ============================================================

christoffersen_independence_test <- function(
    violations
) {
  
  violations <-
    as.integer(
      violations
    )
  
  if (
    length(violations) < 2
  ) {
    
    return(
      tibble::tibble(
        n00 = NA_integer_,
        n01 = NA_integer_,
        n10 = NA_integer_,
        n11 = NA_integer_,
        pi01 = NA_real_,
        pi11 = NA_real_,
        christoffersen_independence_lr =
          NA_real_,
        christoffersen_independence_p_value =
          NA_real_
      )
    )
  }
  
  previous <-
    violations[
      -length(
        violations
      )
    ]
  
  current <-
    violations[
      -1
    ]
  
  n00 <-
    sum(
      previous == 0 &
        current == 0
    )
  
  n01 <-
    sum(
      previous == 0 &
        current == 1
    )
  
  n10 <-
    sum(
      previous == 1 &
        current == 0
    )
  
  n11 <-
    sum(
      previous == 1 &
        current == 1
    )
  
  denominator_0 <-
    n00 + n01
  
  denominator_1 <-
    n10 + n11
  
  # If one type of previous state never occurs,
  # transition probabilities cannot be separately
  # identified.
  
  if (
    denominator_0 == 0 ||
    denominator_1 == 0
  ) {
    
    return(
      tibble::tibble(
        
        n00 = n00,
        n01 = n01,
        n10 = n10,
        n11 = n11,
        
        pi01 = NA_real_,
        pi11 = NA_real_,
        
        christoffersen_independence_lr =
          NA_real_,
        
        christoffersen_independence_p_value =
          NA_real_
      )
    )
  }
  
  pi01 <-
    n01 /
    denominator_0
  
  pi11 <-
    n11 /
    denominator_1
  
  total_transitions <-
    n00 +
    n01 +
    n10 +
    n11
  
  unconditional_transition_probability <-
    (
      n01 + n11
    ) /
    total_transitions
  
  
  # --------------------------------------------
  # Null: same violation probability regardless
  # of the previous day's state
  # --------------------------------------------
  
  null_log_likelihood <-
    binomial_log_likelihood(
      
      successes =
        n01 + n11,
      
      failures =
        n00 + n10,
      
      probability =
        unconditional_transition_probability
    )
  
  
  # --------------------------------------------
  # Alternative:
  # separate P(hit | previous no-hit)
  # and P(hit | previous hit)
  # --------------------------------------------
  
  alternative_log_likelihood <-
    
    binomial_log_likelihood(
      successes =
        n01,
      
      failures =
        n00,
      
      probability =
        pi01
    ) +
    
    binomial_log_likelihood(
      successes =
        n11,
      
      failures =
        n10,
      
      probability =
        pi11
    )
  
  
  lr_independence <-
    -2 *
    (
      null_log_likelihood -
        alternative_log_likelihood
    )
  
  lr_independence <-
    max(
      0,
      lr_independence
    )
  
  p_value_independence <-
    stats::pchisq(
      lr_independence,
      df = 1,
      lower.tail = FALSE
    )
  
  
  tibble::tibble(
    
    n00 = n00,
    n01 = n01,
    n10 = n10,
    n11 = n11,
    
    pi01 = pi01,
    pi11 = pi11,
    
    christoffersen_independence_lr =
      lr_independence,
    
    christoffersen_independence_p_value =
      p_value_independence
  )
}


# ============================================================
# Apply tests to each model/confidence combination
# ============================================================

backtest_groups <-
  dynamic_var_forecasts |>
  dplyr::group_by(
    model,
    confidence_level
  ) |>
  dplyr::group_split()


var_backtesting_tests <-
  purrr::map_dfr(
    backtest_groups,
    function(group_data) {
      
      group_data <-
        group_data |>
        dplyr::arrange(
          date
        )
      
      model_name <-
        group_data$model[1]
      
      confidence_level <-
        group_data$confidence_level[1]
      
      expected_probability <-
        1 -
        confidence_level
      
      violations <-
        group_data$violation
      
      
      # ------------------------------------------
      # Kupiec test
      # ------------------------------------------
      
      kupiec_results <-
        kupiec_test(
          violations =
            violations,
          
          expected_probability =
            expected_probability
        )
      
      
      # ------------------------------------------
      # Christoffersen independence
      # ------------------------------------------
      
      independence_results <-
        christoffersen_independence_test(
          violations =
            violations
        )
      
      
      # ------------------------------------------
      # Conditional coverage
      # ------------------------------------------
      
      if (
        is.finite(
          kupiec_results$kupiec_lr
        ) &&
        is.finite(
          independence_results$
          christoffersen_independence_lr
        )
      ) {
        
        conditional_coverage_lr <-
          kupiec_results$kupiec_lr +
          independence_results$
          christoffersen_independence_lr
        
        conditional_coverage_p_value <-
          stats::pchisq(
            conditional_coverage_lr,
            df = 2,
            lower.tail = FALSE
          )
        
      } else {
        
        conditional_coverage_lr <-
          NA_real_
        
        conditional_coverage_p_value <-
          NA_real_
      }
      
      
      # ------------------------------------------
      # Combine results
      # ------------------------------------------
      
      dplyr::bind_cols(
        
        tibble::tibble(
          model =
            model_name,
          
          confidence_level =
            confidence_level
        ),
        
        kupiec_results,
        
        independence_results,
        
        tibble::tibble(
          
          conditional_coverage_lr =
            conditional_coverage_lr,
          
          conditional_coverage_p_value =
            conditional_coverage_p_value
        )
      )
    }
  )


# ============================================================
# Add pass / reject indicators
# ============================================================

var_backtesting_tests <-
  var_backtesting_tests |>
  dplyr::mutate(
    
    kupiec_result =
      dplyr::if_else(
        kupiec_p_value <
          test_significance_level,
        
        "Reject",
        
        "Do not reject"
      ),
    
    independence_result =
      dplyr::case_when(
        
        is.na(
          christoffersen_independence_p_value
        ) ~
          "Not available",
        
        christoffersen_independence_p_value <
          test_significance_level ~
          "Reject",
        
        TRUE ~
          "Do not reject"
      ),
    
    conditional_coverage_result =
      dplyr::case_when(
        
        is.na(
          conditional_coverage_p_value
        ) ~
          "Not available",
        
        conditional_coverage_p_value <
          test_significance_level ~
          "Reject",
        
        TRUE ~
          "Do not reject"
      )
  ) |>
  dplyr::arrange(
    confidence_level,
    model
  )


# ============================================================
# Transition-count table
# ============================================================

violation_transition_counts <-
  var_backtesting_tests |>
  dplyr::select(
    
    model,
    confidence_level,
    
    n00,
    n01,
    n10,
    n11,
    
    pi01,
    pi11
  )


# ============================================================
# Compact model-validation summary
# ============================================================

var_model_validation_summary <-
  var_backtesting_tests |>
  dplyr::transmute(
    
    model =
      model,
    
    confidence_level =
      confidence_level,
    
    expected_violations =
      expected_violations,
    
    actual_violations =
      actual_violations,
    
    kupiec_p_value =
      kupiec_p_value,
    
    independence_p_value =
      christoffersen_independence_p_value,
    
    conditional_coverage_p_value =
      conditional_coverage_p_value,
    
    unconditional_coverage =
      kupiec_result,
    
    independence =
      independence_result,
    
    conditional_coverage =
      conditional_coverage_result
  )


# ============================================================
# Save outputs
# ============================================================

readr::write_csv(
  var_backtesting_tests,
  backtesting_tests_path
)

readr::write_csv(
  violation_transition_counts,
  transition_counts_path
)

readr::write_csv(
  var_model_validation_summary,
  model_validation_summary_path
)


# ============================================================
# Display results
# ============================================================

print(
  var_model_validation_summary |>
    dplyr::mutate(
      
      confidence_level =
        scales::percent(
          confidence_level,
          accuracy = 1
        ),
      
      kupiec_p_value =
        round(
          kupiec_p_value,
          4
        ),
      
      independence_p_value =
        round(
          independence_p_value,
          4
        ),
      
      conditional_coverage_p_value =
        round(
          conditional_coverage_p_value,
          4
        )
    )
)


message(
  "Statistical VaR backtesting completed successfully."
)