# ============================================================
# Canadian Bank Market Risk VaR Engine
# Project configuration
# ============================================================

required_packages <- c(
  "here",
  "tidyverse",
  "tidyquant",
  "quantmod",
  "xts",
  "zoo",
  "lubridate",
  "PerformanceAnalytics",
  "scales"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste(
      "The following packages are missing:",
      paste(missing_packages, collapse = ", "),
      "\nInstall them using renv::install()."
    )
  )
}

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  dplyr.summarise.inform = FALSE
)

set.seed(2026)

ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 12)
)

message("Market Risk VaR Engine setup loaded successfully.")