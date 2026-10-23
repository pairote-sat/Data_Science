# ============================================================
# Generate synthetic data for the tidyr lecture
# ============================================================

library(tidyverse)

set.seed(295)

# Create data folder if it does not already exist
dir.create("data", showWarnings = FALSE)


# ------------------------------------------------------------
# 1. Column headers are values
#    Example: annual claim counts
#    Use pivot_longer() to tidy
# ------------------------------------------------------------

n <- 100

policy_base <- tibble(
  policy_id = 1001:(1000 + n),
  risk_group = sample(
    c("Young", "Middle", "Older"),
    n,
    replace = TRUE,
    prob = c(0.30, 0.45, 0.25)
  ),
  vehicle_type = sample(
    c("Car", "SUV", "Pickup"),
    n,
    replace = TRUE,
    prob = c(0.55, 0.25, 0.20)
  )
)

claim_rate <- case_when(
  policy_base$risk_group == "Young" ~ 0.14,
  policy_base$risk_group == "Middle" ~ 0.09,
  TRUE ~ 0.07
)

exposure <- round(runif(n, 0.5, 1), 2)

claims_by_year <- policy_base |>
  mutate(exposure = exposure) |>
  mutate(
    `2022` = rpois(n, claim_rate * exposure),
    `2023` = rpois(n, claim_rate * exposure),
    `2024` = rpois(n, claim_rate * exposure * 1.03),
    `2025` = rpois(n, claim_rate * exposure * 1.06),
    `2026` = rpois(n, claim_rate * exposure * 1.09)
  )

write_csv(
  claims_by_year,
  "data/tidyr_01_claims_by_year.csv"
)


# ------------------------------------------------------------
# 2. Multiple variables stored in one column
#    Example: policy characteristics
#    Use pivot_wider() to tidy
# ------------------------------------------------------------

policy_measures <- policy_base |>
  mutate(
    age = case_when(
      risk_group == "Young" ~ sample(18:30, n(), replace = TRUE),
      risk_group == "Middle" ~ sample(31:55, n(), replace = TRUE),
      TRUE ~ sample(56:75, n(), replace = TRUE)
    ),
    exposure = round(runif(n(), 0.5, 1), 2),
    nclaims = rpois(
      n(),
      case_when(
        risk_group == "Young" ~ 0.14,
        risk_group == "Middle" ~ 0.09,
        TRUE ~ 0.07
      ) * exposure
    ),
    premium = round(
      10000 * exposure *
        case_when(
          risk_group == "Young" ~ 1.25,
          risk_group == "Middle" ~ 1.00,
          TRUE ~ 0.90
        )
    )
  ) |>
  pivot_longer(
    cols = c(age, exposure, nclaims, premium),
    names_to = "measure",
    values_to = "value"
  )

write_csv(
  policy_measures,
  "data/tidyr_02_policy_measures.csv"
)


# ------------------------------------------------------------
# 3. Multiple variables stored in column names
#    Example: claim count and claim amount by year
#    Use pivot_longer() and split the column names
# ------------------------------------------------------------

claims_year_measure <- policy_base |>
  mutate(
    `2023_count` = rpois(n, claim_rate),
    `2023_amount` = if_else(
      `2023_count` > 0,
      round(`2023_count` * rgamma(n, shape = 2, rate = 1 / 12000)),
      0
    ),
    `2024_count` = rpois(n, claim_rate),
    `2024_amount` = if_else(
      `2024_count` > 0,
      round(`2024_count` * rgamma(n, shape = 2, rate = 1 / 12000)),
      0
    ),
    `2025_count` = rpois(n, claim_rate),
    `2025_amount` = if_else(
      `2025_count` > 0,
      round(`2025_count` * rgamma(n, shape = 2, rate = 1 / 12000)),
      0
    ),
    `2026_count` = rpois(n, claim_rate),
    `2026_amount` = if_else(
      `2026_count` > 0,
      round(`2026_count` * rgamma(n, shape = 2, rate = 1 / 12000)),
      0
    )
  )

write_csv(
  claims_year_measure,
  "data/tidyr_03_claims_year_measure.csv"
)


# ------------------------------------------------------------
# 4. Variables stored in both rows and columns
#    Example: portfolio summary by risk group
#    Requires more than one reshaping idea
# ------------------------------------------------------------

portfolio_summary <- expand_grid(
  risk_group = c("Young", "Middle", "Older"),
  measure = c("exposure", "nclaims", "premium")
) |>
  mutate(
    `2024` = case_when(
      measure == "exposure" ~ round(runif(n(), 80, 150), 1),
      measure == "nclaims" ~ sample(5:25, n(), replace = TRUE),
      measure == "premium" ~ sample(150000:300000, n(), replace = TRUE)
    ),
    `2025` = case_when(
      measure == "exposure" ~ round(`2024` * runif(n(), 1.02, 1.10), 1),
      measure == "nclaims" ~ sample(5:28, n(), replace = TRUE),
      measure == "premium" ~ sample(160000:320000, n(), replace = TRUE)
    ),
    `2026` = case_when(
      measure == "exposure" ~ round(`2025` * runif(n(), 1.02, 1.10), 1),
      measure == "nclaims" ~ sample(5:30, n(), replace = TRUE),
      measure == "premium" ~ sample(170000:340000, n(), replace = TRUE)
    )
  )

write_csv(
  portfolio_summary,
  "data/tidyr_04_portfolio_summary.csv"
)
