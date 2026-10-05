library(tidyverse)
library(janitor)

dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)

prepare_mortality <- function(file, outcome_name) {

  dat <- read_csv(file, show_col_types = FALSE) |>
    clean_names()

  # World Bank files often use country_code.
  # Rename it to iso for easier merging later.
  if ("country_code" %in% names(dat)) {
    dat <- dat |>
      rename(iso = country_code)
  }

  dat_long <- dat |>
    pivot_longer(
      cols = matches("^x?[0-9]{4}$"),
      names_to = "year",
      values_to = outcome_name
    ) |>
    mutate(
      year = as.integer(str_extract(year, "[0-9]{4}")),
      iso = toupper(iso)
    ) |>
    filter(year >= 2000, year <= 2019) |>
    select(iso, year, all_of(outcome_name))

  return(dat_long)
}


# Apply the function to all four mortality data sets

maternal <- prepare_mortality(
  "data/raw/maternal_mortality.csv",
  "maternal_mortality"
)

infant <- prepare_mortality(
  "../data/raw/infant_mortality.csv",
  "infant_mortality"
)

neonatal <- prepare_mortality(
  "../data/raw/neonatal_mortality.csv",
  "neonatal_mortality"
)

under5 <- prepare_mortality(
  "../data/raw/under5_mortality.csv",
  "under5_mortality"
)