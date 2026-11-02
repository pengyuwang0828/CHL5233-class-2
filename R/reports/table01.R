library(tidyverse)
library(gtsummary)
library(flextable)

# Read final data
final_data <- readr::read_csv(
  "data/processed/final_data.csv",
  show_col_types = FALSE
)

# Use the common study period 2000-2017
table1_data <- final_data |>
  filter(year <= 2017) |>
  mutate(
    armed_conflict_lag1 = factor(
      armed_conflict_lag1,
      levels = c(0, 1),
      labels = c("No armed conflict", "Armed conflict")
    ),
    oecd = factor(
      oecd,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    earthquake = factor(
      earthquake,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    drought = factor(
      drought,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# Create Table 1
table1_output <- table1_data |>
  tbl_summary(
    by = armed_conflict_lag1,

    include = c(
      maternal_mortality,
      under5_mortality,
      infant_mortality,
      neonatal_mortality,
      gdp_1000,
      oecd,
      pop_dens,
      urban,
      age_dep,
      male_edu,
      temp,
      rainfall_1000,
      earthquake,
      drought
    ),

    label = list(
      maternal_mortality ~ "Maternal mortality",
      under5_mortality ~ "Under-5 mortality",
      infant_mortality ~ "Infant mortality",
      neonatal_mortality ~ "Neonatal mortality",
      gdp_1000 ~ "GDP per capita (1000s)",
      oecd ~ "OECD member",
      pop_dens ~ "Population density",
      urban ~ "Urban population",
      age_dep ~ "Age dependency",
      male_edu ~ "Male education",
      temp ~ "Temperature",
      rainfall_1000 ~ "Rainfall (1000s)",
      earthquake ~ "Earthquake",
      drought ~ "Drought"
    ),

    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),

    missing = "always",
    missing_text = "Missing",
    missing_stat = "{N_miss}"
  ) |>
  add_overall(last = FALSE) |>
  bold_labels()

# Convert to Word-friendly table
table1_word <- as_flex_table(table1_output)

# Save Word file
flextable::save_as_docx(
  "Table 1" = table1_word,
  path = "R/reports/table1.docx"
)

### Coding agent suggest me using label function to cahnge the varable name and can be more readable.