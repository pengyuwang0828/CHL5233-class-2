library(tidyverse)

# Read final data
final_data <- readr::read_csv(
  "data/processed/final_data.csv",
  show_col_types = FALSE
)

# Prepare maternal mortality data
matmor <- final_data |>
  select(
    iso,
    year,
    maternal_mortality
  ) |>
  filter(year < 2018) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(
    matmor_2000 = maternal_mortality[year == 2000][1],
    diff_matmor = maternal_mortality - matmor_2000
  ) |>
  ungroup()

# Select countries where maternal mortality increased
# between 2000 and 2017
increase_countries <- matmor |>
  filter(
    year == 2017,
    diff_matmor > 0
  ) |>
  pull(iso)

# Keep all years for those countries
figure_data <- matmor |>
  filter(iso %in% increase_countries)

# Check selected countries
increase_countries

maternal_plot <- ggplot(
  figure_data,
  aes(
    x = year,
    y = maternal_mortality,
    color = iso,
    group = iso
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    title = "Maternal Mortality Trends in Countries with an Increase from 2000 to 2017",
    x = "Year",
    y = "Maternal mortality",
    color = "Country"
  ) +
  theme_minimal()

maternal_plot

ggsave(
  filename = "R/reports/fig01_maternal_mortality.png",
  plot = maternal_plot,
  width = 10,
  height = 7,
  dpi = 300
)