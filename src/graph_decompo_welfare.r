options(repos = c(CRAN = "https://cloud.r-project.org/"))

install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("readr")
install.packages("tidyr")
install.packages("tibble")
install.packages("brant")
install.packages("countrycode")
library(countrycode)
library(brant)
library(tibble)
library(ggplot2)
library(tidyr)
library(readxl)
library(dplyr)
library(readr)
library(MASS)
library(scales)

rm(list = ls())

# Import welfare gains decomposition data
welfare_data <- read_csv("cap_and_share/output/welfare_gains_2050_global_cap_share_vs_bau.csv")

cols_to_percent <- c(
  "damages_avoided",
  "transfer_diff",
  "growth",
  "abat_cost",
  "reduction_inequalities",
  "residual_tot",
  "total_welfare_gains"
)

welfare_data <- welfare_data %>%
  mutate(across(all_of(cols_to_percent), ~ .x * 100))

welfare_data <- welfare_data %>%
  mutate(
    Country_name = countrycode(
      Country,
      origin = "iso3c",
      destination = "country.name"
    )
  )

welfare_data <- welfare_data %>%
  mutate(
    Country_name = if_else(
      Country == "COD",
      "D.R.C",
      Country_name
    )
  )
welfare_data <- welfare_data %>%
  mutate(
    Country_name = if_else(
      Country == "Global",
      "Global",
      Country_name
    )
  )

welfare_data <- welfare_data %>%
  mutate(
    Country_name = if_else(
      Country == "European Union (27)",
      "EU 27",
      Country_name
    )
  )



# Select relevant columns and reshape for stacked bar chart
# Columns to stack: damages_avoided, transfer_diff, growth, abat_cost, reduction_inequalities, residual_tot
welfare_long <- welfare_data %>%
  dplyr::select(Country_name, damages_avoided, transfer_diff, growth, abat_cost, reduction_inequalities, residual_tot, total_welfare_gains) %>%
  pivot_longer(
    cols = c(damages_avoided, transfer_diff, growth, abat_cost, 
             reduction_inequalities, residual_tot),
    names_to = "Component",
    values_to = "Value"
  )
# Rename components for better labels
welfare_long <- welfare_long %>%
  mutate(Component = case_when(
    Component == "damages_avoided" ~ "Avoided damages",
    Component == "transfer_diff" ~ "Transfers",
    Component == "growth" ~ "Growth effect",
    Component == "abat_cost" ~ "Abatement cost",
    Component == "reduction_inequalities" ~ "Reduced inequalities",
    Component == "residual_tot" ~ "Residual",
    TRUE ~ Component
  ))

welfare_long <- welfare_long %>%
  mutate(
    Country_name = factor(
      Country_name,
      levels = c("Global",
        sort(unique(Country_name[!Country_name %in% c("EU 27", "Global")])),
        "EU 27"
      )
    )
  )

welfare_data <- welfare_data %>%
  mutate(
    Country_name = factor(
      Country_name,
      levels = levels(welfare_long$Country_name)
    )
  )
# Create stacked bar chart with total welfare gains as point
welfare_plot <- ggplot() +
  geom_col(
    data = welfare_long,
    aes(x = Country_name, y = Value, fill = Component),
    position = "stack",
    width = 0.7
  ) +
  geom_point(
    data = welfare_data,
    aes(x = Country_name, y = total_welfare_gains),
    size = 5,
    shape = 4,  # cross shape
    stroke = 2.5
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Welfare gains decomposition (Global Cap & Share vs BAU, 2050)",
    x = "Country",
    y = "Welfare Change (Variation in EDE Consumption)",
    fill = "Component",
    caption = "X = Total welfare gains"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(0.8, "cm"),
    plot.caption = element_text(
      size = 12,
      hjust = 0.5)
  )

print(welfare_plot)

# Save the plot
ggsave(
  filename = "cap_and_share/graphs/welfare_decomposition_2050.pdf",
  plot = welfare_plot,
  width = 10,
  height = 7,
  dpi = 300
)

