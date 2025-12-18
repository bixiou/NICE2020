options(repos = c(CRAN = "https://cloud.r-project.org/"))

install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("readr")
install.packages("tidyr")
install.packages("tibble")
install.packages("brant")
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

# Select relevant columns and reshape for stacked bar chart
# Columns to stack: damages_avoided, transfer_diff, growth, abat_cost, reduction_inequalities, residual_tot
welfare_long <- welfare_data %>%
  dplyr::select(Country, damages_avoided, transfer_diff, growth, abat_cost, reduction_inequalities, residual_tot, total_welfare_gains) %>%
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

# Create stacked bar chart with total welfare gains as point
welfare_plot <- ggplot() +
  geom_col(
    data = welfare_long,
    aes(x = Country, y = Value, fill = Component),
    position = "stack",
    width = 0.7
  ) +
  geom_point(
    data = welfare_data,
    aes(x = Country, y = total_welfare_gains),
    size = 3,
    shape = 4,  # cross shape
    stroke = 1.5
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Welfare gains decomposition (Global Cap & Share vs BAU, 2050)",
    x = "Country",
    y = "Welfare gains (thousand USD2017 per capita per year)",
    fill = "Component",
    caption = "Cross (×) indicates total welfare gains"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.x = element_blank()
  )

print(welfare_plot)

# Save the plot
ggsave(
  filename = "cap_and_share/graphs/welfare_decomposition_2050.png",
  plot = welfare_plot,
  width = 10,
  height = 7,
  dpi = 300
)

