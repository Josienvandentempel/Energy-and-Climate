#Load the data 
library(readxl)
renewables_Brazil_in_TWh <- read_excel("C:/Users/josie/Downloads/RU/Master year 2/Energy and Climate/renewables_Brazil in TWh.xlsx", 
                                       n_max = 5)
View(renewables_Brazil_in_TWh)

data = renewables_Brazil_in_TWh

library(tidyr)
library(dplyr)

renewables <- data %>%
  rename(Source = Year) %>%   # <-- rename the first column
  pivot_longer(
    cols = -Source,           # pivot all the year columns
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(Year = as.numeric(Year))

View(renewables)


#hydro bottom
renewables <- renewables %>%
  mutate(Source = factor(Source, levels = c("Solar", "Wind", "Geothermal, Biomass and Other", "Biofuels","Hydro")))



library(ggplot2)

ggplot(renewables, aes(x = Year, y = Value, fill = Source)) +
  geom_area(alpha = 0.7) +
  labs(
    title = "Brazil's Renewable Energy Generation",
    x = "Year",
    y = "Energy (TWh)",
    fill = "Source"
  ) +
  theme_minimal()



totals <- renewables %>%
  group_by(Year) %>%
  summarise(Total = sum(Value), .groups = "drop")

ggplot(renewables, aes(x = Year, y = Value, fill = Source)) +
  geom_area(alpha = 0.7, color = "black", size = 0.2) +   # outlines for each stack
  geom_line(data = totals, aes(x = Year, y = Total), 
            inherit.aes = FALSE, color = "black", size = 0.2) +  # total outline
  labs(
    title = "Brazil's Renewable Energy Generation 1990 - 2024",
    x = "Year",
    y = "Energy (TWh)",
    fill = "Source"
  ) +
  theme_minimal(base_size = 16)
  theme(
  plot.title = element_text(size = 20, face = "bold"),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  legend.position = c(0.05, 0.95),
  legend.justification = c("left", "top"),
  legend.background = element_rect(fill = "white", color = "black")
)


##
  ggplot(renewables, aes(x = Year, y = Value, fill = Source)) +
    geom_area(alpha = 0.7, color = "black", size = 0.2) +
    geom_line(data = totals, aes(x = Year, y = Total), 
              inherit.aes = FALSE, color = "black", size = 0.2) +
    labs(
      title = "Brazil's Renewable Energy Generation 1990 - 2024",
      x = "Year",
      y = "Energy (TWh)",
      fill = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.position = c(0.05, 0.95),
      legend.justification = c("left", "top"),
      #legend.background = element_rect(fill = "white", color = "black")
    )





