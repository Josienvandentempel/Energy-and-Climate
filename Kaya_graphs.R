#import the original data and then copy it as data 
library(readxl)
World_Energy_Balances_Highlights_2024 <- read_excel("C:/Users/josie/Downloads/RU/Master year 2/Energy and Climate/WorldEnergyBalancesHighlights2024.xlsx", 
                                                sheet = "TimeSeries_1971-2023", skip = 1)
View(World_Energy_Balances_Highlights_2024)

data = World_Energy_Balances_Highlights_2024


#install package needed for slicing later
install.packages("dplyr")
library(dplyr)


# Filter rows where Country == "Brazil"
brazil_data <- data[data$Country == "Brazil", ]
# View the result
View(brazil_data)

#Select our relevant data
regions <- c("Brazil", "Argentina", "Non-OECD Total", "OECD Total", "World")
selected_data <- data[data$Country %in% regions, ]
View(selected_data)

selected_data[selected_data$Country == "Brazil"]

brazil_elec <- data[
  data$Country == "Brazil" &
    data$Product == "Electricity" &
    data$Flow == "Total final consumption (PJ)",
]

#View(brazil_elec)


#Plot Brazil Electricity TFC over the years
years <- paste0(1990:2022)  # creates "1990", "1991", ..., "2022"
years_numeric <- 1990:2022
row_values = row_values <- as.numeric(electricity[, years][1, ])  # first row

plot(
  years_numeric, 
  row_values,
  type = "o",                # lines + points
  xlab = "Year",
  ylab = "Value (PJ)",
  main = paste(brazil_elec$Country[1], "-", brazil_elec$Product[1], "-", brazil_elec$Flow[1])
)














library(ggplot2)

## GG PLOT

library(dplyr)

regions <- c("Brazil", "Argentina", "OECD Total", "Non-OECD Total", "World")
regions2 <- c("Brazil", "Argentina")



# Filter rows for the selected regions, Product = "Electricity", Flow = "Total final consumption (PJ)"
selected_data <- data %>%
  filter(
    Country %in% regions2,
    Product == "Electricity",
    Flow == "Total final consumption (PJ)"
  )


#pivot to long format

library(tidyr)
#install.packages("tidyr")

# Keep only year columns and pivot
long_data <- selected_data %>%
  pivot_longer(
    cols = `1990`:`2022`,   # all years
    names_to = "Year",
    values_to = "Value"
  )

# Convert Year and Value to numeric
long_data$Year <- as.numeric(long_data$Year)
long_data$Value <- as.numeric(long_data$Value)

#Plot with ggplot2

ggplot(long_data, aes(x = Year, y = Value, color = Country)) +
  geom_line(linewidth = .8) +
  geom_point() +
  labs(
    title = "Electricity: Total Final Consumption (PJ) for Selected Regions",
    x = "Year",
    y = "Total Final Consumption (PJ)",
    color = "Region"
  ) +
  theme_minimal()













#World population
regs = c('Brazil', 'Argentina', 'OECD members', 'World')
population_data = data_world_bank_population[data_world_bank_population$`Country Name` %in% regs, ]

View(population_data)

#First, we add the non-OECD population
year_cols <- names(population_data)[5:ncol(population_data)]


# Get the row for OECD and World
oecd_row <- population_data[population_data$'Country Name' == "OECD members", ]
world_row <- population_data[population_data$'Country Name' == "World", ]


# Subtract OECD from World for each year
non_oecd_years <- world_row[, year_cols] - oecd_row[, year_cols]


non_oecd_row <- world_row[, 1:4]              # take metadata columns as placeholders
non_oecd_row$`Country Name` <- "Non-OECD"    # set country name
non_oecd_row[, year_cols] <- non_oecd_years   # add computed values

population_data <- rbind(population_data, non_oecd_row)

#View(population_data)


brazil_data <- population_data[population_data$'Country Name'== "Brazil",]

View(brazil_data)

#install.packages("tidyverse")
#library(tidyverse)

brazil_long <- brazil_data %>%
  pivot_longer(cols = starts_with("19") | starts_with("20"), # all year columns
               names_to = "Year",
               values_to = "Population") %>%
  mutate(Year = as.numeric(Year))  # convert Year to numeric




# Plot population
ggplot(brazil_long, aes(x = Year, y = Population/1000000)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Population of Brazil over time",
       x = "Year",
       y = "Population (millions of peope)") +
  theme_minimal()


brazil_recent <- brazil_long %>%
  filter(Year >= 2005)


ggplot(brazil_recent, aes(x = Year, y = Population/1000000)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Brazil Population",
       x = "Year",
       y = "Population (millions of people)") +
  theme_minimal()









# GREENHOUSE GAS EMISSIONS
GHG_brazil <- GHGHighlights [c(131), ]

View(GHG_brazil)

GHG_brazil_long <- GHG_brazil %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),  # all year columns
    names_to = "Year",
    values_to = "GHG_emissions"
  ) %>%
  mutate(Year = as.numeric(Year))





ggplot(GHG_brazil_recent, aes(x = Year, y = GHG_emissions)) +
  geom_line(aes(group = 1), color = "blue") +
  geom_point(color = "blue") +
  labs(
    title = "Brazil GHG emissions from energy",
    x = "Year",
    y = expression(Million~tonnes~of~CO[2]~eq)
  ) +
  theme_minimal()

GHG_brazil_recent <- GHG_brazil_recent %>%
  mutate(GHG_emissions = as.numeric(GHG_emissions))

library(scales)  # for pretty_breaks()

ggplot(GHG_brazil_recent, aes(x = Year, y = GHG_emissions)) +
  geom_line(aes(group = 1), color = "blue") +
  geom_point(color = "blue") +
  labs(
    title = "Brazil GHG emissions from energy",
    x = "Year",
    y = expression(Million~tonnes~of~CO[2]~eq)
  ) +
  scale_y_continuous(breaks = seq(0, max(GHG_brazil_recent$GHG_emissions), by = 50)) +
  theme_minimal()





#KAYA



library(readxl)
Kaya_data <- read_excel("C:/Users/josie/Downloads/RU/Master year 2/Energy and Climate/GHGHighlights.XLS", sheet = "KAYA", range = "A4:BB859")


Kaya_data_brazil <- Kaya_data [c(850, 851,852,853,854), ]
View(Kaya_data_brazil)






install.packages("dplyr")
library(dplyr)


library(tibble)

# put indicator names into rownames, select only year columns, transpose
mat <- Kaya_data_brazil %>%
  column_to_rownames(var = names(Kaya_data_brazil)[2]) %>%
  select(matches("^19|^20"))

# transpose and restore Year as a column
Kaya_transposed <- as.data.frame(t(mat)) %>%
  rownames_to_column("Year") %>%
  mutate(Year = as.integer(Year))

# convert columns to numeric
Kaya_transposed <- Kaya_transposed %>%
  mutate(across(-Year, ~ as.numeric(as.character(.))))

glimpse(Kaya_transposed)

View(Kaya_transposed)
Kaya_transposed[, 2]

library(ggplot2)


# 
# ggplot(Kaya_transposed, aes(x = Year, y = `CO2 emissions`)) +
#   geom_line(color = "blue") +
#   geom_point(color = "red") +
#   labs(
#     title = expression(Brazil~CO[2]~emissions~over~time),
#     x = "Year",
#     y = expression(CO[2]~emissions)
#   ) +
#   theme_minimal()



library(dplyr)

#Rename the column names
Kaya_transposed <- Kaya_transposed %>%
  rename(
    `Carbon intensity (CO2/TES)` = `Carbon intensity: ESCII (CO2/TES)`,
    `GDP per capita`             = `GDP per population (GDP per capita)`
  )

library(tidyr)

Kaya_long <- Kaya_transposed %>%
  pivot_longer(
    cols = -Year,
    names_to = "Indicator",
    values_to = "Value"
  )

ggplot(Kaya_long, aes(x = Year, y = Value)) +
  geom_line(color = "blue") +
  facet_wrap(~ Indicator, scales = "free_y") +
  labs(
    title = "Brazil Kaya Identity Indicators",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal()



#one long image
ggplot(Kaya_long, aes(x = Year, y = Value)) +
  geom_line(color = "blue") +
  facet_wrap(~ Indicator, scales = "free_y", ncol = 1) +  # stack vertically
  labs(
    title = "Brazil Kaya Identity Indicators",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#FROM 2000 ONWARDS
Kaya_long <- Kaya_transposed %>%
  pivot_longer(
    cols = -Year,
    names_to = "Indicator",
    values_to = "Value"
  )

# Filter for Year >= 2000 and plot
Kaya_long %>%
  filter(Year >= 2000) %>%
  ggplot(aes(x = Year, y = Value)) +
  geom_line(color = "blue") +
  facet_wrap(~ Indicator, scales = "free_y", ncol = 1) +
  labs(
    title = "Brazil Kaya Identity Indicators (2000 onwards)",
    x = "Year",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








#Final plot, used in the report
View(Kaya_long)

Kaya_long <- Kaya_long %>%
  filter(Indicator != "CO2 emissions")


Kaya_long <- Kaya_long %>%
  mutate(Indicator = factor(Indicator, levels = c(  "GDP per capita","Population", "Energy intensity (TES/GDP)","Carbon intensity (CO2/TES)")))


ggplot(Kaya_long, aes(x = Year, y = Value, color = Indicator)) +
  geom_line(size = 1) +        # thicker lines
  labs(
    title = "Kaya graphs with respect to reference year 2000",
    x = "Year",
    y = "% with respect to 2000",
    color = NULL                 # removes legend title
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = c(0.10, 1.0),     # top-left inside plot
    legend.justification = c("left", "top")
    #legend.background = element_rect(fill = "white")
  )




#Data analysis to see which Kaya factor plays the biggest role:
CO2_emissions = Kaya_transposed[, 2]
Population= Kaya_transposed[,3]
GDP = Kaya_transposed[,4]
Energy_intensity = Kaya_transposed[,5]
Carbon_intensity = Kaya_transposed[,6]

fit = lm(CO2_emissions~ Population+ GDP + Energy_intensity+Carbon_intensity- 1)
summary(fit)

#fit$coefficients gives:
#Population       GDP           Energy_intensity    Carbon_intensity 
#1.08136795       0.71974390      -0.85152456       0.00935088 

summary(fit)$coefficients[,2]






