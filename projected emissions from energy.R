#Load data
library(readxl)
Statistical_Review_of_World_Energy_Data <- read_excel("C:/Users/josie/Downloads/RU/Master year 2/Energy and Climate/Statistical Review of World Energy Data.xlsx", 
                                                      sheet = "CO2 from Energy", skip = 2)
View(Statistical_Review_of_World_Energy_Data)

#rename
data1 = Statistical_Review_of_World_Energy_Data

#select Brazil
Brazil_row1 = data1[data1$`Million tonnes of carbon dioxide`=='Brazil', ]
brz1 = Brazil_row1[3,]


library(tidyr)
library(dplyr)


brazil1 <- brz1 %>%
  select(-((ncol(data)-2):ncol(data)))

brazil_want = brazil1[,1:58]

brazil_longer <- brazil_want %>%
  pivot_longer(
    cols = -`Million tonnes of carbon dioxide`,           # all columns except "Country"
    names_to = "Year", 
    values_to = "Value"
  )


library(dplyr)
library(ggplot2)
library(tidyr)

brazil_longer <- brazil_longer %>%
  mutate(Year = as.numeric(Year))



View(brazil_longer)

#fix that 2024 is na:
brazil_longer$Year[is.na(brazil_longer$Year)] <- 2024
View(brazil_longer)


library(dplyr)
library(ggplot2)

# Suppose your Brazil data is in brazil_long with columns Year, Value
# We'll project from several base years
base_years <- c(2000, 2005, 2010, 2015, 2020)

# Store projections in a list
proj_list <- lapply(base_years, function(start_year) {
  df <- brazil_longer %>% filter(Year >= start_year & Year <= 2024)
  
  # Fit linear model
  model <- lm(Value ~ Year, data = df)
  
  # Predict for 2025:2030
  new_years <- data.frame(Year = 2025:2050)
  preds <- predict(model, newdata = new_years)
  
  data.frame(
    Year = new_years$Year,
    Value = preds,
    BaseYear = start_year
  )
})

# Combine all projections
proj_df <- do.call(rbind, proj_list)

#add red dots values 
value_2005 <- brazil_longer$Value[brazil_longer$Year == 2005]
new_value <- value_2005 * (1 - 0.53)
new_year <- 2030

new_value

#THIS IS THE CORRECT PLOT
ggplot() + 
  geom_point(data = data.frame(year = new_year, value = new_value),
             aes(x = year, y = value),
             color = "red", size = 3) +
  geom_text(data = data.frame(year = new_year, value = new_value),
            aes(x = year, y = value, label = "−53% emissions"),
            vjust = -1, color = "red")  + 
  geom_point(data = data.frame(year = 2050, value = 0),
             aes(x = year, y = value),
             color = "red", size = 3) +
  geom_text(data = data.frame(year = 2050, value = 0),
            aes(x = year, y = value, label = "−100%"),
            vjust = -1, color = "red")+
  geom_line(data = brazil_longer, aes(x = Year, y = Value), color = "black", size = 0.6) +
  #geom_point(data = brazil_longer, aes(x = Year, y = Value), color = "black") +
  geom_line(data = proj_df, aes(x = Year, y = Value, color = factor(BaseYear)), linetype = "dashed") +
  labs(
    title = "Brazil Emissions From Energy Sector: Observed & Projected",
    x = "Year",
    y = "Million tonnes of carbon dioxide emissions",
    color = "Projection from"
  ) +
  theme_minimal()



