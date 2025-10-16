#load the data 
library(readxl)
non_renewables <- read_excel("C:/Users/josie/Downloads/RU/Master year 2/Energy and Climate/non-renewables.xlsx", n_max = 4)
View(non_renewables)

data = non_renewables

library(tidyr)
library(dplyr)

non_renew = data%>%
  pivot_longer(
  cols = starts_with("19") | starts_with("20"),
  names_to = "Year",
  values_to = "Value",

)

#Rename the first column to Source
colnames(non_renew)[1] <- "Source"
View(non_renew)

#Remove the second column
non_renew <- non_renew[, -2]   

non_renew <- non_renew %>%
  mutate(
    Year = as.numeric(Year),
    Value = as.numeric(Value),
    Source = as.character(Source)
  )

#Rename to coal
non_renew <- non_renew %>%
  mutate(Source = ifelse(Source == "Coal, peat and oil shale", "Coal", Source))

#Adjust the order
non_renew <- non_renew %>%
  mutate(Source = factor(Source, levels = c("Nuclear", "Coal", "Natural gas","Oil")))

View(non_renew)

library(ggplot2)

ggplot(non_renew, aes(x = Year, y = Value, fill = Source)) +
  geom_area(alpha = 0.7, color = "black", size = 0.3) +  # stacked area with thin outlines
  labs(
    title = "Brazil's Non-Renewable Total Energy Supply 1990-2024",
    x = "Year",
    y = "Energy (PJ)",  # adjust units as needed
    fill = NULL                 # remove legend title
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = c(0.05, 0.95),
    legend.justification = c("left", "top")#,
    #legend.background = element_rect(fill = "white", color = "black")
  )



















