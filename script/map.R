# Install and load necessary libraries
install.packages("countrycode")
library(ggplot2)
library(countrycode)
library(tidyverse)

# Read data
unaffordable_diet <- read_csv("1- share-healthy-diet-unaffordable.csv")

# Filter and rename columns for unaffordable_diet_2021
unaffordable_diet_2021 <- unaffordable_diet %>% 
  filter(Year == "2021") %>% 
  rename(
    "Share" = `Share of the population who cannot afford a healthy diet`,
    "Country" = Entity
  )

# Create mapdata
mapdata <- map_data("world")
mapdata$CountryCode <- countrycode(
  sourcevar = mapdata$region,
  origin = "country.name",
  destination = "iso3c"
)

# Perform left join
mapdata <- left_join(
  mapdata,
  unaffordable_diet_2021,
  by = c("CountryCode" = "Code")
) %>% 
  filter(region != "Antarctica")

# Create map
map1 <- ggplot(mapdata, aes(x = long, y = lat, group = group, fill = Share)) +
  geom_polygon(colour = "white", linewidth = 0.1) + 
  scale_fill_gradient(
    name = "%",
    high = "#B3633B", 
    low = "#FFD29D",
    na.value = "gray",
    space = "Lab",
    guide = "colourbar"
  ) +
  theme_void() +
  theme(
    legend.position = c(0.18, 0.35),
    legend.text = element_text(colour = "white", size = 10),
    legend.title = element_text(colour = "white", size = 15),
    legend.key.size = unit(1.5, "lines")
  )

map1

# Save map1 as map.png
ggsave("map.png", map3, bg = "transparent", width = 10, height = 5, dpi = 250)



