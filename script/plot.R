# Load required libraries
library(tidyverse)
library(countrycode)
library(ggplot2)

# Read data
population_growth <- read_csv("natural-population-growth.csv")
unaffordable_diet <- read_csv("1- share-healthy-diet-unaffordable.csv")

# Filter and rename columns for population_growth
population_growth <- population_growth %>% 
  filter(Year > 2016 & Year < 2022) %>% 
  rename("Natural growth" = "Natural growth rate - Sex: all - Age: all - Variant: estimates") %>% 
  select(-5)

# Filter and rename columns for unaffordable_diet
unaffordable_diet_2021 <- unaffordable_diet %>% 
  filter(Year == "2021") %>% 
  rename("Share" = `Share of the population who cannot afford a healthy diet`)

# Add Continent column using countrycode
population_growth$Continent <- countrycode(population_growth$Entity, origin = "country.name", destination = "continent")

# Join the two dataframes
pop_growth_vs_unaffordable_diet <- inner_join(population_growth,
                                              unaffordable_diet_2021, 
                                              by = c("Year", "Code"), 
                                              relationship = "many-to-many") %>% 
  filter(!is.na(Code), !is.na(Continent)) %>% 
  select("Natural growth", "Share", "Continent")

# Define colour palette
palette <- c("Africa" = "#FFD29D", "other continents" = "grey")

# Create the ggplot
plot <- ggplot(pop_growth_vs_unaffordable_diet, 
               aes(x = `Natural growth`, y = `Share`, 
                   colour = ifelse(Continent == "Africa", "Africa", 
                                  "other continents"))) + 
  geom_point(size = 4.5) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.grid = element_line(linewidth = 0.3),
    panel.background = element_rect(fill="transparent"),
    plot.background = element_rect(fill="transparent", colour=NA),
    legend.background = element_rect(fill="transparent"),
    panel.grid.major = element_line(colour = "white"),
    axis.text.x = element_text(colour = "white"),
    axis.text.y = element_text(colour = "white"),
    axis.title.x = element_text(size = 15, colour = "white"),
    axis.ticks.x = element_line(colour = "white"),
    axis.ticks.y = element_line(colour = "white"),
    axis.text.y.left = element_text(size = 15, colour = "white"),
    axis.text.x.bottom = element_text(size = 15, colour = "white"),
    legend.key=element_rect(colour= NA, fill = NA)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_colour_manual(values = palette) +
  labs(colour = "") +
  ylab("") +
  guides(colour = guide_legend(override.aes = list(size = 15))) +
  theme(legend.text = element_text(size = 15, colour = "white"))

plot

# Save the plot
ggsave("ggplot.png", plot, bg = "transparent")

