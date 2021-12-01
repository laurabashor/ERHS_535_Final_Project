## Laura's working script for plots for final project

library(readr)
library(tidyverse)
library(sf)
library(plotly)

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

str(plastics)
head(plastics)

plastics %>%
  pull(country) %>%
  unique() %>%
  length()
#69 countries in the dataset

#some countries are all caps, fix this:
plastics <- plastics %>%
  mutate(country = str_to_title(country))

#what if we just keep the grand total rows for each country and year
totals_2019 <- plastics %>%
  filter(parent_company == "Grand Total")
#but we only have these "grand total" rows for 2019
#can we get these for both 2019 and 2020?
totals <- plastics %>%
  filter(parent_company != "Grand Total") %>%
  group_by(country, year) %>%
  summarize(total = sum(grand_total))
  
#check if I'm getting the same numbers
totals %>%
  filter(year == "2019") %>%
  head()

totals_2019 %>%
  select(country, grand_total) %>%
  head()

#yay! they are the same, so now we have totals for both years
#can move forward with just the totals df
rm(totals_2019)

#I'm wondering, did they increase or decrease?

pollution_change <- totals %>%
  pivot_wider(id_col = "country", 
              names_from = "year", 
              values_from = "total") %>%
  na.omit() %>%
  mutate(difference = (`2020` - `2019`)) %>%
  mutate(color = (difference > 0))

#unfortunately we only have 38 countries left
#but it's still interesting

p <- pollution_change %>%
  ggplot() +
  geom_point(aes(x = difference, 
                 y = reorder(country, difference),
                 color = color)) +
  scale_color_manual(values = c("orange", "blue")) +
  geom_segment(aes(y = country, yend = country,
                   x = 0, xend = difference)) +
  labs(y = "", x = "Change in pollution from 2019 to 2020") +
  theme_classic() +
  theme(legend.position = "none")

p
