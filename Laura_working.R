## Laura's working script for plots for final project

library(readr)
library(tidyverse)
library(sf)
library(plotly)
library(leaflet)
library(mapview)

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

str(plastics)
head(plastics)

plastics %>%
  filter(year == "2019") %>%
  pull(country) %>%
  unique() %>%
  length()
#69 countries in the dataset

#some countries are all caps, fix this:
plastics <- plastics %>%
  mutate(country = str_to_title(country))

#UK has two different names
plastics["country"][plastics["country"] == "United Kingdom Of Great Britain & Northern Ireland"] <- "United Kingdom"


#what if we just keep the grand total rows for each country and year
totals_2019 <- plastics %>%
  filter(parent_company == "Grand Total")
#but we only have these "grand total" rows for 2019
#can we get these for both 2019 and 2020?

totals <- plastics %>%
  filter(parent_company != "Grand Total") %>%
  group_by(country, year, volunteers) %>%
  summarize(total = sum(grand_total)) %>%
  mutate(total_per_volunteer = total/volunteers) #normalize the total plastic by the number of volunteers that year

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
              values_from = "total_per_volunteer") %>%
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
  scale_color_brewer(palette = "Paired") +
  # scale_color_manual(values = c("orange", "blue")) +
  geom_segment(aes(y = country, yend = country,
                   x = 0, xend = difference)) +
  labs(y = "", x = "Change in pollution from 2019 to 2020 \n(pieces of plastic per volunteer)") +
  theme_classic() +
  theme(legend.position = "none")

p


#let's look at plastic types

plastics_long <- plastics %>%
  filter(parent_company != "Grand Total") %>%
  select(!empty) %>%
  pivot_longer(!c(country, year, parent_company, num_events, volunteers), 
               names_to = "plastic_type", values_to = "count") %>%
  filter(plastic_type != "grand_total") %>%
  na.omit()
  
plastics_long_2019 <- plastics_long %>%
  filter(year == "2019") 

ggplot(plastics_long_2019) +
  geom_bar(aes(x= reorder(plastic_type, count), 
                 y=count), 
               stat = "identity") +
  labs(x= "", y = "") +
  theme_classic() +
  coord_flip()

#what about the difference between years?

plastics_long <- plastics_long %>%
  mutate(count_per_volunteer = count/volunteers) %>% #gotta normalize
  mutate(year = factor(year)) %>%
  filter(plastic_type != "o") %>%
  mutate(plastic_type = fct_recode(plastic_type, 
                                   "Polystyrene" = "ps",
                                   "High density polyethylene" = "hdpe",
                                   "Low density polyethylene" = "ldpe",
                                   "Polyester" = "pet", 
                                   "Polypropylene" = "pp",
                                   "PVC" = "pvc"))


ggplot(plastics_long) +
  geom_col(aes(y = plastic_type,
               x = count_per_volunteer, 
               fill = year), position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  labs(y = "", x = "Pieces of plastic per volunteer") +
  theme_classic()
  
#get some lat long data for countries

countries <- read_csv("/Users/_lbashor/Dropbox/SARS-CoV-2 cat manuscript/cat ms results/R analysis cats/felid_phylogeny_R/countries.csv") %>%
  rename(country = Country)

anti_join(plastics_long, countries, by = "country") %>%
  pull(country) %>%
  unique()

plastics_long <- plastics_long %>%
  filter(country != "Empty")

plastics_long["country"][plastics_long["country"] == "Cote D_ivoire"] <- "Cote d'Ivoire"
countries["country"][countries["country"] == "Tanzania, United Republic of"] <- "Tanzania"
plastics_long["country"][plastics_long["country"] == "United States of America"] <- "United States"

plastics_long <- plastics_long %>%
  right_join(countries, by = "country") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) 


plastics_long_2019 <- plastics_long %>%
  filter(year == "2019") 

world_map <- map_data("world") %>%
  rename(country = region)

plastic_pollution_map <- right_join(plastics_long_2019, world_map, by = "country")

leaflet(plastic_pollution_map)

world_map <- world_map %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) 

ggplot() +
  geom_sf(data = world_map)

## equation for pollution index
## scale_fill_log10
