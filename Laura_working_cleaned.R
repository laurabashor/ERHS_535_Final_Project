#cleaned up 

library(readr)
library(tidyverse)

#read in data
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

#some countries are all caps, fix this:
plastics <- plastics %>%
  mutate(country = str_to_title(country))

#UK has two different names
plastics["country"][plastics["country"] == "United Kingdom Of Great Britain & Northern Ireland"] <- "United Kingdom"

#calculate total count of plastic per volunteer
totals <- plastics %>%
  filter(parent_company != "Grand Total") %>%
  group_by(country, year, volunteers) %>%
  summarize(total = sum(grand_total)) %>%
  mutate(total_per_volunteer = total/volunteers) #normalize the total plastic by the number of volunteers that year

#what is the overall change by country 
pollution_change <- totals %>%
  pivot_wider(id_col = "country", 
              names_from = "year", 
              values_from = "total_per_volunteer") %>%
  na.omit() %>%
  mutate(difference = (`2020` - `2019`)) %>%
  mutate(color = (difference > 0))

#we only have 38 countries where we have data for both 2019 and 2020

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

#what about change from 2019 to 2020 by plastic type
plastics_long <- plastics %>%
  filter(parent_company != "Grand Total") %>%
  select(!empty) %>%
  pivot_longer(!c(country, year, parent_company, num_events, volunteers), 
               names_to = "plastic_type", values_to = "count") %>%
  filter(plastic_type != "grand_total") %>%
  na.omit() %>%
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

p2 <- ggplot(plastics_long) +
  geom_col(aes(y = plastic_type,
               x = count_per_volunteer, 
               fill = year), position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  labs(y = "", x = "Pieces of plastic per volunteer") +
  theme_classic()

p2



#index
# $count per volunteer = \frac{plastic count}{number of volunteers}$
  
  