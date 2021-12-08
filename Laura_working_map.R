## trying to make the map a spatial object

library(sf)

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

plastics <- plastics %>%
  mutate(count_per_volunteer = (grand_total / volunteers)) %>% #normalize plastic totals by the number of volunteers
  select(!empty) %>% #don't need this column-- it's meaning is unclear
  filter(parent_company != "Grand Total") %>% #we can calculate these totals ourselves, not in the company column
  mutate(country = str_to_title(country)) #clean up country names 

#UK has two different names, fix this
plastics["country"][plastics["country"] == "United Kingdom Of Great Britain & Northern Ireland"] <- "United Kingdom"

#clean up data for mapping (Taru)
plastic_pollution_cleaned <- plastics %>%
  filter(year == "2019") %>%
  group_by(country, year, volunteers) %>%
  summarize(total = sum(grand_total)) %>%
  mutate(plastic_waste_total = sum(total), 
         total_per_volunteer = total/volunteers) %>%
  select(c("country", "total_per_volunteer", "plastic_waste_total"))

world_map <- map_data("world") %>%
  rename(country = region)

world_map["country"][world_map["country"] == "USA"] <- "United States Of America"
world_map["country"][world_map["subregion"] == "Hong Kong"] <- "Hong Kong"
world_map["country"][world_map["country"] == "Ivory Coast"] <- "Cote D_ivoire"
world_map["country"][world_map["country"] == "Taiwan"] <- "Taiwan_ Republic Of China (Roc)"
world_map["country"][world_map["country"] == "UK"] <- "United Kingdom"

plastic_pollution_map <- right_join(plastic_pollution_cleaned, 
                                    world_map, 
                                    by = "country")

map <- plastic_pollution_map %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

ggplot(map) +
  geom_sf(size = 1)

library(maps)

world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))

plastic_pollution_cleaned %>%
  anti_join(world1, by = c("country" = "ID")) %>%
  pull(country)

plastic_pollution_cleaned["country"][plastic_pollution_cleaned["country"] == "United States Of America"] <- "USA"
plastic_pollution_cleaned["country"][plastic_pollution_cleaned["country"] == "Cote D_ivoire"] <- "Ivory Coast"
plastic_pollution_cleaned["country"][plastic_pollution_cleaned["country"] == "Taiwan_ Republic Of China (Roc)"] <- "Taiwan"
plastic_pollution_cleaned["country"][plastic_pollution_cleaned["country"] == "United Kingdom"] <- "UK"

plastic_pollution_map <- right_join(plastic_pollution_cleaned, 
                                    world1, 
                                    by = c("country" = "ID")) %>%
  st_as_sf()

ggplot() + 
  geom_sf(data = plastic_pollution_map, 
          aes(fill = total_per_volunteer)) +
  scale_fill_gradient(high ="red", 
                      low = "lightblue", 
                      na.value = "white") +
  theme_classic() +
  theme(legend.position = "bottom")



ggplot(plastic_pollution_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = total_per_volunteer), color = "black")+
  # scale_fill_viridis_c(na.value = "white") +
  scale_fill_gradient(high ="red", low = "lightblue", na.value = "white") +
  theme_classic() +
  theme(legend.position = "bottom")



plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

plastics <- plastics %>%
  mutate(count_per_volunteer = (grand_total / volunteers)) %>% #normalize plastic totals by the number of volunteers
  select(!empty) %>% #don't need this column-- it's meaning is unclear
  filter(parent_company != "Grand Total") %>% #we can calculate these totals ourselves, not in the company column
  mutate(country = str_to_title(country)) #clean up country names 

#UK has two different names, fix this & fix all the other country names that conflict
plastics["country"][plastics["country"] == "United Kingdom Of Great Britain & Northern Ireland"] <- "United Kingdom"
plastics["country"][plastics["country"] == "United States Of America"] <- "USA"
plastics["country"][plastics["country"] == "Cote D_ivoire"] <- "Ivory Coast"
plastics["country"][plastics["country"] == "Taiwan_ Republic Of China (Roc)"] <- "Taiwan"
plastics["country"][plastics["country"] == "United Kingdom"] <- "UK"

#clean up data for mapping
plastic_pollution_cleaned <- plastics %>%
  filter(!is.na(grand_total)) %>%
  group_by(country, year, volunteers) %>%
  summarize(total = sum(grand_total)) %>%
  mutate(plastic_waste_total = sum(total), 
         total_per_volunteer = total/volunteers) %>%
  select(c("country", "total_per_volunteer", "plastic_waste_total"))

world_map <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE)) %>%
  rename(country = ID)

plastic_pollution_cleaned %>%
  filter(year == "2019") %>%
  pull(country) %>%
  unique() %>%
  length() #52 countries with data for 2019

plastic_pollution_cleaned %>%
  filter(year == "2020") %>%
  pull(country) %>%
  unique() %>%
  length() #55 countries with data for 2020

plastic_pollution_cleaned %>%
  filter(year == "2019") %>%
  right_join(world_map, by = "country") %>%
  st_as_sf() %>%
  ggplot() + 
  geom_sf(aes(fill = total_per_volunteer)) +
  # scale_fill_viridis_c(na.value = "white") +
  scale_fill_gradient(high = "#E64B3599",
                      low = "#4DBBD599",
                      na.value = "white") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Global plastic pollution in 2019", 
       subtitle = "Data collected by volunteers in 52 countries",
       fill = "Pieces of plastic\nper volunteer")

library(leaflet)
library(mapview)

plastic_pollution_cleaned %>%
  filter(year == "2019") %>%
  right_join(world_map, by = "country") %>%
  st_as_sf() %>%
  mapview(zcol = "plastic_waste_total")

pal <- colorNumeric(
  palette = "Blues",
  na.color = "grey",
  domain = plastic_pollution_cleaned$total_per_volunteer)

plastic_pollution_cleaned %>%
  filter(year == "2019") %>%
  right_join(world_map, by = "country") %>%
  st_as_sf() %>%
  leaflet() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(total_per_volunteer))


