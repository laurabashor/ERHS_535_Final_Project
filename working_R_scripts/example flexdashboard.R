library(flexdashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(DT)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv") 

ufos_co <- ufo_sightings %>%
  filter(country == "us") %>%
  mutate(date_documented = mdy(date_documented)) %>%
  mutate(date_time = mdy_hm(date_time)) %>%
  filter(state == "co") %>%
  mutate(year = year(date_time))


### Chart A

ufo_leaf <- ufos_co %>%
  leaflet() %>%
  addTiles() %>%
  # fitBounds(-102, .03, 37, -109, 03, 41) %>%  ################ What is this
  addCircleMarkers(ufos_co$longitude,
                   ufos_co$latitude,
                   color = ufos_co$encounter_length,
                   radius = 10,
                   fill = T,
                   fillOpacity = 0.2,
                   opacity = 0.6,
                   popup = paste(ufos_co$city_area,
                                 ufos_co$date_time,
                                 sep = ","))

ufo_leaf


### Chart B

ufos_co_new <- ufos_co %>%
  mutate(year = year(date_time)) %>%
  group_by(year) %>%
  summarise(count = n()) 

toplot <- ufos_co_new %>%
  #  group_by(date) %>%
  ggplot() +
  geom_line(aes(x = year, y = count)) +
  labs(x = "Year", y = "Sightings", title = "Sightings per Year") +
  theme_classic()


ggplotly(toplot)



### Chart C

ufo_table <- ufos_co %>%
  select(city_area, year) %>%
  rename(location = city_area) %>%
  datatable(options = list(pagelength = 12)) 

ufo_table 