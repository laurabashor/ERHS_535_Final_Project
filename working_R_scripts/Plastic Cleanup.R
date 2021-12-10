library(flexdashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(DT)
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-01-26')
tuesdata <- tidytuesdayR::tt_load(2021, week = 5)

plastics <- tuesdata$plastics

# Or read in the data manually

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

#Maps: sf and/or leaflet

#Count of parent companies: 10823
plastics %>%
  pull(parent_company) %>%
  unique() %>%
  length()

gtot <- plastics %>%
  select(grand_total) %>%
  arrange(desc(TRUE))
gtot

library(dplyr)
#Table of plastic companies
plastic_companies <- plastics %>%
  filter(year == "2019") %>%
  select(parent_company, grand_total) %>%
  filter(parent_company != "Grand Total") %>%
  filter(parent_company != "null") %>%
  filter(parent_company != "NULL") %>%
  filter(parent_company != "Unbranded") %>%
  na.omit() %>%
  group_by(parent_company) %>%
  summarise(ggrand = sum(grand_total)) %>%
  arrange(desc(ggrand)) %>%
  ungroup() %>%
  slice(1:20) %>%
  datatable(options = list(pagelength = 12))
plastic_companies

#Unbranded grand total is 364075
sort(grand_total, descending = TRUE) %>%
  
#time series, another data set, top 20 companies, compare years, limit to 2019; no times series





##############################




head(plastic_companies)

yr_plast_nin <- plastics %>%
  filter(year == "2019")

yr_plast_tw <- plastics %>%
  filter(year == "2020")

#make column for 2019 and 2020 and difference
#coord_sf
#index
#copy 


