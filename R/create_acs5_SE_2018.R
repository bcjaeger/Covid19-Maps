
library(totalcensus)  
library(readr)
library(tidyverse)

# run line below if this is first time using totalcensus
# set_path_to_census('C:/Users/bcjaeger/Desktop/UAB/census_data/')

county_acs5_se <- read_acs5year(
  year = 2018,
  states = c("AL","MS","LA"),
  table_contents = c(
    "white = B02001_002",
    "black = B02001_003",
    "asian = B02001_005"
  ),
  summary_level = "050"
) %>% 
 # dplyr::select(GEOID,population,NAME) %>% 
  rename(pop=population)

write_rds(county_acs5_se, 'data/county_acs5_SE_2018.rds')
