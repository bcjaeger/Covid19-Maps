
library(totalcensus)  
library(readr)

# run line below if this is first time using totalcensus
# set_path_to_census('C:/Users/bcjaeger/Desktop/UAB/census_data/')

county_acs5 <- read_acs5year(
  year = 2018,
  states = "AL",
  table_contents = c(
    "white = B02001_002",
    "black = B02001_003",
    "asian = B02001_005"
  ),
  summary_level = "050"
) %>% 
  dplyr::select(GEOID,population,NAME) %>% 
  mutate(county=Hmisc::capitalize(str_remove(NAME, " County, Alabama"))) %>% 
  rename(pop=population)

write_rds(county_acs5, 'data/county_acs5_AL_2018.rds')