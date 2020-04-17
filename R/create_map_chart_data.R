suppressPackageStartupMessages({
  library(tigris)
  library(tidyverse)
  library(glue)
  library(sp)
  library(rgdal)
  library(leaflet)
  library(ggplot2)
  library(geoR)
  library(leafpop)
  library(lubridate)  
})


options(tigris_use_cache = TRUE)

nyt <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

county_acs5_se <- readr::read_rds('data/county_acs5_SE_2018.rds')

covid <- read_csv(nyt) %>%
  filter(state %in% c("Alabama","Mississippi","Louisiana")) %>%
  filter(county != "Unknown") %>% 
  mutate(NAME = str_c(county,"County,",state,sep=" ")) %>% 
  mutate(NAME = str_replace(NAME," County, Louisiana", " Parish, Louisiana"))%>% 
  left_join(county_acs5_se, by = "NAME") %>%
  mutate(cases_rate = cases / pop * 10000,
         deaths_rate = deaths / pop * 10000)

covid_count = covid %>% 
  group_by(NAME) %>% 
  summarise(
    sum_cases = max(cases), 
    sum_deaths = max(deaths),
    sum_cases_rate = plyr::round_any(max(cases_rate),0.01),
    sum_deaths_rate = plyr::round_any(max(deaths_rate),0.01),
    pop = max(pop),
    white=max(white),
    black=max(black),
    asian=max(asian)
  )


char_count <- counties(cb = TRUE, state = c("AL","MS","LA")) 

char_count@data = char_count@data  %>% 
  mutate(state = case_when(
    STATEFP == "01" ~ "Alabama",
    STATEFP == "28" ~ "Mississippi",
    STATEFP == "22" ~ "Louisiana"
  )) %>% 
  mutate(NAME1 = str_c(NAME,"County,",state,sep=" ")) %>% 
  mutate(NAME1 = str_replace(NAME1," County, Louisiana", " Parish, Louisiana")) %>% 
  mutate(NAME2 = paste(NAME1,"L2"), 
         NAME3 = paste(NAME1,"L3"), 
         NAME4 = paste(NAME1,"L4"))


char_count = char_count %>% 
  geo_join(
    covid_count,
    by_sp = "NAME1",
    by_df = "NAME",
    how = "left"
  )



# create labels for counties

labels <- glue(
  "<strong>{char_count@data$NAME1} County </strong> <br/>",
  "Population: {char_count@data$pop} <br/>",
  "Cases, total {char_count@data$sum_cases} <br/>",
  "Deaths, total: {char_count@data$sum_deaths} <br/>",
  "Cases, rate: {char_count@data$sum_cases_rate} <br/>",
  "Deaths, rate: {char_count@data$sum_deaths_rate} <br/>"
) %>% 
  lapply(htmltools::HTML)

pal1 <- colorBin(
  palette = "Greens",
  na.color = NA,
  bins = c(0, 1, 10, 25, 100, 250, 1000,max(covid_count$sum_cases)),
  domain = char_count@data$sum_cases
)

pal2 <- colorBin(
  palette = "Blues",
  na.color = NA,
  bins = c(0, 1, 5, 50, max(covid_count$sum_deaths)),
  domain = char_count@data$sum_deaths
)

# pal3 <- colorNumeric(
#   palette = "Greens",
#   na.color = NA,
#   domain = char_count@data$sum_cases_rate)

pal3 <- colorBin(
  palette = "Greens",
  na.color = NA,
  bins = c(0, 1, 5, 10, 100, max(covid_count$sum_cases_rate)),
  domain = char_count@data$sum_cases_rate
)

# pal4 <- colorNumeric(
#   palette = "Blues",
#   na.color = NA,
#   domain = char_count@data$sum_deaths_rate)

pal4 <- colorBin(
  palette = "Blues",
  na.color = NA,
  bins = c(0, 0.1, 0.5, 1, 5,max(covid_count$sum_deaths_rate)),
  domain = char_count@data$sum_deaths_rate
)


p1 <- list()
p2 <- list()
p3 <- list()
p4 <- list()

counties <- distinct(char_count@data, NAME1)

ymin <- 0

for(i in seq(nrow(counties))) {
  
  ggdata <- covid %>% 
    filter(NAME == counties[[1]][i]) 
  
  ymax = plyr::round_any(
    max(ggdata$cases,na.rm = T) + 1, 
    accuracy = 5, 
    f = ceiling
  )
  
  p1[[i]] <- ggplot(ggdata) + 
    aes(x = date, y = cases) +
    geom_line(color = 'grey', linetype = 2) + 
    geom_point(color = 'darkgreen') + 
    scale_x_date(
      limits = c(min(ggdata$date), today()),
      date_breaks = "5 days",
      labels = scales::date_format("%b-%d")
    ) + 
    scale_y_continuous(limits = c(ymin, ymax), 
                       breaks = scales::pretty_breaks()) +
    theme_minimal() +
    labs(
      x = '', title =counties[[1]][i],
      y = 'Total number of cases identified'
    ) + 
    theme(
      #panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = 'top',
      legend.justification = 'left',
      legend.direction = 'horizontal',
      legend.text = element_text(size = 12, face = 'bold'),
      text = element_text(size = 12, face = 'bold')
    )
  
  ymax = plyr::round_any(
    max(ggdata$deaths) + 1, 
    accuracy = 5, 
    f = ceiling
  )
  
  p2[[i]] <- ggplot(ggdata) + 
    aes(x = date, y = deaths) +
    geom_line(color = 'grey', linetype = 2) + 
    geom_point(color = 'darkblue') + 
    scale_x_date(
      limits = c(min(ggdata$date), today()),
      date_breaks = "5 days",
      labels = scales::date_format("%b-%d")
    ) + 
    scale_y_continuous(limits = c(ymin, ymax), 
                       breaks = scales::pretty_breaks()) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    theme_minimal() +
    labs(
      x = '', 
      title = counties[[1]][i],
      y = 'Total number of deaths identified'
    ) + 
    theme(
      #panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = 'top',
      legend.justification = 'left',
      legend.direction = 'horizontal',
      legend.text = element_text(size = 12, face = 'bold'),
      text = element_text(size = 12, face = 'bold')
    )
  
  ymax = plyr::round_any(
    max(ggdata$cases_rate) + 1, 
    accuracy = 5, 
    f = ceiling
  )
  
  p3[[i]] <- ggplot(ggdata) + 
    aes(x = date, y = cases_rate) +
    geom_line(color = 'grey', linetype = 2) + 
    geom_point(color = 'darkgreen') + 
    scale_x_date(
      limits = c(min(ggdata$date), today()),
      date_breaks = "5 days",
      labels = scales::date_format("%b-%d")
    ) + 
    scale_y_continuous(limits = c(ymin, ymax), 
                       breaks = scales::pretty_breaks()) +
    theme_minimal() +
    labs(
      x = '', title = counties[[1]][i],
      y = 'Rate of cases identified'
    ) + 
    theme(
      #panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = 'top',
      legend.justification = 'left',
      legend.direction = 'horizontal',
      legend.text = element_text(size = 12, face = 'bold'),
      text = element_text(size = 12, face = 'bold')
    )
  
  ymax = plyr::round_any(
    max(ggdata$deaths_rate) + 1, 
    accuracy = 5, 
    f = ceiling
  )
  
  p4[[i]] <- ggplot(ggdata) + 
    aes(x = date, y = deaths_rate) +
    geom_line(color = 'grey', linetype = 2) + 
    geom_point(color = 'darkblue') + 
    scale_x_date(
      limits = c(min(ggdata$date), today()),
      date_breaks = "5 days",
      labels = scales::date_format("%b-%d")
    ) + 
    scale_y_continuous(limits = c(ymin, ymax), 
                       breaks = scales::pretty_breaks()) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    theme_minimal() +
    labs(
      x = '', 
      title = counties[[1]][i],
      y = 'Rate of deaths identified'
    ) + 
    theme(
      #panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = 'top',
      legend.justification = 'left',
      legend.direction = 'horizontal',
      legend.text = element_text(size = 12, face = 'bold'),
      text = element_text(size = 12, face = 'bold')
    )
  
  
}