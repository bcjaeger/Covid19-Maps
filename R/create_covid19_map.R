

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
  library(totalcensus)  
})


# run line below if this is first time using totalcensus
# set_path_to_census('C:/Users/bcjaeger/Desktop/UAB/census_data/')

options(tigris_use_cache = TRUE)

nyt <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

covid <- read_csv(nyt) %>%
  filter(state == "Alabama")

covid_count = covid %>%
  group_by(county) %>%
  summarise(sum_cases = max(cases), sum_deaths = max(deaths))

county_acs5 <- read_acs5year(
  year = 2018,
  states = "AL",
  table_contents = c(
    "white = B02001_002",
    "black = B02001_003",
    "asian = B02001_005"
  ),
  summary_level = "050"
)

char_count <- counties(cb = TRUE, state = "AL")

county_acs51 <- dplyr::select(county_acs5, GEOID,population) 

char_count1 <- geo_join(
  char_count,
  covid_count,
  by_sp = "NAME",
  by_df = "county",
  how = "left"
)

# create labels for counties

labels <- glue(
  "<strong>{char_count1@data$NAME} County </strong> <br/>",
  "Cases: {char_count1@data$sum_cases} <br/>",
  "Deaths: {char_count1@data$sum_deaths}"
) %>% 
  lapply(htmltools::HTML)

pal1 <- colorBin(
  palette = "Greens",
  na.color = NA,
  bins = c(0, 1, 5, 25, 100, 250, 500),
  domain = char_count1@data$sum_cases
)

pal2 <- colorBin(
  palette = "Blues",
  na.color = NA,
  bins = c(0, 1, 10, 25, max(covid_count$sum_deaths)),
  domain = char_count1@data$sum_deaths
)

p1 <- list()
p2 <- list()

counties <- distinct(char_count@data, NAME)

ymin <- 0

for(i in seq(nrow(counties))) {
  
  ggdata <- covid %>% 
    filter(county == counties[[1]][i]) 
  
  ymax = plyr::round_any(
    max(ggdata$cases) + 1, 
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
      x = '', title = paste(counties[[1]][i], 'county'),
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
      title = paste(counties[[1]][i], 'county'),
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
  

}

output <- char_count1 %>% 
  leaflet %>% 
  # add base map
  addProviderTiles("OpenStreetMap.France") %>% 
  # add counties
  addPolygons(fillColor = ~pal1(sum_cases),
    weight = 2,
    opacity = 0.3,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    popup = popupGraph(p1),
    highlight = highlightOptions(weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels, group="Cases")%>% 
  addPolygons(fillColor = ~pal2(sum_deaths),
    weight = 2,
    opacity = 0.3,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    popup = popupGraph(p2),
    highlight = highlightOptions(weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels, group = "Deaths") %>%
  addLayersControl(
    overlayGroups = c("Cases", "Deaths"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(pal = pal1,
    values = ~sum_cases,
    opacity = 0.7,
    title = htmltools::HTML("Cases per <br> County"),
    position = "bottomright",
    group="Cases") %>% 
  addLegend(pal = pal2,
    values = ~sum_deaths,
    opacity = 0.7,
    title = htmltools::HTML("Deaths per <br> County"),
    position = "bottomright",
    group="Deaths"
  ) %>%
  hideGroup(group=c("Deaths")) 

                    
htmlwidgets::saveWidget(output, 
  file="index.html", 
  selfcontained=T
)


