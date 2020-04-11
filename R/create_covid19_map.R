

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

source("R/AddTitle.R")

options(tigris_use_cache = TRUE)

nyt <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

county_acs5 <- read_rds('data/county_acs5_AL_2018.rds')

covid <- read_csv(nyt) %>%
  filter(state == "Alabama") %>%
  left_join(county_acs5, by = "county") %>%
  mutate(cases_rate = cases / pop * 10000,
    deaths_rate = deaths / pop * 10000)

covid_count = covid %>% 
  group_by(county) %>% 
  summarise(
    sum_cases = max(cases), 
    sum_deaths = max(deaths),
    sum_cases_rate = plyr::round_any(max(cases_rate),0.01),
    sum_deaths_rate = plyr::round_any(max(deaths_rate),0.01),
    pop = max(pop)
  )


char_count <- counties(cb = TRUE, state = "AL") %>% 
  geo_join(
  covid_count,
  by_sp = "NAME",
  by_df = "county",
  how = "left"
)

# create labels for counties

labels <- glue(
  "<strong>{char_count@data$NAME} County </strong> <br/>",
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
  bins = c(0, 1, 5, 25, 100, 250, max(covid_count$sum_cases)),
  domain = char_count@data$sum_cases
)

pal2 <- colorBin(
  palette = "Blues",
  na.color = NA,
  bins = c(0, 1, 10, max(covid_count$sum_deaths)),
  domain = char_count@data$sum_deaths
)

pal3 <- colorNumeric(
  palette = "Greens",
  na.color = NA,
  domain = char_count@data$sum_cases_rate)

pal4 <- colorNumeric(
  palette = "Blues",
  na.color = NA,
  domain = char_count@data$sum_deaths_rate)


p1 <- list()
p2 <- list()
p3 <- list()
p4 <- list()

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
      x = '', title = paste(counties[[1]][i], 'county'),
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
      title = paste(counties[[1]][i], 'county'),
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

output_count <- char_count %>%
  leaflet() %>%
  # add base map; this is blank to keep the plot from being too busy
  # minzoom = 7 keeps the user focussed on the southeastern US
  addProviderTiles("CartoDB.Positron", 
    options = tileOptions(minZoom = 7)) %>%
  # add counties
  addPolygons(
    fillColor = ~ pal1(sum_cases),
    weight = 2,
    opacity = 0.3,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    popup = popupGraph(p1),
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    group = "Cases, total"
  )  %>%
  addPolygons(
    fillColor = ~ pal2(sum_deaths),
    weight = 2,
    opacity = 0.3,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    popup = popupGraph(p2),
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    group = "Deaths, total"
  ) %>%
  # add counties
  addPolygons(
    fillColor = ~ pal3(sum_cases_rate),
    weight = 2,
    opacity = 0.3,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    popup = popupGraph(p3),
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(textsize = "15px"),
    group = "Cases, rate"
  )  %>%
  addPolygons(
    fillColor = ~ pal4(sum_deaths_rate),
    weight = 2,
    opacity = 0.3,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    popup = popupGraph(p4),
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(textsize = "15px"),
    group = "Deaths, rate"
  ) %>% 
  addLayersControl(
    baseGroups = c("Cases, total", "Deaths, total", 
      "Cases, rate", "Deaths, rate"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    pal = pal1,
    values = ~ sum_cases,
    opacity = 0.7,
    title = htmltools::HTML("Cases per <br> County"),
    position = "bottomright",
    group = "Cases, total"
  ) %>%
  addLegend(
    pal = pal2,
    values = ~ sum_deaths,
    opacity = 0.7,
    title = htmltools::HTML("Deaths per <br> County"),
    position = "bottomright",
    group = "Deaths, total"
  ) %>%
  addLegend(pal = pal3,
    values = ~sum_cases_rate,
    opacity = 0.7,
    title = htmltools::HTML("Case Rate <br> per 10,000"),
    position = "bottomleft",
    group="Cases, rate"
  ) %>% 
  addLegend(pal = pal4,
    values = ~sum_deaths_rate,
    opacity = 0.7,
    title = htmltools::HTML("Death rate <br> per 10,000"),
    position = "bottomleft",
    group="Deaths, rate"
  ) %>% 
  setMaxBounds(
    lng1 = -84.9,
    lng2 = -88.505825,
    lat1 = 37.715583,
    lat2 = 30.038578
  ) %>% 
  addTitle(text="Counts and rates of Covid 19",
    color = "black",
    fontSize = "25px",
    fontFamily = "Sans",
    leftPosition = 15,
    topPosition = 2) %>% 
  addTitle(text="New York Time: 04/10/2020",
    color = "black",
    fontSize = "15px",
    fontFamily = "Sans",
    leftPosition = 14.75,
    topPosition = 10)

htmlwidgets::saveWidget(output_count, file="index.html", selfcontained=T)



