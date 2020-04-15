---
title: "COVID 19 Alabama"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
---

```{r setup, include=FALSE}
library(flexdashboard)
library(tigris)
library(magrittr)
library(tidyverse)
library(sp)
library(spdep)
library(raster)
library(rgdal)
library(leaflet)
library(ggplot2)
library(geoR)
library(dplyr)
library(SpatialEpi)
library(INLA)
library(lattice)
library(leafpop)
library(magrittr)
library(lubridate)
options(tigris_use_cache = TRUE)

library(totalcensus)

covid = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
  filter(state=="Alabama")

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

char_count <- counties(cb = TRUE, state="AL")

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


county_acs51 = county_acs5 %>% dplyr::select(GEOID,population) 

char_count1 <- geo_join(char_count, 
                        covid_count, 
                        by_sp = "NAME", 
                        by_df = "county",
                        how = "left")
# create labels for counties

labels <- sprintf("<strong>%s County </strong> <br/> Cases %s <br/> Deaths %s <br/>",
                  char_count1@data$NAME, char_count1@data$sum_cases,
                  char_count1@data$sum_deaths) %>% lapply(htmltools::HTML)

pal1 <- colorBin(
  palette = "Greens",
  na.color = NA,
  bins=c(0,1,5,25,100,250,500),
  domain = char_count1@data$sum_cases)

pal2 <- colorBin(
  palette = "Blues",
  na.color = NA,
  bins=c(0,1,10,25,max(covid_count$sum_deaths)),
  domain = char_count1@data$sum_deaths)

p1 = list()
p2 = list()

counties = distinct(char_count@data,NAME)

for(i in 1:67){
  upper = plyr::round_any(max(covid2$cases[covid2$county==counties[[1]][i]]),10,f=ceiling)
  p1[[i]] = ggplot(covid2[covid2$county==counties[[1]][i],], aes(x=date,y=cases) )+
    geom_line()+ggtitle(paste(counties[[1]][i])) +
    scale_y_continuous(name = "# Cases",limits=c(0,upper))+
    scale_x_date(name="Date",limits=c(as.Date("2020-03-13"),today()),
                 date_breaks = "2 days",labels=scales::date_format("%b-%d"))+
    theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  upper = plyr::round_any(max(covid2$deaths[covid2$county==counties[[1]][i]]),5,f=ceiling)
  p2[[i]] = ggplot(covid2[covid2$county==counties[[1]][i],], aes(x=date,y=deaths) )+
    geom_line()+ggtitle(paste(counties[[1]][i])) +
    scale_y_continuous(name = "# Deaths",limits=c(0,upper))+
    scale_x_date(name="Date",limits=c(as.Date("2020-03-13"),today()),
                 date_breaks = "2 days",labels=scales::date_format("%b-%d"))+
    theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

```

Column {data-width=650}
-----------------------------------------------------------------------

### Map 

```{r}

output$map <- renderLeaflet({
map = char_count1 %>% 
  leaflet %>% 
  # add base map
  addProviderTiles("OpenStreetMap.France") %>% 
  #addProviderTiles("OpenMapSurfer.Roads") %>%
  #addProviderTiles("OpenMapSurfer.AdminBounds") %>% 
  # add counties
  addPolygons(fillColor = ~pal1(sum_cases),
              weight = 2,
              opacity = 0.3,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
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
              highlight = highlightOptions(weight = 2,
                                           color = "#666",
                                           dashArray = "",
                                           fillOpacity = 0.7,
                                           bringToFront = TRUE),
              label = labels, group = "Deaths") %>%
  #addPopupGraphs(p1,group="Cases") %>% 
  #addPopupGraphs(c(p1,p2),group=list(c("Cases","Deaths")))  %>% 
  addLayersControl(
    overlayGroups = c("Cases", "Deaths"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(pal = pal1,
            values = ~sum_cases,
            opacity = 0.7,
            title = htmltools::HTML("Cases per <br>
                                    County"),
            position = "bottomright",
            group="Cases") %>% 
  addLegend(pal = pal2,
            values = ~sum_deaths,
            opacity = 0.7,
            title = htmltools::HTML("Deaths per <br>
                                    County"),
            position = "bottomright",
            group="Deaths") %>%
  hideGroup(group=c("Deaths")) 
  
map
  
})  

click_county <- eventReactive(input$map_shape_click, {
  x <- input$map_shape_click
  y <- x$id
  return(y)
})

# Drag event for the scatterplot; will grab county of selected points
sub <- reactive({
  eventdata <- event_data('plotly_selected', source = 'source')
  
  if (is.null(eventdata)) {
    
    return(NULL) # do nothing
    
  } else {
    
    counties1 <- eventdata[['key']]
    
    if (length(counties1) == 0) {
      
      counties1 <- 'abcdefg' # a hack but it's working - set to something that can't be selected
      
    }
    
    if (!(counties1 %in% metro()$NAME)) {
      
      return(NULL) # if there is not a match, do nothing as well
      
    } else {
      
      # Give back a sp data frame of the selected tracts
      sub <- metro()[metro()$NAME %in% counties1, ]
      return(sub)
      
    }
    
  }
})

county_data <- reactive({
  
  # Fetch data for the clicked tract
  return(metro()@data[metro()@data$NAME == click_county(), ])
})
leafletOutput('map')  


```

Column {data-width=350}
-----------------------------------------------------------------------

### Table

```{r}
output$rateplot <- renderHighchart({
  chart <- highchart() %>%
    hc_chart(type = 'column') %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(categories = c('Cases', 'Deaths'), title = list(text = 'Rate per 10,000')) %>%
    hc_yAxis(title = list(text = 'Rate')) %>%
    hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
    hc_add_series(name = 'Population, 2010', data = c(county_data()$case_rate,
                                                      county_data()$death_rate) %>%
    hc_title(text = paste0('County: ', as.character(county_data()$NAME)), 
             align = 'left') %>%
    hc_subtitle(text = paste0('BLAH: ', as.character(round(county_data()$entropy, 2))), 
                align = 'left') %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_colors(c('#d01010', '#d01010')) %>%
    hc_tooltip(enabled = FALSE)
  chart
})
highchartOutput('rateplot')

```

### Line Graph

```{r}
# Here, we draw the diversity gradient with ggplotly
output$scatter <- renderPlotly({
  
  key <- metro()$NAME # This will uniquely identify counties for Plotly
  
  p1a <- ggplot(metro()@data) + 
    geom_line(alpha = 0.4, aes(y=cases, x=date, key = key)) + 
    theme_minimal(base_size = 14) + 
    xlab('Date') + ylab('') 
  
  g <- ggplotly(p1a, source = 'source') %>% 
    layout(dragmode = 'lasso', 
           yaxis = list(title = 'Cases'), 
           margin = list(l = 100), 
           font = list(family = 'Open Sans', size = 16))
  

  
})  
plotlyOutput('scatter')

```

