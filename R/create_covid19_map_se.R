
#Call create_map_chart_data to set up files for map

source("R/create_map_chart_data.R")
source("R/AddTitle.R")

states <- aggregate(char_count[, "STATEFP"], by = list(ID = char_count@data$STATEFP), 
                    FUN = unique, dissolve = T)

output_count <-char_count %>%
  leaflet() %>%
  # add base map; this is blank to keep the plot from being too busy
  # minzoom = 7 keeps the user focussed on the southeastern US
  addProviderTiles("CartoDB.Positron", 
    options = tileOptions(minZoom = 7)) %>%
  # add counties
  addPolygons(
    fillColor = ~ pal1(sum_cases),
    weight = 1,
    opacity = 1,
    color = "black",
    dashArray = "3",
    fillOpacity = 0.7,
    #popup = popupGraph(p1),
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(textsize = "15px"),
    group = "Cases, total"
  )   %>%
  addPolylines(data = states, color = "black", opacity = 1, weight = 3) %>%
  addPolygons(
    fillColor = ~ pal2(sum_deaths),
    weight = 1,
    opacity = 0.3,
    color = "black",
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
    labelOptions = labelOptions(textsize = "15px"),
    group = "Deaths, total"
  ) %>%
  # add counties
  addPolygons(
    fillColor = ~ pal3(sum_cases_rate),
    weight = 1,
    opacity = 0.3,
    color = "black",
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
    weight = 1,
    opacity = 0.3,
    color = "black",
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
    position = "topleft",
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
  )  %>% 
  addTitle(text = "Counts and rates of Covid 19",
    color = "black",
    fontSize = "25px",
    fontFamily = "Sans",
    leftPosition = 15,
    topPosition = 1) %>% 
  addTitle(text = as.character(
    glue("Source: github.com/nytimes/covid-19-data <br/> updated {today()}")
  ),
    color = "black",
    fontSize = "15px",
    fontFamily = "Sans",
    leftPosition = 14.75,
    topPosition = 5)

htmlwidgets::saveWidget(output_count, file="index.html", selfcontained=T)



