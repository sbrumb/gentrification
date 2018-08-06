hh_inc_leaflet <- hh_inc_allmetros %>%
  filter(grepl("DC", metro_name)) %>%
  as("Spatial")

pal <- colorFactor(palette="RdYlBu", domain = hh_inc_leaflet$change,
                   na.color="transparent")

labels <- sprintf(
  "<strong>%s</strong><br/>
  Median household income (2000 decile): %g<br/>
  Median household income (2016 decile): %g<br/>
  MSA: %s<br/>
  Median metro household income (2016): %g",
  hh_inc_leaflet$NAME.x, hh_inc_leaflet$decile_2000, hh_inc_leaflet$decile_2016,
  hh_inc_leaflet$metro_name, hh_inc_leaflet$median_2016.x
) %>% lapply(htmltools::HTML)

leaflet(data = hh_inc_leaflet) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addProviderTiles(providers$OpenMapSurfer.AdminBounds) %>%
  addPolygons(fillColor = ~pal(hh_inc_leaflet$change),
              stroke = FALSE, fillOpacity = .5,
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, values = ~hh_inc_leaflet$change, opacity = 0.4,
            title = "Neighborhood type<br/>2000-2016", position = "bottomleft")


labels <- sprintf(
  "<strong>%s</strong><br/>
  Intersects: %g<br/>",
  arlington_bg2$GEOID,
  arlington_bg2$rail
) %>% lapply(htmltools::HTML)

leaflet(data = arlington_bg2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = tod_buffer2, fillColor = "red", color = "red",
              weight = 1) %>%
  addPolygons(weight = 1,
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))