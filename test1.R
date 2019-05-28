library(geojsonio)
require(leaflet)
require(htmltools)

spdf <- geojson_read("data/europe.geo.json",  what = "sp")
names(spdf)



density <- state.x77[, "Population"] / state.x77[, "Area"]

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white')
)

plot_geo() %>%
  add_trace(
    z = ~density, text = state.name, span = I(0),
    locations = state.abb, locationmode = 'USA-states'
  ) %>%
  layout(geo = g)