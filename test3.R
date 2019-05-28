library(rnaturalearth)
library(plotly)
world <- ne_countries(returnclass = "sf")
class(world)
#> [1] "sf"    "data.frame"
plot_ly(world, color = I("gray90"), stroke = I("black"), span = I(1))
