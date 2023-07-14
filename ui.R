

# Load libraries ----------------------------------------------------------
library(pacman)
p_load(shiny, leaflet, raster, tidyverse, sf, RColorBrewer)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Interactive Cacao yield Map"),
  sidebarLayout(
    sidebarPanel(
      # h3("Seleccione el año a visualizar"),
      # selectInput("year", "Año", choices = c(2018, 2019), selected = 2018),
      #br(),
      h3("Select floration month: "),
      selectInput("mnth1","Month", choices = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
, selected = "Enero"),
      h3("Select percentile for historical precipitation:"),
     sliderInput("slider1", "Percentile:", min = 80, 100, value = 80, step = 1),
    uiOutput("bxplot1")
    ),
    mainPanel(
      leafletOutput("map1", height = "1100px")
    )
  )
)
