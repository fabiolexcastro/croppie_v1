

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(shiny, leaflet, tidyverse, sf, RColorBrewer, lubridate, terra, raster)
# setwd("D:/CGIAR/Castro, Fabio (Alliance Bioversity-CIAT) - ShinyApp/v1")
# setwd("C:/Users/acmendez/Downloads/v2")

source("process.R")
tble <- read_csv('www/cropie_data_v2.csv') %>% 
  drop_na(date_hrvs) %>% 
  mutate(date_hrvs = as.Date(date_hrvs, "%m/%d/%y"))
names(tble)[1] <- "id"
r1 <- terra::rast("www/chirps_1981-2022_v3.tif")
r1 <- terra::crop(r1, c(-81.345, -73, -14, -2))

mnts <- tibble(month_floracion = c('enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre'), month = 1:12)

# Server ------------------------------------------------------------------
server <- function(input, output){
  
  outs <- reactiveValues()
 
  output$map1 <- renderLeaflet({
    
    # Definimos las paletas de colores para cada año
    leaflet() %>% 
      addProviderTiles(provider = "CartoDB.Positron")
      
    
  })
  
  observeEvent(input$mnth1, {
    
    outs$filt_db <- tble %>% 
      filter(month_floracion == tolower(input$mnth1))
  
    
    leafletProxy("map1") %>%
      clearGroup( group = "m1") %>% 
      clearGroup( group = "m2") %>%
      clearGroup( group = "Prec cont") %>%
      clearGroup( group = "Prec class") %>%
      clearControls() %>% 
      addMarkers(layerId =  outs$filt_db$id,
                 lng = outs$filt_db$lon, 
                 lat = outs$filt_db$lat, 
                 popup = paste0('<b>Locality:</b> ', outs$filt_db$dpto, "<br>", 
                                "<b>Floration month:</b>", outs$filt_db$month_floracion, "<br>",
                                "<b>Yield 1:</b>", outs$filt_db$cos_kgha_1, " Kg/ha", "<br>",
                                "<b>Yield 2:</b>", outs$filt_db$cos_kgha_2, " Kg/ha"),
                 group = "m1") 
    
  
  })
  
  
  output$bxplot1 <- renderUI({
    req(outs$rasts, outs$filt_db)
    
    tagList(
      h3(paste0("Boxplot rendimiento Coop. ", outs$rasts$cpr)),
      plotOutput("bx1")
      
    )
  })

  output$bx1 <- renderPlot({
    req(outs$rasts, outs$filt_db)
    
   
    to_plot <- outs$filt_db
    
    to_plot <- as_tibble(cbind(dplyr::select(to_plot, lon, lat, cos_kgha_1, cos_kgha_2), terra::extract(outs$rasts$stk, dplyr::select(to_plot, lon, lat))))
    
    to_plot <- mutate(to_plot, class = ifelse(act < prc, 'Por debajo', 'Por encima'), class = factor(class, levels = c('Por debajo', 'Por encima')))
    
    if(nrow(to_plot) != 1){
      ggplot(data = to_plot, aes(x = class, y = cos_kgha_2, fill = class)) + 
        geom_boxplot() + 
        scale_fill_manual(values =  c("#41b6c4", "#fed98e"))+
        xlab("Clasificiación de la Precipitación")+
        ylab("Rendimiento (Kg/ha)")+
        labs(fill = '') +
        theme_bw() + 
        theme(legend.position = 'bottom')
      
    }else{
      showModal(modalDialog(
        title = "Error",
        "Very low number of data to make boxplot.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
   
    
  })

  observeEvent(c(input$map1_marker_click, input$slider1), {
    #req(outs$filt_db)
    req(input$map1_marker_click)
   
    clicked_mrk <- input$map1_marker_click
    tble <- outs$filt_db
    tble$clicked <- FALSE
    tble[tble$id == clicked_mrk$id, "clicked"] <- TRUE
    
  
    outs$rasts <- my_function(pnt = tble, 
                              gd = clicked_mrk$id, 
                              r1 = r1, 
                              mnts = mnts, 
                              percentile = input$slider1/100)
    
    r1 <- raster::raster(outs$rasts$r_cont)
    pal1 <- colorNumeric(palette = c("#f0f9e8",
                           "#bae4bc",
                           "#7bccc4",
                           "#43a2ca",
                           "#0868ac"),
                        values(r1),
                        na.color = "transparent")
    
    r2 <- raster::raster(outs$rasts$r_bin)
    r2[is.nan(r2)] <- NA
    r2 <- raster::as.factor(r2)
    pal2 <- colorFactor (palette = c("#fed98e", "#41b6c4"),values(r2),
                         na.color = "transparent")
    
    pal2.1 <- colorFactor (palette = c("#fed98e", "#41b6c4"),ifelse(values(r2) == 0 , "Por debajo", "Por encima"),
                         na.color = "transparent")
  
    
    leafletProxy("map1") %>% 
      clearGroup( group = "m1") %>% 
      clearGroup( group = "m2") %>%
      clearGroup( group = "Prec cont") %>%
      clearGroup( group = "Prec class") %>%
      clearControls() %>% 
      addMarkers(layerId =  tble$id,
                 lng = tble$lon, 
                 lat = tble$lat, 
                 popup = paste0('<b>Locality:</b> ', tble$dpto, "<br>", 
                                "<b>Floration month:</b>", tble$month_floracion, "<br>",
                                "<b>Yield 1:</b>", tble$cos_kgha_1, " Kg/ha", "<br>",
                                "<b>Yield 2:</b>", tble$cos_kgha_2, " Kg/ha"),
                 group = "m1") %>% 
      removeMarker(layerId = clicked_mrk$id) %>% 
      addAwesomeMarkers(
        lng = clicked_mrk$lng,
        lat = clicked_mrk$lat,
        layerId = clicked_mrk$id,
        group = "m2",
        popup = paste0('<b>Locality:</b> ', tble$dpto, "<br>", 
                       "<b>Floration month:</b>", tble$month_floracion, "<br>",
                       "<b>Yield 1:</b>", tble$cos_kgha_1, " Kg/ha", "<br>",
                       "<b>Yield 2:</b>", tble$cos_kgha_2, " Kg/ha" ),
        icon = awesomeIcons(
          icon = 'ios-close',
          iconColor = 'white',
          library = 'ion',
          markerColor = "red"
        )) %>% 
      addRasterImage(r2, colors = pal2, opacity = 0.8, group = "Prec class") %>%
      addLegend(pal = pal2.1, values = ifelse(values(r2) == 0 , "Por debajo", "Por encima"),
                title = "Precipitation binary") %>% 
      addRasterImage(r1, colors = pal1, opacity = 0.8, group = "Prec cont") %>%
      addLegend(pal = pal1, values = values(r1),
                title = "Precipitation (mm)") %>% 
      addLayersControl(
        baseGroups = c("Prec class", "Prec cont"),
        options = layersControlOptions(collapsed = FALSE)
      )
      
    
  })
 
  
}
