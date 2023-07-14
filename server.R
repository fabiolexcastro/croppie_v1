

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(shiny, leaflet, tidyverse, sf, RColorBrewer, lubridate, terra, raster)
# setwd("D:/CGIAR/Castro, Fabio (Alliance Bioversity-CIAT) - ShinyApp/v1")
# setwd("C:/Users/acmendez/Downloads/v2")

# Functions ---------------------------------------------------------------
my_function <- function(pnts, gd, chrp, mnts, percentile){
  
  cat('To make ', gd, '\n')
  
  pnt <- filter(pnts, id == gd)
  yr_hrv <- year(pnt$date_hrvs)
  mn_flo <- filter(mnts, month_floracion == pnt$month_floracion) %>% pull(2)
  mn_flo_s <- mn_flo:(mn_flo+2)
  mn_flo_s <- ifelse(mn_flo_s > 12, mn_flo_s-12, mn_flo_s)
 
  pos <- grep(paste0(1981:yr_hrv, collapse = '|'), names(chrp), value = F)
  stk <- chrp[[pos]]
  stk <- stk[[grep(paste0('.', mn_flo_s, '$', collapse = '|'), names(stk))]]
  vls <- terra::extract(stk, pnt[,c('lon', 'lat')]) %>% 
    gather(var, value) %>% 
    filter(var != 'ID') %>% 
    mutate(year = str_sub(var, 11, 14)) %>% 
    group_by(year) %>% 
    dplyr::summarise(value = sum(value)) %>% 
    pull(2)
 
  prc <- rownames_to_column(data.frame(q = quantile(vls, seq(0.8, 1, 0.01))))
  colnames(prc) <- c('percentile', 'prec')
  prc$percentile <- as.numeric(gsub("%","",prc$percentile ))
  
  precipitation_hist <- prc[prc$percentile == percentile, "prec"]
  
  # Precipitation (date)
  
  if(any(mn_flo_s == c(11, 12, 1)) ){
    yr <- c(yr_hrv, yr_hrv, yr_hrv+1)
  }else if(any(mn_flo_s == c(12, 1, 2))){
    yr <- c(yr_hrv, yr_hrv+1, yr_hrv+1)
  }else{
    yr <- rep(yr_hrv, 3)
  }
  
  rst <- chrp[[grep( paste0(yr, '.', mn_flo_s, '$', collapse = "|") , names(chrp), value = FALSE)]]
  vle <- sum(terra::extract(rst, pnt[,c('lon', 'lat')])[-1])
  
  r_cont <- sum(rst)
  
  mtx <- matrix(c(0, precipitation_hist, 0, precipitation_hist , Inf, 1), ncol=3, byrow=TRUE )
  
  r_bin  <- terra::classify(r_cont, mtx,  include.lowest=F, right=F, others = 0)
  
  return(list(r_cont = r_cont, r_bin = r_bin))
  
}

tble <- read_csv('www/cropie_data_cll.csv') %>% drop_na(date_hrvs)
names(tble)[1] <- "id"
r1 <- terra::rast("www/inputs/chirps_1981-2022.tif")
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
    h3("Boxplot rendimiento"),
    plotOutput("bx1")
    
  )
})

output$bx1 <- renderPlot({
  req(outs$rasts, outs$filt_db)
  to_plot <- outs$filt_db
  to_plot$vals <- terra::extract(outs$rasts$r_bin, to_plot[, c("lon", "lat")] )[,2]
  to_plot$vals <- ifelse(to_plot$vals == 0, "Por debajo", "Por arriba")
  
  to_plot %>% 
    ggplot()+
    geom_boxplot(aes(x = vals, y = cos_kgha_2, group = vals, fill = vals))+
    scale_fill_manual(values =  c("#41b6c4", "#fed98e"))+
    xlab("Clasificiación de la Precipitación")+
    ylab("Rendimiento (Kg/ha)")+
    theme_bw()
  
})

observeEvent(c(input$map1_marker_click, input$slider1), {
  #req(outs$filt_db)
  req(input$map1_marker_click)
  
  clicked_mrk <- input$map1_marker_click
  tble <- outs$filt_db
  tble$clicked <- FALSE
  tble[tble$id == clicked_mrk$id, "clicked"] <- TRUE
  
  outs$rasts <- my_function(pnts = tble, 
                            gd = clicked_mrk$id, 
                            chrp = r1, 
                            mnts = mnts, 
                            percentile = input$slider1)
  
  
  
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
