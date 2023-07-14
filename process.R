

## Cropppie project
## Shiny App: https://fabiolex.shinyapps.io/yieldPerv3/

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(shiny, leaflet, glue, tidyverse, sf, RColorBrewer, lubridate, terra, raster)
g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

## Months
mnts <- tibble(month_floracion = c('enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre'), month = 1:12)

# Tabular data ------------------------------------------------------------
rawt <- read_csv('www/cropie_data_cll.csv') %>% drop_na(date_hrvs)
tble <- read_csv('twww/Croppie_line_base_corrected.csv')
names(tble)[1] <- "id"

# Spatial data ------------------------------------------------------------
r1 <- terra::rast("www/chirps_1981-2022.tif")
r1 <- terra::crop(r1, c(-81.345, -73, -14, -2))

ms <- r1[[1]] * 0 + 1
plot(ms); points(tble$lon, tble$lat, pch = 16, col = 'red')

# Extents and extract by mask ---------------------------------------------
e1 <- c(-79.9747170943073, -78.0174669906616, -7.3, -4.8)
e2 <- c(-76, -73, -13, -10)

ms %>% terra::crop(., e1) %>% plot(); points(tble$lon, tble$lat, pch = 16, col = 'red')
ms %>% terra::crop(., e2) %>% plot(); points(tble$lon, tble$lat, pch = 16, col = 'red')

r1 <- mosaic(terra::crop(r1, e1), terra::crop(r1, e2))
plot(r1[[1]]); points(tble$lon, tble$lat)

terra::writeRaster(x = r1, filename = 'www/inputs/chirps_1981-2022_v2.tif')

# Tidy the table ----------------------------------------------------------
colnames(rawt)
tble <- tble %>% dplyr::select(Cooperativa, DEPARTAMENTO, PROVINCIA, DISTRITO, GPS_longitude, GPS_latitude, cosecha_ha_parcela_2, cosecha_ha_anoanterior_2, 
                               month_floracion = floracion_mes,
                               cosecha_ha_anoanterior, date_start = start, date_end = end, date_harvested = Fecha_de_la_posible_cosecha)


tble <- dplyr::select(tble, -cosecha_ha_anoanterior) %>% rename(cos_kgha_1 = cosecha_ha_parcela_2, cos_kgha_2 = cosecha_ha_anoanterior_2)
write.csv(tble, 'www/cropie_data_v2.csv', row.names = FALSE)

# Read the table ----------------------------------------------------------
tble <- suppressMessages(read_csv('www/cropie_data_v2.csv'))
r1 <- terra::rast('www/chirps_1981-2022_v2.tif')
cl <- as_tibble(terra::as.data.frame(r1, xy = T))

table(tble$Cooperativa)
hist(tble$cosecha_ha_anoanterior_2)
hist(tble$cosecha_ha_anoanterior)

pnts <- tble
pnts <- mutate(pnts, gid = 1:nrow(pnts), .before = Cooperativa)

# Months
mnts <- tibble(month_floracion = c('enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre'), month = 1:12)

# Function ----------------------------------------------------------------
my_function <- function(month = 'octubre', pnts = pnts, gd = 23, chrp = r1, percentile = 0.8){
  
  # month <- 'octubre'
  # gd <- 23
  # percentile <- 0.6
  
  pnt <- filter(pnts, month_floracion == month)
  pnt <- filter(pnt, gid == gd)
  cpr <- unique(pnt$Cooperativa) # Que se alumbren todos los puntos de estas cooperativa de otro color
  all <- filter(pnts, Cooperativa == cpr)
  mn_flo <- filter(mnts, month_floracion == pnt$month_floracion) %>% pull(2) %>% .:(.+2) %>% ifelse(. > 12, .-12, .)
  yr_hrv <- year(pnt$date_harvested)
  
  pos <- grep(paste0(1981:yr_hrv, collapse = '|'), names(r1), value = F) 
  stk <- r1[[pos]]
  stk <- stk[[grep(paste0('.', mn_flo, '$', collapse = '|'), names(stk))]]
  
  years <- names(stk) %>% str_sub(., 11, 14) %>% unique()
  stk.sum <- map(.x = 1:length(years), .f = function(y){sum(stk[[grep(years[y], names(stk), value = F)]])}) %>% reduce(., c)
  names(stk.sum) <- glue('prec_sum_{years}')
  
  stk.prc <- terra::app(x = stk.sum, fun = function(x) quantile(x, probs = percentile, na.rm = T))
  
  # Precipitation (date)
  if(any(mn_flo == c(11, 12, 1)) ){
    yr <- c(yr_hrv, yr_hrv, yr_hrv+1)
  }else if(any(mn_flo == c(12, 1, 2))){
    yr <- c(yr_hrv, yr_hrv+1, yr_hrv+1)
  }else{
    yr <- rep(yr_hrv, 3)
  }
  
  act <- r1[[grep( paste0(yr, '.', mn_flo, '$', collapse = "|") , names(r1), value = FALSE)]]
  act_cont <- sum(act)
  
  stk.all <- c(act_cont, stk.prc)
  names(stk.all) <- c('act', 'prc')
  rcl <- stk.all %>% terra::as.data.frame(., xy = T) %>% as_tibble() %>% mutate(rcl = ifelse(act < prc, 0, 1)) %>% dplyr::select(., 1, 2, 5) %>% rast(., type = 'xyz')
  plot(rcl)
  
  # Return 
  return(list(r_cont = act_cont, r_bin = rcl))
  
  # Now to make the boxplot
  bxp.dta <- as_tibble(cbind(dplyr::select(all, GPS_longitude, GPS_latitude, cos_kgha_1, cos_kgha_2), terra::extract(stk.all, dplyr::select(all, GPS_longitude, GPS_latitude))))
  bxp.dta <- mutate(bxp.dta, class = ifelse(act < prc, 'Por debajo', 'Por encima'), class = factor(class, levels = c('Por debajo', 'Por encima')))
  
  gbox <- ggplot(data = bxp.dta, aes(x = class, y = cos_kgha_1, fill = class)) + 
    geom_boxplot() + 
    scale_fill_manual(values =  c("#41b6c4", "#fed98e"))+
    xlab("Clasificiación de la Precipitación")+
    ylab("Rendimiento (Kg/ha)")+
    labs(fill = '') +
    theme_bw() + 
    theme(legend.position = 'bottom')
      
}

my_function(month = 'octubre', pnts = pnts, gd = 23, chrp = r1, percentile = 0.8)



