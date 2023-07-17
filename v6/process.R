

## Cropppie project
## Shiny App: https://fabiolex.shinyapps.io/yieldPerv3/

# Load libraries ----------------------------------------------------------
# library(pacman)
# pacman::p_load(shiny, leaflet, glue, tidyverse, sf, RColorBrewer, lubridate, terra, raster)
# g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)
# 
# ## Months
# mnts <- tibble(month_floracion = c('enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre'), month = 1:12)
# 
# 
# # Read the table ----------------------------------------------------------
# tble <- suppressMessages(read_csv('www/cropie_data_v2.csv'))
# r1 <- terra::rast('www/chirps_1981-2022_v2.tif')
# cl <- as_tibble(terra::as.data.frame(r1, xy = T))
# table(tble$Cooperativa)
# 
# pnts <- tble
# pnts <- mutate(pnts, gid = 1:nrow(pnts), .before = Cooperativa)
# 
# # Months
# mnts <- tibble(month_floracion = c('enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre'), month = 1:12)

# Function ----------------------------------------------------------------
my_function <- function(  pnt = pnts, gd = 23, r1 = r1, percentile = 0.8, mnts){
  
  # month <- 'octubre'
  # gd <- 31
  # percentile <- 0.83
  
  pnt <- filter(pnt, id == gd)
  cpr <- unique(pnt$Cooperativa) # Que se alumbren todos los puntos de estas cooperativa de otro color
  all <- filter(pnt, Cooperativa == cpr)
  mn_flo <- filter(mnts, month_floracion == pnt$month_floracion) %>% pull(2) %>% .:(.+2) %>% ifelse(. > 12, .-12, .)
  yr_hrv <- year(pnt$date_hrvs)
  
  pos <- grep(paste0(1981:yr_hrv, collapse = '|'), names(r1), value = F) 
  stk <- r1[[pos]] 
  stk <- stk[[grep(paste0('.', mn_flo, '$', collapse = '|'), names(stk))]]
  stk_df <- terra::as.data.frame(stk, xy = T)
  rm(stk)
  
  years <- names(stk_df) %>% str_sub(., 11, 14) %>% unique()
  
  suppressMessages({
    aa <- lapply(1:length(years), function(k){
      res <- rowSums( stk_df[, grep(years[k], names(stk_df), value = F)])
      return(res)
    }) %>% bind_cols() 
  names(aa) <- years
  perc <- apply(aa, 1, quantile, probs = percentile, na.rm = T)
  rm(aa)
  stk_df <- bind_cols(stk_df[, 1:2], per = perc)
  stk.prc <- terra::rast(stk_df , type =  'xyz', digits = 3)
  crs(stk.prc) <- crs(r1)
  rm(stk_df)
  })
 
  
  
  
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
  # system.time(
  rcl <- stk.all %>% 
    terra::as.data.frame(., xy = T) %>%
    as_tibble() %>% 
    mutate(rcl = ifelse(act < prc, 0, 1)) %>% 
    dplyr::select(., 1, 2, 5) %>% 
    rast(., type = 'xyz')
  
  crs(rcl) <- crs(r1)
  # )
  #plot(rcl)
  
  # Return
  
  # bxp.dta <- as_tibble(cbind(dplyr::select(all, lon, lat, cos_kgha_1, cos_kgha_2), terra::extract(stk.all, dplyr::select(all, lon, lat))))
  # bxp.dta <- mutate(bxp.dta, class = ifelse(act < prc, 'Por debajo', 'Por encima'), class = factor(class, levels = c('Por debajo', 'Por encima')))
  # 
  # gbox <- ggplot(data = bxp.dta, aes(x = class, y = cos_kgha_1, fill = class)) + 
  #   geom_boxplot() + 
  #   scale_fill_manual(values =  c("#41b6c4", "#fed98e"))+
  #   xlab("Clasificiación de la Precipitación")+
  #   ylab("Rendimiento (Kg/ha)")+
  #   labs(fill = '') +
  #   theme_bw() + 
  #   theme(legend.position = 'bottom')
  
  
  return(list(r_cont = act_cont, r_bin = rcl, stk = stk.all, cpr = cpr))
      
}


# Now to make the boxplot




