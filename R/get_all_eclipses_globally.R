library(swephR)
library(lubridate)
library(ggplot2)

rm(list=ls());cat('\f')

# https://www.astro.com/swisseph/swephprg.htm#_Toc112948985

# get all solar eclipses for next n_yrs


# vars----
n_yrs <- 10
dt_g  <- Sys.time() |> with_tz("UTC")
xyz_local <- c(-78.8986,35.9940)

# preprocess----
log.global_se <- data.frame()
end.dt_g <- dt_g %m+% years(n_yrs)

# error safety
n_stop <- 0

# loop
while(dt_g < end.dt_g){
  n_stop <- n_stop + 1
  stopifnot(n_stop < 1000)
  
  # convert gregorian time to julian time
  
  dt_j  <- swe_utc_to_jd(year     = year(dt_g), 
                         month    = month(dt_g), 
                         day      = mday(dt_g), 
                         houri    = hour(dt_g), 
                         min      = minute(dt_g), 
                         sec      = second(dt_g), 
                         gregflag = SE$GREG_CAL)$dret
  names(dt_j) <- c("UT", "ET")
  
  # convert to ET, TT, UT (all about the same +/- 35 ~seconds)
  dt_et <- dt_j["ET"]
  
  # when will next eclipse be on earth? calculate in ET
  sol_next_glob.et <- swephR::swe_sol_eclipse_when_glob(jd_start  = dt_et, 
                                                        ephe_flag = SE$FLG_JPLEPH, # NASA JPL ephemeris
                                                        ifltype   = c(0), 
                                                        # eclipse type as integer 
                                                        # (SE$ECL_CENTRAL=1,
                                                        # SE$ECL_NONCENTRAL=2, 
                                                        # SE$ECL_TOTAL=4,
                                                        # SE$ECL_ANNULAR=8, 
                                                        # SE$ECL_PARTIAL=16,
                                                        # SE$ECL_ANNULAR_TOTAL=32 or 0 for any)
                                                        backward  = F)$tret[1]
  # convert to greg
  sol_next_glob.temp  <- swe_jdet_to_utc(jd_et = sol_next_glob.et, 
                                         gregflag = SE$GREG_CAL)
  
  sol_next_glob.g <- paste(sol_next_glob.temp$year_out,
                           "-",
                           sol_next_glob.temp$month_out, 
                           "-",
                           sol_next_glob.temp$day_out, 
                           " ", 
                           sol_next_glob.temp$hour_out, 
                           ":", 
                           sol_next_glob.temp$min_out, 
                           ":", 
                           sol_next_glob.temp$sec_out, 
                           sep = "") |> ymd_hms()
  # remove vars
  rm(sol_next_glob.temp, dt_et, dt_j)
  
  
  # where will eclipse be at maximum? 
  sol_next.lonlat <- swephR::swe_sol_eclipse_where(jd_ut = sol_next_glob.et, 
                                                   ephe_flag = SE$FLG_JPLEPH)$pathpos[c(1,2)] # NASA JPL ephemeris)
  # add "height" dim
  sol_next.lonlat[3] <- 0
  
  names(sol_next.lonlat) <- c("lon", "lat", "ht")
  
  # what type of eclipse globally?
  sol_max.attr <- swephR::swe_sol_eclipse_how(jd_ut = sol_next_glob.et, 
                                              ephe_flag = SE$FLG_JPLEPH, 
                                              geopos = sol_next.lonlat)
  
  sol_next.type <- NULL  
  if(sol_max.attr$attr[1] == sol_max.attr$attr[9]){
    sol_next.type <- c(sol_next.type, "Partial Eclipse")
  }
  if(sol_max.attr$attr[2] == sol_max.attr$attr[9]){
    sol_next.type <- c(sol_next.type, "annular and total eclipses")
  }
  
  #sol_next.type <- paste(sol_next.type, sep = ", ", collapse = ", ")
  #print(length(sol_next.type))
  stopifnot(exprs = length(sol_next.type) == 1)
  
  # obscuration 
  if(sol_max.attr$attr[3] > 1 & 
     grepl("annular and total", x = sol_next.type)){
    # is total eclipse
    sol_next.type <- gsub(pattern = "annular and total eclipses", 
                          replacement = "Total Eclipse", 
                          x = sol_next.type)
  }else{
    # is annular eclipse
    sol_next.type <- gsub(pattern = "annular and total eclipses", 
                          replacement = "Annular Eclipse", 
                          x = sol_next.type)
  }
  
  # update log----
  log.global_se <- rbind(log.global_se, 
                         data.frame(time_UTC = sol_next_glob.g, 
                                    type     = sol_next.type, 
                                    lon = sol_next.lonlat["lon"], 
                                    lat = sol_next.lonlat["lat"], 
                                    z_m = sol_next.lonlat["ht"]))
  # update date progression
  
  dt_g <- sol_next_glob.g %m+% days(1)
  
  rm(sol_next.type)
}

log.global_se$type_f <- factor(log.global_se$type, 
                                  levels = c("Partial Eclipse", 
                                             "Annular Eclipse", 
                                             "Total Eclipse"))

# rando.lon <- runif(n = 100, min = -180, max = 180)
# rando.lat <- runif(n = 100, min = -90, max = 90)
# 
# ggplot() + 
#   geom_point(aes(x = rando.lon, y = rando.lat)) + 
#   coord_quickmap(xlim = c(-180,180), 
#                  ylim = c(-90,90))+
#   scale_y_continuous(labels = seq(-90,90, by = 15), 
#                      breaks = seq(-90,90, by = 15), 
#                      limits = c(-90,90))+
#   scale_x_continuous(labels = seq(-180,180,by = 30), 
#                      breaks = seq(-180, 180, by = 30), 
#                      limits = c(-180,180))
