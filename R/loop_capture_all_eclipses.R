

renv::use(
  base64enc         = "base64enc@0.1-3",
  bslib             = "bslib@0.9.0",
  cachem            = "cachem@1.1.0",
  cli               = "cli@3.6.4",
  colorspace        = "colorspace@2.1-1",
  cpp11             = "cpp11@0.5.1",
  crosstalk         = "crosstalk@1.2.1",
  digest            = "digest@0.6.37",
  dplyr             = "dplyr@1.1.4",
  evaluate          = "evaluate@1.0.3",
  fansi             = "fansi@1.0.6",
  farver            = "farver@2.1.2",
  fastmap           = "fastmap@1.2.0",
  fontawesome       = "fontawesome@0.5.3",
  fs                = "fs@1.6.5",
  generics          = "generics@0.1.3",
  glue              = "glue@1.8.0",
  highr             = "highr@0.11",
  htmltools         = "htmltools@0.5.8.1",
  htmlwidgets       = "htmlwidgets@1.6.4",
  jquerylib         = "jquerylib@0.1.4",
  jsonlite          = "jsonlite@1.9.0",
  knitr             = "knitr@1.49",
  labeling          = "labeling@0.4.3",
  lattice           = "lattice@0.22-6",
  lazyeval          = "lazyeval@0.2.2",
  leaflet           = "leaflet@2.2.2",
  leaflet.providers = "leaflet.providers@2.0.0",
  lifecycle         = "lifecycle@1.0.4",
  lubridate         = "lubridate@1.9.4",
  magrittr          = "magrittr@2.0.3",
  memoise           = "memoise@2.0.1",
  mime              = "mime@0.12",
  munsell           = "munsell@0.5.1",
  pillar            = "pillar@1.10.1",
  pkgconfig         = "pkgconfig@2.0.3",
  png               = "png@0.1-8",
  R6                = "R6@2.6.1",
  rappdirs          = "rappdirs@0.3.3",
  raster            = "raster@3.6-31",
  RColorBrewer      = "RColorBrewer@1.1-3",
  Rcpp              = "Rcpp@1.0.14",
  renv              = "renv@1.1.1",
  rlang             = "rlang@1.1.5",
  rmarkdown         = "rmarkdown@2.29",
  sass              = "sass@0.4.9",
  scales            = "scales@1.3.0",
  sp                = "sp@2.2-0",
  swephR            = "swephR@0.3.1",
  terra             = "terra@1.8-21",
  tibble            = "tibble@3.2.1",
  tidyselect        = "tidyselect@1.2.1",
  timechange        = "timechange@0.3.0",
  tinytex           = "tinytex@0.55",
  utf8              = "utf8@1.2.4",
  vctrs             = "vctrs@0.6.5",
  viridisLite       = "viridisLite@0.4.2",
  withr             = "withr@3.0.2",
  xfun              = "xfun@0.50",
  yaml              = "yaml@2.3.10"
)


renv::embed()

library(renv)
library(swephR)
library(leaflet)
#library(ggplot2)
# library(devtools)
# devtools::install_github("chris-prener/censusxy")
#library(censusxy)
library(lubridate)
#library(scales)
library(tibble)
library(dplyr)

rm(list=ls());cat('\f')


fun_get.e <- function(x=-78.938,
                      y=36.001,
                      dtg = with_tz(time = ymd_hms("2025-08-08 00:00:00",      
                                                   tz = "America/New_York"), 
                                    tzone = "UTC"),
                      yrs_fwd = 88){
  require(lubridate)
  # check class of dtg
  stopifnot("POSIXct" %in% class(dtg))
  
  ## Example Input Variables
  input_lonlat     <- c("lon" = x, 
                        "lat" = y) 
  input_gregtime   <- dtg
  look_forward_yrs <- yrs_fwd
  
  # analysis
  end_gregtime     <- input_gregtime %m+% years(look_forward_yrs)
  cur_gregtime     <- input_gregtime
  
  
  error.chk <- 0
  df.out <- NULL
  while(cur_gregtime < end_gregtime){
    # error stop
    error.chk <- error.chk + 1
    stopifnot(error.chk < 10000)
    
    # code
    ## Example Solar Eclipse Calculation
    # convert gregorian time to julian time (ephemeral time i.e. "ET")
    var_jultime <- swe_utc_to_jd(year  = year(cur_gregtime), 
                                 month = month(cur_gregtime), 
                                 day   = mday(cur_gregtime), 
                                 houri = hour(cur_gregtime), 
                                 min   = minute(cur_gregtime), 
                                 sec   = second(cur_gregtime), 
                                 gregflag = SE$GREG_CAL)$dret[2]
    
    # calculate the next solar eclipse visible to input_location----
    temp.sol <- swe_sol_eclipse_when_loc(jd_start  = var_jultime, 
                                         ephe_flag = SE$FLG_MOSEPH, 
                                         geopos    = c(input_lonlat["lon"], 
                                                       input_lonlat["lat"],0), 
                                         backward  = F)
    
    s.nextLocalEcl_Jd <- temp.sol$tret[c(1)]
    names(s.nextLocalEcl_Jd) <- "time_of_max_eclipse"
    
    # calculate next lunar eclipse visible to input_location----
    temp.lun <- swe_lun_eclipse_when_loc(jd_start  = var_jultime, 
                                         ephe_flag = SE$FLG_MOSEPH, 
                                         geopos    = c(input_lonlat["lon"], 
                                                       input_lonlat["lat"],0), 
                                         backward  = F)
    l.nextLocalEcl_Jd <- temp.lun$tret[c(0,2,4)+1]
    names(l.nextLocalEcl_Jd) <- c("time_of_max_eclipse", 
                                  "time_of_partial_phase_start", 
                                  "time_of_total_phase_start")
    
    
    # which was was first? solar or lunar? 
    next_is_solar <- ifelse(test = s.nextLocalEcl_Jd["time_of_max_eclipse"] < 
                              l.nextLocalEcl_Jd["time_of_max_eclipse"], T, F)
    
    
    if(next_is_solar){
      
      
      s.nextEcl.temp <- swe_jdet_to_utc(jd_et    = s.nextLocalEcl_Jd["time_of_max_eclipse"], 
                                        gregflag = SE$GREG_CAL)
      
      ecl_gregtime <- paste(s.nextEcl.temp$year_out,"-",
                            s.nextEcl.temp$month_out,"-",
                            s.nextEcl.temp$day_out," ",
                            s.nextEcl.temp$hour_out,":",
                            s.nextEcl.temp$min_out,":",
                            s.nextEcl.temp$sec_out,
                            sep = "") |>
        ymd_hms()
      
      ecl_type     <- "solar"
      ecl_subtype  <- NA
      obscuration  <- temp.sol$attr[c(2)+1]
      
      rm(s.nextEcl.temp)
      
      df.out <- rbind(df.out, 
                      data.frame(eclipse_type    = ecl_type,
                                 eclipse_subtype = NA,
                                 eclipse_greg    = ecl_gregtime, 
                                 obsc.mag_percent = obscuration))
      
    }else{
      
      l.nextEcl.temp <- swe_jdet_to_utc(jd_et    = l.nextLocalEcl_Jd["time_of_max_eclipse"], 
                                        gregflag = SE$GREG_CAL)
      
      ecl_gregtime <- paste(l.nextEcl.temp$year_out,"-",
                            l.nextEcl.temp$month_out,"-",
                            l.nextEcl.temp$day_out," ",
                            l.nextEcl.temp$hour_out,":",
                            l.nextEcl.temp$min_out,":",
                            l.nextEcl.temp$sec_out,
                            sep = "") |>
        ymd_hms()
      
      ecl_type     <- "lunar"
      ecl_subtype  <- NA
      umbral.mag  <- temp.lun$attr[c(0)+1]
      
      rm(l.nextEcl.temp)
      
      df.out <- rbind(df.out, 
                      data.frame(eclipse_type    = ecl_type, 
                                 eclipse_subtype = NA,
                                 eclipse_greg    = ecl_gregtime, 
                                 obsc.mag_percent = umbral.mag))
    }
    
    
    cur_gregtime <- ecl_gregtime %m+% hours(12)
    
    #print(cur_gregtime)
    
  }
  
  #df.out$eclipse_greg |> with_tz("America/New_York")
  
  # add lonlat
  df.out$lon <- x
  df.out$lat <- y
  
  return(df.out)
}


df <- fun_get.e(yrs_fwd = 10) |> as_tibble()


df %>%
  group_by(eclipse_type, 
           obsc.mag_gte1 = obsc.mag_percent >= 1, 
           start_yr = min(year(eclipse_greg)),
           end_yr   = max(year(eclipse_greg))) %>%
  summarise(n = n(), 
            max_obsc.mag = max(obsc.mag_percent))


# # get this week's lunar eclipse info----
# a.time    <- ymd_hms("2026-08-01 12:00:01 PM") |> with_tz("America/New_York")
# 
# ecl.lunar <- swe_lun_eclipse_when_loc(jd_start = swe_utc_to_jd(year = year(a.time), 
#                                                                month = month(a.time), 
#                                                                day = mday(a.time), 
#                                                                houri = hour(a.time), 
#                                                                min = minute(a.time), 
#                                                                sec = second(a.time), 
#                                                                gregflag = SE$GREG_CAL)$dret[(c(1)+1)], 
#                                       ephe_flag = SE$FLG_MOSEPH, 
#                                       geopos    = c(input_lonlat["lon"], 
#                                                     input_lonlat["lat"],0), 
#                                       backward  = F)
# 
# 
# temp.names <- "* tret[0] time of maximum eclipse
# 
# * tret[1] not used
# 
# * tret[2] time of partial phase begin (indices consistent with solar eclipses)
# 
# * tret[3] time of partial phase end
# 
# * tret[4] time of totality begin
# 
# * tret[5] time of totality end
# 
# * tret[6] time of penumbral phase begin
# 
# * tret[7] time of penumbral phase end
# 
# * tret[8] time of moonrise, if it occurs during the eclipse
# 
# * tret[9] time of moonset, if it occurs during the eclipse"
# 
# 
# temp.names <-   gsub("\\* tret\\[\\d{1,}\\]", "", temp.names) |> 
#   strsplit(split = "\n\n") |> 
#   unlist() |> trimws()
# 
# names(ecl.lunar$tret) <- temp.names
# 
# ecl.lunar$tret 
# 
# 
# temp.names <- "* attr[0] umbral magnitude at tjd
# 
# * attr[1] penumbral magnitude
# 
# * attr[4] azimuth of moon at tjd
# 
# * attr[5] true altitude of moon above horizon at tjd
# 
# * attr[6] apparent altitude of moon above horizon at tjd
# 
# * attr[7] distance of moon from opposition in degrees
# 
# * attr[8] umbral magnitude at tjd (= attr[0])
# 
# * attr[9] saros series number (if available; otherwise -99999999)
# 
# * attr[10] saros series member number (if available; otherwise -99999999)"
# 
# temp.names <-   gsub("\\* attr\\[\\d{1,}\\]", "", temp.names) |> 
#   strsplit(split = "\n\n") |> 
#   unlist() |> trimws()
# 
# names(ecl.lunar$attr) <- paste("not.used", 1:length(ecl.lunar$attr), 
#                                sep = "_")
# 
# names(ecl.lunar$attr)[c(0,1,4,5,6,7,8,9,10)+1] <- temp.names
# 
# 
# moon_pos <- c(alt = unname(ecl.lunar$attr["true altitude of moon above horizon at tjd"]), 
#               azim = unname(ecl.lunar$attr["azimuth of moon at tjd"]),
#               datetime_jd = unname(ecl.lunar$tret["time of maximum eclipse"]), 
#               datetime_greg = NA)
# 
# temp.moongregtime <- swe_jdet_to_utc(jd_et = unlist(unname(moon_pos["datetime_jd"])), 
#                                      gregflag = SE$GREG_CAL)
# 
# moon_pos["datetime_greg"] <- paste(temp.moongregtime$year_out, "-", 
#                                    temp.moongregtime$month_out, "-", 
#                                    temp.moongregtime$day_out, " ",
#                                    temp.moongregtime$hour_out, ":",
#                                    temp.moongregtime$min_out, ":",
#                                    temp.moongregtime$sec_out,
#                                    
#                                    sep = "", collapse = "") |> ymd_hms()
# 
# 
# moon_pos["datetime_greg"] <- as_datetime(moon_pos["datetime_greg"]) |> 
#   with_tz("America/New_York")
# 
# 
# for(i in 1:length(ecl.lunar$tret)){
#   ecl.lunar$tret[i] 
#   
#   temp.time <- swe_jdet_to_utc(jd_et = unlist(unname(ecl.lunar$tret[i])), 
#                                gregflag = SE$GREG_CAL)
#   
#   ecl.lunar$tret[i] <- paste(temp.time$year_out, "-", 
#                              temp.time$month_out, "-", 
#                              temp.time$day_out, " ",
#                              temp.time$hour_out, ":",
#                              temp.time$min_out, ":",
#                              temp.time$sec_out,
#                              
#                              sep = "", collapse = "") |> ymd_hms() 
# }
# 
# ecl.lunar$tret |> 
#   as_datetime() |> 
#   with_tz("America/New_York") |> 
#   sort()
# 
# lunar.times <- ecl.lunar$tret[c(1:7)]
# 
# names(lunar.times) <- trimws(gsub("time of", "", trimws(gsub("\\(.*\\)", "", 
#                                                              names(lunar.times))))) |> 
#   gsub(pattern = " ", replacement = "_") |> 
#   gsub(pattern = "_end", replacement = ".end") |> 
#   gsub(pattern = "_begin", replacement = ".begin")
# 
# lunar.times <- as_datetime(lunar.times) |> 
#   with_tz("America/New_York") |> 
#   sort()
# lunar.times <- lunar.times[names(lunar.times) != "not_used"]
# 
# lunar.times <- strftime(lunar.times, 
#          format = "%A %I:%M%p  %Z")
# 
# lt2 <- as.data.frame(lunar.times)
# 
# lt2$lunar_phase <- rownames(lt2)
# rownames(lt2) <- 1:nrow(lt2)
# 
# colnames(lt2)[1] <- "local_time"
# 
# ecl.lunar$attr[c("azimuth of moon at tjd",
#                  "true altitude of moon above horizon at tjd")]
# 
# lt2
# 
# as_datetime(var_jultime)
# 
# 
# 
# leaflet() |> 
#   leaflet::addTiles() |> 
#   leaflet::addMarkers(lng = input_lonlat["lon"], 
#                       lat = input_lonlat["lat"], 
#                       label = "my_loc")
# 
