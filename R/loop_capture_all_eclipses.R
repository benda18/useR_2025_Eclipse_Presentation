

renv::use(
  cpp11      = "cpp11@0.5.1",
  generics   = "generics@0.1.3",
  lubridate  = "lubridate@1.9.4",
  Rcpp       = "Rcpp@1.0.14",
  renv       = "renv@1.1.1",
  swephR     = "swephR@0.3.1",
  timechange = "timechange@0.3.0"
)



#make cooords for eclipse path




renv::embed()

library(renv)
library(swephR)
#library(leaflet)
#library(ggplot2)
# library(devtools)
# devtools::install_github("chris-prener/censusxy")
#library(censusxy)
library(lubridate)
#library(scales)


rm(list=ls());cat('\f')

## Example Input Variables
input_lonlat     <- c("lon" = -78.938, "lat" = 36.001)  # Duke Univ
input_gregtime   <- ymd_hms("2025-08-08 00:00:00",      # Start of useR! 2025
                            tz = "America/New_York") |> 
  with_tz("UTC") 

input_gregtime <- Sys.time()

look_forward_yrs <- 25

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
  s.nextLocalEcl_Jd <- swe_sol_eclipse_when_loc(jd_start  = var_jultime, 
                                                ephe_flag = SE$FLG_MOSEPH, 
                                                geopos    = c(input_lonlat["lon"], 
                                                              input_lonlat["lat"],0), 
                                                backward  = F)$tret[c(1)]
  
  names(s.nextLocalEcl_Jd) <- "time_of_max_eclipse"
  
  # calculate next lunar eclipse visible to input_location----
  l.nextLocalEcl_Jd <- swe_lun_eclipse_when_loc(jd_start  = var_jultime, 
                                                ephe_flag = SE$FLG_MOSEPH, 
                                                geopos    = c(input_lonlat["lon"], 
                                                              input_lonlat["lat"],0), 
                                                backward  = F)$tret[c(0,2,4)+1]
  
  names(l.nextLocalEcl_Jd) <- c("time_of_max_eclipse", 
                                "time_of_partial_phase_start", 
                                "time_of_total_phase_start")
  
  
  # which was was first? solar or lunar? 
  next_is_solar <- ifelse(test = s.nextLocalEcl_Jd["time_of_max_eclipse"] > 
                            l.nextLocalEcl_Jd["time_of_max_eclipse"], T, F)
  
  
  if(next_is_solar){
    ecl_type     <- "solar"
    ecl_subtype  <- NA
    obscuration  <- NA
    
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
    rm(s.nextEcl.temp)
    
    df.out <- rbind(df.out, 
                    data.frame(eclipse_type = ecl_type, 
                               eclipse_dt   = ecl_gregtime))
    
  }else{
    ecl_type     <- "lunar"
    ecl_subtype  <- NA
    obsucration  <- NA
    
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
    rm(l.nextEcl.temp)
    
    df.out <- rbind(df.out, 
                    data.frame(eclipse_type = ecl_type, 
                               eclipse_dt   = ecl_gregtime))
  }
  
  
  cur_gregtime <- ecl_gregtime %m+% hours(12)
  
  print(cur_gregtime)
  
}

df.out


# ## Example Solar Eclipse Calculation
# # convert gregorian time to julian time (ephemeral time i.e. "ET")
# var_jultime <- swe_utc_to_jd(year  = year(input_gregtime), 
#                              month = month(input_gregtime), 
#                              day   = mday(input_gregtime), 
#                              houri = hour(input_gregtime), 
#                              min   = minute(input_gregtime), 
#                              sec   = second(input_gregtime), 
#                              gregflag = SE$GREG_CAL)$dret[2]
# 
# # calculate the next solar eclipse visible to input_location----
# s.nextLocalEcl_Jd <- swe_sol_eclipse_when_loc(jd_start  = var_jultime, 
#                                               ephe_flag = SE$FLG_MOSEPH, 
#                                               geopos    = c(input_lonlat["lon"], 
#                                                             input_lonlat["lat"],0), 
#                                               backward  = F)$tret[c(1)]
# 
# names(s.nextLocalEcl_Jd) <- "time_of_max_eclipse"
# 
# # calculate next lunar eclipse visible to input_location----
# l.nextLocalEcl_Jd <- swe_lun_eclipse_when_loc(jd_start  = var_jultime, 
#                                               ephe_flag = SE$FLG_MOSEPH, 
#                                               geopos    = c(input_lonlat["lon"], 
#                                                             input_lonlat["lat"],0), 
#                                               backward  = F)$tret[c(0,2,4)+1]
# 
# names(l.nextLocalEcl_Jd) <- c("time_of_max_eclipse", 
#                               "time_of_partial_phase_start", 
#                               "time_of_total_phase_start")
# 
# l.ecl_type <- ifelse(l.nextLocalEcl_Jd["time_of_total_phase_start"] == 0, 
#                      yes = "partial", 
#                      no  = "total")
# 
# s.ecl_type        <- NA
# s.ecl_obscuration <- NA
# 
# # convert to gregorian time
# s.nextEcl.temp <- swe_jdet_to_utc(jd_et    = s.nextLocalEcl_Jd, 
#                                   gregflag = SE$GREG_CAL)
# 
# s.output_gregtime <- paste(s.nextEcl.temp$year_out,"-",
#                            s.nextEcl.temp$month_out,"-",
#                            s.nextEcl.temp$day_out," ",
#                            s.nextEcl.temp$hour_out,":",
#                            s.nextEcl.temp$min_out,":",
#                            s.nextEcl.temp$sec_out,
#                            sep = "") |>
#   ymd_hms()
# 
# # # Will the eclipse be visible from our location? 
# # s.output_visible <- swe_sol_eclipse_how(jd_ut     = s.nextLocalEcl_Jd, 
# #                                         ephe_flag = SE$FLG_MOSEPH, 
# #                                         geopos    = c(input_lonlat["lon"], 
# #                                                       input_lonlat["lat"],0))
# # s.output_obscuration <- s.output_visible$attr[3]
# # 
# # s.output_visible <- ifelse(s.output_visible$`return` == 0 | # "no eclipse visible" 
# #                              s.output_visible$attr[3] == 0, # % of sun blocked by moon == 0.0
# #                            yes = F, no = T) |> 
# #   ifelse(yes = F,
# #          no  = T)
# 
