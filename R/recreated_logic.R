# libraries
library(renv)
library(swephR)
library(lubridate)
library(leaflet)
library(data.table)
library(tibble)

# setup----
rm(list=ls());cat('\f')

# UT = universal time
# ET = ephemeris time
# TT = terrestrial time

# set clock time----

se_total   <- ymd_hms("2024-04-08 12:30:00 AM", tz = "America/New_York")
se_annular <- ymd_hms("2023-10-14 12:30:00 AM", tz = "America/New_York")

var.nowutc <- Sys.time() |> with_tz("UTC")

var.nowutc <- se_total |> 
  with_tz("UTC")



# check for utc time
stopifnot(tz(var.nowutc) == "UTC")

# set lon-lat
var_lon <- -78.921
var_lat <-  36.048

# convert clock time to TT 
var.nowjul  <- swe_utc_to_jd(year     = year(var.nowutc), 
                             month    = month(var.nowutc), 
                             day      = mday(var.nowutc), 
                             houri    = hour(var.nowutc), 
                             min      = minute(var.nowutc), 
                             sec      = second(var.nowutc), 
                             gregflag = SE$GREG_CAL)$dret

names(var.nowjul) <- c("UT", "ET")

var.nowet <- var.nowjul["ET"]

# next solar eclipse----
next_solar <- swe_sol_eclipse_when_loc(jd_start  = var.nowet,
                                       ephe_flag = SE$FLG_MOSEPH,
                                       geopos    = c(var_lon,var_lat,0), #lon,lat,h.meters
                                       backward  = FALSE)
solar_times <- next_solar$tret[c(1)]
names(solar_times) <- c("max_eclipse")

solar_dt_greg.utc <- swephR::swe_jdet_to_utc(jd_et = solar_times,
                                             gregflag = SE$GREG_CAL)

solar_dt_greg.utc <- paste(solar_dt_greg.utc$year_out, "-", 
                           solar_dt_greg.utc$month_out, "-", 
                           solar_dt_greg.utc$day_out, " ", 
                           solar_dt_greg.utc$hour_out, ":", 
                           solar_dt_greg.utc$min_out, ":", 
                           solar_dt_greg.utc$sec_out,
                           sep = "", collapse = "") |> 
  ymd_hms(tz = "UTC")

solar_dt_greg.EDT <- solar_dt_greg.utc |> with_tz(tzone = "America/New_York")
solar_date <- as_date(solar_dt_greg.EDT)

# solar eclipse type as hyped by media - for an eclipse visible at x/y
soltype_media <-  swe_sol_eclipse_where(jd_ut = solar_times, 
                                        ephe_flag = SE$FLG_MOSEPH)

soltype_media_xy   <- c(lon = soltype_media$pathpos[1], 
                        lat = soltype_media$pathpos[2])

soltype_media_attr <- soltype_media$attr[c(1,2,3,9)]

names(soltype_media_attr) <- c("fraction of solar diameter covered by the moon", 
                               "ratio of lunar diameter to solar one", 
                               "fraction of solar disc covered by moon (obscuration)", 
                               "eclipse magnitude (= attr[0] or attr[1] depending on eclipse type)")

# TO-DO----
# GET ECLIPSE TYPES: TOTAL VS ANNULAR, TOTAL VS PARTIAL
se_type <- expand.grid(media = c("total", "annular"), 
                       local = c("totality", "partial"))

# solar eclipse as seen locally

soltype_local <- swe_sol_eclipse_how(jd_ut = solar_times,
                                     ephe_flag = SE$FLG_MOSEPH,
                                     geopos    = c(var_lon,var_lat,0))

soltype_local_xy   <- c(lon = var_lon, 
                        lat = var_lat)

soltype_local_attr <-  soltype_local$attr[c(1,2,3,9)]

names(soltype_local_attr) <- c("fraction of solar diameter covered by the moon", 
                               "ratio of lunar diameter to solar one", 
                               "fraction of solar disc covered by moon (obscuration)", 
                               "eclipse magnitude (= attr[0] or attr[1] depending on eclipse type)")




# next lunar eclipse----
next_lunar <- swe_lun_eclipse_when_loc(jd_start  = var.nowet,
                                       ephe_flag = SE$FLG_MOSEPH,
                                       geopos    = c(var_lon, var_lat,0), #lon,lat,h.meters
                                       backward  = FALSE)$tret[c(3,1,4)]



# output----

out.list <- list(df_media = data.frame(event_type = "peak_observed", 
                                       
                                       lon        = NA, 
                                       lat        = NA, 
                                       eclipse_type = c("solar", "lunar"), 
                                       eclipse_subtype = c(NA, NA),
                                       datetime_utc = NA, 
                                       date_utc     = NA), 
                 df_local = data.frame(event_type = "locally_observed", 
                                       
                                       lon        = var_lon, 
                                       lat        = var_lat, 
                                       eclipse_type = c("solar", "lunar"), 
                                       eclipse_subtype = c(NA, NA),
                                       datetime_utc = c(solar_dt_greg.utc, 
                                                        NA), 
                                       date_utc     = c(as_date(solar_dt_greg.utc), 
                                                        NA))) 

out.list

out.df <- rbind(out.list$df_media, 
                out.list$df_local) |> 
  as_tibble()

out.df$date_utc <- out.df$date_utc |> as_date()
out.df$datetime_utc <- out.df$datetime_utc |> as_datetime()


out.df
