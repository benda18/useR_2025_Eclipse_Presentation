# libraries
library(renv)
library(swephR)
library(lubridate)

# setup----
rm(list=ls());cat('\f')

# UT = universal time
# ET = ephemeris time
# TT = terrestrial time

# set clock time----

se_total   <- ymd_hms("2078-05-11 12:30:00 AM", tz = "America/New_York")
se_annular <- ymd_hms("2093-07-23 12:30:00 AM", tz = "America/New_York")

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

# solar eclipse type as hyped by media - for an eclipse visible at x/y
soltype_media <-  swe_sol_eclipse_where(jd_ut = solar_times, 
                                        ephe_flag = SE$FLG_MOSEPH)

soltype_media_attr <- soltype_media$attr[c(1,2,3,9)]

names(soltype_media_attr) <- c("fraction of solar diameter covered by the moon", 
                               "ratio of lunar diameter to solar one", 
                               "fraction of solar disc covered by moon (obscuration)", 
                               "eclipse magnitude (= attr[0] or attr[1] depending on eclipse type)")

# TO-DO----
# GET ECLIPSE TYPES: TOTAL VS ANNULAR, TOTAL VS PARTIAL
se_type <- expand.grid(media = c("total", "annular"), 
                       local = c("totality", "partial"))

# solar eclipse as seen at x/y

type_solar <- swe_sol_eclipse_how(jd_ut = solar_times,
                                  ephe_flag = SE$FLG_MOSEPH,
                                  geopos    = c(var_lon,var_lat,0))

# partial eclipse
if(type_solar$attr[9] == type_solar$attr[1]){sol_type <- "partial"}

# annular or total
if(type_solar$attr[9] == type_solar$attr[2]){
  # annular
  if(type_solar$attr[2] < 1){
    sol_type <- "annular"
  }
  # total 
  if(type_solar$attr[2] >= 1){
    sol_type <- "total"
  }
}

# where is the eclipse maximal? 


# next lunar eclipse----
next_lunar <- swe_lun_eclipse_when_loc(jd_start  = var.nowet,
                                       ephe_flag = SE$FLG_MOSEPH,
                                       geopos    = c(var_lon, var_lat,0), #lon,lat,h.meters
                                       backward  = FALSE)$tret[c(3,1,4)]

