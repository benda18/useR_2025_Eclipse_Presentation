library(renv)
library(swephR)
library(lubridate)

# https://www.astro.com/swisseph/swephprg.htm#_Toc112948985

rm(list=ls());cat('\f')

# set date-time-UTC----
time_utc <- ymd_hms("2024-04-01 04:00:00 AM",
                    tz = Sys.timezone()) |>  
  with_tz("UTC")

# set lon-lat----
xy.here      <- c(x = -78.8986, y = 35.9940, z = 0)

# convert utc time to julian time (ut & et)----
time.jul  <- swe_utc_to_jd(year     = year(time_utc), 
                          month    = month(time_utc), 
                          day      = mday(time_utc), 
                          houri    = hour(time_utc), 
                          min      = minute(time_utc), 
                          sec      = second(time_utc), 
                          gregflag = SE$GREG_CAL)$dret

names(time.jul) <- c("UT", "ET")


# next eclipse (any type) for a given location----
next_solar <- swe_sol_eclipse_when_loc(jd_start  = time.jul["UT"],
                                       ephe_flag = SE$FLG_MOSEPH,
                                       geopos    = c(xy.here[1],xy.here[2],xy.here[3]), #lon,lat,h.meters
                                       backward  = FALSE)

next_lunar <- swe_lun_eclipse_when_loc(jd_start  = time.jul["UT"],
                                       ephe_flag = SE$FLG_MOSEPH,
                                       geopos    = c(xy.here[1],xy.here[2],xy.here[3]), #lon,lat,h.meters
                                       backward  = FALSE)


# jul date-time next solar eclipse: maximum eclipse
nextsolmax_dt <- next_solar$tret[1] 
# jul date-time next lunar eclipse: maximum eclipse
nextlunmax_dt <- next_lunar$tret[1]

swephR::swe_jdet_to_utc(jd_et = nextsol_dt, 
                        gregflag = SE$GREG_CAL)

# swephR::swe_revjul(jd = juliandt_ut, gregflag = SE$GREG_CAL) does not work

tret.swe_sol_eclipse_when_loc <- c("tret[1]   time of maximum eclipse
tret[2]   time of first contact
tret[3]   time of second contact
tret[4]   time of third contact
tret[5]   time of forth contact
tret[6]   time of sunrise between first and forth contact
tret[7]   time of sunset between first and forth contact") |> 
  strsplit( "tret") |> 
  unlist() |> 
  trimws()

tret.swe_sol_eclipse_when_loc <- ifelse(tret.swe_sol_eclipse_when_loc == "",
                                        NA, 
                                        tret.swe_sol_eclipse_when_loc)
tret.swe_sol_eclipse_when_loc <- tret.swe_sol_eclipse_when_loc[!is.na(tret.swe_sol_eclipse_when_loc)]

attr.swe_sol_eclipse_when_loc <- c("attr[1]   fraction of solar diameter covered by moon; with total/annular eclipses, it results in magnitude acc. to IMCCE.",
                                   "attr[2]   ratio of lunar diameter to solar one",
                                   "attr[3]   fraction of solar disc covered by moon (obscuration)",
                                   "attr[4]   diameter of core shadow in km",
                                   "attr[5]   azimuth of sun at tjd",
                                   "attr[6]   true altitude of sun above horizon at tjd",
                                   "attr[7]   apparent altitude of sun above horizon at tjd",
                                   "attr[8]   elongation of moon in degrees",
                                   "attr[9]   magnitude acc. to NASA; = attr[1] for partial and attr[2] for annular and total eclipses",
                                   "attr[10]   saros series number (if available; otherwise -99999999)", 
                                   "attr[11]  saros series member number (if available; otherwise -99999999)")

