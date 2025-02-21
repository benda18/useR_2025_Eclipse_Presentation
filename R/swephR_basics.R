library(renv)
library(swephR)
library(lubridate)

rm(list=ls());cat('\f')

time.now_utc <- Sys.time() |> with_tz("UTC")
xy.here      <- c(x = -78.8986, y = 35.9940, z = 0)


date_jul     <- swephR::swe_julday(year     = year(time.now_utc), 
                                   month    = month(time.now_utc), 
                                   day      = mday(time.now_utc), 
                                   hourd    = hour(time.now_utc) + (minute(time.now_utc)/60), 
                                   gregflag = SE$GREG_CAL)

?swephR::swe_jdet_to_utc()


solecl_a <- swe_sol_eclipse_when_loc(jd_start  = date_jul,
                                     ephe_flag = SE$FLG_MOSEPH,
                                     geopos    = c(xy.here[1],xy.here[2],xy.here[3]), #lon,lat,h.meters
                                     backward  = FALSE)

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

