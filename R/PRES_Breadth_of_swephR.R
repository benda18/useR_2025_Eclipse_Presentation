renv::use(
  askpass      = "askpass@1.2.1",
  censusxy     = "chris-prener/censusxy@bdcb5141fe6a9dfc15c6b1fee6dde24376ef2bda",
  cli          = "cli@3.6.4",
  codetools    = "codetools@0.2-20",
  colorspace   = "colorspace@2.1-1",
  cpp11        = "cpp11@0.5.1",
  curl         = "curl@6.2.1",
  doParallel   = "doParallel@1.0.17",
  dplyr        = "dplyr@1.1.4",
  fansi        = "fansi@1.0.6",
  farver       = "farver@2.1.2",
  foreach      = "foreach@1.5.2",
  generics     = "generics@0.1.3",
  ggplot2      = "ggplot2@3.5.1",
  glue         = "glue@1.8.0",
  gtable       = "gtable@0.3.6",
  httr         = "httr@1.4.7",
  isoband      = "isoband@0.2.7",
  iterators    = "iterators@1.0.14",
  jsonlite     = "jsonlite@1.9.1",
  labeling     = "labeling@0.4.3",
  lattice      = "lattice@0.22-6",
  lifecycle    = "lifecycle@1.0.4",
  lubridate    = "lubridate@1.9.4",
  magrittr     = "magrittr@2.0.3",
  MASS         = "MASS@7.3-61",
  Matrix       = "Matrix@1.7-1",
  mgcv         = "mgcv@1.9-1",
  mime         = "mime@0.12",
  munsell      = "munsell@0.5.1",
  nlme         = "nlme@3.1-166",
  openssl      = "openssl@2.3.2",
  pillar       = "pillar@1.10.1",
  pkgconfig    = "pkgconfig@2.0.3",
  R6           = "R6@2.6.1",
  RColorBrewer = "RColorBrewer@1.1-3",
  Rcpp         = "Rcpp@1.0.14",
  renv         = "renv@1.1.1",
  rlang        = "rlang@1.1.5",
  scales       = "scales@1.3.0",
  swephR       = "swephR@0.3.1",
  sys          = "sys@3.4.3",
  tibble       = "tibble@3.2.1",
  tidyselect   = "tidyselect@1.2.1",
  timechange   = "timechange@0.3.0",
  utf8         = "utf8@1.2.4",
  vctrs        = "vctrs@0.6.5",
  viridisLite  = "viridisLite@0.4.2",
  withr        = "withr@3.0.2"
)

renv::embed()

library(renv)
library(swephR)
library(dplyr)
library(lubridate)
# devtools::install_github("chris-prener/censusxy")
library(censusxy)
library(ggplot2)

rm(list=ls());cat('\f')

input_lonlat   <- c("lon" = -78.938, "lat" = 36.001, z = 0)  # Duke Univ
input_gregtime <- ymd_hms("2025-08-08 01:00:00",      # Start of useR! 2025
                          tz = "America/New_York") |> 
  with_tz("UTC") %m+% years(1)

## Example Solar Eclipse Calculation
# convert gregorian time to julian time (ephemeral time i.e. "ET")
var_jultime <- swe_utc_to_jd(year  = year(input_gregtime), 
                             month = month(input_gregtime), 
                             day   = mday(input_gregtime), 
                             houri = hour(input_gregtime), 
                             min   = minute(input_gregtime), 
                             sec   = second(input_gregtime), 
                             gregflag = SE$GREG_CAL)$dret[2]

# calculate the next solar eclipse visible anywhere on earth
nextGlobEcl_Jd <- swe_sol_eclipse_when_glob(jd_start  = var_jultime, 
                                            ephe_flag = SE$FLG_MOSEPH, 
                                            ifltype   = 0, 
                                            backward  = F)$tret[c(1)]

# functions----

swephR::swe_azalt(jd_ut      = nextGlobEcl_Jd, 
                  coord_flag = SE$EQU2HOR, 
                  geopos     = input_lonlat, 
                  atpress    = 15,      # atmospheric pressure (hPa) 
                  attemp     = 1013.25, # atmospheric temperature (c), 
                  xin        = c(186,22))

ggplot() + 
  geom_point(aes(x = ))
