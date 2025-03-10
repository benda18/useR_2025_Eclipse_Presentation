renv::use(
  askpass           = "askpass@1.2.1",
  base64enc         = "base64enc@0.1-3",
  bslib             = "bslib@0.9.0",
  cachem            = "cachem@1.1.0",
  censusxy          = "chris-prener/censusxy@bdcb5141fe6a9dfc15c6b1fee6dde24376ef2bda",
  cli               = "cli@3.6.4",
  codetools         = "codetools@0.2-20",
  colorspace        = "colorspace@2.1-1",
  cpp11             = "cpp11@0.5.1",
  crosstalk         = "crosstalk@1.2.1",
  curl              = "curl@6.2.1",
  digest            = "digest@0.6.37",
  doParallel        = "doParallel@1.0.17",
  evaluate          = "evaluate@1.0.3",
  fansi             = "fansi@1.0.6",
  farver            = "farver@2.1.2",
  fastmap           = "fastmap@1.2.0",
  fontawesome       = "fontawesome@0.5.3",
  foreach           = "foreach@1.5.2",
  fs                = "fs@1.6.5",
  generics          = "generics@0.1.3",
  ggplot2           = "ggplot2@3.5.1",
  glue              = "glue@1.8.0",
  gtable            = "gtable@0.3.6",
  highr             = "highr@0.11",
  htmltools         = "htmltools@0.5.8.1",
  htmlwidgets       = "htmlwidgets@1.6.4",
  httr              = "httr@1.4.7",
  isoband           = "isoband@0.2.7",
  iterators         = "iterators@1.0.14",
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
  MASS              = "MASS@7.3-61",
  Matrix            = "Matrix@1.7-1",
  memoise           = "memoise@2.0.1",
  mgcv              = "mgcv@1.9-1",
  mime              = "mime@0.12",
  munsell           = "munsell@0.5.1",
  nlme              = "nlme@3.1-166",
  openssl           = "openssl@2.3.2",
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
  sys               = "sys@3.4.3",
  terra             = "terra@1.8-21",
  tibble            = "tibble@3.2.1",
  timechange        = "timechange@0.3.0",
  tinytex           = "tinytex@0.55",
  utf8              = "utf8@1.2.4",
  vctrs             = "vctrs@0.6.5",
  viridisLite       = "viridisLite@0.4.2",
  withr             = "withr@3.0.2",
  xfun              = "xfun@0.50",
  yaml              = "yaml@2.3.10"
)



#make cooords for eclipse path

renv::embed()

library(renv)
library(swephR)
library(leaflet)
library(ggplot2)
# library(devtools)
# devtools::install_github("chris-prener/censusxy")
library(censusxy)
library(lubridate)
library(scales)


rm(list=ls());cat('\f')

## Example Input Variables
input_lonlat   <- c("lon" = -78.938, "lat" = 36.001)  # Duke Univ
input_gregtime <- ymd_hms("2025-08-08 01:00:00",      # Start of useR! 2025
                          tz = "America/New_York") |> 
  with_tz("UTC") 

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

# convert to gregorian time
nextEcl.temp <- swe_jdet_to_utc(jd_et    = nextGlobEcl_Jd, 
                                gregflag = SE$GREG_CAL)

output_gregtime <- paste(nextEcl.temp$year_out,"-",
                         nextEcl.temp$month_out,"-",
                         nextEcl.temp$day_out," ",
                         nextEcl.temp$hour_out,":",
                         nextEcl.temp$min_out,":",
                         nextEcl.temp$sec_out,
                         sep = "") |>
  ymd_hms()

# Will the eclipse be visible from our location? 
output_visible <- swe_sol_eclipse_how(jd_ut     = nextGlobEcl_Jd, 
                                      ephe_flag = SE$FLG_MOSEPH, 
                                      geopos    = c(input_lonlat["lon"], 
                                                    input_lonlat["lat"],0))
output_obscuration <- output_visible$attr[3]

output_visible <- ifelse(output_visible$`return` == 0 | # "no eclipse visible" 
                           output_visible$attr[3] == 0, # % of sun blocked by moon == 0.0
                         yes = F, no = T) |> 
  ifelse(yes = F,
         no  = T)

# Where is the best viewing location geometrically? 
output_xy <- swe_sol_eclipse_where(jd_ut     = nextGlobEcl_Jd, 
                                   ephe_flag = SE$FLG_MOSEPH)$pathpos[c(1,2)]
names(output_xy) <- c("lon", "lat")


## Example Returned Values
print(list("Eclipse_DateTime"      = output_gregtime, 
           "Visible_Locally"       = output_visible,
           "Obscuration_Locally"   = percent(output_obscuration,
                                             accuracy = 0.1),
           "Maximal_View.Globally" = output_xy))
