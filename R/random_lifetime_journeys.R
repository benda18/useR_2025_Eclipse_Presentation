renv::use(
  base64enc         = "base64enc@0.1-3",
  bslib             = "bslib@0.9.0",
  cachem            = "cachem@1.1.0",
  cli               = "cli@3.6.4",
  colorspace        = "colorspace@2.1-1",
  cpp11             = "cpp11@0.5.1",
  crosstalk         = "crosstalk@1.2.1",
  digest            = "digest@0.6.37",
  evaluate          = "evaluate@1.0.3",
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
  terra             = "terra@1.8-21",
  timechange        = "timechange@0.3.0",
  tinytex           = "tinytex@0.55",
  viridisLite       = "viridisLite@0.4.2",
  xfun              = "xfun@0.50",
  yaml              = "yaml@2.3.10"
)



renv::embed()

library(renv)
library(lubridate)
#library(ggplot2)
library(leaflet)

rm(list=ls());cat('\f')

x.bounds <- c(-180,180)
y.bounds <- c(-90, 90)
avg.days <- 28000

cur.xy <- c(x = 0, y = 0)


# calcs----
avg.yrs  <- avg.days / 365.25
life_multiplier <- 1.1

begin.dt <- Sys.Date()


calc_lifespan <- function(days = avg.days, 
                          mult = life_multiplier){
  out <- 0
  for(i in 1:(days*life_multiplier)){
    
    if(sample(i:(days*life_multiplier), size = 1) == days){
      out <- i 
      return(out)
      stop()
    }
  }
  
  out <- i
  
  
}

calc_lifespan()

set.seed(1234)
x <- replicate(n = 15, calc_lifespan())
fivenum(x)
hist(x)


for(i in x){
 print( i)
}

out.df <- NULL
for(i in 1:length(x)){
  out.df <- rbind(out.df, 
                  data.frame())
}
