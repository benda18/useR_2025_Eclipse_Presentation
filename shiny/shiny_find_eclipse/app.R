renv::use(
  assertthat        = "assertthat@0.2.1",
  attempt           = "attempt@0.3.1",
  base64enc         = "base64enc@0.1-3",
  bslib             = "bslib@0.9.0",
  cachem            = "cachem@1.1.0",
  cli               = "cli@3.6.4",
  colorspace        = "colorspace@2.1-1",
  commonmark        = "commonmark@1.9.2",
  cpp11             = "cpp11@0.5.1",
  crayon            = "crayon@1.5.3",
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
  geoloc            = "ColinFay/geoloc@5d6eb6220cc721adc083064788ea3c65e60d2909",
  glue              = "glue@1.8.0",
  highr             = "highr@0.11",
  htmltools         = "htmltools@0.5.8.1",
  htmlwidgets       = "htmlwidgets@1.6.4",
  httpuv            = "httpuv@1.6.15",
  jpeg              = "jpeg@0.1-10",
  jquerylib         = "jquerylib@0.1.4",
  jsonlite          = "jsonlite@1.9.1",
  knitr             = "knitr@1.49",
  labeling          = "labeling@0.4.3",
  later             = "later@1.4.1",
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
  promises          = "promises@1.3.2",
  qrcode            = "qrcode@0.3.0",
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
  shiny             = "shiny@1.10.0",
  sourcetools       = "sourcetools@0.1.7-1",
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
  xtable            = "xtable@1.8-4",
  yaml              = "yaml@2.3.10"
)

#
# This is a Shiny web application.
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

renv::embed()

library(renv)
#library(readr)
library(shiny)
library(jpeg)
library(swephR)
library(lubridate)
library(dplyr)
#library(censusxy)
library(scales)
#library(ggplot2)
library(glue)
library(qrcode)
library(leaflet)
# remotes::install_github("ColinFay/geoloc")
library(geoloc)

# renv::snapshot()


ui <- fluidPage(
  
  # Application title
  titlePanel("Future Solar and Lunar Eclipses Visible from Your Current Location", ),
  sidebarLayout(
    sidebarPanel(
      shiny::selectInput(inputId = "n_fut_yrs",
                         label = "Years to Look into the Future:",
                         choices = c("1 year" = 1, 
                                     "5 years" = 5, 
                                     "10 years" = 10,
                                     "25 years" = 25, 
                                     "50 years" = 50,
                                     "75 years" = 75),
                         selected = 5,
                         multiple = F),
      shiny::sliderInput(inputId = "obs_in", 
                         label = "Minimum Solar Eclipse Obscuration Cutoff:", 
                         min = 0, max = 100, value = 10, step = 5, 
                         post = "%"),
      shiny::radioButtons(inputId = "sel_type", 
                          label = "Select Type(s) of Eclipse to Show", 
                          choices = c("All Eclipses" = "Solar|Lunar", 
                                      "Solar Only" = "Solar", 
                                      "Lunar Only" = "Lunar"), 
                          selected = "Solar|Lunar"),
      shiny::checkboxInput("cb_total.ecl", 
                           value = F,
                           label = "Show Only Total Eclipses"), 
      shiny::checkboxInput("cb_totality", 
                           value = F, 
                           label = "Show Only when in Path of Totality"),
      geoloc::button_geoloc(inputId = "myBtn", ("Click to Start")),
      leafletOutput("lf_map"),
      
      
    ),
    
    mainPanel(
      # BLOCK RESOURCES MAIN PANEL----
      wellPanel(
        fluidRow(strong(uiOutput("tab.linkedin"))),
        fluidRow(strong(uiOutput("tab.github"))),
        fluidRow(strong(uiOutput("tab.venmo"))),
        fluidRow("Special thanks to reddit users u/danielsixfive and u/QuackingUp23")
      ),
      #/BRMP
      
      wellPanel(
        fluidRow(strong("Data Table Will Load Below (may take a moment)")),
      ),
      shiny::tableOutput(outputId = "logtable"),
      shiny::plotOutput(outputId = "qr_url", 
                        height = "200px"),
      wellPanel(
        fluidRow(strong("OTHER ECLIPSE WEBAPPS")), 
        fluidRow(uiOutput("tab.PT")), 
        fluidRow(uiOutput("tab.NE"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  ewlun_url <- function(ecl_date, 
                        ecltype){
    require(glue)
    require(lubridate)
    w.year  <- year(ecl_date)
    w.month <- lubridate::month(ecl_date,label=T,abbr=T)
    w.mday  <- mday(ecl_date)
    w.mday  <- ifelse(nchar(w.mday) == 1,
                      paste("0", w.mday, sep = "", collapse = ""),
                      w.mday)
    w.cenA  <- floor(w.year/100)*100+1
    w.cenB  <- w.cenA + 99
    
    ecltype <- ifelse(ecltype %in% c("Penumbral", 
                                     "penumbral"), "NPenumbral", ecltype)
    et <- toupper(substr(ecltype,1,1))
    glue("https://eclipsewise.com/lunar/LEprime/{w.cenA}-{w.cenB}/LE{w.year}{w.month}{w.mday}{et}prime.html")
    #glue("https://eclipsewise.com/oh/ec{year(ecl_date)}.html#LE{year(ecl_date)}{lubridate::month(ecl_date,abbr=T,label=T)}{mday(ecl_date)}{et}")
    
  }
  #ewlun_url(mdy("Aug 28, 2026"),"Penumbral")
  
  eclipsewise_url <- function(ecl_date,
                              ecltype = c("Total Eclipse", 
                                          "Annular", 
                                          "Partial", 
                                          "Hybrid")){
    require(glue)
    require(lubridate)
    w.year  <- year(ecl_date)
    w.month <- as.character(lubridate::month(ecl_date, label = F))
    w.month <- ifelse(nchar(w.month) == 1,
                      paste("0", w.month, sep = "", collapse = ""),
                      w.month)
    w.mday  <- mday(ecl_date)
    w.mday  <- ifelse(nchar(w.mday) == 1,
                      paste("0", w.mday, sep = "", collapse = ""),
                      w.mday)
    w.cenA  <- floor(w.year/100)*100+1
    w.cenB  <- w.cenA + 99
    
    et <- toupper(substr(ecltype,1,1))
    
    return(glue("https://eclipsewise.com/solar/SEping/{w.cenA}-{w.cenB}/SE{w.year}-{w.month}-{w.mday}{et}.gif"))
  }
  
  output$lf_map <- renderLeaflet({
    req(input$myBtn_lon)
    req(input$myBtn_lat)
    leaflet() %>%
      addTiles() %>%
      setView(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), zoom = 8) %>%
      addMarkers(as.numeric(input$myBtn_lon), as.numeric(input$myBtn_lat), 
                 label = "You're here!")
  })
  
  output$logtable <- shiny::renderTable({
    # vars----
    start.date      <- with_tz(Sys.Date(), tzone = "America/New_York") #input$date_in 
    max.year        <- year(start.date) + as.numeric(input$n_fut_yrs)
    
    # do work----
    var.lon <- input$myBtn_lon 
    var.lat <- input$myBtn_lat 
    
    # crosswalks
    cw_se.subtype <- data.frame(ephe_flag.input = c(4,8,16,32), 
                                return.output   = c(5,9,18,33),
                                ephe_flag.name  = c("Total Eclipse", "Annular", 
                                                    "Partial", "Hybrid"))
    
    ####
    n <- 0
    
    log.ecls <- NULL
    req(input$myBtn_lon)
    req(input$myBtn_lat)
    while(year(start.date) < max.year){
      n <- n + 1
      if(n > 2000){
        stop("too many searches - ERROR")
      }
      a.date.ju <- swephR::swe_utc_to_jd(year = year(start.date), 
                                         month = lubridate::month(start.date), 
                                         day   = mday(start.date), 
                                         houri = 0, 
                                         min   = 30, 
                                         sec   = 0, 
                                         gregflag = 1)$dret[2]
      
      # # temp----
      # a.date.ju <- swephR::swe_utc_to_jd(year = sample(1970:2100,1), 
      #                                    month = sample(1:12,1), 
      #                                    day   = sample(1:25,1), 
      #                                    houri = 0, 
      #                                    min   = 30, 
      #                                    sec   = 0, 
      #                                    gregflag = 1)$dret[2]
      # # /temp
      
      # get next solar eclipse----
      when_next <- swe_sol_eclipse_when_loc(jd_start = a.date.ju, 
                                            ephe_flag = 4, 
                                            geopos = c(x = var.lon, 
                                                       y = var.lat, 
                                                       z = 10), 
                                            backward = F)
      
      # search solar eclipse type
      
      ecl_subtype  <- swe_sol_eclipse_when_glob(jd_start = when_next$tret[2]-1, 
                                                ifltype = 0, # any type of eclipse
                                                ephe_flag = 4, 
                                                backward = F)$return
      
      ecl_subtype <- cw_se.subtype[cw_se.subtype$return.output == ecl_subtype & 
                                     !is.na(cw_se.subtype$return.output),]$ephe_flag.name
      
      
      # ecl_total <- swe_sol_eclipse_when_glob(jd_start = when_next$tret[2]-1, 
      #                                        ifltype = SE$ECL_TOTAL,#SE$ECL_CENTRAL|SE$ECL_NONCENTRAL,
      #                                        ephe_flag = 4, 
      #                                        backward = F)
      # ecl_annular <- swe_sol_eclipse_when_glob(jd_start = when_next$tret[2]-1, 
      #                                          ifltype = SE$ECL_ANNULAR,
      #                                          ephe_flag = 4, 
      #                                          backward = F)
      # ecl_partial <- swe_sol_eclipse_when_glob(jd_start = when_next$tret[2]-1, 
      #                                          ifltype = SE$ECL_PARTIAL,
      #                                          ephe_flag = 4, 
      #                                          backward = F)
      # ecl_hybrid <- swe_sol_eclipse_when_glob(jd_start = when_next$tret[2]-1, 
      #                                         ifltype = SE$ECL_ANNULAR_TOTAL,
      #                                         ephe_flag = 4, 
      #                                         backward = F)
      
      # ecl_type222 <- c("Total Eclipse", "Annular", 
      #                  "Partial", "Hybrid")[which(abs(c(ecl_total$tret[2] - when_next$tret[2],
      #                                                   ecl_annular$tret[2] - when_next$tret[2],
      #                                                   ecl_partial$tret[2] - when_next$tret[2],
      #                                                   ecl_hybrid$tret[2] - when_next$tret[2])) == 
      #                                               min(abs(c(ecl_total$tret[2] - when_next$tret[2],
      #                                                         ecl_annular$tret[2] - when_next$tret[2],
      #                                                         ecl_partial$tret[2] - when_next$tret[2],
      #                                                         ecl_hybrid$tret[2] - when_next$tret[2]))))]
      # # ecl_typecheck0 <- min(abs(c(ecl_total$tret[2] - when_next$tret[2],
      # #                         ecl_annular$tret[2] - when_next$tret[2],
      # #                         ecl_partial$tret[2] - when_next$tret[2],
      # #                         ecl_hybrid$tret[2] - when_next$tret[2])))
      
      ecl_type222 <- ecl_subtype
      
      # get next lunar eclipse----
      when_next.lun <- swe_lun_eclipse_when_loc(jd_start = a.date.ju, 
                                                ephe_flag = 4, 
                                                geopos = c(x = var.lon, 
                                                           y = var.lat, 
                                                           z = 10), 
                                                backward = F)
      # get next lunar eclipse type
      
      ecl_type222.lun <- swe_lun_eclipse_when(when_next.lun$tret[3]-1, 
                                              ifltype = 0, 
                                              ephe_flag = 4, 
                                              backward = F)$return
      
      # ecl_total.lun <- swe_lun_eclipse_when(jd_start = when_next.lun$tret[3]-1, 
      #                                       ifltype = SE$ECL_TOTAL, # 4 return
      #                                       ephe_flag = 4, 
      #                                       backward = F)
      # ecl_penumbral.lun <- swe_lun_eclipse_when(jd_start = when_next.lun$tret[3]-1, 
      #                                           ifltype = SE$ECL_PENUMBRAL, # 64 return
      #                                           ephe_flag = 4, 
      #                                           backward = F)
      # ecl_partial.lun <- swe_lun_eclipse_when(jd_start = when_next.lun$tret[3]-1, 
      #                                         ifltype = SE$ECL_PARTIAL, # 16 return
      #                                         ephe_flag = 4, 
      #                                         backward = F)
      
      cw_le.subtype <- data.frame(name = c("Total Eclipse", "Penumbral", 
                                           "Partial"), 
                                  return_val = c(4,64,16))
      
      ecl_type222.lun <- cw_le.subtype$name[cw_le.subtype$return_val == ecl_type222.lun]
      # ecl_type222.lun <- c("Total Eclipse", "Penumbral", 
      #                      "Partial")[which(abs(c(ecl_total.lun$tret[3] - when_next.lun$tret[3],
      #                                             ecl_penumbral.lun$tret[3] - when_next.lun$tret[3],
      #                                             ecl_partial.lun$tret[3] - when_next.lun$tret[3])) == 
      #                                         min(abs(c(ecl_total.lun$tret[3] - when_next.lun$tret[3],
      #                                                   ecl_penumbral.lun$tret[3] - when_next.lun$tret[3],
      #                                                   ecl_partial.lun$tret[3] - when_next.lun$tret[3]))))]
      
      
      # # temp manual fix for penumbral eclipses
      # if(length(ecl_type222.lun) == 3){
      #   ecl_type222.lun <- "Penumbral"
      # }
      
      
      # NEXT DATE
      temp.nextdate <- ymd_hms(paste(swephR::swe_jdet_to_utc(when_next$tret[1], 1), 
                                     sep = "-", collapse = "-"))
      temp.nextdate.lun <- ymd_hms(paste(swephR::swe_jdet_to_utc(when_next.lun$tret[1], 1), 
                                         sep = "-", collapse = "-"))
      
      #temp.nextobs <- max(when_next$attr[c(1,3)]) # p
      temp.nextobs <- when_next$attr[c(3)]
      
      log.ecls <- rbind(log.ecls,
                        data.frame(Date        = strftime(x = temp.nextdate, 
                                                          format = "%b %d, %Y"),
                                   Type        = "Solar",
                                   Sub_Type    = ecl_type222,
                                   Obscuration = temp.nextobs, 
                                   See_Also    = eclipsewise_url(ecl_date = temp.nextdate, 
                                                                 ecltype = ecl_type222)))
      log.ecls <- rbind(log.ecls,
                        data.frame(Date        = strftime(x = temp.nextdate.lun, 
                                                          format = "%b %d, %Y"),
                                   Type        = "Lunar",
                                   Sub_Type    = ecl_type222.lun,
                                   Obscuration = NA_real_, 
                                   See_Also    = ewlun_url(ecl_date = temp.nextdate.lun, 
                                                           ecltype  = ecl_type222.lun)))
      
      temp.utc <- temp.nextdate
      temp.jd  <- swe_utc_to_jd(year = year(temp.utc),
                                month = lubridate::month(x = temp.utc, label = F),
                                day = mday(temp.utc),
                                houri = hour(temp.utc),
                                min = minute(temp.utc),
                                sec = second(temp.utc),
                                gregflag = 1)$dret[2] # |>
      # as.integer() |>
      # unique()
      
      
      if(year(start.date) >= max.year){
        break
        #is_totality <- T
        # next.obs <- temp.nextobs
        # start.date <- as_date(temp.nextdate)
      }else{
        start.date <- as_date(min(c(temp.nextdate.lun, 
                                    temp.nextdate))) + days(2)
        next.obs <- temp.nextobs
      }
    }
    
    log.ecls$Obscuration[log.ecls$Obscuration >= 1 & !is.na(log.ecls$Obscuration)]  <- 1
    log.ecls <- log.ecls[(log.ecls$Obscuration*100) >= input$obs_in | 
                           is.na(log.ecls$Obscuration),]
    
    
    #log.ecls$Obscuration <- scales::percent(log.ecls$Obscuration, accuracy = 0.1)
    ##https://stackoverflow.com/questions/21909826/r-shiny-open-the-urls-from-rendertable-in-a-new-tab
    log.ecls$See_Also <- paste0("[<a href='",  
                                log.ecls$See_Also,
                                "' target='_blank'>see eclipse path</a>]")
    
    log.ecls$See_Also[log.ecls$Type == "Lunar"] <-
      gsub(pattern = "eclipse path", 
           replacement = "eclipse info", 
           x = log.ecls$See_Also[log.ecls$Type == "Lunar"])
    
    
    # checkbox_totaleclipse
    if(input$cb_total.ecl){
      log.ecls <- log.ecls[log.ecls$Sub_Type == "Total Eclipse",]
    }
    # checkbox_totality
    if(input$cb_totality){
      #log.ecls <- log.ecls[log.ecls$Obscuration == "100.0%" & log.ecls$Type == "Solar",]
      log.ecls <- log.ecls[log.ecls$Obscuration >= 1 & log.ecls$Type == "Solar",]
    }
    
    log.ecls <- log.ecls[!duplicated(log.ecls),]
    
    #log.ecls[grepl(pattern = input$sel_type, 
    #              x = log.ecls$Type),]
  }, 
  sanitize.text.function = function(x) x
  )
  
  
  # RESOURCES----
  url.venmo <- a("Want to help cover hosting costs? Venmo: @Tim_J_Bender", 
                 href = "https://venmo.com/u/Tim_J_Bender", 
                 target = "_blank")
  output$tab.venmo <- renderUI({
    tagList(url.venmo)
  })
  
  url.github <- a("Source Code (Github)", 
                  href = "https://github.com/benda18/eclipse/blob/main/shiny_all_eclipsesV2/app.R", 
                  target = "_blank")
  output$tab.github <- renderUI({
    tagList(url.github)
  })
  
  url.linkedin <- a("Created by Tim Bender (LinkedIn)", 
                    href = "https://www.linkedin.com/in/tim-bender-238870171/", 
                    target = "_blank")
  output$tab.linkedin <- renderUI({
    tagList(url.linkedin)
  })
  #/RESOURCES
  url.AE <- a("* Every Solar Eclipse Visible from Your Address for 75 Years",
              href = "https://tim-bender.shinyapps.io/shiny_all_eclipses/",
              target = "_blank")
  output$tab.AE <- renderUI({
    tagList(url.AE)
  })
  url.PT <- a("* Total Eclipse of April 8, 2024 - What to Expect from Your Location",
              href = "https://tim-bender.shinyapps.io/shiny_eclipse_planner/",
              target = "_blank")
  output$tab.PT <- renderUI({
    tagList(url.PT)
  })
  
  
  output$qr_url <- renderPlot({
    qr_app <- qrcode::qr_code(x = "https://tim-bender.shinyapps.io/shiny_all_eclipses/", 
                              ecl = "H")
    qr_app_logo <- add_logo(qr_app, 
                            logo = "www/QRLOGO.jpg")
    plot(qr_app_logo)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
