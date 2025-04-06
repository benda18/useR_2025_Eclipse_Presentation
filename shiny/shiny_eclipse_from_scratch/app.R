
# renv::embed()

library(renv)
library(shiny)
library(geoloc)
library(data.table)
#library(leaflet)
library(swephR)
library(lubridate)

status()
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("TITLE"),
  
  
  sidebarLayout(
    sidebarPanel(
      # Model Tuning Options Here
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
      # Get X,Y coordinate info from user
      geoloc::button_geoloc(inputId = "lonlatBtn", ("Click to Start")),
      tableOutput(outputId = "xy_table")
    ),
    
    mainPanel(
      tableOutput(outputId = "eclipse_table")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # reactive xy
  xy_input <- reactive({
    var.lon <- input$lonlatBtn_lon 
    var.lat <- input$lonlatBtn_lat 
  })
  
  xy_input <- eventReactive(input$cxy_go, {
    input$addr_in
  })
  
  
  # construct X,Y coordinate Table
  output$xy_table <- shiny::renderTable({
    xy_input()
    var.utc <- Sys.time() |> as.POSIXlt(tz = "UTC")
    var.jd  <- swe_utc_to_jd(year  = year(var.utc), 
                             month = month(var.utc), 
                             day   = mday(var.utc), 
                             houri = hour(var.utc), 
                             min   = minute(var.utc), 
                             sec   = second(var.utc), 
                             gregflag = SE$GREG_CAL)$dret[2]
    
    data.frame(variable = c("longitude", "latitude", 
                            "time_UTC", "julian_time"), 
               value = c(var.lon, var.lat, 
                         as.numeric(var.utc), var.jd))
  }
  #sanitize.text.function = function(x) x
  )
  
  
  # output$eclipse_table <- ({
  #   fun_get.e <- function(x,y,dtg,yrs_fwd){
  #     require(lubridate)
  #     # check class of dtg
  #     stopifnot("POSIXct" %in% class(dtg))
  #     
  #     ## Example Input Variables
  #     input_lonlat     <- c("lon" = x, 
  #                           "lat" = y) 
  #     input_gregtime   <- dtg
  #     look_forward_yrs <- yrs_fwd
  #     
  #     # analysis
  #     end_gregtime     <- input_gregtime %m+% years(look_forward_yrs)
  #     cur_gregtime     <- input_gregtime
  #     
  #     # loop
  #     error.chk <- 0
  #     df.out <- NULL
  #     while(cur_gregtime < end_gregtime){
  #       # error stop
  #       error.chk <- error.chk + 1
  #       stopifnot(error.chk < 10000)
  #       
  #       # code
  #       ## Example Solar Eclipse Calculation
  #       # convert gregorian time to julian time (ephemeral time i.e. "ET")
  #       var_jultime <- swe_utc_to_jd(year  = year(cur_gregtime), 
  #                                    month = month(cur_gregtime), 
  #                                    day   = mday(cur_gregtime), 
  #                                    houri = hour(cur_gregtime), 
  #                                    min   = minute(cur_gregtime), 
  #                                    sec   = second(cur_gregtime), 
  #                                    gregflag = SE$GREG_CAL)$dret[2]
  #       
  #       # calculate the next solar eclipse visible to input_location----
  #       temp.sol <- swe_sol_eclipse_when_loc(jd_start  = var_jultime, 
  #                                            ephe_flag = SE$FLG_MOSEPH, 
  #                                            geopos    = c(input_lonlat["lon"], 
  #                                                          input_lonlat["lat"],0), 
  #                                            backward  = F)
  #       
  #       s.nextLocalEcl_Jd <- temp.sol$tret[c(1)]
  #       names(s.nextLocalEcl_Jd) <- "time_of_max_eclipse"
  #       
  #       s.subtype <- swe_sol_eclipse_when_glob(jd_start  = var_jultime, 
  #                                              ephe_flag = SE$FLG_MOSEPH, 
  #                                              ifltype   = 0, 
  #                                              backward  = F)$return
  #       
  #       s.subtype.temp <- data.frame(number  = c(5,9,18,33),
  #                                    subtype = c("Total Eclipse", "Annular", 
  #                                                "Partial", "Hybrid")) 
  #       s.subtype <- s.subtype.temp[s.subtype.temp$number == s.subtype,]$subtype
  #       rm(s.subtype.temp)
  #       
  #       # calculate next lunar eclipse visible to input_location----
  #       temp.lun <- swe_lun_eclipse_when_loc(jd_start  = var_jultime, 
  #                                            ephe_flag = SE$FLG_MOSEPH, 
  #                                            geopos    = c(input_lonlat["lon"], 
  #                                                          input_lonlat["lat"],0), 
  #                                            backward  = F)
  #       l.nextLocalEcl_Jd <- temp.lun$tret[c(0,2,4)+1]
  #       names(l.nextLocalEcl_Jd) <- c("time_of_max_eclipse", 
  #                                     "time_of_partial_phase_start", 
  #                                     "time_of_total_phase_start")
  #       l.subtype <- swe_lun_eclipse_when(jd_start  = var_jultime, 
  #                                         ephe_flag = SE$FLG_MOSEPH, 
  #                                         ifltype   = 0, 
  #                                         backward  = F)
  #       
  #       # which was was first? solar or lunar? 
  #       next_is_solar <- ifelse(test = s.nextLocalEcl_Jd["time_of_max_eclipse"] < 
  #                                 l.nextLocalEcl_Jd["time_of_max_eclipse"], T, F)
  #       
  #       
  #       if(next_is_solar){
  #         
  #         
  #         s.nextEcl.temp <- swe_jdet_to_utc(jd_et    = s.nextLocalEcl_Jd["time_of_max_eclipse"], 
  #                                           gregflag = SE$GREG_CAL)
  #         
  #         ecl_gregtime <- paste(s.nextEcl.temp$year_out,"-",
  #                               s.nextEcl.temp$month_out,"-",
  #                               s.nextEcl.temp$day_out," ",
  #                               s.nextEcl.temp$hour_out,":",
  #                               s.nextEcl.temp$min_out,":",
  #                               s.nextEcl.temp$sec_out,
  #                               sep = "") |>
  #           ymd_hms()
  #         
  #         ecl_type     <- "solar"
  #         ecl_subtype  <- NA
  #         obscuration  <- temp.sol$attr[c(2)+1]
  #         
  #         rm(s.nextEcl.temp)
  #         
  #         df.out <- rbind(df.out, 
  #                         data.frame(eclipse_type     = ecl_type,
  #                                    eclipse_subtype  = NA,
  #                                    eclipse_greg     = ecl_gregtime, 
  #                                    obsc.mag_percent = obscuration,
  #                                    url              = NA))
  #         
  #       }else{
  #         
  #         l.nextEcl.temp <- swe_jdet_to_utc(jd_et    = l.nextLocalEcl_Jd["time_of_max_eclipse"], 
  #                                           gregflag = SE$GREG_CAL)
  #         
  #         ecl_gregtime <- paste(l.nextEcl.temp$year_out,"-",
  #                               l.nextEcl.temp$month_out,"-",
  #                               l.nextEcl.temp$day_out," ",
  #                               l.nextEcl.temp$hour_out,":",
  #                               l.nextEcl.temp$min_out,":",
  #                               l.nextEcl.temp$sec_out,
  #                               sep = "") |>
  #           ymd_hms()
  #         
  #         ecl_type     <- "lunar"
  #         ecl_subtype  <- l.subtype
  #         umbral.mag  <- temp.lun$attr[c(0)+1]
  #         
  #         rm(l.nextEcl.temp)
  #         
  #         ewlun_url(ecl_gregtime, ecltype = ecl_subtype)
  #         
  #         df.out <- rbind(df.out, 
  #                         data.frame(eclipse_type     = ecl_type, 
  #                                    eclipse_subtype  = NA,
  #                                    eclipse_greg     = ecl_gregtime, 
  #                                    obsc.mag_percent = umbral.mag,
  #                                    url              = NA))
  #       }
  #       
  #       
  #       cur_gregtime <- ecl_gregtime %m+% hours(12)
  #       
  #       #print(cur_gregtime)
  #       
  #     }
  #     
  #     #df.out$eclipse_greg |> with_tz("America/New_York")
  #     
  #     # add lonlat
  #     df.out$lon <- x
  #     df.out$lat <- y
  #     
  #     return(df.out)
  #   }
  #   
  #   out.df <- fun_get.e(x = input$lonlatBtn_lon, 
  #                       y = input$lonlatBtn_lon, 
  #                       dtg = Sys.time(), 
  #                       yrs_fwd = input$n_fut_yrs)
  #   
  #   out.df
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
