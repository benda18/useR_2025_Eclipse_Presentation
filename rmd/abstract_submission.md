---
title: "Every Eclipse Visible from Your Current Location for the Rest of Your Life"
# author: "Tim Bender"
output:
  html_document: 
    code_download: false
    code_folding: show
    toc: true
    toc_float: true
    toc_depth: 2
    keep_md: true
params:
  name: Timothy J. Bender
  email: bendertj@gmail.com
  phone: "+1 (513) 477-8924"
  #subtitle: ""
  # date: "Submitted: 2025-03-10" 
---

<br>Submitted: 2025-03-10 <br>Author: Timothy J. Bender <br> Email: bendertj@gmail.com <br> Phone: +1 (513) 477-8924 <br> 





## ***Abstract***

*One hobbyist's attempt to use R to never again miss a solar eclipse:* 

Last winter (2023-24) I built a shiny app to use as a planning tool for the April 2024 North American Total Eclipse.  As I learned how to use the astronomical calculation tools within the swephR package I started to explore historical eclipse events. 

Since 2017 there have been 3 major solar eclipse events visible in North Carolina: 
-   a total solar eclipse on August 21, 2017 (n% obsc.), 
-   an annular solar eclipse on October 14, 2023 (n% obsc.), and 
-   a total solar eclipse on April 8, 2024 (n% obsc.)

I didn't even know about the 2023 eclipse because as an annular eclipse[^3] (lacking a view of the sun's corona), it wasn't as hyped.

[^3]: An annular solar eclipse happens when the Moon passes between the Sun and Earth, but when it is at or near its farthest point from Earth. Because the Moon is farther away from Earth, it appears smaller than the Sun and does not completely cover the Sun. As a result, the Moon appears as a dark disk on top of a larger, bright disk, creating what looks like a ring around the Moon. (Source: [NASA](https://science.nasa.gov/eclipses/types/#h-annular-solar-eclipse))

**HOWEVER, I remembered a day in the fall of 2023 when the sky began to darken for no apparent reason** and realized a few months later as I was making this shiny app that I had experienced a solar eclipse without even realizing it.

That's the moment I knew I needed a different eclipse planning tool - a way to never miss a solar (or lunar) eclipse again, no matter where I'm at on Earth.  

A [shiny webapp](https://tim-bender.shinyapps.io/shiny_all_eclipses/)[^1] that calculates all visible solar and lunar eclipses for up to the next 75 years specific to your current lon/lat coordinate location. 


#### **Background - oops we missed an eclipse**

-   After driving my school-age kids about 9 hours to see totality during the [North American total solar eclipse](https://science.nasa.gov/eclipses/future-eclipses/eclipse-2024/) of April 2024, I realized that we had missed the annular solar eclipse just [7 months earlier](https://en.wikipedia.org/wiki/Solar_eclipse_of_October_14,_2023) that was less-well publicized. Not wanting to miss any opportunity to see an eclipse again I began considering my problem.

#### **Problem - it's hard finding good information on upcoming celestial events**

-   Reliable eclipse predictions are freely available online though they are usually generalized to a state-level geography. In April 2024 there were parts of Dallas, TX that could view totality (image below, left) for more than 4 minutes, and other parts that were outside the path of totality and saw only a partial eclipse (image below, right).

![View of Totality](images/totality.jpg){width="254"}![Partial View (outside path of totality)](images/partialEcl.jpg){width="250"}

#### **Solution - Build a shiny app**

-   A shiny app that will use your current location (or a specific address input by user) to identify every solar and lunar eclipse viewable from that location for the next (n) years.\
-   Current functioning webapp: Future Solar and Lunar Eclipses Visible from Your Current Location ([Shiny webapp](https://tim-bender.shinyapps.io/shiny_all_eclipses/))
-   Relying primarily on the following packages/libraries:
-   *swephR* ([via CRAN](https://cran.r-project.org/package=swephR)): High Precision Swiss Ephemeris package. Used to determine when, where and how solar and lunar eclipses will occur (among other novel celestial events).
-   *leaflet* ([via CRAN](https://cran.r-project.org/package=leaflet)): Create Interactive Web Maps with the JavaScript 'Leaflet' Library
-   *shiny* ([via CRAN](https://cran.r-project.org/package=shiny)): Web Application Framework for R.
-   *censusxy* ([via github](https://github.com/chris-prener/censusxy)): Designed to provide easy access to the U.S. Census Bureau Geocoding Tools
-   *qrcode* ([via CRAN](https://cran.r-project.org/package=qrcode)): Create static QR codes in R. Implemented so that users could share between mobile devices.

<!-- -   App I/Os: -->

<!--     -   number of years (n) to look into the future (I) -->

<!--     -   A date_time value to search from (I) -->

<!--     -   A lon/lat coordinate pair derived from the user's i.p. address (with permission) (I) -->

<!--     -   A USPS mailing address to geocode a lon/lat coordinate pair for a specific viewing location (I) -->

<!--     -   A date_time value for the every eclipse visible from the (I) viewing location for the next (n) years (O) -->

<!--     -   Eclipse type (solar/lunar) (O) -->

<!--     -   Eclipse sub type (total/partial...) (O) -->

<!--     -   Obscuration (% of sun blocked by moon as seen from viewing location - solar eclipse only) (O) -->

<!--     -   A lon/lat coordinate pair for the location of the maximal view of the eclipse on Earth (O)  -->

<!--     -   A dynamic leaflet map showing the lon/lat coordinate pairs (I & O) -->


``` r
# SOME CODE to capture solar eclipse information 
library(swephR)
library(lubridate)
library(scales)

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
  ifelse(yes = "Visible from input location",
         no  = "Not visible from input location")

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
```

```
## $Eclipse_DateTime
## [1] "2025-09-21 19:40:51 UTC"
## 
## $Visible_Locally
## [1] "Not visible from input location"
## 
## $Obscuration_Locally
## [1] "0.0%"
## 
## $Maximal_View.Globally
##       lon       lat 
## 153.38805 -61.03134
```

<!-- ```{r Static Map, echo=T, message=FALSE, warning=FALSE, results="hide", eval = FALSE} -->

<!-- # SOME CODE for a static map verification of input lon/lat -->

<!-- library(ggmap) -->

<!-- library(dplyr) -->

<!-- library(sf) -->

<!-- ## Must run once per session -->

<!-- # register_stadiamaps(key = "your_key_here", write = FALSE) -->

<!-- bbox <- make_bbox(lon = c(input_lonlat["lon"], output_lonlat["lon"]),  -->

<!--                   lat = c(input_lonlat["lat"], output_lonlat["lat"]),  -->

<!--                   f   = var_map.f) -->

<!-- map.stamen <- get_stadiamap(bbox = bbox,  -->

<!--                             zoom = 3,  -->

<!--                             maptype = "stamen_toner_lite", -->

<!--                             crop = T,  -->

<!--                             color = "color", -->

<!--                             force = T, -->

<!--                             size = 1) -->

<!-- ggmap(map.stamen) + -->

<!--   geom_label(aes(x = input_lonlat["lon"],  -->

<!--                  y = input_lonlat["lat"]),  -->

<!--              label = "Duke University",  -->

<!--              color = "blue", alpha = 0.1) + -->

<!--   geom_label(aes(x = output_lonlat["lon"],  -->

<!--                  y = output_lonlat["lat"]),  -->

<!--              label = "Eclipse Maximal",  -->

<!--              color = "brown", alpha = 0.1) -->

<!-- ``` -->

<!-- ![](figure/Static%20Map-1.png) -->









## ***Broad Topic Covered***

#### **You thought time was confusing on earth**

-   Astronomers still use the Julian Calendar to track the cosmos

#### **Communication & Ease-of-Use**

-   Longitude / Latitude\
-   User Experience

#### **Knowing what to do VS Knowing how to do**

## Learning Goals

-   main goal

-   You don't have to know ~~everything~~ ~~anything~~ as much as you think you do.

-   Evolving your skills and knowledge throughout a project lifecycle: pros and cons.

## Timezone Preference(s)

-   I live about 15 minutes from Duke University so any presentation I would prefer in-person if possible. Otherwise America/New_York \<--\> America/Los_Angeles

## Duration

-   Open to any of the following:
    -   Talk (15-20 minutes) - (in-person)
    -   Lightning Talk (5 minutes) - (in-person)
    -   Poster / visual presentation

## Language

-   English

## Intended Audience

-   Eclipse seekers
-   Beginners and hobbyists looking for:
    -   project idea inspiration,
    -   a better understanding on how to approach the project life cycle, and
    -   managing project creep

## Pedagogical Method of Teaching

-   For this I would approach in a lecture-style while allowing for interaction. I'm a seasoned public speaker in front of local elected and appointed officials and in engaging public meetings.

## Maximum Number of Attendees

-   I would imagine the number of attendees interested would be relatively low due to the field intersections - astronomy x geospatial x public_communication. But if you are asking how many people I would be comfortable presenting in front of, there is no lower or upper bounds.

## Motivation to Teach this Tutorial at useR!

-   I enjoy making things in R (mostly as a hobby right now), I made a thing that's pretty neat and would like to share my experience in the hope that there are others who could benefit from either technical or general information shared.
-   I became VERY MOTIVATED when I saw that useR! 2025 was at Duke University in Durham as I live only a few miles away. It just seemed like the opportunity was right.

## Supplemental Material Links

#### Tutorial materials

-   Future Solar and Lunar Eclipses Visible from Your Current Location ([Shiny webapp](https://tim-bender.shinyapps.io/shiny_all_eclipses/))

#### Web Page (if applicable)

## Prerequisites / requirements

-   None

## License, Material sharing, recording consent

-   Eclipse Predictions by Fred Espenak (diagrams in-app only), [www.EclipseWise.com](www.EclipseWise.com) - The use of diagrams and maps is permitted provided they are NOT altered (except for re-sizing) and the embedded credit line is NOT removed or covered.

## ***Biography***

#### Tim Bender, (title)

-   Bachelor of Urban Planning from the University of Cincinnati.\

-   \~15 years local government experience as an urban planner and transit planner. Tim was part of a team that helped deploy Google Transit for his transit agency in Kentucky in 2008, being among the first 50-ish agencies worldwide to go live.\

-   Journey with R began with a desire to log transit vehicle real-time location data from an api for analysis but I had no programming experience or knowledge of how to approach the problem. I wouldn't successfully solve this problem for about 5 years of self-guided learning.

-   [LinkedIn](https://www.linkedin.com/in/tim-bender-238870171/)

-   [github](https://github.com/benda18)
