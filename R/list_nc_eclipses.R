renv::use(
  cli          = "cli@3.6.4",
  colorspace   = "colorspace@2.1-1",
  cpp11        = "cpp11@0.5.1",
  dplyr        = "dplyr@1.1.4",
  fansi        = "fansi@1.0.6",
  farver       = "farver@2.1.2",
  generics     = "generics@0.1.3",
  ggplot2      = "ggplot2@3.5.1",
  glue         = "glue@1.8.0",
  gtable       = "gtable@0.3.6",
  isoband      = "isoband@0.2.7",
  labeling     = "labeling@0.4.3",
  lattice      = "lattice@0.22-6",
  lifecycle    = "lifecycle@1.0.4",
  lubridate    = "lubridate@1.9.4",
  magrittr     = "magrittr@2.0.3",
  MASS         = "MASS@7.3-61",
  Matrix       = "Matrix@1.7-1",
  mgcv         = "mgcv@1.9-1",
  munsell      = "munsell@0.5.1",
  nlme         = "nlme@3.1-166",
  pillar       = "pillar@1.10.1",
  pkgconfig    = "pkgconfig@2.0.3",
  R6           = "R6@2.6.1",
  RColorBrewer = "RColorBrewer@1.1-3",
  Rcpp         = "Rcpp@1.0.14",
  renv         = "renv@1.1.1",
  rlang        = "rlang@1.1.5",
  scales       = "scales@1.3.0",
  swephR       = "swephR@0.3.1",
  swephRdata   = "swephRdata@0.0.1",
  tibble       = "tibble@3.2.1",
  tidyselect   = "tidyselect@1.2.1",
  timechange   = "timechange@0.3.0",
  utf8         = "utf8@1.2.4",
  vctrs        = "vctrs@0.6.5",
  viridisLite  = "viridisLite@0.4.2",
  withr        = "withr@3.0.2"
)

library(renv)
library(lubridate)
library(dplyr)
library(ggplot2)
library(swephR)
library(swephRdata)

# install.packages("swephRdata", repos = "https://rstub.r-universe.dev", type = "source")

renv::embed()
renv::snapshot()

rm(list=ls());cat('\f')

dates_txt <- "August 30, 1905 (P)
    January 3, 1908 (P) †
    June 28, 1908 (P)
    June 17, 1909 (P)
    April 28, 1911 (P)
    April 17, 1912 (P)
    October 10, 1912 (P) †
    February 3, 1916 (P)
    June 8, 1918 (P)
    November 22, 1919 (P)
    November 10, 1920 (P)
    September 10, 1923 (P)
    January 24, 1925 (P)
    April 28, 1930 (P)
    August 31, 1932 (P)
    February 3, 1935 (P)
    April 19, 1939 (P)
    April 7, 1940 (P)
    July 9, 1945 (P)
    November 23, 1946 (P)
    November 12, 1947 (P)
    March 7, 1951 (P)

	

    September 1, 1951 (A)
    June 30, 1954 (P)
    October 2, 1959 (P)
    September 20, 1960 (P)
    July 31, 1962 (P)
    July 20, 1963 (P)
    May 9, 1967 (P)
    September 11, 1969 (P)
    March 7, 1970 (T)
    July 10, 1972 (P)
    December 24, 1973 (P)
    December 13, 1974 (P)
    October 12, 1977 (P)
    February 26, 1979 (P)
    May 30, 1984 (A)
    October 3, 1986 (P)
    July 11, 1991 (P)
    May 10, 1994 (P)
    February 26, 1998 (P)
    August 11, 1999 (P) †
    December 25, 2000 (P)

	

    December 14, 2001 (P)
    June 10, 2002 (P)
    April 8, 2005 (P)
    May 20, 2012 (P)
    November 3, 2013 (P)
    October 23, 2014 (P)
    August 21, 2017 (T)
    June 10, 2021 (P)
    October 14, 2023 (P)
    April 8, 2024 (P)
    March 29, 2025 (P)
    → August 12, 2026 (P)
    January 26, 2028 (P)
    January 14, 2029 (P)
    November 14, 2031 (P)
    January 5, 2038 (P)
    July 2, 2038 (P)
    November 4, 2040 (P)
    August 12, 2045 (P)
    June 11, 2048 (P)
    May 31, 2049 (P)

	

    November 14, 2050 (P)
    March 30, 2052 (P)
    January 27, 2055 (P)
    July 12, 2056 (P)
    July 1, 2057 (P)
    June 22, 2066 (P)
    June 11, 2067 (P)
    December 6, 2067 (P)
    November 24, 2068 (P)
    September 23, 2071 (P)
    November 15, 2077 (P)
    May 11, 2078 (T)
    May 1, 2079 (P)
    February 27, 2082 (P)
    February 16, 2083 (P)
    September 23, 2090 (P)
    February 7, 2092 (P)
    July 23, 2093 (P)
    December 7, 2094 (P)
    September 14, 2099 (T)
    March 10, 2100 (P)"


dates_v <- dates_txt %>%
  strsplit(x = ., 
           split = "\n") %>% 
  unlist() %>%
  trimws() %>%
  .[! . %in% ""]

suffix_cross_v <- NULL


for(i in 1:length(dates_v)){
  temp <- dates_v[i] %>%
    substr(., 
           start = nchar(dates_v[i]), 
           stop = nchar(dates_v[i]))
  suffix_cross_v <- c(suffix_cross_v, temp)
  
  temp <- NA
}

suffix_cross_v <- gsub(pattern = "\\)", 
     replacement = "", 
     x = suffix_cross_v)

suffix_cross_v <- !suffix_cross_v == ""

eclipse_date_v <- dates_v %>%
  gsub("\\(.*$", "", .) %>%
  trimws() %>%
  lubridate::mdy()


eclipse_type_v <- dates_v %>%
  gsub("^.*\\(|\\).*$", "", .)


df_ecl <- data.frame(ecl_date = eclipse_date_v, 
           ecl_type = eclipse_type_v, 
           sufx     = suffix_cross_v)

df_ecl <- df_ecl[order(df_ecl$ecl_date),]


df_ecl$ecl_type[df_ecl$ecl_type == "P"] <- "partial"
df_ecl$ecl_type[df_ecl$ecl_type == "T"] <- "total"
df_ecl$ecl_type[df_ecl$ecl_type == "A"] <- "annular" 

as_tibble(df_ecl)


ggplot() + 
  geom_point(data = df_ecl[!df_ecl$ecl_date %in% 
                             ymd(c("20240408", "20231014", "20170821")),], 
             aes(x = ecl_date, y = ecl_type)) +
  geom_point(data = df_ecl[df_ecl$ecl_date %in% 
                             ymd(c("20240408", "20231014", "20170821")),], 
             aes(x = ecl_date, y = ecl_type), 
             color = "red") 

df_ecl$days_bw <- as.numeric(c(NA,diff(df_ecl$ecl_date)))


df_ecl[df_ecl$ecl_date >= (Sys.Date() %m-% years(50)) & 
         df_ecl$ecl_date <= (Sys.Date() %m+% years(100)),] %>%
  ggplot(data = ., 
         aes(x = ecl_date, y = ecl_type))+
  geom_point() 


df_pop <- data.frame(year = factor(c(2017, 2023, 2024)), 
                     pop  = c(325.1, 334.9, 340.1)*1000000)
df_ppl <- data.frame(year = factor(c(2017, 2023, 2024)), 
                     popN  = c(12,6.6,31.6)*1000000)

data(package = "swephR")

ggplot() + 
  geom_col(data = mutate(left_join(df_pop, df_ppl),
                         pct_n = popN/pop), 
           aes(x = year, y = pct_n)) +
  scale_y_continuous(labels = scales::percent, 
                     limits =c(0,NA), 
                     breaks = seq(0,1,by = 0.01))
