renv::use(
  cli        = "cli@3.6.4",
  cpp11      = "cpp11@0.5.1",
  dplyr      = "dplyr@1.1.4",
  fansi      = "fansi@1.0.6",
  generics   = "generics@0.1.3",
  glue       = "glue@1.8.0",
  lifecycle  = "lifecycle@1.0.4",
  lubridate  = "lubridate@1.9.4",
  magrittr   = "magrittr@2.0.3",
  pillar     = "pillar@1.10.1",
  pkgconfig  = "pkgconfig@2.0.3",
  R6         = "R6@2.6.1",
  renv       = "renv@1.1.1",
  rlang      = "rlang@1.1.5",
  tibble     = "tibble@3.2.1",
  tidyselect = "tidyselect@1.2.1",
  timechange = "timechange@0.3.0",
  utf8       = "utf8@1.2.4",
  vctrs      = "vctrs@0.6.5",
  withr      = "withr@3.0.2"
)

library(renv)
library(lubridate)
library(dplyr)

renv::embed()
renv::snapshot()

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
