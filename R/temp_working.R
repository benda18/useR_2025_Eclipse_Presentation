library(renv)


renv::status()
# renv::snapshot()


pkgs <- "base64enc
digest
evaluate
glue
highr
htmltools
jsonlite
knitr
magrittr
mime
rmarkdown
stringi
stringr
xfun
yaml" |> 
  strsplit("\n") |> 
  unlist()

renv::install(packages = pkgs)
