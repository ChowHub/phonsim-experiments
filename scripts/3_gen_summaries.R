# Generate Report
setwd('summary')
library(pander)
library(knitr)
name = "summary"
knit(paste0(name, ".Rmd"), encoding = "utf-8")
Pandoc.brew(file = paste0(name, ".md"), output = name, convert = "html")
setwd('..')