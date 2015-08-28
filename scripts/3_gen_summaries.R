# Generate Report
setwd('summary')
library(pander)
library(knitr)
source('create_summary.R')
Pandoc.brew(file = 'summary.md', output = 'summary.html', convert = "html")
setwd('..')
