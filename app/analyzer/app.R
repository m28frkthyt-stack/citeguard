options(shiny.maxRequestSize = 1000 * 1024^2)

library(shiny)
library(shinyjs)
library(dplyr)
library(purrr)
library(stringr)
library(DT)
library(tibble)
library(httr2)
library(metacheck)
library(digest)
library(htmltools)
library(later)

for (f in list.files("../R", pattern = "\\.R$", full.names = TRUE)) source(f)

source("ui.R")
source("server.R")

shinyApp(ui, server)
