library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyWidgets)
library(shinyBS)
library(plotly)
library(gt)
library(dashboardthemes)
library(gt)


# load data
# load universal variables
Ninputs <- readxl::read_xlsx("data/NmodelInputsUpdate.xlsx", na = "NA") %>%
  filter(crop != "Dry lot" &
           crop != "Pasture seeding")
crops <- sort(unique(Ninputs$crop))
soils <- read.csv("data/soilsForNitrate.csv")
counties <- sort(unique(soils$County))
denit <- read_csv("data/denitr.csv")
