# Libraries ---------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(janitor)
library(tidyverse)
library(leaflet)
library(tsibble)
library(shinythemes) # Themes maybe?
library(ggthemes)
library(infer)
library(shinydashboard)
library(tsibbledata)
library(fable)
library(slider)
library(feasts)
library(tidyquant)
library(sf)
library(rgdal)
library(lubridate)

# Source scripts & read clean data ----------------------------------------

source("scripts/clean_data.R")
clean_beds <- read_csv("clean_data/clean_beds.csv")
clean_ae <- read_csv("clean_data/clean_ae.csv")
clean_admissions <- read_csv("clean_data/clean_admissions_speciality_data.csv")
