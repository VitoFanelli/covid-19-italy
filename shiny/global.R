#--- global file shiny app Italy Covid-19

# [1] Setup ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(highcharter)
library(DT)
library(shiny)
library(shinydashboard)
library(TTR)
library(shinycssloaders)

load("dataset.RData")