#--- global file shiny app Italy Covid-19

# [1] Setup ---------------------------------------------------------------

setwd("~/Desktop/Data Science/covid-19-italy/shiny")

library(tidyverse)
library(lubridate)
library(highcharter)
library(DT)
library(shiny)
library(shinydashboard)
library(TTR)
library(shinycssloaders)

# [2] Import --------------------------------------------------------------

data <- read.csv("/Users/vito/Desktop/Data Science/covid-19/dati-regioni/dpc-covid19-ita-regioni.csv", 
                 h = T, sep = ",", stringsAsFactors = F)

# [3] Cleaning ------------------------------------------------------------

data$data <- as.Date(data$data)
data$denominazione_regione[data$codice_regione == 4] <- "Trentino Alto Adige"

# regional dataset
regions <- data %>% 
  select(
    date = data,
    region = denominazione_regione,
    new_cases = nuovi_positivi,
    actual_cases = totale_positivi,
    healed = dimessi_guariti,
    dead = deceduti,
    total_cases = totale_casi,
    tested = tamponi
  ) %>% 
  group_by(region, date) %>% 
  summarise(
    new_cases = sum(new_cases),
    actual_cases = sum(actual_cases),
    healed = sum(healed),
    dead = sum(dead),
    total_cases = sum(total_cases),
    tested = sum(tested)
  ) %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(
    daily_healed = c(NA,diff(healed)),
    daily_dead = c(NA,diff(dead)),
  ) %>% 
  ungroup() %>% 
  mutate(mortality = round(dead/total_cases*100,1))

regions$daily_healed[is.na(regions$daily_healed)] <- regions$healed[regions$date == "2020-02-24"]
regions$daily_dead[is.na(regions$daily_dead)] <- regions$dead[regions$date == "2020-02-24"]
regions$mortality[is.na(regions$mortality)] <- 0

# italy dataset
italy <- regions %>% 
  group_by(date) %>% 
  summarise(
    new_cases = sum(new_cases),
    actual_cases = sum(actual_cases),
    healed = sum(healed),
    dead = sum(dead),
    total_cases = sum(total_cases),
    tested = sum(tested),
    daily_healed = sum(daily_healed),
    daily_dead = sum(daily_dead)
  ) %>% 
  ungroup() %>% 
  mutate(mortality = round(dead/total_cases*100,1))

# [4] Save ----------------------------------------------------------------

save(regions, italy, file = "./dataset.RData")

