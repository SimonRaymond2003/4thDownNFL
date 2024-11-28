# global.R

# Libraries
library(readr)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(DT)
library(shinyWidgets)
library(shinyjs)
library(lubridate)
library(data.table)
library(later)

# Define app directory
APP_DIR <- "C:/Users/simon/Dropbox/My_Projects/NFL_4th_Down_App"

# Data loading
df_pbp <- fread(file.path(APP_DIR, "data/df_pbp.csv"))
pff_stats_no_grades <- fread(file.path(APP_DIR, "data/pff_stats_no_grades.csv"))
data_all <- fread(file.path(APP_DIR, "data/all_data.csv"))
decisions_data <- fread(file.path(APP_DIR, "data/decisions_data.csv"))