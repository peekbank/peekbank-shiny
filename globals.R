# GLOBAL.R
# This is the global for all of the shiny apps in numberbank-shiny 

# --------------------- LIBRARIES ---------------------
library(shiny)
library(shinyBS)
library(shinythemes)
library(feather)
library(ggthemes)
library(magrittr)
library(childesr)
library(tidytext)
library(forcats)
library(ggrepel)
library(ggbeeswarm)
library(tidyverse)
library(here)

ggplot2::theme_set(ggthemes::theme_few())

# --------------------- CONSTANTS ---------------------

DAYS_PER_YEAR <- 365.25
DAYS_PER_MONTH <- DAYS_PER_YEAR / 12
MONTHS_PER_YEAR <- 12
MIN_N_FOR_BIGRAMS <- 50

# --------------------- SHARED DATA LOADING ---------------------

# load some demo data to play with 
d_trials <- read_csv(here('demo_data/trials.csv'))
d_subjects <- read_csv(here('demo_data/subjects.csv'))
d_datasets <- read_csv(here('demo_data/datasets.csv'))

# --------------------- ENABLE BOOKMARKING ---------------------
# Note: input elements must be generated in server.r for this to work
enableBookmarking(store = "url")
