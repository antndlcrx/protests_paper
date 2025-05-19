##################################################
# main file for the "Differentiated Repression: Evidence from Russian Protests" paper
##################################################

#### Set Up ####
pacman::p_load(tidyverse, rio, ggplot2, readr, broom, readxl,
               sandwich, lmtest, lubridate, margins, stargazer,
               sjPlot, interplot, ggeffects, quanteda)

Sys.getlocale()
# Set this to a locale that supports Cyrillic, for example, on Windows
Sys.setlocale("LC_ALL", "Russian")


#### load data ####

# we perform the following steps of data preprocessing in python for convenience and efficiency: 
# 1. annotation of protest even data by protest organiser
# 2. annotation of protest event data by protest type

# the scripts below already contain both annotations and build up on them.

data_codes = "data/region_codes.xlsx"

acled <- import("data/acled_with_org_indicator.csv") 

codes = data <- openxlsx::read.xlsx(data_codes, sheet = "Sheet1")
codes <- codes %>%
  mutate(region_name_en = str_trim(region_name_en))


#### clean data ####
source("data_cleaning_add_variables.R", encoding = "UTF-8")

#### data analyses ####
# conducts analyses reported in table 1 and figures 2 and 3 #
# conducts robustness checks reported in table A6 and Figure A1
source("analyses_and_visualisations.R", encoding = "UTF-8")

#### crowd size and source of reporting patterns visualisation ####
# creates figures A2 and A3
source("crowd_size_and_source_visualisation.R", encoding = "UTF-8")
