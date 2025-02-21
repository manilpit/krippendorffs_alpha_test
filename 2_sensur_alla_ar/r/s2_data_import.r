# Data import script
source("1_setup.R")

# Import h22
setwd("2022 høst")
h22 <- readxl::read_excel("komplett_sensur_BV_h22.xlsx", sheet = "Full sensur")
