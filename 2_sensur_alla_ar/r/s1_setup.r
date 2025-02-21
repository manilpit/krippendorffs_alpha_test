library(activeDir)
# set_wd_to_current()

# Setup script - laster pakker og setter arbeidsmappe
rm(list = ls()) 

# Installer nødvendige pakker hvis de mangler
required_packages <- c("tidyverse", "readxl", "dplyr", "tidyr", 
                      "irr", "krippendorffsalpha", "parallel", "doParallel")

for(pkg in required_packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
