#########################################
# MSc Main project examining trajectories of change for PTSD patients
# during trauma focused CBT 
#
# Author: 220225638
# Date: 2023-08-13
# Description: Configuration file for setup and file path specification
#########################################

##################################
# Load libraries
##################################
library(here)
library(haven)
library(dplyr)
library(lme4)
library(nlme)
library(ggplot2)
library(lmerTest)
library(MuMIn)
library(performance)
library(tidyverse)
library(fastDummies)
library(ResourceSelection)
library(lmtest)

##################################
# Set file directories
##################################
raw_dir = here("raw/")
figs_dir = here("figs/")
processed_dir = here("processed/")

##################################
# Set file paths 
##################################
raw_data_path = paste0(raw_dir, "PTSD_Long_dataset_v1.sav")
processed_data_path = paste0(processed_dir,"PTSD_processed.sav")

primary_graph_path = paste0(figs_dir, "Primary_graph.png")

sensitivity_graph_path = paste0(figs_dir, "Sensitivity_1_graph.png")
