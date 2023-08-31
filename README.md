# psy6009_PTSD_project

MSc Main project examining trajectories of change for PTSD patients
during trauma focused CBT 

## Project guide

This project can be run fully from the `main.R` script but the code has been split into 8 r script files including `main`. This is for better organisation, ease of maintainability and debugging.

Note: if you're downloading this project as a zip folder, extract the folder from the zip package and save locally

## Repository structure

.gitignore : ignore specific files intentionally from being tracked by git

.Rhistory : automatically generated by rstudio

.Rprofile : a file generated by renv that is run when r is started 


figs : contains saved graphs

LICENSE : publishing license from github

notes : contains the codebook

processed : contains processed data files

PSY6009_220225638 : r project file generated by r studio

raw : contains files of raw data 

README.md : this file

renv : a folder containing files generated by renv including activate.R for loading the correct packages and               versions

renv.lock : a file generated by renv to create a snapshot of all the versions and dependancies 

script : contains all the r scripts for the project

### Script folder

config.R : Configuration file for setup and file path specification

data-management.R : Data management file for raw data import, cleaning and export

main.R : Main script for running data management and visualisation

junk.R : Deleted when cleaning up scripts but wanted to keep

primary_analysis.R : code for multilevel modelling 

secondary_analysis.R : code for logistic regression 

sensitivity_1.R : code for multilevel modelling 20 sessions or less 

sensitivity_2.R : code for logistic regression session 8 
