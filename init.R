# Install required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(plumber, httr, dplyr, jsonlite)
