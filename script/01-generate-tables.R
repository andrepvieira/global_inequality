# clean up workspace

rm(list=ls())

# close all figure windows created with x11()

graphics.off()

# LOAD PACKAGES ----

library(tidyverse)

# LOAD DATA ----

hum_right_clean_tbl
v_dem_clean_tbl
wbl_clean_tbl
v_dem_gender_clean_tbl

# MAKE CHARTS ----


