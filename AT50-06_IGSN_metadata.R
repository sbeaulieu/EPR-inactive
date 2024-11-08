# script to compile metadata table for IGSNs
# AT50-06 rock samples not saved, no physical archiving just sub-samples
# Stace Beaulieu
# 2024-11-08

library(readxl)
library(dplyr)

# read in the input data file downloaded from Google Drive
# - 1 rock sampling event table from Michael Meneses with columns added

event_renav <- read_excel("C:/Users/sbeaulieu/Downloads/Sulfide_Rock_IMG_Metadata_20241108.xlsx")
# check that the date column is read in as YYYY-MM-DD
# check that the hours column is read in as text not date

# for the Event metadata
# only keep the columns up to `alt_meters`
event_renav_only <- event_renav[,1:16]

# remove columns related to images, then distinct should remove extra rows
event_renav_only <- select(event_renav_only, -c("Filename"))
event_renav_only_distinct <- distinct(event_renav_only)

# only include rock sampling events
event_renav_only_distinct <- dplyr::filter(event_renav_only_distinct, samplingProtocol == "rock grab")

# quick plot to check geographic coordinates
plot(event_renav_only_distinct$decimalLongitude, event_renav_only_distinct$decimalLatitude, xlim = c(-104.30,-104.28))

# rename subset of columns for IGSN template
# eventID > "Sample Name"
# decimalLatitude > Latitude
# decimalLongitude > Longitude
# locality > Locality
# Dive > "Launch ID"


# add columns for IGSN template
event_renav_only_distinct$Material = "Rock"
event_renav_only_distinct$"Field name (informal classification)" = "volcanogenic massive sulfide"
event_renav_only_distinct$Classification = "Hydrothermal>Sulfide"
# $"Sample description" = "Inactive sulfide" # check that Meg uses for AT50-20
# $"Collection method" = "Grab>HOV"
# $"Collection method description" fill with concatenated string "Alt(m),Hdg(deg): ",value in alt_meters,",",value in heading_degrees
# $Purpose = case_when "Sample Name" is 5134 R2 and 5142 R1 use "microbiology and mineralogy"
#   else use "fauna and microbiology and mineralogy" for the 8 rock samples in BCO-DMO package
# $"Elevation start" = depth_meters * -1
# $"Elevation unit" = "meters"
# $"Navigation type" = "renav"
# see what Meg uses for AT50-20 Primary physiographic feature and Name of physiographic feature
# $"Field program/Cruise" = "AT5006" # check if IGSN uses R2R cruise DOIs
# $"Platform type" = "Ship"
# $"Platform name" = "Atlantis"
# $"Launch platform name" = "Alvin"
# $"Launch type" = "HOV"
# $"Collector/Chief Scientist" = "Arellano"
# $"Collection date" = concatenated string value in dd/mm/yyyy,"T", value in hh:mm:ssss,"Z"
# $"Collection date precision" = "time"
# for "Current archive" see if they have controlled vocab term for consumed

