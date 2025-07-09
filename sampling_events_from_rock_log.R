# script to derive sampling event metadata from rock log
# Stace Beaulieu
# 2025-07-09

#Load required packages

library(readxl)
library(dplyr)

#Set working directory

setwd("/Users/sbeaulieu/Downloads")

#Read in data sheets
# download from project Google Drive
# by default readxl reads in the first sheet/tab of the EXCEL file

# authoritative at-sea sheet
input_rock_log <-readxl::read_xlsx("AT50-33_Sulfide_rock_log_V6_2025-03-03.xlsx")

# subset the columns that I want to keep for GIS layer
rock_log_selected_columns <- input_rock_log |>
  select('Sample Time (UTC)','Latitude RENAV (dd)','Longitude RENAV','Depth VEHICLE','Sample I.D.','Vent I.D. [source: e.g., "2021 Vent Locations based on RR2102 ROV Jason Diving" if that is what Shawn continues using]','Vent (Inactive Rock) Category at seafloor [from Vent category key Rose-Jones.docx]')

# distinct rows, rename column headers
rock_log_selected_distinct <- rock_log_selected_columns |>
  distinct() |>
  rename(Feature = 'Vent I.D. [source: e.g., "2021 Vent Locations based on RR2102 ROV Jason Diving" if that is what Shawn continues using]', 'Rock Type' = 'Vent (Inactive Rock) Category at seafloor [from Vent category key Rose-Jones.docx]')
# sort by time column because slurp(s) precede rock grab
rock_log_selected_distinct_sorted <- rock_log_selected_distinct[order(rock_log_selected_distinct$'Sample Time (UTC)'),]
# write.csv(rock_log_selected_distinct_sorted, file = "sampling_events_from_rock_log_AT50-33.csv", row.names = FALSE)


