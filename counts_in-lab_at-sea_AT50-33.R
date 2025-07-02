# script to integrate in-lab with at-sea counts
# Stace Beaulieu 
# 2025-07-02

#Load required packages

library(readxl)
library(dplyr)

#Set working directory

setwd("/Users/sbeaulieu/Downloads")

#Read in data sheets
# download from project Google Drive
# by default readxl reads in the first sheet/tab of the EXCEL file

# authoritative at-sea sheet
input_AtSea <-readxl::read_xlsx("macrofauna_AT50-33_At-sea_per_eventID_20250626.xlsx", skip=1)
# date downloaded in-lab WORKING COPY
input_InLab_EB <-readxl::read_xlsx("macrofauna_AT50-33_in-lab_Bogomolni_20250702.xlsx", skip=3)
input_InLab_WH <-readxl::read_xlsx("macrofauna_AT50-33_in-lab_Hamlin_20250702.xlsx", skip=3)
# add line to read in AB's in-lab AT50-33

#Join on Morphotype column
input_joined_EB <- dplyr::full_join(input_AtSea, input_InLab_EB, c("Morphotype" = "Morphotype_AT50-33_At-sea_20250626"))
input_joined_EB_WH <- dplyr::full_join(input_joined_EB, input_InLab_WH, c("Morphotype" = "Morphotype_AT50-33_At-sea_20250626"))

#Select a specific rock to check counts
rock_prefix <- "AL5288-R01"
subset <- dplyr::select(input_joined_EB_WH, 'revised template', Morphotype, starts_with(rock_prefix))
# write.csv(subset, file = paste(rock_prefix,".csv",sep=""), row.names = FALSE)
