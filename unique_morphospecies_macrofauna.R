# quick script to create Appendix for Photo Guide EPR inactive macrofauna
# Stace Beaulieu
# 2026-06-12

# input: 
# download "TEMPLATE_unique_macrofauna_morphospecies_WORKING_COPY" from Google folder as xlsx
# rename suffix to today's date

# output is expected to be a csv file that you open in EXCEL to adjust column spacing and print to PDF

# import libraries
library(readxl)
library(dplyr)
library(readr)

# set path
setwd("C:/Users/sbeaulieu/Downloads")

# load data
full_data <- read_excel("TEMPLATE_unique_macrofauna_morphospecies_2026-06-12.xlsx", skip = 3)

# keep only the unique morphospecies
uniq_morph <- dplyr::filter(full_data, consider_for_checklist_unique_morphospecies == "y")

# keep only the columns for the Appendix printout
Appendix_uniq_morph <- dplyr::select(uniq_morph, "Table of Contents", morphospecies, identificationRemarks, kingdom, phylum, class, order, family, genus, species)

# save output file
readr::write_csv(Appendix_uniq_morph, "Appendix_uniq_morph_2026-06-12.csv", na = "")
