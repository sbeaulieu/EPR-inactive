# quick script to create Appendix for Photo Guide EPR inactive macrofauna
# and to create data table for BCO-DMO
# Stace Beaulieu
# 2026-07-05

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
full_data <- read_excel("TEMPLATE_unique_macrofauna_morphospecies_2026-07-05.xlsx", skip = 3)

# keep only the unique morphospecies
uniq_morph <- dplyr::filter(full_data, consider_for_checklist_unique_morphospecies == "y")

# keep only the columns for the Appendix printout
Appendix_uniq_morph <- dplyr::select(uniq_morph, "Table of Contents", morphospecies, identificationRemarks, kingdom, phylum, class, order, family, genus, species)
# ultimately decided to not include identificationRemarks in Appendix

# save output Appendix file
readr::write_csv(Appendix_uniq_morph, "Appendix_uniq_morph_2026-07-05.csv", na = "")

# keep only the columns for the data table for BCO-DMO
# exclude deprecated or just used for harmonization
for_bcodmo <- dplyr::select(full_data, "Table of Contents",
                            morphospecies,
                            identificationRemarks,
                            scientificName,
                            scientificNameID,
                            associatedMedia,
                            occurrenceID,
                            associatedSequences,
                            consider_for_checklist_unique_morphospecies,
                            kingdom,
                            phylum,
                            class,
                            order,
                            family,
                            genus,
                            species)

# save output data table for BCO-DMO
readr::write_csv(for_bcodmo, "BCO-DMO_macrofauna_morphospecies_2026-07-05.csv", na = "")
# need to strip the bottom row
# filter order_unique_morphospecies is not NA
