# quick script to create Appendix for Photo Guide EPR inactive macrofauna
# and to create data table for BCO-DMO
# and to check filenames for photos to zip for BCO-DMO
# Stace Beaulieu
# 2026-07-17

# inputs: 
# download "TEMPLATE_unique_macrofauna_morphospecies_WORKING_COPY" from Google folder as xlsx
# rename suffix to today's date
# .txt filenames from Mullineaux lab D drive

# outputs:
# Appendix csv file that you open in EXCEL to adjust column spacing and print to PDF
# BCO-DMO csv file

# import libraries
library(readxl)
library(dplyr)
library(readr)
library(tidyr) # associatedMedia has pipe separator

# set path
setwd("C:/Users/sbeaulieu/Downloads")

# load data
full_data <- read_excel("TEMPLATE_unique_macrofauna_morphospecies_2026-07-17.xlsx", skip = 3)

# keep only the unique morphospecies
uniq_morph <- dplyr::filter(full_data, consider_for_checklist_unique_morphospecies == "y")

# keep only the columns for the Appendix printout
Appendix_uniq_morph <- dplyr::select(uniq_morph, "Table of Contents", morphospecies, identificationRemarks, kingdom, phylum, class, order, family, genus, species)
# ultimately decided to not include identificationRemarks in Appendix

# save output Appendix file
#readr::write_csv(Appendix_uniq_morph, "Appendix_uniq_morph_2026-07-05.csv", na = "")

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
                            species,
                            category_in_Ayinde_Best_template)

# rename category_in_Ayinde_Best_template
# this is superset containing morphotypes in 2025 DSRI paper
for_bcodmo <- dplyr::rename(for_bcodmo, morphotypes_2025 = category_in_Ayinde_Best_template)

# need to strip the bottom 4 rows ("eggcases", "eukaryote unk", "coil?" and counter)
for_bcodmo <- dplyr::filter(for_bcodmo, row_number() <= n() - 4)

# save output data table for BCO-DMO
#readr::write_csv(for_bcodmo, "BCO-DMO_macrofauna_morphospecies_2026-07-17.csv", na = "")


# to confirm that filenames in BCO-DMO associatedMedia match the files in D drive
Ddrive <- readr::read_table("Ddrive_filenames_photos_2026-07-10.txt") # note parsing error if space in filename
Media <- for_bcodmo["associatedMedia"]
Media_long <- separate_longer_delim(Media, associatedMedia, delim = "|")
# if filenames match perfectly then full_join should have same number rows as Media_long
# use keep = TRUE to determine if D drive missing any files
confirm_filenames <- full_join(Media_long, Ddrive, c("associatedMedia" = "Ddrive_filenames"), keep = TRUE)
#write.csv(confirm_filenames, "associatedMedia_vs_Ddrive_filenames_2026-07-10_1130.csv")

