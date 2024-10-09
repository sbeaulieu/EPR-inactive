# script to compile data table for BCO-DMO
# AT50-06 snail identifications by Chong Chen
# Stace Beaulieu
# 2024-10-09

library(readxl)
library(dplyr)

# read in the input data files
# 5 input data files in Google Drive
# - 2 identification tables from Chong Chen with columns added (Formalin file has sheet added for taxa)
# - 2 sorted sample tables from Michael Meneses with columns added
# - 1 rock sampling event table from Michael Meneses with columns added

IDsEthanol <- read_excel("C:/Users/sbeaulieu/Downloads/IDs_by_Tube_EthanolPreserved_Chong_Chen_20241009.xlsx")
IDsFormalin <- read_excel("C:/Users/sbeaulieu/Downloads/IDs_by_Tube_FormalinFixed_Chong_Chen_20241009.xlsx", sheet = "Sheet1")
taxa <- read_excel("C:/Users/sbeaulieu/Downloads/IDs_by_Tube_FormalinFixed_Chong_Chen_20241009.xlsx", sheet = "taxa")

sortedEthanol <- read_excel("C:/Users/sbeaulieu/Downloads/AT50-06 EtOH samples and images data_20241009.xlsx", sheet = "Sample Images Data Sheet")
sortedFormalin <- read_excel("C:/Users/sbeaulieu/Downloads/AT50-06 Formalin-fixed samples and images data_20241009.xlsx", sheet = "Image Data Sheet")

event_renav <- read_excel("C:/Users/sbeaulieu/Downloads/Sulfide_Rock_IMG_Metadata_20241009.xlsx")

# prepare the input data tables for joins

# # confirm that unique for column "Position of subsample in shipped cryobox" is same for IDs and sorted
# sort(unique(IDsEthanol$`Position of subsample in shipped cryobox`))
# sort(unique(sortedEthanol$`Position of subsample in shipped cryobox`))

# need to join on column "occurrenceID_suffix"
# confirm that unique for column "occurrenceID_suffix" is same for IDs and sorted
sort(unique(IDsEthanol$occurrenceID_suffix))
sort(unique(sortedEthanol$occurrenceID_suffix))
sort(unique(IDsFormalin$occurrenceID_suffix))
sort(unique(sortedFormalin$occurrenceID_suffix))

# only keep the columns up to occurrenceID_suffix
sortedEthanol_only <- sortedEthanol[,1:18]
sortedFormalin_only <- sortedFormalin[,1:16]

# # remove columns related to images, then distinct should remove extra rows `Position of subsample in shipped cryobox`
# # then check that number rows equiv to length unique above
# sortedEthanol_only <- select(sortedEthanol_only, -c("Image ID","# of Individuals in Image"))
# sortedEthanol_only_distinct <- distinct(sortedEthanol_only)
# length(unique(sortedEthanol$`Position of subsample in shipped cryobox`))

# only keep the sorted rows with value manually entered for occurrenceID_suffix
# dplyr filter column occurrenceID_suffix is not NA
sortedEthanol_only_distinct <- filter(sortedEthanol_only, !is.na(occurrenceID_suffix))
sortedFormalin_only_distinct <- filter(sortedFormalin_only, !is.na(occurrenceID_suffix))

# for the Event metadata
# only keep the columns up to `depth_meters`
event_renav_only <- event_renav[,1:13]
# remove columns related to images, then distinct should remove extra rows
event_renav_only <- select(event_renav_only, -c("Filename"))
event_renav_only_distinct <- distinct(event_renav_only)
# quick plot to check geographic coordinates
plot(event_renav_only_distinct$decimalLongitude, event_renav_only_distinct$decimalLatitude, xlim = c(-104.30,-104.28))


# join IDs with sorted sample table
# number of rows should match between IDs and sorted distinct because using occurrenceID_suffix
# want left_join for IDs rows

# IDs_sortedEthanol <- left_join(IDsEthanol, sortedEthanol_only_distinct, 
#                                by = "occurrenceID_suffix")
IDs_sortedEthanol <- left_join(IDsEthanol, sortedEthanol_only_distinct, 
                               by = c("occurrenceID_suffix", "verbatimLabel", "Position of subsample in shipped cryobox"))
IDs_sortedFormalin <- left_join(IDsFormalin, sortedFormalin_only_distinct, 
                               by = c("occurrenceID_suffix", "verbatimLabel", "Position of subsample in shipped cryobox"))

# join taxa on verbatimIdentification
IDs_taxa_sortedEthanol <- left_join(IDs_sortedEthanol, taxa)
IDs_taxa_sortedFormalin <- left_join(IDs_sortedFormalin, taxa)

# join with rock or slurp sampling event
# IDs_taxa_sortedEthanol_event <- left_join(IDs_taxa_sortedEthanol, event_renav_only_distinct,
#                                           by = "eventID")
IDs_taxa_sortedEthanol_event <- left_join(IDs_taxa_sortedEthanol, event_renav_only_distinct, 
                                          by = c("eventID", "Dive", "locality"))
# confirm 7 samples for IDs Ethanol
sort(unique(IDs_taxa_sortedEthanol_event$eventID))

IDs_taxa_sortedFormalin_event <- left_join(IDs_taxa_sortedFormalin, event_renav_only_distinct, 
                                          by = c("eventID", "Dive", "locality"))
# note there are only 6 not 7 samples for IDs Formalin
# we will not be providing eventID AL5134-LuckysM-R003 because it was not a match to the greatest amount of specimens per IDs_by_Tube
# the only possible match would be formalin_C2_1 M. galeronae 
sort(unique(IDs_taxa_sortedFormalin_event$eventID))


# add columns
# ethanol
IDs_taxa_sortedEthanol_event <- IDs_taxa_sortedEthanol_event %>%
  mutate(occurrenceID = paste(occurrenceID_prefix, occurrenceID_suffix),
         minimumDepthInMeters = trunc(depth_meters - 5),
         maximumDepthInMeters = trunc(depth_meters + 5)) # expect vehicle depth to be off by about 5 meters 

IDs_taxa_sortedEthanol_event$occurrenceRemarks = NA # not needed for ethanol samples
IDs_taxa_sortedEthanol_event$coordinateUncertaintyInMeters = 10 # uncertainty in Wu et al. DOI:10.1029/2021GC010213

# formalin
IDs_taxa_sortedFormalin_event <- IDs_taxa_sortedFormalin_event %>%
  mutate(occurrenceID = paste(occurrenceID_prefix, occurrenceID_suffix),
         minimumDepthInMeters = trunc(depth_meters - 5),
         maximumDepthInMeters = trunc(depth_meters + 5)) # expect vehicle depth to be off by about 5 meters 

IDs_taxa_sortedFormalin_event$associatedSequences = NA # not needed for formalin samples
IDs_taxa_sortedFormalin_event$occurrenceRemarks = "this species may also have occurred on other rocks collected on this dive"
# since there's uncertainty for formalin specimens in assigning occurrenceID to eventID
# add coordinateUncertaintyInMeters 30 m for Luckys Mound 
# add coordinateUncertaintyInMeters 100 m for Sentry Spire because samples span southern to northern edifice
# case when locality
IDs_taxa_sortedFormalin_event <- IDs_taxa_sortedFormalin_event %>%
  mutate(coordinateUncertaintyInMeters = case_when(locality == "Lucky's Mound" ~ 30
                                                   ,TRUE ~ 100)) # there are only 2 localities

# select the columns for the BCO-DMO to OBIS data product
IDs_taxa_sortedEthanol_event_select <- IDs_taxa_sortedEthanol_event %>%
  select(occurrenceID, kingdom, verbatimIdentification, scientificName, scientificNameID, taxonRank, identifiedBy, individualCount, associatedSequences, otherCatalogNumbers, occurrenceStatus, basisOfRecord, verbatimLabel, occurrenceRemarks, eventID, eventDate, locality, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, minimumDepthInMeters, maximumDepthInMeters, relatedResourceID, samplingProtocol)
IDs_taxa_sortedFormalin_event_select <- IDs_taxa_sortedFormalin_event %>%
  select(occurrenceID, kingdom, verbatimIdentification, scientificName, scientificNameID, taxonRank, identifiedBy, individualCount, associatedSequences, otherCatalogNumbers, occurrenceStatus, basisOfRecord, verbatimLabel, occurrenceRemarks, eventID, eventDate, locality, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, minimumDepthInMeters, maximumDepthInMeters, relatedResourceID, samplingProtocol)

# combine ethanol + formalin tables after the columns are in the right order
gastropods_inactive_AT5006_BCODMO <- bind_rows(IDs_taxa_sortedEthanol_event_select, IDs_taxa_sortedFormalin_event_select)

# write.csv(gastropods_inactive_AT5006_BCODMO,"C:/Users/sbeaulieu/Downloads/gastropods_inactive_AT50-06_BCO-DMO_20241009.csv", row.names=FALSE)
