# script to create wide format At-Sea counts for AT50-33
# Stace Beaulieu
# 2025-04-22

# columns needed from long format AT50-33_InactiveSulfides_Macrofauna_Data sheet
# Alvin Dive ID, Rock association, Event ID, Bulk or Subsample, Morphotype, Count
# Alvin Dive ID and Rock association are not necessary but good to confirm manual entry into sheet

#Loading in the packages

library(readxl)
library(dplyr)
library(tidyr)

#Setting wd

setwd("/Users/sbeaulieu/Downloads")

#Loading in the datasheet
# download from project Google Drive
# by default this will read in the first sheet/tab of the EXCEL file

input <-readxl::read_xlsx("AT50-33_InactiveSulfides_Macrofauna_Data_postAL5294_14FEB2025_FINAL.xlsx")

# subset to selected columns

input_subset <- select(input, 'Alvin Dive ID', 'Rock association', 'Event ID (AL####-R#-S/R/W)', 'Bulk or Subsample', Morphotype, Count)

# note there is 1 Melanodrymia sp. that was lost so...
# once I subset rows to Morphotype not NA, need to save subsample plus NA
# and Morphotypes to exclude
input_subset <- filter(input_subset, !is.na(Morphotype))
input_subset <- filter(input_subset, Morphotype != "bulk")
input_subset$Bulk_or_Subsample <- input_subset$`Bulk or Subsample`
input_subset <- filter(input_subset, Bulk_or_Subsample != "bulk"|is.na(Bulk_or_Subsample))
input_subset <- filter(input_subset, Morphotype != "tissue" & Morphotype != "rock")

input_subset$eventID <- input_subset$`Event ID (AL####-R#-S/R/W)`
# hard-coding to harmonize 
input_subset$Morphotype <- gsub("Neolepetopsis.*", "Neolepetopsis", input_subset$Morphotype)
input_subset <- input_subset %>% 
  mutate(Morphotype = replace(Morphotype, Morphotype == "Protist", "cauliflower protist"))
# in case capitalization matters for group_by
input_subset$Morphotype <- tolower(input_subset$Morphotype)

# per eventID
input_subset_eventID <- select(input_subset, eventID, Morphotype, Count)
# group by eventID then by morphotype
input_subset_eventID_grouped <- input_subset_eventID %>%
  group_by(eventID, Morphotype) %>%
  summarise(Total = sum(Count))

# pivot per eventID from long to wide
wide_eventID <- pivot_wider(
  input_subset_eventID_grouped,
  id_cols = Morphotype,
  names_from = eventID,
  values_from = Total
)
# write.csv(wide_eventID,"wide_per_eventID_At-Sea_AT50-33.csv")

# per rock
input_subset_rock <- unite(input_subset, Dive_Rock, `Alvin Dive ID`, `Rock association`, sep = "_", remove = TRUE, na.rm = FALSE)
input_subset_rock <- select(input_subset_rock, Dive_Rock, Morphotype, Count)

# group by rock then by morphotype
input_subset_rock_grouped <- input_subset_rock %>%
  group_by(Dive_Rock, Morphotype) %>%
  summarise(Total = sum(Count))

# pivot per rock from long to wide
wide_rock <- pivot_wider(
  input_subset_rock_grouped,
  id_cols = Morphotype,
  names_from = Dive_Rock,
  values_from = Total
)
# write.csv(wide_rock,"wide_per_rock_At-Sea_AT50-33.csv")