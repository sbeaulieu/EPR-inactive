# script to integrate in-lab with at-sea counts
# Stace Beaulieu, Emmanuelle Bogomolni
# 2025-07-11

#Load required packages

library(readxl)
library(dplyr)
library(ggplot2)

#Set working directory

setwd("/Users/sbeaulieu/Downloads")

#Read in data sheets
# download from project Google Drive
# by default readxl reads in the first sheet/tab of the EXCEL file

# authoritative at-sea sheet
input_AtSea <-readxl::read_xlsx("macrofauna_AT50-33_At-sea_per_eventID_20250626.xlsx", skip=1)
# date downloaded in-lab WORKING COPY
input_InLab_EB <-readxl::read_xlsx("macrofauna_AT50-33_in-lab_Bogomolni_20250708.xlsx", skip=3)
input_InLab_WH <-readxl::read_xlsx("macrofauna_AT50-33_in-lab_Hamlin_20250708.xlsx", skip=3)
# also read in AB's in-lab AT50-33
# note that AB columns are type chr due to Present and Absent
input_InLab_AB <-readxl::read_xlsx("AT50-33_Macrofauna_Counts_Best_20250703.xlsx", skip=9)
InLab_AB_numeric <- input_InLab_AB %>%
  mutate(
    across(starts_with('AL'),
    function(x) case_when(
    x == "Present" ~ 1,
    x == "Absent" ~ 0,
    TRUE ~ as.numeric(x)
    )
  )
)
# also read in sampling events from rock log
# to be able to add feature and rock type to plots
input_rock_log <- read.csv("../Documents/QGIS_project_EPR_inactive_AT50-33/sampling_events_from_rock_log_AT50-33_20250709.csv")

#Join in-lab with at-sea counts
# Join InLab EB and WH on Morphotype column
input_joined_EB <- dplyr::full_join(input_AtSea, input_InLab_EB, c("Morphotype" = "Morphotype_AT50-33_At-sea_20250626"))
# note that EB and WH sheets also include category_in_Ayinde_Best_template 
# note that EB and WH sheets also include high_taxon_rank 
input_joined_EB_WH <- dplyr::full_join(input_joined_EB, input_InLab_WH, c("Morphotype" = "Morphotype_AT50-33_At-sea_20250626", "category_in_Ayinde_Best_template" = "category_in_Ayinde_Best_template", "high_taxon_rank" = "high_taxon_rank"))
# Join InLab AB on category AB (= Morphotype DSR) column
input_joined_EB_WH_AB <- dplyr::full_join(input_joined_EB_WH, InLab_AB_numeric, c("category_in_Ayinde_Best_template" = "Morphotype DSR"))
# add here: write.csv
# consider adding top-most rows for Feature and Rock Type

#Select a specific rock to check counts
rock_prefix <- "AL5288-R01"
subset <- dplyr::select(input_joined_EB_WH_AB, high_taxon_rank, 'revised template', Morphotype, starts_with(rock_prefix))
# write.csv(subset, file = paste(rock_prefix,".csv",sep=""), row.names = FALSE)
# group by column high_taxon_rank
subset_grouped_taxon <- subset |>
  group_by(high_taxon_rank) |>
  summarise_at(vars(starts_with(rock_prefix)), sum, na.rm = TRUE) # AB will have NAs
# need to specify the order for rows here or when plotting

# prototype plot for taxon count totals 1 rock
subset_grouped_taxon_total <- mutate(subset_grouped_taxon, total = rowSums(across(where(is.numeric))))
subset_grouped_taxon_total$'Sample.I.D.' <- rock_prefix # to be able to join with rock log
joined_one_rock <- left_join(subset_grouped_taxon_total, input_rock_log, by = 'Sample.I.D.')
joined_one_rock |>
  ggplot(aes(x = high_taxon_rank, y = total, fill = Feature)) +
  geom_col(position = "dodge") +
  labs(
    title = "Taxon Count Totals per Rock by Feature",
    x = "Taxon", y = "Total Count", fill = "Feature"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

  