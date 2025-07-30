# script to integrate in-lab with at-sea counts
# plus plot grouped taxon counts and relative abundance
# plus NMDS
# cruises AT50-20 and AT50-33
# Stace Beaulieu, Emmanuelle Bogomolni
# 2025-07-30

#Load required packages

library(readxl)
library(dplyr)
library(ggplot2)
library(data.table) # for transpose function
library(vegan)
library(scales) # for NMDS ggplot

#Set working directory

setwd("/Users/sbeaulieu/Downloads")

#Read in data sheets
# download from project Google Drive
# by default readxl reads in the first sheet/tab of the EXCEL file
# presently requires 7 input files but when these are finalized we can save an 'All_Combined' intermediate file

# authoritative at-sea sheet AT50-33
input_AtSea <-readxl::read_xlsx("macrofauna_AT50-33_At-sea_per_eventID_20250626.xlsx", skip=1)
# date downloaded in-lab WORKING COPY
input_InLab_EB <-readxl::read_xlsx("macrofauna_AT50-33_in-lab_Bogomolni_20250730.xlsx", skip=3)
input_InLab_WH <-readxl::read_xlsx("macrofauna_AT50-33_in-lab_Hamlin_20250724.xlsx", skip=3)
# also read in AB's in-lab AT50-33
# note that AB columns are type chr due to Present and Absent
input_InLab_AB <-readxl::read_xlsx("AT50-33_Macrofauna_Counts_Best_20250724.xlsx", skip=9)
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

#Loading in the AT50-20 datasheets
Snails_Harris<-readxl::read_xlsx("AT50-20_Macrofauna_Counts_Harris_HARMONIZED_20250724.xlsx", skip = 9)
Snails_Best<-readxl::read_xlsx("AT50-20_Macrofauna_Counts_Best_20250724.xlsx", skip = 9)

# also read in sampling events from rock log AT50-33
# 2025-07-25 manually added 2 rock samples from AT50-20
# spatiotemporal metadata from AT5020RockArchiveMetadata4IGSNFinal_20250115.xlsx
# rock type color from Mullineaux et al. 2025 DOI:10.1016/j.dsr.2025.104475
# to be able to add feature and rock type to plots
input_rock_log <- read.csv("sampling_events_from_rock_log_AT50-20_AT50-33_20250725.csv")


#Join data sheets

#AT50-33 Join in-lab with at-sea counts
# Join InLab EB and WH on Morphotype column
input_joined_EB <- dplyr::full_join(input_AtSea, input_InLab_EB, c("Morphotype" = "Morphotype_AT50-33_At-sea_20250626"))
# note that EB and WH sheets also include category_in_Ayinde_Best_template 
# note that EB and WH sheets also include high_taxon_rank 
input_joined_EB_WH <- dplyr::full_join(input_joined_EB, input_InLab_WH, c("Morphotype" = "Morphotype_AT50-33_At-sea_20250626", "category_in_Ayinde_Best_template" = "category_in_Ayinde_Best_template", "high_taxon_rank" = "high_taxon_rank"))
# Join InLab AB on category AB (= Morphotype DSR) column
input_joined_EB_WH_AB <- dplyr::full_join(input_joined_EB_WH, InLab_AB_numeric, c("category_in_Ayinde_Best_template" = "Morphotype DSR"))

#AT50-20 Join counts
#Combining Snails_Best and Snails_Harris using the join function
#these include at-sea and in-lab
Snails_AT5020<-full_join(Snails_Best, Snails_Harris, by="...2")
# note that MH and AB columns are type chr due to Present and Absent
Snails_AT5020_numeric <- Snails_AT5020 %>%
  mutate(
    across(starts_with('AL'),
           function(x) case_when(
             x == "Present" ~ 1,
             x == "Absent" ~ 0,
             TRUE ~ as.numeric(x)
           )
    )
  )

# join AT50-20 with AT50-33 counts
All_Combined_wide<-full_join(input_joined_EB_WH_AB, Snails_AT5020_numeric, by = c("category_in_Ayinde_Best_template" ="...2" ))
# note this has a bunch of extra columns
# write.csv(All_Combined_wide, file = "All_Combined_wide_20250730.csv", row.names = FALSE)


# #Select a specific rock to check counts
# rock_prefix <- "AL5292-R02"
# subset <- dplyr::select(input_joined_EB_WH_AB, high_taxon_rank, 'revised template', Morphotype, starts_with(rock_prefix))
# # write.csv(subset, file = paste(rock_prefix,".csv",sep=""), row.names = FALSE)
# # group by column high_taxon_rank
# subset_grouped_taxon <- subset |>
#   group_by(high_taxon_rank) |>
#   summarise_at(vars(starts_with(rock_prefix)), sum, na.rm = TRUE) # AB will have NAs
# # need to specify the order for rows here or when plotting
# 
# # prototype plot for taxon count totals 1 rock
# subset_grouped_taxon_total <- mutate(subset_grouped_taxon, total = rowSums(across(where(is.numeric))))
# subset_grouped_taxon_total$'Sample.I.D.' <- rock_prefix # to be able to join with rock log
# joined_one_rock <- left_join(subset_grouped_taxon_total, input_rock_log, by = 'Sample.I.D.')
# joined_one_rock |>
#   ggplot(aes(x = high_taxon_rank, y = total, fill = Feature)) +
#   geom_col(position = "dodge") +
#   labs(
#     title = "Taxon Count Totals per Rock by Feature",
#     x = "Taxon", y = "Total Count", fill = "Feature"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "top"
#   )

# # repeating to see if we can plot another rock with same structure to data frame
# #Select a specific rock to check counts
# rock_prefix2 <- "AL5294-R02"
# subset2 <- dplyr::select(input_joined_EB_WH_AB, high_taxon_rank, 'revised template', Morphotype, starts_with(rock_prefix2))
# # write.csv(subset, file = paste(rock_prefix,".csv",sep=""), row.names = FALSE)
# # group by column high_taxon_rank
# subset2_grouped_taxon <- subset2 |>
#   group_by(high_taxon_rank) |>
#   summarise_at(vars(starts_with(rock_prefix2)), sum, na.rm = TRUE) # AB will have NAs
# subset2_grouped_taxon_total <- mutate(subset2_grouped_taxon, total = rowSums(across(where(is.numeric))))
# subset2_grouped_taxon_total$'Sample.I.D.' <- rock_prefix2 # to be able to join with rock log
# joined2_one_rock <- left_join(subset2_grouped_taxon_total, input_rock_log, by = 'Sample.I.D.')
# 
# # prototype plot for taxon count totals 2 rocks
# # can't row bind until remove the columns specific to rock_prefix
# joined_one_rock_to_bind <- select(joined_one_rock, high_taxon_rank, total, 'Sample.I.D.', Feature, 'Rock.Type')
# joined2_one_rock_to_bind <- select(joined2_one_rock, high_taxon_rank, total, 'Sample.I.D.', Feature, 'Rock.Type')
# two_rocks <- rbind(joined_one_rock_to_bind, joined2_one_rock_to_bind)
# two_rocks |>
#   ggplot(aes(x = high_taxon_rank, y = total, fill = Feature)) +
#   geom_col(position = "dodge") +
#   labs(
#     title = "Taxon Count Totals per Rock by Feature",
#     x = "Taxon", y = "Total Count", fill = "Feature"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "top"
#   )

# next block of code for grouped taxon plot
# initialize data frame to row bind multiple rocks grouped taxon total
multiple_rocks_grouped_taxon_total <- data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("high_taxon_rank", "total", "Sample.I.D.", "Feature", "Rock.Type"))))

my_list <- list("AL5224-LM-R01", "AL5288-R01", "AL5288-R02", "AL5292-R02", "AL5292-R03", "AL5294-R02", "AL5294-R04")

for (rock_prefix in my_list) {
  one_rock <- dplyr::select(All_Combined_wide, high_taxon_rank, 'revised template', Morphotype, starts_with(rock_prefix))
  # group by column high_taxon_rank then sum for total per rock
  one_rock_grouped_taxon <- one_rock |>
    group_by(high_taxon_rank) |>
    summarise_at(vars(starts_with(rock_prefix)), sum, na.rm = TRUE) # AB will have NAs
  one_rock_grouped_taxon_total <- mutate(one_rock_grouped_taxon, total = rowSums(across(where(is.numeric))))
  one_rock_grouped_taxon_total$'Sample.I.D.' <- rock_prefix # to be able to join with rock log
  joined_one_rock <- left_join(one_rock_grouped_taxon_total, input_rock_log, by = 'Sample.I.D.')
  joined_one_rock_to_bind <- select(joined_one_rock, high_taxon_rank, total, 'Sample.I.D.', Feature, 'Rock.Type')
  multiple_rocks_grouped_taxon_total <- rbind(multiple_rocks_grouped_taxon_total, joined_one_rock_to_bind)
}

# plot multiple rocks grouped taxon total counts
# not colored yet by Rock.Type
multiple_rocks_grouped_taxon_total |>
  ggplot(aes(x = high_taxon_rank, y = total, fill = Sample.I.D.)) +
  geom_col(position = "dodge") +
  labs(
    title = "Taxon Count Totals per Rock",
    x = "Taxon", y = "Total Count", fill = "Sample.I.D."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# plot multiple rocks grouped taxon total relative abundance
# need to confirm this is working for multiple rocks from same Feature
#Graphing by taxa group and coloring by feature
multiple_rocks_grouped_taxon_total |>
  group_by(Feature) |>
  mutate(relative_abundance = total / sum(total)) |>
  ungroup() |>
  ggplot(aes(x = reorder(high_taxon_rank, -relative_abundance), y = relative_abundance, fill = Feature)) +
  geom_col(position = "dodge") +
  labs(
    title = "Relative Abundance by Feature",
    x = "Taxon", y = "Relative Abundance", fill = "Feature"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


# next block of code ultimately for NMDS 
# select multiple rocks ultimately for NMDS from both cruises
rocks_for_nmds <- c("AL5222-SS-R03", "AL5224-LM-R01", "AL5292-R03", "AL5294-R02")
multiple_rocks_per_event <- dplyr::select(All_Combined_wide, high_taxon_rank, 'revised template', Morphotype, category_in_Ayinde_Best_template, starts_with(rocks_for_nmds))

# initialize data frame to column bind multiple rocks total
#multiple_rocks_total <- data.frame(matrix(ncol=0,nrow=56))
multiple_rocks_total <- select(All_Combined_wide, high_taxon_rank, 'revised template', Morphotype, category_in_Ayinde_Best_template)

for (rock_prefix in rocks_for_nmds) {
  one_rock_total <- multiple_rocks_per_event |>
    select(starts_with(rock_prefix)) |>
    mutate(total = rowSums(across(where(is.numeric)))) |>
    rename(!!rock_prefix := total) |>
    select(rock_prefix)
  multiple_rocks_total <- cbind(multiple_rocks_total, one_rock_total)
}
# write.csv(multiple_rocks_total, file = "multiple_rocks_total_wide.csv", row.names = FALSE)

# from multiple_rocks_total remove rows not fully assessed across all samples, remove extra columns, then transpose for vegan
multiple_rocks_for_nmds <- multiple_rocks_total |>
  filter(category_in_Ayinde_Best_template != "NA") |>
  filter(Morphotype != "copepod unk") |>
  filter(Morphotype != "cauliflower protist")
# check how many counts lost
# counts_lost <- multiple_rocks_total |>
#   filter(category_in_Ayinde_Best_template == "NA")
multiple_rocks_for_nmds <- multiple_rocks_for_nmds |>
  select(-high_taxon_rank, -`revised template`, -category_in_Ayinde_Best_template)
t_multiple_rocks_for_nmds <- transpose(multiple_rocks_for_nmds)
rownames(t_multiple_rocks_for_nmds) <- colnames(multiple_rocks_for_nmds)
colnames(t_multiple_rocks_for_nmds) <- t_multiple_rocks_for_nmds[1,]
t_multiple_rocks_for_nmds <- t_multiple_rocks_for_nmds[-1, ]

# data for vegan NMDS -----------------
data_for_vegan <- as.data.frame(sapply(t_multiple_rocks_for_nmds, as.numeric))
rownames(data_for_vegan) <- rownames(t_multiple_rocks_for_nmds)
# if any NA values confirm fill with zero
data_for_vegan[is.na(data_for_vegan)] <- 0

#not sure if this is set up correctly yet for NMDS using relative abundance
set.seed(50) #arbitrary value for random number generator
data_nmds <- metaMDS(data_for_vegan, distance = "bray", autotransform = FALSE, k=2, trymax=100)
plot(data_nmds, type="t")

# plot with ggplot
# Set figure base size
figures_base_size <- 7

# make a dataframe with the nmds points and rock identifier
nmds_plot_data <- data.frame(
  NMDS1 = data_nmds$points[, 1],
  NMDS2 = data_nmds$points[, 2],
  rock_id = rownames(data_nmds$points))

#join input rock log with nmds data
nmds_plot_data <- nmds_plot_data |>
  left_join(input_rock_log, by = c("rock_id" = "Sample.I.D."))

# Plot
ggplot(nmds_plot_data, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(shape = Feature, color = Rock.Type), size = 4, stroke=1) +
  coord_fixed()+
#  scale_shape_manual(values = c(16, 17, 5, 8, 6, 3, 15)) +
  scale_shape_manual(values = c(16, 17, 5, 8)) +
  scale_color_manual(values = c(
    "yellow" = "#F39C12",
    "rusty" = "#A93226",
    "green" = "green4"
  )) +
  theme_bw(base_size = figures_base_size) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_rect(fill=NA, colour="black", size = 1)) +
  theme(axis.text = element_text(colour="black", size = 12),
        axis.title = element_text(colour="black", size = 14))+
  guides(color = guide_legend(override.aes = list(shape = 15, size = 4))) +
  scale_x_continuous(labels = label_number(accuracy = 0.1), limits = c(-2.4, 2.4)) +  # 1 decimal place
  scale_y_continuous(labels = label_number(accuracy = 0.1), limits = c(-2.4, 2.4))+
  labs(
    color = "Rock Type",
    shape = "Feature"
  )

