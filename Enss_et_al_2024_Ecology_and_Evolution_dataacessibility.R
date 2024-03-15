##### 01. SETUP #########
#package  installation
pckg <- installed.packages() # create list of all installed packages
req.pkg <- c("readxl","MASS","DescTools", "dplyr","broom", "RColorBrewer", "openxlsx","writexl", "tidyr", "ggplot2","corrplot","cowplot", "vegan", "codyn", "grid", "gridExtra", "stringr") # list of required packages #readxl: read_excel, dplyr: left_join, openxlsx: write.xlsx, tidyr: pivot_longer
inst.pkg <- req.pkg[!(req.pkg %in% pckg[,1])] # which packages need to be installed -> required packages that are not in installed packages
install.packages(inst.pkg) # install missing packages
lapply(req.pkg, library, character.only=T) # apply library to all required packages
rm(inst.pkg,req.pkg, pckg) # remove objects as they are not needed anymore



##### 02. DATA IMPORT, CLEANUP AND PREPARATION #########
# set working directory
setwd("C:/Users/Julian/sciebo/Universität Ordner HP/1.Promotion/1. Paper/R statistics_data_acessibility")


# read in raw data
all_data <- read_excel("Enss_et_al_2024_Ecology_and_Evolution_alldata.xlsx")

#exchange RBA3 P0 with RB22 P0 for Baetis sp., Gammarus pulex, Ephemera danica as the taxa were not found at RBA3 P0
A3_P0 <- read_excel("P0 for A3 from RB22.xlsx")

# Connect both dataframes 
all_data <- bind_rows(all_data, A3_P0)

# Read in the feedingtypes.xlsx file
feedingtypes <- read_excel("C:/Users/Julian/sciebo/Universität Ordner HP/1.Promotion/Statistik/R statistic methodology paper first try out/2. Correlation Foodsource Feedingtype/feedingtypes.xlsx")

# Merge the two data frames by the taxon column
all_data <- merge(all_data, feedingtypes, by.x = "taxon", by.y = "Taxon", all.x = TRUE)

# Rename the Feedingtype column to feeding_type
colnames(all_data)[colnames(all_data) == "Feedingtype"] <- "feeding_type"

#Delete all rows where feedingtype is NA (Few single taxa like Limnephilidae gen sp.)
all_data <- all_data[!is.na(all_data$feeding_type), ]

#Group the data by site, distance, and feeding_type
grouped_data <- all_data %>%
  group_by(site, distance, feeding_type)

# Calculate the mean and standard deviation of mean_15N for each group
summary_data <- grouped_data %>%
  summarise(mean_15N_mean = mean(mean_15N, na.rm = TRUE),
            mean_15N_sd = sd(mean_15N, na.rm = TRUE))

# Find the matching values for site = "RB22" and distance = "0"
replacement_values <- summary_data %>%
  filter(site == "RB22" & distance == "0") %>%
  select(mean_15N_mean, mean_15N_sd)

# Replace the values for site = "RBA3" and distance = "0" with the matching values
summary_data <- summary_data %>%
  mutate(
    mean_15N_mean = ifelse(site == "RBA3" & distance == "0", replacement_values$mean_15N_mean, mean_15N_mean),
    mean_15N_sd = ifelse(site == "RBA3" & distance == "0", replacement_values$mean_15N_sd, mean_15N_sd)
  )

# Set NA values in summary_data$mean_15N_sd to 0
summary_data$mean_15N_sd[is.na(summary_data$mean_15N_sd)] <- 0

# Specify the file path where you want to save the Excel file
file_path <- ("C:/Users/Julian/sciebo/Universität Ordner HP/1.Promotion/1. Paper/R statistics_data_acessibility/summary_data.xlsx")

# Save the data frame as an Excel file
write_xlsx(summary_data, file_path)




#### 03. TABLE 5 #####
#Pairwise Spearman correlations (rho) of ?? 15N of phytobenthos (PB) and particulate organic matter (POM) with 
#macroinvertebrate specimens of different feeding types. Correlations are based on mean enrichment levels 
#across all sampling reaches and grouped by distance downstream of the isotope inlet
#NOTE: The output of this section (raw_data_table_5) was afterwards manually formatted in Excel to achieve the final look.

# Create a new dataframe with selected columns from summary_data
correlations <- summary_data %>%
  select(site, distance, feeding_type, mean_15N_mean)

# Print the first few rows of the correlations dataframe
head(correlations)

correlations_BY22 <- correlations %>%
  filter(site == "BY22")

correlations_RB21 <- correlations %>%
  filter(site == "RB21")

correlations_RB22 <- correlations %>%
  filter(site == "RB22")

correlations_SB21 <- correlations %>%
  filter(site == "SB21")

correlations_RBA3 <- correlations %>%
  filter(site == "RBA3")



########## Site: BY22

correlations_BY22 <- correlations %>%
  filter(site == "BY22")

# Initialize an empty data frame to store correlation results
correlation_results_BY22 <- data.frame(
  feeding_type1 = character(),
  feeding_type2 = character(),
  correlation = numeric(),
  p_value = numeric(),
  num_values_compared = integer(),  # New column
  stringsAsFactors = FALSE
)

# Loop through each feeding type combination
for (type1 in c("PB", "POM")) {
  for (type2 in feeding_types_to_compare) {
    
    # Filter data for the two feeding types
    data_type1 <- correlations_BY22 %>%
      filter(feeding_type == type1)
    
    data_type2 <- correlations_BY22 %>%
      filter(feeding_type == type2)
    
    # Find common distances
    common_distances <- intersect(data_type1$distance, data_type2$distance)
    
    if (length(common_distances) > 1) {  # Ensure there are at least two common distances
      # Filter data for common distances
      data_type1 <- data_type1 %>%
        filter(distance %in% common_distances)
      
      data_type2 <- data_type2 %>%
        filter(distance %in% common_distances)
      
      # Calculate Spearman correlation
      correlation_result_BY22 <- cor.test(data_type1$mean_15N_mean, data_type2$mean_15N_mean, method = "spearman")
      
      # Store the results in the data frame
      correlation_results_BY22 <- rbind(correlation_results_BY22, data.frame(
        feeding_type1 = type1,
        feeding_type2 = type2,
        correlation = correlation_result_BY22$estimate,
        p_value = correlation_result_BY22$p.value,
        num_values_compared = length(common_distances)  # Store the number of values compared
      ))
    }
  }
}

# Print the correlation results
print(correlation_results_BY22)


########## Site: RB22

correlations_RB22 <- correlations %>%
  filter(site == "RB22")

# Initialize an empty data frame to store correlation results
correlation_results_RB22 <- data.frame(
  feeding_type1 = character(),
  feeding_type2 = character(),
  correlation = numeric(),
  p_value = numeric(),
  num_values_compared = integer(),  # New column
  stringsAsFactors = FALSE
)

# Loop through each feeding type combination
for (type1 in c("PB", "POM")) {
  for (type2 in feeding_types_to_compare) {
    
    # Filter data for the two feeding types
    data_type1 <- correlations_RB22 %>%
      filter(feeding_type == type1)
    
    data_type2 <- correlations_RB22 %>%
      filter(feeding_type == type2)
    
    # Find common distances
    common_distances <- intersect(data_type1$distance, data_type2$distance)
    
    if (length(common_distances) > 1) {  # Ensure there are at least two common distances
      # Filter data for common distances
      data_type1 <- data_type1 %>%
        filter(distance %in% common_distances)
      
      data_type2 <- data_type2 %>%
        filter(distance %in% common_distances)
      
      # Calculate Spearman correlation
      correlation_result_RB22 <- cor.test(data_type1$mean_15N_mean, data_type2$mean_15N_mean, method = "spearman")
      
      # Store the results in the data frame
      correlation_results_RB22 <- rbind(correlation_results_RB22, data.frame(
        feeding_type1 = type1,
        feeding_type2 = type2,
        correlation = correlation_result_RB22$estimate,
        p_value = correlation_result_RB22$p.value,
        num_values_compared = length(common_distances)  # Store the number of values compared
      ))
    }
  }
}

# Print the correlation results
print(correlation_results_RB22)



########## Site: RB21

correlations_RB21 <- correlations %>%
  filter(site == "RB21")

# Initialize an empty data frame to store correlation results
correlation_results_RB21 <- data.frame(
  feeding_type1 = character(),
  feeding_type2 = character(),
  correlation = numeric(),
  p_value = numeric(),
  num_values_compared = integer(),  # New column
  stringsAsFactors = FALSE
)

# Loop through each feeding type combination
for (type1 in c("PB", "POM")) {
  for (type2 in feeding_types_to_compare) {
    
    # Filter data for the two feeding types
    data_type1 <- correlations_RB21 %>%
      filter(feeding_type == type1)
    
    data_type2 <- correlations_RB21 %>%
      filter(feeding_type == type2)
    
    # Find common distances
    common_distances <- intersect(data_type1$distance, data_type2$distance)
    
    if (length(common_distances) > 1) {  # Ensure there are at least two common distances
      # Filter data for common distances
      data_type1 <- data_type1 %>%
        filter(distance %in% common_distances)
      
      data_type2 <- data_type2 %>%
        filter(distance %in% common_distances)
      
      # Calculate Spearman correlation
      correlation_result_RB21 <- cor.test(data_type1$mean_15N_mean, data_type2$mean_15N_mean, method = "spearman")
      
      # Store the results in the data frame
      correlation_results_RB21 <- rbind(correlation_results_RB21, data.frame(
        feeding_type1 = type1,
        feeding_type2 = type2,
        correlation = correlation_result_RB21$estimate,
        p_value = correlation_result_RB21$p.value,
        num_values_compared = length(common_distances)  # Store the number of values compared
      ))
    }
  }
}

# Print the correlation results
print(correlation_results_RB21)





########## Site: SB21

correlations_SB21 <- correlations %>%
  filter(site == "SB21")

# Initialize an empty data frame to store correlation results
correlation_results_SB21 <- data.frame(
  feeding_type1 = character(),
  feeding_type2 = character(),
  correlation = numeric(),
  p_value = numeric(),
  num_values_compared = integer(),  # New column
  stringsAsFactors = FALSE
)

# Loop through each feeding type combination
for (type1 in c("PB", "POM")) {
  for (type2 in feeding_types_to_compare) {
    
    # Filter data for the two feeding types
    data_type1 <- correlations_SB21 %>%
      filter(feeding_type == type1)
    
    data_type2 <- correlations_SB21 %>%
      filter(feeding_type == type2)
    
    # Find common distances
    common_distances <- intersect(data_type1$distance, data_type2$distance)
    
    if (length(common_distances) > 1) {  # Ensure there are at least two common distances
      # Filter data for common distances
      data_type1 <- data_type1 %>%
        filter(distance %in% common_distances)
      
      data_type2 <- data_type2 %>%
        filter(distance %in% common_distances)
      
      # Calculate Spearman correlation
      correlation_result_SB21 <- cor.test(data_type1$mean_15N_mean, data_type2$mean_15N_mean, method = "spearman")
      
      # Store the results in the data frame
      correlation_results_SB21 <- rbind(correlation_results_SB21, data.frame(
        feeding_type1 = type1,
        feeding_type2 = type2,
        correlation = correlation_result_SB21$estimate,
        p_value = correlation_result_SB21$p.value,
        num_values_compared = length(common_distances)  # Store the number of values compared
      ))
    }
  }
}

# Print the correlation results
print(correlation_results_SB21)


########## Site: RBA3

correlations_RBA3 <- correlations %>%
  filter(site == "RBA3")

# Initialize an empty data frame to store correlation results
correlation_results_RBA3 <- data.frame(
  feeding_type1 = character(),
  feeding_type2 = character(),
  correlation = numeric(),
  p_value = numeric(),
  num_values_compared = integer(),  # New column
  stringsAsFactors = FALSE
)

# Loop through each feeding type combination
for (type1 in c("PB", "POM")) {
  for (type2 in feeding_types_to_compare) {
    
    # Filter data for the two feeding types
    data_type1 <- correlations_RBA3 %>%
      filter(feeding_type == type1)
    
    data_type2 <- correlations_RBA3 %>%
      filter(feeding_type == type2)
    
    # Find common distances
    common_distances <- intersect(data_type1$distance, data_type2$distance)
    
    if (length(common_distances) > 1) {  # Ensure there are at least two common distances
      # Filter data for common distances
      data_type1 <- data_type1 %>%
        filter(distance %in% common_distances)
      
      data_type2 <- data_type2 %>%
        filter(distance %in% common_distances)
      
      # Calculate Spearman correlation
      correlation_result_RBA3 <- cor.test(data_type1$mean_15N_mean, data_type2$mean_15N_mean, method = "spearman")
      
      # Store the results in the data frame
      correlation_results_RBA3 <- rbind(correlation_results_RBA3, data.frame(
        feeding_type1 = type1,
        feeding_type2 = type2,
        correlation = correlation_result_RBA3$estimate,
        p_value = correlation_result_RBA3$p.value,
        num_values_compared = length(common_distances)  # Store the number of values compared
      ))
    }
  }
}

# Print the correlation results
print(correlation_results_RBA3)



# Add a "site" column to correlation_results_RBA3
correlation_results_RBA3 <- correlation_results_RBA3 %>%
  mutate(site = "RBA3")

# Add a "site" column to correlation_results_RB21
correlation_results_RB21 <- correlation_results_RB21 %>%
  mutate(site = "RB21")

# Add a "site" column to correlation_results_RB22
correlation_results_RB22 <- correlation_results_RB22 %>%
  mutate(site = "RB22")

# Add a "site" column to correlation_results_SB21
correlation_results_SB21 <- correlation_results_SB21 %>%
  mutate(site = "SB21")

# Add a "site" column to correlation_results_BY22
correlation_results_BY22 <- correlation_results_BY22 %>%
  mutate(site = "BY22")

# Combine all the correlation results dataframes into one
raw_data_table_5 <- bind_rows(
  correlation_results_RBA3,
  correlation_results_RB21,
  correlation_results_RB22,
  correlation_results_SB21,
  correlation_results_BY22
)

# Print the first few rows of the combined dataframe
head(raw_data_table_5)

# Specify the file path 
file_path <- ("C:/Users/Julian/sciebo/Universität Ordner HP/1.Promotion/1. Paper/R statistics_data_acessibility/summary_data.xlsx")

# Save the data frame as an Excel file
write_xlsx(raw_data_table_5, file_path)

#NOTE: The output of this section (raw_data_table_5) was afterwards manually formatted in Excel to achieve the final look.


#### 04. FIGURE 4 ####
#Mean ??15N contents of investigated macroinvertebrate specimens (per feeding type) and of food sources 
#along the five sampling reaches. Reference marks the non-enriched site 50 m upstream of the 
#isotope inlet. (For better clarity, SDs around mean values are not shown)
#Note: Axis title and legend where manually adjusted in Powerpoint after export from R

# Filter the data for the specified sites
selected_sites <- c("BY22", "RBA3", "RB22", "RB21", "SB21")
filtered_data <- all_data[all_data$site %in% selected_sites, ]



# Group and summarize the data to calculate the mean for each distance and feeding_type
aggregated_data <- filtered_data %>%
  group_by(site, distance, feeding_type) %>%
  summarize(mean_15N = mean(mean_15N, na.rm = TRUE))

custom_shapes <- c("active filter feeder" = 0, "grazer" = 1, "passive filter feeder" = 2, "PB" = 3, "POM" = 4, "predator" = 5, "shredder" = 6)
custom_colors <- c("active filter feeder" = "#E41A1C", "grazer" = "#377EB8", "passive filter feeder" = "cyan", "PB" = "purple3", "POM" = "#FF7F00", "predator" = "green2", "shredder" = "#A65628")
# Create a custom shape-color mapping data frame
shape_color_mapping <- data.frame(
  shape = custom_shapes,
  color = custom_colors)

plot <- ggplot(aggregated_data, aes(x = as.factor(distance), y = mean_15N)) +
  geom_point(aes(shape = feeding_type, color = feeding_type), size = 3) +
  geom_line(aes(group = feeding_type, color = feeding_type), size = 1, lineend = "round", alpha = 0.7) +
  labs(
    x = "Distance downstream to enrichment inlet [m]",
    y = expression(paste("Mean ", delta^15, "N [???]"))
  ) +
  theme_minimal() +
  facet_wrap(~site, ncol = 2, nrow = 3, scales = "free_y") +
  scale_x_discrete(labels = custom_labels) +
  scale_shape_manual(values = custom_shapes, name = "Feeding Type", labels = names(custom_shapes)) +
  scale_color_manual(values = custom_colors, name = "Feeding Type", labels = names(custom_colors)) +
  theme(
    axis.line = element_line(color = "black", size = 0.2),
    legend.position = "bottom"
  )

# Display the plot
print(plot)


##### 05. TABLE 4 #####

# Differences in the mean ?? 15N of macroinvertebrate feeding types and 
# investigated food sources between enriched and non-enriched reference sites. Significance was tested using individual t-Tests. 

# Create a new dataframe fd_enrichment_mean_plot
fd_enrichment_mean_plot <- data.frame(
  taxon = all_data$taxon,
  site = all_data$site,
  mean_15N = all_data$mean_15N,
  feeding_type = all_data$feeding_type,
  distance= all_data$distance)

# Filter the data to include only rows where distance is not equal to 0
filtered_data <- fd_enrichment_mean_plot %>%
  filter(distance != 0)

# Filter the data to include only rows where distance IS  0
filtered_data_P0 <- fd_enrichment_mean_plot %>%
  filter(distance == 0)
# List of feeding types that exist in both filtered_data and filtered_data_P0
common_feeding_types <- intersect(unique(filtered_data$feeding_type), unique(filtered_data_P0$feeding_type))

# Create a list to store the t-test results
t_test_results_list <- list()

# Perform t-test and store results for each common feeding type
for (feeding_type in common_feeding_types) {
  # Filter data for the current feeding type in both datasets
  data_filtered <- filtered_data %>%
    filter(feeding_type == .data$feeding_type)
  
  data_P0_filtered <- filtered_data_P0 %>%
    filter(feeding_type == .data$feeding_type)
  
  # Perform t-test for the current feeding type
  t_test_result <- t.test(data_filtered$mean_15N, data_P0_filtered$mean_15N)
  
  # Store t-test result in the list
  t_test_results_list[[feeding_type]] <- t_test_result
}

# Print t-test results for each common feeding type
for (i in seq_along(t_test_results_list)) {
  feeding_type <- common_feeding_types[i]
  cat("T-Test results for", feeding_type, ":\n")
  print(t_test_results_list[[i]])
  cat("\n")
}

# TTEST results for each feeding group PO (Control) vs downstream to enrichment inlet
combined_data_TTEST <- rbind(data_P0_filtered, data_filtered)
# Create a grouping variable to distinguish between the Control and downstream sites
combined_data_TTEST$group <- ifelse(combined_data_TTEST$distance == 0, "Control", "Downstream")


# Check the structure of your data
str(combined_data_TTEST)

# Perform t-tests for each feeding group
t_test_results <- by(combined_data_TTEST, combined_data_TTEST$feeding_type, function(subset) {
  t_test_result <- t.test(mean_15N ~ group, data = subset)
  return(data.frame(feeding_type = unique(subset$feeding_type), 
                    t_statistic = t_test_result$statistic,
                    df = t_test_result$parameter,
                    p_value = t_test_result$p.value))
})

# Combine the results into a data frame
t_test_results_df <- do.call(rbind, t_test_results)

# Print the results
print(t_test_results_df)


##### 06. TABLE 6 #####

# Differences in the mean ??15N of macroinvertebrates and investigated food sources at the sites 
# 50 m - 300 m and 500 m - 2000 m downstream to the enrichment inlet. 
# Significance was tested using individual t-tests. 

# Assuming your dataframe is named combined_data_TTEST
combined_data_TTEST <- combined_data_TTEST %>%
  mutate(peak_test = case_when(
    distance %in% c(50, 100, 200, 300) ~ "G1",
    distance %in% c(500, 750, 1000, 1500, 2000) ~ "G2",
    TRUE ~ NA_character_  # If distance doesn't match any condition, you can set it to NA or any other value
  ))
# Keep only rows where peak_test is "G1" or "G2"
combined_data_TTEST <- combined_data_TTEST[combined_data_TTEST$peak_test %in% c("G1", "G2"), ]


# Perform t-tests for each feeding group
t_test_results <- by(combined_data_TTEST, combined_data_TTEST$site, function(subset) {
  t_test_result <- t.test(mean_15N ~ peak_test, data = subset)
  return(data.frame(site = unique(subset$site), 
                    t_statistic = t_test_result$statistic,
                    df = t_test_result$parameter,
                    p_value = t_test_result$p.value))
})

# Combine the results into a data frame
t_test_results_df <- do.call(rbind, t_test_results)

# Print the results
print(t_test_results_df)  




#### 07. TABLE 2   ####
#Enrichment (??15N) of investigated taxa and two food sources at nine distances downstream of the isotope
#inlet. Feeding types reflect the main assignment acc. to freshwaterecology.info (Schmidt-Kloiber & Hering, 2015). 
#Reference measurements were derived from samples taken 50 m upstream of the isotope inlet. N = number of specimens 
#analysed for each taxon. Bold values indicate enrichment levels above Reference + 2 SD (Macneale et al., 2004). 
#Empty lines indicate that a species has not been detected at this distance
#NOTE: The output of this section ("table_2.xlsx") was afterwards manually formatted in Excel to 
#achieve the final look of Table 2.

# Get unique values from raw_data$taxon
unique_taxon <- unique(all_data$taxon)

# Create an empty new_table with one row for each unique value
new_table <- data.frame(
  taxon = unique_taxon,
  feeding_type = character(length(unique_taxon)),
  n = integer(length(unique_taxon)),
  `0` = numeric(length(unique_taxon)),
  `50` = numeric(length(unique_taxon)),
  `100` = numeric(length(unique_taxon)),
  `200` = numeric(length(unique_taxon)),
  `300` = numeric(length(unique_taxon)),
  `500` = numeric(length(unique_taxon)),
  `750` = numeric(length(unique_taxon)),
  `1000` = numeric(length(unique_taxon)),
  `1500` = numeric(length(unique_taxon)),
  `2000` = numeric(length(unique_taxon))
)

# Print the updated new_table
print(new_table)

# Merge the new_table with feedingtypes to add the feeding_type column
merged_table <- merge(new_table, feedingtypes, by.x = "taxon", by.y = "Taxon", all.x = TRUE)

# Select the 'Feedingtype' column and assign it to the 'feeding_type' column in merged_table
merged_table$feeding_type <- merged_table$Feedingtype

# Remove the 'Feedingtype' column
merged_table <- merged_table[, !(names(merged_table) == "Feedingtype")]

# Print the updated merged_table with the 'feeding_type' column added
print(merged_table)


# Select only the columns you want to keep (excluding ...3, ...4, ...5, ...6, and ...7)
merged_table <- subset(merged_table, select = -c(...3, ...4, ...5, ...6, ...7))

# Print the filtered_table
print(merged_table)

# Remove the 'X' letter from column names
colnames(merged_table) <- gsub("X", "", colnames(merged_table))

# Print the updated merged_table
print(merged_table)


# Calculate the sum of 'n' for each unique 'taxon' value in all_data
taxon_sums <- aggregate(n ~ taxon, data = all_data, FUN = sum)

# Merge the sums back into merged_table based on 'taxon' value
merged_table <- merge(merged_table, taxon_sums, by.x = "taxon", by.y = "taxon", all.x = TRUE)

# Print the updated merged_table with the 'n' column filled
print(merged_table)

# Use table to count the entries in raw_data$taxon for each merged_table$taxon
taxon_counts <- table(raw_data$taxon)

# Map the counts to merged_table$n based on 'taxon' value
merged_table$n <- taxon_counts[match(merged_table$taxon, names(taxon_counts))]

# Print the updated merged_table with the 'n' column filled
print(merged_table)

# Create the mean_sd table by grouping and summarizing raw_data
mean_sd <- raw_data %>%
  group_by(taxon, distance) %>%
  summarise(mean = mean(mean_15N), sd = sd(mean_15N))

# Print the mean_sd table
print(mean_sd)

# Loop through each 'distance' category in mean_sd$distance
for (distance in unique(mean_sd$distance)) {
  # Filter mean_sd for the specific 'distance' and 'taxon' combinations
  filtered_mean_sd <- mean_sd[mean_sd$distance == distance, ]
  
  # Loop through each 'taxon' in filtered_mean_sd
  for (taxon in unique(filtered_mean_sd$taxon)) {
    # Find the corresponding row in merged_table
    row_index <- which(merged_table$taxon == taxon)
    
    # Check if the row exists in merged_table
    if (length(row_index) > 0) {
      # Get the mean value from filtered_mean_sd
      mean_value <- filtered_mean_sd[filtered_mean_sd$taxon == taxon, "mean"]
      
      # Assign the mean value to the corresponding column in merged_table
      column_name <- as.character(distance)
      merged_table[row_index, column_name] <- mean_value
    }
  }
}

# Print the updated merged_table
print(merged_table)


#Round values on two digits
merged_table[, c('0', '50', '100', '200', '300', '500', '750', '1000', '1500', '2000')] <- 
  round(merged_table[, c('0', '50', '100', '200', '300', '500', '750', '1000', '1500', '2000')], digits = 2)

print(merged_table)



# Loop through each 'distance' category in mean_sd$distance
for (distance in unique(mean_sd$distance)) {
  # Filter mean_sd for the specific 'distance' and 'taxon' combinations
  filtered_mean_sd <- mean_sd[mean_sd$distance == distance, ]
  
  # Loop through each 'taxon' in filtered_mean_sd
  for (taxon in unique(filtered_mean_sd$taxon)) {
    # Find the corresponding row in merged_table
    row_index <- which(merged_table$taxon == taxon)
    
    # Check if the row exists in merged_table
    if (length(row_index) > 0) {
      # Get the mean and sd values from filtered_mean_sd
      mean_value <- round(filtered_mean_sd[filtered_mean_sd$taxon == taxon, "mean"], digits = 2)
      sd_value <- round(filtered_mean_sd[filtered_mean_sd$taxon == taxon, "sd"], digits = 2)
      
      # Format the value to append (sd) in parentheses after mean with a space
      formatted_value <- paste(mean_value, " (", sd_value, ")", sep = "")
      
      # Assign the formatted value to the corresponding column in merged_table
      column_name <- as.character(distance)
      merged_table[row_index, column_name] <- formatted_value
    }
  }
}

# Print the updated merged_table
print(merged_table)

# Specify the file path
file_path <- "C:/Users/Julian/sciebo/Universität Ordner HP/1.Promotion/1. Paper/R statistics_data_acessibility/table_2.xlsx" 

# Export 'merged_table' to an Excel file
write_xlsx(merged_table, path = file_path)

# Confirm that the file has been saved
cat("Excel file saved to:", file_path, "\n")


#### 08. 

#### 09. TABLE 3 #####
#Density estimates and number of enriched specimens of investigated taxa 
#within the enriched sections of the five sampling reaches
#NOTE: The output of this section ("table 3 data.xlsx") was afterwards manually formatted in Excel to achieve the final look of Table 3.

table_3 <- read_excel("density x enrichment distance.xlsx")
head (table_3)

# round all values
table_3$`1m` <- round(table_3$`1m`, 2)
table_3$`SD` <- round(table_3$`SD`, 2)
# combine columns
table_3$`1m ± SD` <- paste(table_3$`1m`, "±", table_3$`SD`)
head (table_3)

# Save your data frame as an XLSX file
write_xlsx(table_3, "table 3 data.xlsx")
