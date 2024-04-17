# Preliminary -------------------------------------------------------------
# INSTALL LIBRARIES
install.packages("vroom")
install.packages("stringr")
install.packages("tidyverse")
install.packages("stats")
install.packages("dplyr")
install.packages("tidyr")
install.packages("zoo")
install.packages("pbapply")


# IMPORT LIBRARIES
library(vroom)
library(stringr)
library(tidyverse)
library(stats)
library(dplyr)
library(tidyr)
library(zoo)

library(pbapply)
library(purrr)

# CSV Manipulation -------------------------------------------------------------

# Making list of csvs
setwd("G:/My Drive/YKD_gridded/2016") #change directory year here to run different years

filesIce <- list.files(full.names = FALSE)

# FUNCTION (merging files on ID + adding date column based on filename)
download_dataI <- function(filesIce){
  
  temp <- read_csv(paste0(filesIce)) %>% 
    as_tibble() %>% 
    # mutate("date" = str_sub(filesIce, 9, 18))
  # alternate mutate line so that date will be pulled correctly from filenames 
    mutate(date = as.Date(str_extract(filesIce, "\\d{4}_\\d{2}_\\d{2}"), format = "%Y_%m_%d"))
  
  return(temp)
  
}

# APPLY function
Icedata <- filesIce %>%
  map(download_dataI) %>%
  reduce(bind_rows)

## Tiled data aggregation -----------------------------------------------------
#add areas together for same Lake_ID/date (for lakes split by tiles)
aggregated_Icedata <- Icedata %>%
  group_by(Lake_ID, date) %>%
  summarise(
    iceArea = sum(iceArea, na.rm = TRUE),
    cloudArea = sum(cloudArea, na.rm = TRUE),
    clearArea = sum(clearArea, na.rm = TRUE),
    measuredArea = cloudArea + clearArea,
    .groups = 'drop'
  )

#add totalArea and centroid back in 
aggregated_Icedata <- aggregated_Icedata %>%
  left_join(Icedata %>% 
              select(Lake_ID, totalArea, centroid) %>%
              distinct(Lake_ID, .keep_all = TRUE), by = "Lake_ID")

#remove zeros data
aggregated_Icedata <- aggregated_Icedata %>%
  filter(iceArea != 0 | clearArea != 0 | cloudArea != 0)


### AGGREGATED DATA QUALITY CHECKS -----------------------------------------------------
area_discrepancy_check <- aggregated_Icedata %>%
  mutate(discrepancy = abs(totalArea - measuredArea)) %>%
  mutate(discrepancy_percentage = (discrepancy / totalArea) * 100) %>%
  filter(discrepancy > 0)  # Filter to show only rows where there is a discrepancy

# Inspect the results for any significant discrepancies
print(area_discrepancy_check)


# #visualize ALL
# ggplot(area_discrepancy_check, aes(x = Lake_ID, y = discrepancy_percentage)) +
#   geom_point(stat = "identity") +
#   theme_minimal() +
#   labs(title = "Discrepancy Between Total Area and Measured Area",
#        x = "Lake ID",
#        y = "% Area Discrepancy")
# 
# # Histogram of discrepancy percentages
# ggplot(area_discrepancy_check, aes(x = discrepancy_percentage)) +
#   geom_histogram(binwidth = .5) +
#   labs(title = "Distribution of Area Discrepancy Percentages",
#        x = "Discrepancy Percentage (%)",
#        y = "Frequency")




# #visualize ONLY SIGNIFICANT discrepancies
# significant_discrepancies <- area_discrepancy_check %>%
#   filter(abs(discrepancy_percentage) > 5)
# 
# print(significant_discrepancies)
# 
# # Visualizing significant discrepancies
# ggplot(significant_discrepancies, aes(x = Lake_ID, y = discrepancy_percentage)) +
#   geom_bar(stat = "identity") +
#   theme_minimal() +
#   labs(title = "Significant Discrepancies in Lake Areas",
#        x = "Lake ID",
#        y = "Discrepancy Percentage (%)") +
#   ylim(0, 100)





# #Discrepancy stats
# summary_stats_discrepancy <- area_discrepancy_check %>%
#   summarise(
#     mean_discrepancy_percentage = mean(discrepancy_percentage, na.rm = TRUE),
#     median_discrepancy_percentage = median(discrepancy_percentage, na.rm = TRUE),
#     sd_discrepancy_percentage = sd(discrepancy_percentage, na.rm = TRUE),
#     max_discrepancy_percentage = max(discrepancy_percentage, na.rm = TRUE),
#     min_discrepancy_percentage = min(discrepancy_percentage, na.rm = TRUE)
#   )
# 
# print(summary_stats_discrepancy)



### SPLIT / NON SPLIT QA -----------------------------------------------------
split_lake_ids <- c(71700, 141415, 131, 11320, 165, 145, 131572, 130653, 137357, 90005, 47936, 112621, 109893, 110107,	117420,	117423,	117432,	117434,	117575,	117618,	117639,	118105,	118109,	118110,	118123,	118150,	118184,	118223,	118224,	118287, 143629,	145, 165,	166, 543,	584, 33540,	33600,	33601,	33615,	33619,	33621,	33957,	33966,	33969,	102399,	102407,	102436,	106073,	106125, 88571,	99776,	99780,	99835,	99836,	100245,	100296,	100297,	100517,	100540,	100951, 69257,	34498,	34531,	34565,	34596,	42629,	42698,	42710,	42714, 85558,	92422,	92432,	101753,	101757,	101764,	84098,	83867,	88355,	120318,	120385, 131348,	131381,	131382,	131494,	124914, 127015,	110444)   # Fill in with IDs of split lakes

non_split_lake_ids <- c(58992, 17667, 44349, 136945, 71241, 37062, 75877, 38786, 86507, 43058, 63948, 110841, 20327, 46095, 46096, 46102, 46121, 46558, 51850, 94733, 94741, 95009, 80652, 80836, 81421, 82472, 82543, 12514, 13849, 13979, 14005, 22354, 22382, 22430, 56239, 56259, 58445, 106180, 106302, 106360, 113261, 113541, 114049, 116632, 115711, 115713, 116716, 62546, 62605, 62764, 64811, 64908, 66756, 69323, 69370, 69571, 69738, 70196, 70299, 70345, 70605, 70688, 70820, 75108, 75190, 75238, 128852, 128954, 128996, 129052, 129130, 129189, 129225, 129772, 129810, 129934, 132381, 132395, 139673, 139691, 139724, 139890, 139989, 82677, 82937, 82999, 83094, 83115, 83139, 83180, 83709)  # Fill in with IDs of non-split lakes

split_check <- area_discrepancy_check %>%
  mutate(Type = case_when(
    Lake_ID %in% split_lake_ids ~ "Split",
    Lake_ID %in% non_split_lake_ids ~ "Non-Split",
    TRUE ~ "Unknown"  # Label lakes not in either list as 'Unknown'
  ))

# Calculate stats for split lakes
split_stats <- split_check %>%
  filter(Type == "Split") %>%
  summarise(
    mean_discrepancy_percentage = mean(discrepancy_percentage, na.rm = TRUE),
    median_discrepancy_percentage = median(discrepancy_percentage, na.rm = TRUE),
    sd_discrepancy_percentage = sd(discrepancy_percentage, na.rm = TRUE)
  )

# Calculate stats for non-split lakes
non_split_stats <- split_check %>%
  filter(Type == "Non-Split") %>%
  summarise(
    mean_discrepancy_percentage = mean(discrepancy_percentage, na.rm = TRUE),
    median_discrepancy_percentage = median(discrepancy_percentage, na.rm = TRUE),
    sd_discrepancy_percentage = sd(discrepancy_percentage, na.rm = TRUE)
  )

# Combine the stats for easy comparison
combined_stats <- bind_rows(split_stats, non_split_stats)
print(combined_stats)


### CONSERVATIVE DATASET -----------------------------------------------------
# removes all split lakes as well as those >5% discrepancy, as determined above
setwd("G:/My Drive")
split_lake_ids_csv <- read.csv("split_lakes.csv")
split_lake_ids <- split_lake_ids_csv$Lake_ID

# Filter out all split lakes 
non_split_lake_data <- area_discrepancy_check %>%
  filter(!Lake_ID %in% split_lake_ids)

# Filter out lakes with >5% discrepancy
cleaned_Icedata <- non_split_lake_data %>%
  filter(discrepancy_percentage <= 5)


















# Data Filtering -----------------------------------------------------

#date formatting
cleaned_Icedata$date <- as.Date(cleaned_Icedata$date, format = "%Y_%m_%d")


## Clouds -----------------------------------------------------

#add cloud ratio column to Idata
Idata_cloudratio <- cleaned_Icedata %>%
  group_by(Lake_ID, date) %>%
  mutate(cloudRatio = (cloudArea) / (totalArea)) %>% #add cloudRatio column
  ungroup()

## Removals  -----------------------------------------------------
ifilt <- Idata_cloudratio %>%
  filter(iceArea != 0 | clearArea != 0 | cloudArea != 0)

#apply cloud ratio filter for ice
ifilt_cloud <- ifilt %>%
  filter(cloudRatio < 0.1) #remove rows w/ > 10% clouds

## Median Filter ---------------------------------------------------------------
# Apply a 5-observation median filter to iceArea for each lake group
ifilt_cloud_medfilt <- ifilt_cloud %>%
  group_by(Lake_ID) %>%
  mutate(median_filtered_iceArea = zoo::rollapply(iceArea, width = 5, FUN = median, fill = NA, align = "center", partial = FALSE))

# Filter out rows where median_filtered_iceArea is NA
filtered_data <- ifilt_cloud_medfilt %>%
  filter(!is.na(median_filtered_iceArea))

## Normalizing dataframe -------------------------------------------------------
ifilt_cloud_medfilt_norm <- ifilt_cloud_medfilt %>%
  mutate(iceArea_percent = (median_filtered_iceArea / measuredArea) * 100)


























# Add temperature data -------------------------------------------------------
setwd("G:/My Drive/Kusk_TempData")

#read temp csv
Lake_Temperature_2020 <- read_csv('Lake_Temperature_2020.csv')

# Ensure 'date' is in Date format and 'Lake_ID' matches the type in ifilt_cloud_medfilt_norm
Lake_Temperature_2020$date <- as.Date(Lake_Temperature_2020$date, format = "%Y-%m-%d")  # Adjust the format as necessary

#merge datasets
final_dataset <- ifilt_cloud_medfilt_norm %>%
  left_join(Lake_Temperature_2020, by = c("Lake_ID", "date"))
### why isn't temp data working? -------------------------------------------------------
# Check unique Lake_IDs and dates in both datasets
unique_lakes_main <- unique(ifilt_cloud_medfilt_norm$Lake_ID)
unique_lakes_temp <- unique(Lake_Temperature_2020$Lake_ID)

# Check for lakes in the main dataset that are not in the temperature dataset
missing_lakes <- setdiff(unique_lakes_main, unique_lakes_temp)

# Do the same for dates
unique_dates_main <- unique(ifilt_cloud_medfilt_norm$date)
unique_dates_temp <- unique(Lake_Temperature_2020$date)

missing_dates <- setdiff(unique_dates_main, unique_dates_temp)

# Inspect the missing lakes and dates
print(missing_lakes)
print(missing_dates)

















# Breakup Date Calculation -----------------------------------------------------
## Via Linear Interpolation ----------------------------------------------------
#function to estimate breakup date for one lake
estimate_breakup_date <- function(lake_data) {
  results <- data.frame(Date = as.Date(character()), Breakup_Estimate = as.Date(character()))
  
  for (i in 1:(nrow(lake_data) - 1)) {
    # Check for NA values in the current and next row
    if (!is.na(lake_data$iceArea_percent[i]) && !is.na(lake_data$iceArea_percent[i + 1])) {
      if (lake_data$iceArea_percent[i] > 25 && lake_data$iceArea_percent[i + 1] <= 25) {
        # Linear interpolation to estimate the breakup date
        date1 <- lake_data$date[i]
        date2 <- lake_data$date[i + 1]
        percent1 <- lake_data$iceArea_percent[i]
        percent2 <- lake_data$iceArea_percent[i + 1]
        
        # Calculate the weight for interpolation based on iceArea_percent
        weight <- (25 - percent1) / (percent2 - percent1)
        
        # Calculate the estimated breakup date
        days_between <- as.numeric(date2 - date1)
        estimated_date <- date1 + days_between * weight
        
        # Append the estimated breakup date to the results dataframe
        results <- rbind(results, data.frame(Breakup_Estimate = estimated_date,  Lower_Bound = date1, Upper_Bound = date2))
      }
    }
  }
  
  return(results)
}

# Apply the function to each Lake_ID group
breakup_dates <- ifilt_cloud_medfilt_norm %>%
  group_by(Lake_ID) %>%
  do(estimate_breakup_date(.)) %>%
  ungroup()

## Via averaging ----------------------------------------------------
breakup_dates$breakup_date_avgd <- with(breakup_dates, Lower_Bound + as.numeric(Upper_Bound - Lower_Bound) / 2)



















#Visualization -----------------------------------------------------------------
#all_lakes <- c(18760, 23797, 12693, 1836, 25356, 25393, 8165, 22520, 15701, 18169, 2876, 23096, 17726, 3533, 2285, 23192, 2876, 4850, 8822, 3352, 17673, 19378)
all_lakes <- c(6,22,190,196,210,218,238,250,4469,4607,4475,6442,6520,7246,8813,63159,35403)

#subset normalized dataframe for selected lakes
data_forVis <- ifilt_cloud_medfilt_norm[ifilt_cloud_medfilt_norm$Lake_ID %in% all_lakes, ]

data_forVis_complete <- data_forVis %>%
  filter(!is.na(iceArea_percent))


## All Lakes -----------------------------------------------------------------

#all lakes w/breakup date dot and callout date 
# NEED TO ADJUST DATES for each year
ggplot(data_forVis_complete, aes(x = date)) +
  geom_point(aes(y = iceArea_percent, color = "Ice"), size = 2) +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks", expand = c(0, 0)) +
  scale_color_manual(values = c("Ice" = "#A2B5AC")) +
  geom_hline(yintercept = 25, linetype = "dashed", color = "#C4846E") +
  # geom_point(data = breakup_dates, aes(x = Breakup_Estimate, y = 25), color = "red3", size = 2) +
  # geom_text(data = breakup_dates, aes(x = Breakup_Estimate, y = 25, label = format(Breakup_Estimate, "%b %d")), vjust = -1, color = "red3", size = 3, nudge_x = 10) +
  # geom_point(data = breakup_dates, aes(x = breakup_date_avgd, y = 25), color = "tan4", size = 2) +
  # geom_text(data = breakup_dates, aes(x = Breakup_Estimate, y = 25, label = format(breakup_date_avgd, "%b %d")), vjust = +2, color = "tan4", size = 3, nudge_x = -10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  lims(x = as.Date(c("2016-04-01", "2016-06-30"))) +
  labs(title = "2018 Ice Breakup",
       x = "Date",
       y = "Area (% ice)") +
  facet_wrap(~Lake_ID) +
  ylim(0, 100) +
  theme_grey()


















# Size Analysis ----------------------------------------------------------------

# Summarize the totalArea by Lake_ID to get a single value per lake
ifilt_cloud_medfilt_summary <- ifilt_cloud_medfilt %>%
  group_by(Lake_ID) %>%
  summarise(totalArea = mean(totalArea, na.rm = TRUE))

# Perform a left join to add the summarized totalArea to breakup_dates
breakup_dates_with_area <- breakup_dates %>%
  left_join(ifilt_cloud_medfilt_summary, by = "Lake_ID")

## bin by size -------------------------------------------------------------
#Jenks
install.packages("classInt")
library(classInt)

# Choose the number of groups
number_of_bins <- 5

# Calculate the natural breaks in the totalArea data
breaks <- classIntervals(breakup_dates_with_area$totalArea, n = number_of_bins, style = "jenks")$brks

# Create a new factor variable in the dataframe for the groupings
breakup_dates_with_area$size_group_jenks <- cut(breakup_dates_with_area$totalArea, breaks = breaks, include.lowest = TRUE, labels = 1:number_of_bins)

# View the first few rows of the dataframe to see the groupings
head(breakup_dates_with_area)



# Quintiles
breakup_dates_with_area <- breakup_dates_with_area %>%
  mutate(size_group_quantiles = ntile(totalArea, 5))

# Convert size_group_quantiles to a factor
breakup_dates_with_area$size_group_quantiles <- as.factor(breakup_dates_with_area$size_group_quantiles)




## Calculate average breakup date and standard deviation for each size group----
breakup_metrics_jenks <- breakup_dates_with_area %>%
  group_by(size_group_jenks) %>%
  summarise(
    Average_Breakup_Date = mean(as.numeric(Breakup_Estimate), na.rm = TRUE),
    SD_Breakup_Date = sd(as.numeric(Breakup_Estimate), na.rm = TRUE),
    Count = n()  # This will count the number of observations in each group
  )

# Convert the average breakup date from numeric back to Date
breakup_metrics_jenks$Average_Breakup_Date <- as.Date(breakup_metrics_jenks$Average_Breakup_Date, origin = "1970-01-01")

#repeat for quintile 
breakup_metrics_quint <- breakup_dates_with_area %>%
  group_by(size_group_quantiles) %>%
  summarise(
    Average_Breakup_Date = mean(as.numeric(Breakup_Estimate), na.rm = TRUE),
    SD_Breakup_Date = sd(as.numeric(Breakup_Estimate), na.rm = TRUE),
    Count = n()  # This will count the number of observations in each group
  )

# Convert the average breakup date from numeric back to Date
breakup_metrics_quint$Average_Breakup_Date <- as.Date(breakup_metrics_quint$Average_Breakup_Date, origin = "1970-01-01")


#visualize data, even though no statistically significant difference
# Boxplot
ggplot(breakup_dates_with_area, aes(x = size_group_jenks, y = Breakup_Estimate)) +
  geom_boxplot() +
  labs(title = "Breakup Date by Lake Size Group (Jenks)",
       x = "Size Group",
       y = "Breakup Date") +
  theme_minimal()


# Scatterplot with jitter to avoid overplotting
ggplot(breakup_dates_with_area, aes(x = size_group_jenks, y = Breakup_Estimate)) +
  geom_jitter(aes(color = size_group_jenks), width = 0.2) +
  labs(title = "Breakup Date by Lake Size Group (Jenks)",
       x = "Size Group",
       y = "Breakup Date") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

#again for quintiles 
# Boxplot
ggplot(breakup_dates_with_area, aes(x = size_group_quantiles, y = Breakup_Estimate)) +
  geom_boxplot() +
  labs(title = "Breakup Date by Lake Size Group (Quintiles)",
       x = "Size Group",
       y = "Breakup Date") +
  theme_minimal()


# Scatterplot with jitter to avoid overplotting
ggplot(breakup_dates_with_area, aes(x = size_group_quantiles, y = Breakup_Estimate)) +
  geom_jitter(aes(color = size_group_quantiles), width = 0.2) +
  labs(title = "Breakup Date by Lake Size Group (Quintiles)",
       x = "Size Group",
       y = "Breakup Date") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")


## Statistical Test ------------------------------------------------------------
#ANOVA assumptions test


# ANOVA to test for differences in average breakup dates across size groups
anova_results <- aov(as.numeric(Breakup_Estimate) ~ size_group_quantiles, data = breakup_dates_with_area)
anova_summary <- summary(anova_results)

# Print the summary of ANOVA results
print(anova_summary)


# Then get the residuals
residuals <- residuals(anova_results)

# Perform the Shapiro-Wilk test on the residuals
shapiro.test(residuals)


kruskal.test(Breakup_Estimate ~ size_group_jenks, data = breakup_dates_with_area)





#what are the ranges of values in each size category?
# Calculate min and max totalArea for each Jenks group
jenks_area_ranges <- breakup_dates_with_area %>%
  group_by(size_group_jenks) %>%
  summarise(
    Min_Area = min(totalArea, na.rm = TRUE),
    Max_Area = max(totalArea, na.rm = TRUE)
  )

# View the area ranges for each Jenks group
print(jenks_area_ranges)

