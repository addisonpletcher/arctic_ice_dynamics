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
setwd("G:/My Drive/YKD_gridded/2016")

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
# # APPLY function with progress bar
# Icedata <- pblapply(filesIce, download_dataI) %>% 
#   reduce(bind_rows)



# Data Filtering -----------------------------------------------------

#date formatting
Icedata$date <- as.Date(Icedata$date, format = "%Y_%m_%d")


## Clouds -----------------------------------------------------

#add cloud ratio column to Idata
Idata_cloudratio <- Icedata %>%
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

## Normalizing dataframe -------------------------------------------------------
ifilt_cloud_medfilt_norm <- ifilt_cloud_medfilt %>%
  mutate(iceArea_percent = (median_filtered_iceArea / totalArea) * 100)









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
all_lakes <- c(18760, 23797, 12693, 1836, 25356, 25393, 8165, 22520, 15701, 18169, 2876, 23096, 17726, 3533, 2285, 23192, 2876, 4850, 8822, 3352, 17673, 19378)

#subset normalized dataframe for selected lakes
data_forVis <- ifilt_cloud_medfilt_norm[ifilt_cloud_medfilt_norm$Lake_ID %in% all_lakes, ]

## All Lakes -----------------------------------------------------------------

#all lakes w/breakup date dot and callout date
ggplot(data_forVis, aes(x = date)) +
  geom_point(aes(y = iceArea_percent, color = "Ice"), size = 2) +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks", expand = c(0, 0)) +
  scale_color_manual(values = c("Ice" = "#A2B5AC")) +
  geom_hline(yintercept = 25, linetype = "dashed", color = "#C4846E") +
  # geom_point(data = breakup_dates, aes(x = Breakup_Estimate, y = 25), color = "red3", size = 2) +
  # geom_text(data = breakup_dates, aes(x = Breakup_Estimate, y = 25, label = format(Breakup_Estimate, "%b %d")), vjust = -1, color = "red3", size = 3, nudge_x = 10) +
  # geom_point(data = breakup_dates, aes(x = breakup_date_avgd, y = 25), color = "tan4", size = 2) +
  # geom_text(data = breakup_dates, aes(x = Breakup_Estimate, y = 25, label = format(breakup_date_avgd, "%b %d")), vjust = +2, color = "tan4", size = 3, nudge_x = -10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  lims(x = as.Date(c("2018-04-01", "2018-06-30"))) +
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

