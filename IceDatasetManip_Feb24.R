# IMPORT LIBRARIES
library(vroom)
library(stringr)
library(tidyverse)
library(stats)
library(dplyr)
library(tidyr)
# CSV Manipulation -------------------------------------------------------------
## Ice Data --------------------------------------------------------------------
# Making list of csvs
setwd("G:/My Drive/YKD_feb19")

filesIce <- list.files(full.names = FALSE)

# FUNCTION (merging files on ID + adding date column based on filename)
download_dataI <- function(filesIce){
  
  temp <- read_csv(paste0(filesIce)) %>% 
    as_tibble() %>% 
    mutate("date" = str_sub(filesIce, 16, 25)) #alter numbers here based on file name change 
  
  return(temp)
  
}

# APPLY function
Icedata <- filesIce %>% 
  map(download_dataI) %>% 
  reduce(bind_rows)

# Pre-Merge Filtering -----------------------------------------------------

#date filter
Idata$date <- as.Date(Idata$date, format = "%Y_%m_%d")

# #add totalArea as column to Wdata (based on maximum cloudArea)
# Wdata_date <- Wdata_date %>% 
#   group_by(Lake_ID) %>%
#   mutate(totalArea = max(cloudArea)) %>%
#   ungroup()
# #add totalArea as column to Idata (based on maximum cloudArea)
# Idata_date <- Idata_date %>% 
#   group_by(Lake_ID) %>%
#   mutate(totalArea = max(cloudArea)) %>%
#   ungroup()

### Clouds -----------------------------------------------------
#add cloud ratio column to Wdata
# Wdata_date <- Wdata_date %>%
#   group_by(Lake_ID, date) %>%
#   mutate(cloudRatio = (cloudArea) / (totalArea)) %>% #add cloudRatio column
#   ungroup()
#add cloud ratio column to Idata
Idata_cloudratio <- Icedata %>%
  group_by(Lake_ID, date) %>%
  mutate(cloudRatio = (cloudArea) / (totalArea)) %>% #add cloudRatio column
  ungroup()

### Zeros -----------------------------------------------------
# #remove all rows that have zeros for all data (water)
# wfilt <- Wdata_date %>%
#   filter(waterArea != 0 | clearArea_buff != 0 | cloudArea != 0)
#remove all rows that have zeros for all data (ice)
ifilt <- Idata_cloudratio %>%
  filter(iceArea != 0 | clearArea != 0 | cloudArea != 0)

# #apply cloud ratio filter for water
# wfilt_cloud <- wfilt %>%
#   filter(cloudRatio < 0.1) #remove rows w/ > 10% clouds
#apply cloud ratio filter for ice
ifilt_cloud <- ifilt %>%
  filter(cloudRatio < 0.1) #remove rows w/ > 10% clouds

### Outliers -----------------------------------------------------
#### Z Score -----------------------------------------------------
# # add Z scores as column, WATER
# wfilt_cloud_z <- wfilt_cloud %>%
#   group_by(Lake_ID) %>%
#   mutate(z_score = scale(waterArea))
# # add Z scores as column, ICE
# ifilt_cloud_z <- ifilt_cloud %>%
#   group_by(Lake_ID) %>%
#   mutate(z_score = scale(iceArea))
# 
# z_score_threshold <- 3.0
# 
# # filter: 3 std away, WATER
# wfilt_cloud_z <- wfilt_cloud_z %>%
#   filter(abs(z_score) <= z_score_threshold)
# # filter: 3 std away, ICE
# ifilt_cloud_z <- ifilt_cloud_z %>%
#   filter(abs(z_score) <= z_score_threshold)

#### Median Filter -----------------------------------------------------
# Apply a 5-observation median filter to iceArea for each lake group
install.packages("zoo")
library(zoo)

ifilt_cloud_medfilt <- ifilt_cloud %>%
  group_by(Lake_ID) %>%
  mutate(median_filtered_iceArea = zoo::rollapply(iceArea, width = 5, FUN = median, fill = NA, align = "center", partial = FALSE))

#### Hampel -----------------------------------------------------
# install.packages("pracma")
# library(pracma)
# 
# #set threshold
# hampel_threshold <- 3.0
# 
# 
# ##### Hampel not functioning, error returned:  `hampel_score` must be size 16 or 1, not 2.
# #apply hampel filter
# ifilt_cloud_hamp <- ifilt_cloud %>%
#   group_by(Lake_ID) %>%
#   mutate(hampel_score = hampel(iceArea, k = hampel_threshold))
# 
# # Filter based on the Hampel scores for ICE
# ifilt_cloud_hampel_filtered <- ifilt_cloud_hampel %>%
#   filter(abs(hampel_score) <= threshold * mad(iceArea))

#Visualization -----------------------------------------------------------------

all_lakes <- c(18760, 23797, 12693, 1836, 25356, 25393, 8165, 22520, 15701, 18169, 2876, 23096, 17726, 3533, 2285, 23192, 2876, 4850, 8822, 3352, 17673, 19378)

# Normalizing dataframe
ifilt_cloud_medfilt_norm <- ifilt_cloud_medfilt %>%
  mutate(iceArea_percent = (median_filtered_iceArea / totalArea) * 100)

#subset dataframe for all normalized lakes
filtered_lake_data <- ifilt_cloud_medfilt_norm[ifilt_cloud_medfilt_norm$Lake_ID %in% all_lakes, ]

## All Lakes -----------------------------------------------------------------

#all lakes w/breakup date dot and callout date
ggplot(filtered_lake_data, aes(x = date)) +
  geom_point(aes(y = iceArea_percent, color = "Ice"), size = 2) +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks", expand = c(0, 0)) +
  scale_color_manual(values = c("Ice" = "#A2B5AC")) +
  geom_hline(yintercept = 25, linetype = "dashed", color = "#C4846E") +
  geom_point(data = breakup_dates, aes(x = Breakup_Estimate, y = 25), color = "red3", size = 2) +
  geom_text(data = breakup_dates, aes(x = Breakup_Estimate, y = 25, label = format(Breakup_Estimate, "%b %d")), vjust = -1, color = "red3", size = 3, nudge_x = 10) +
  geom_point(data = breakup_dates, aes(x = breakup_date_avgd, y = 25), color = "tan4", size = 2) +
  geom_text(data = breakup_dates, aes(x = Breakup_Estimate, y = 25, label = format(breakup_date_avgd, "%b %d")), vjust = +2, color = "tan4", size = 3, nudge_x = -10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  lims(x = as.Date(c("2018-04-01", "2018-06-30"))) +
  labs(title = "2018 Ice Breakup",
       x = "Date",
       y = "Area (% ice)") +
  facet_wrap(~Lake_ID) +
  ylim(0, 100) +
  theme_grey()

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
breakup_dates <- filtered_lake_data %>%
  group_by(Lake_ID) %>%
  do(estimate_breakup_date(.)) %>%
  ungroup()

## Via averaging ----------------------------------------------------
breakup_dates$breakup_date_avgd <- with(breakup_dates, Lower_Bound + as.numeric(Upper_Bound - Lower_Bound) / 2)
