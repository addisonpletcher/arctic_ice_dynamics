# Preliminary ------------------------------------------------------------------
install.packages("vroom")
installed.packages("stringr")
install.packages("tidyverse")
install.packages("stats")
install.packages("dplyr")

# IMPORT LIBRARIES
library(vroom)
library(stringr)
library(tidyverse)
library(stats)
library(dplyr)
library(tidyr)
# CSV Manipulation -------------------------------------------------------------

## Water data ------------------------------------------------------------------
#Gdrive
setwd("G:/My Drive/YKD_AGU")

# Making list of csvs
filesW <- list.files('water', full.names = FALSE)

#Gdrive
setwd("G:/My Drive/YKD_AGU/water")

# FUNCTION (merging files on ID + adding date column based on filename)
download_dataW <- function(filesW){
  
  temp <- read_csv(paste0(filesW)) %>% 
    as_tibble() %>% 
    mutate("date" = str_sub(filesW, 15, 24)) #alter numbers here based on file name change 
  
  return(temp)
  
}

#Gdrive
setwd("G:/My Drive/YKD_AGU/water")

# APPLY function
Wdata <- filesW %>% 
  map(download_dataW) %>% 
  reduce(bind_rows)

Wdata <- Wdata %>%
  rename(clearArea_buff = clearArea)

## Ice Data --------------------------------------------------------------------
#Gdrive
setwd("G:/My Drive/YKD_AGU")

# Making list of csvs
filesI <- list.files('ice', full.names = FALSE)

#Gdrive
setwd("G:/My Drive/YKD_AGU/ice")

# FUNCTION (merging files on ID + adding date column based on filename)
download_dataI <- function(filesI){
  
  temp <- read_csv(paste0(filesI)) %>% 
    as_tibble() %>% 
    mutate("date" = str_sub(filesI, 13, 22)) #alter numbers here based on file name change 
  
  return(temp)
  
}

#Gdrive
setwd("G:/My Drive/YKD_AGU/ice")

# APPLY function
Idata <- filesI %>% 
  map(download_dataI) %>% 
  reduce(bind_rows)



# Pre-Merge Filtering -----------------------------------------------------

#date filter
Wdata$date <- as.Date(Wdata$date, format = "%Y_%m_%d")
Idata$date <- as.Date(Idata$date, format = "%Y_%m_%d")

# Left join Wdata to Idata based on Lake_ID and date
Idata <- left_join(Idata, select(Wdata, Lake_ID, date, cloudArea), by = c("Lake_ID", "date"))


# Filter Wdata for specific date range
Wdata_date <- Wdata %>%
  filter((date >= as.Date("2018-05-22") & date <= as.Date("2018-10-27")))

# Filter Idata for specific date range
Idata_date <- Idata %>%
  filter((date >= as.Date("2018-04-01") & date <= as.Date("2018-07-30")))
#filter((date >= as.Date("2018-04-01") & date <= as.Date("2018-05-21")) |
#(date >= as.Date("2018-10-28") & date <= as.Date("2018-11-21")))

#add totalArea as column to Wdata (based on maximum cloudArea)
Wdata_date <- Wdata_date %>% 
  group_by(Lake_ID) %>%
  mutate(totalArea = max(cloudArea)) %>%
  ungroup()
#add totalArea as column to Idata (based on maximum cloudArea)
Idata_date <- Idata_date %>% 
  group_by(Lake_ID) %>%
  mutate(totalArea = max(cloudArea)) %>%
  ungroup()

### Clouds -----------------------------------------------------
#add cloud ratio column to Wdata
Wdata_date <- Wdata_date %>%
  group_by(Lake_ID, date) %>%
  mutate(cloudRatio = (cloudArea) / (totalArea)) %>% #add cloudRatio column
  ungroup()
#add cloud ratio column to Idata
Idata_date <- Idata_date %>%
  group_by(Lake_ID, date) %>%
  mutate(cloudRatio = (cloudArea) / (totalArea)) %>% #add cloudRatio column
  ungroup()

### Zeros -----------------------------------------------------
#remove all rows that have zeros for all data (water)
wfilt <- Wdata_date %>%
  filter(waterArea != 0 | clearArea_buff != 0 | cloudArea != 0)
#remove all rows that have zeros for all data (ice)
ifilt <- Idata_date %>%
  filter(iceArea != 0 | clearArea != 0 | cloudArea != 0)

#apply cloud ratio filter for water
wfilt_cloud <- wfilt %>%
  filter(cloudRatio < 0.1) #remove rows w/ > 10% clouds
#apply cloud ratio filter for ice
ifilt_cloud <- ifilt %>%
  filter(cloudRatio < 0.1) #remove rows w/ > 10% clouds

### Outliers -----------------------------------------------------
#### Z Score -----------------------------------------------------
# add Z scores as column, WATER
wfilt_cloud_z <- wfilt_cloud %>%
  group_by(Lake_ID) %>%
  mutate(z_score = scale(waterArea))
# add Z scores as column, ICE
ifilt_cloud_z <- ifilt_cloud %>%
  group_by(Lake_ID) %>%
  mutate(z_score = scale(iceArea))

z_score_threshold <- 3.0

# filter: 3 std away, WATER
wfilt_cloud_z <- wfilt_cloud_z %>%
  filter(abs(z_score) <= z_score_threshold)
# filter: 3 std away, ICE
ifilt_cloud_z <- ifilt_cloud_z %>%
  filter(abs(z_score) <= z_score_threshold)

#### Median Filter -----------------------------------------------------
# Apply a 5-observation median filter to iceArea for each lake group
install.packages("zoo")
library(zoo)

ifilt_cloud_med <- ifilt_cloud %>%
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

#merged <- merge(ifilt_cloud_z, wfilt_cloud_z, by = c("Lake_ID", "date"), all = TRUE)
# head(merged)

#lake lists
big_lakes <- c(18760, 23797, 12693, 1836, 25356, 25393, 8165, 22520, 15701, 18169)
small_lakes <- c(2876, 23096, 17726, 3533, 2285, 23192, 2876, 4850, 8822, 3352)
all_lakes <- c(18760, 23797, 12693, 1836, 25356, 25393, 8165, 22520, 15701, 18169, 2876, 23096, 17726, 3533, 2285, 23192, 2876, 4850, 8822, 3352, 17673, 19378)

# Normalizing dataframe
ifilt_cloud_med <- ifilt_cloud_med %>%
  mutate(iceArea_percent = (median_filtered_iceArea / totalArea) * 100)

#subset dataframe for all normalized lakes
filtered_lake_data <- ifilt_cloud_med[ifilt_cloud_med$Lake_ID %in% all_lakes, ]

## All Lakes -----------------------------------------------------------------

#starting plot
ggplot(filtered_lake_data, aes(x = date)) +
  geom_point(aes(y = iceArea_percent, color = "Ice"), size = 2) +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks", expand = c(0, 0)) +
  scale_color_manual(values = c("Ice" = "#A2B5AC")) +
  geom_hline(yintercept = 25, linetype = "dashed", color = "#C4846E") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  lims(x = as.Date(c("2018-04-01", "2018-06-30"))) +
  labs(title = "2018 Ice Breakup",
       x = "Date",
       y = "Area (sq m)") +
  facet_wrap(~Lake_ID) +
  ylim(0, 100) +
  theme_grey() 

#all lakes with breakup date line
ggplot(filtered_lake_data, aes(x = date)) +
  geom_point(aes(y = iceArea_percent, color = "Ice"), size = 2) +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks", expand = c(0, 0)) +
  scale_color_manual(values = c("Ice" = "#A2B5AC")) +
  geom_hline(yintercept = 25, linetype = "dashed", color = "#C4846E") +
  geom_vline(data = breakup_dates, aes(xintercept = as.numeric(Breakup_Estimate)), linetype = "solid", color = "red", size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  lims(x = as.Date(c("2018-04-01", "2018-06-30"))) +
  labs(title = "2018 Ice Breakup",
       x = "Date",
       y = "Area (sq m)") +
  facet_wrap(~Lake_ID) +
  ylim(0, 100) +
  theme_grey()

#all lakes with breakup date dot, error bars? NOT working need to fix (TODO)
ggplot(filtered_lake_data, aes(x = date)) +
  geom_point(aes(y = iceArea_percent, color = "Ice"), size = 2) +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks", expand = c(0, 0)) +
  scale_color_manual(values = c("Ice" = "#A2B5AC")) +
  geom_hline(yintercept = 25, linetype = "dashed", color = "#C4846E") +
  geom_point(data = breakup_dates, aes(x = Breakup_Estimate, y = 25), color = "red", size = 1) +
  geom_errorbar(data = breakup_dates, aes(x = Breakup_Estimate, ymin = 25, ymax = 25, xmin = Lower_Bound, xmax = Upper_Bound), color = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  lims(x = as.Date(c("2018-04-01", "2018-06-30"))) +
  labs(title = "2018 Ice Breakup",
       x = "Date",
       y = "Area (%)") +
  facet_wrap(~Lake_ID) +
  ylim(0, 100) +
  theme_grey()

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




## Big/Small Lakes -----------------------------------------------------------------
#subset dataframes
lake_data_small <- merged[merged$Lake_ID %in% small_lakes, ]
lake_data_big <- merged[merged$Lake_ID %in% big_lakes, ]

#big and small subset dataframes from median filter method
lake_data_small_med <- ifilt_cloud_med[ifilt_cloud_med$Lake_ID %in% small_lakes, ]
lake_data_big_med <- ifilt_cloud_med[ifilt_cloud_med$Lake_ID %in% big_lakes, ]

# Big lakes Graph 
ggplot(lake_data_big_med, aes(x = date)) +
  #geom_point(aes(y = waterArea, color = "Water"), size = 2) +
  geom_point(aes(y = median_filtered_iceArea, color = "Ice"), size = 2) +
  facet_wrap(~Lake_ID) +
  scale_x_date(date_labels = "%b", date_breaks = "1 week", expand = c(0, 0)) +
  # geom_smooth(aes(y = waterArea), method = "lm", color = "#3B7780", linetype = "solid") +
  # geom_smooth(aes(y = iceArea), method = "lm", color = "grey", linetype = "solid") +
  #geom_smooth(aes(y = waterArea), method = "auto", color = "#3B7780", linetype = "solid") +
  #geom_smooth(aes(y = iceArea), method = "auto", color = "grey", linetype = "solid") +
  labs(title = "Large Lakes",
       x = "Date",
       y = "Area (sq m)") +
  scale_color_manual(values = c("Water" = "#3B7780", "Ice" = "#A2B5AC")) +
  theme_grey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 6400000) +
  lims(x = as.Date(c("2018-04-01", "2018-06-30")))

# Small lakes graph
ggplot(lake_data_small_med, aes(x = date)) +
  #geom_point(aes(y = waterArea, color = "Water"), size = 2) +
  geom_point(aes(y = median_filtered_iceArea, color = "Ice"), size = 2) +
  facet_wrap(~Lake_ID) +
  scale_x_date(date_labels = "%b", date_breaks = "1 week", expand = c(0, 0)) +
  #geom_smooth(aes(y = waterArea), method = "auto", color = "#3B7780", linetype = "solid") +
  #geom_smooth(aes(y = iceArea), method = "auto", color = "grey", linetype = "solid") +
  labs(title = "Small Lakes",
       x = "Date",
       y = "Area (sq m)") +
  scale_color_manual(values = c("Water" = "#3B7780", "Ice" = "#A2B5AC")) +
  theme_grey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 140000) +
  lims(x = as.Date(c("2018-04-01", "2018-06-30")))
# scale_y_continuous(labels = scales::number_format(scale = 1e2))

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


