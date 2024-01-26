
# Preliminary ------------------------------------------------------------------
# IMPORT LIBRARIES
library(vroom)
library(stringr)
library(tidyverse)
library(stats)
library(dplyr)

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
  filter((date >= as.Date("2018-04-01") & date <= as.Date("2018-05-21")) |
          (date >= as.Date("2018-10-28") & date <= as.Date("2018-11-21")))

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

#Gabe test -----------------------------------------------------------------

merged <- merge(ifilt_cloud_z, wfilt_cloud_z, by = c("Lake_ID", "date"), all = TRUE)
# head(merged)

big_lakes <- c(18760, 14198)
small_lakes <- c(2876, 23096)

lake_data_small <- merged[merged$Lake_ID %in% small_lakes, ]
lake_data_big <- merged[merged$Lake_ID %in% big_lakes, ]
#other lake of comparable size include 14198, 23797, 19561, 18760
#maybe do one group of 4 of small lakes and one group of 4 of larger lakes to see difference in data availability???
#smaller lake group: 23096, 2285, 2876, 20623

# # Water Time Series Plot for single Lake
# ggplot(lake_data, aes(x = date, y = waterArea)) +
#   geom_point(color = "blue") +
#   geom_smooth() +
#   labs(title = "Water Time Series for Lake 1234",
#        x = "Date",
#        y = "Water Area") +
#   theme_minimal()
# 
# 
# # Ice Time Series Plot for single Lake
# ggplot(lake_data, aes(x = date, y = iceArea)) +
#   geom_point(color = "blue") +
#   geom_smooth() +
#   labs(title = "Ice Time Series for Lake 1234",
#        x = "Date",
#        y = "Ice Area") +
#   theme_minimal()

# Test graph with data for both 
ggplot(lake_data_big, aes(x = date)) +
  geom_point(aes(y = waterArea, color = "Water"), size = 2) +
  geom_point(aes(y = iceArea, color = "Ice"), size = 2) +
  facet_wrap(~Lake_ID) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0)) +
  # geom_smooth(aes(y = waterArea), method = "lm", color = "#3B7780", linetype = "solid") +
  # geom_smooth(aes(y = iceArea), method = "lm", color = "grey", linetype = "solid") +
  geom_smooth(aes(y = waterArea), method = "auto", color = "#3B7780", linetype = "solid") +
  geom_smooth(aes(y = iceArea), method = "auto", color = "grey", linetype = "solid") +
  labs(title = "Large Lakes",
       x = "Date",
       y = "Area (sq m)") +
  scale_color_manual(values = c("Water" = "#3B7780", "Ice" = "#A2B5AC")) +
  theme_grey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 6400000)

ggplot(lake_data_small, aes(x = date)) +
  geom_point(aes(y = waterArea, color = "Water"), size = 2) +
  geom_point(aes(y = iceArea, color = "Ice"), size = 2) +
  facet_wrap(~Lake_ID) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0)) +
  geom_smooth(aes(y = waterArea), method = "auto", color = "#3B7780", linetype = "solid") +
  geom_smooth(aes(y = iceArea), method = "auto", color = "grey", linetype = "solid") +
  labs(title = "Small Lakes",
       x = "Date",
       y = "Area (sq m)") +
  scale_color_manual(values = c("Water" = "#3B7780", "Ice" = "#A2B5AC")) +
  theme_grey() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 140000)
# scale_y_continuous(labels = scales::number_format(scale = 1e2))

# Outliers and such -------------------------------------------------------

# add Z scores as column, by each lake
dataZ <- data %>%
  group_by(Lake_ID) %>%
  mutate(z_score = scale(waterArea))

z_score_threshold <- 3.0

filtered_data_z <- dataZ %>%
  filter(abs(z_score) <= z_score_threshold)

#check how much was removed 
removed_rows <- nrow(dataZ) - nrow(filtered_data_z)
cat("Number of removed rows:", removed_rows, "\n")


# #Visualize
# Z <- filtered_data_z %>%
#   filter(Lake_ID  == '12772'| Lake_ID  == '12555' | Lake_ID  == '7146' | Lake_ID  == '5698') %>%
#   filter(date >= as.Date("2018-06-01") & date <= as.Date("2018-09-30")) %>%
#   filter(waterArea != 0)
# 
# 
# ggplot(Z, aes(x=date, y=waterArea, group = 1)) + 
#   geom_point() +
#   facet_wrap(~Lake_ID) +
#   scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 weeks", expand = c(0, 0)) +
#   #scale_x_discrete(guide = guide_axis(angle = 45)) +
#   ggtitle("Outlier Removal Test - Z")

##########   BELOW HERE GOOD STUFF BUT ABOVE STUFF IS ACTUALLY FOR AGU     ####################
# Merging Datasets --------------------------------------------------------
merged_data <- Idata_date %>%
  full_join(Wdata_date, by = "Lake_ID")

merged_data1 <- merged_data %>%
  mutate(
    waterArea = coalesce(waterArea_Wdata, iceArea_Idata),
    clearArea_buff = coalesce(clearArea_buff_Wdata, clearArea_Idata),
    cloudArea = coalesce(cloudArea_Wdata, cloudArea_Idata),
    clearArea = coalesce(clearArea_Wdata, clearArea_Idata),
    iceArea = coalesce(iceArea_Idata, waterArea_Wdata),
    centroid = coalesce(centroid_Idata, centroid_Wdata),
    date = coalesce(date_Idata, date_Wdata)
  ) %>%
  select(-ends_with("_Idata"), -ends_with("_Wdata"))


# Data Manipulation -------------------------------------------------------

#add totalArea as column (based on maximum cloudArea)
merged_data_cloud <- merged_data %>% 
  group_by(Lake_ID) %>%
  mutate(totalArea = max(cloudArea)) %>%
  ungroup()

#add cloud ratio column
merged_data_cloud2 <- merged_data_cloud %>%
  group_by(Lake_ID, date) %>%
  mutate(cloudRatio = (cloudArea) / (totalArea)) %>% #add cloudRatio column
  ungroup()

#remove all rows that have zeros for all data
merged_data_cloud2_3 <- merged_data_cloud2 %>%
  filter(waterArea != 0 | clearArea != 0 | cloudArea != 0)

merged_data_cloud2_4 <- merged_data_cloud2 %>%
  filter(iceArea != 0 | waterArea != 0 | clearArea != 0 | cloudArea != 0)


#apply cloud ratio filter
merged_data_cloud2_3_rmvcld <- merged_data_cloud2_3 %>%
  filter(cloudRatio < 0.1) #remove rows w/ > 10% clouds

merged_data_cloud2_4_rmvcld <- merged_data_cloud2_4 %>%
  filter(cloudRatio < 0.1) #remove rows w/ > 10% clouds

# Filter by date


# Outliers and such -------------------------------------------------------

# add Z scores as column, by each lake
dataZ <- data %>%
  group_by(Lake_ID) %>%
  mutate(z_score = scale(waterArea))

z_score_threshold <- 3.0

filtered_data_z <- dataZ %>%
  filter(abs(z_score) <= z_score_threshold)

#check how much was removed 
removed_rows <- nrow(dataZ) - nrow(filtered_data_z)
cat("Number of removed rows:", removed_rows, "\n")


#Visualize
Z <- filtered_data_z %>%
  filter(Lake_ID  == '12772'| Lake_ID  == '12555' | Lake_ID  == '7146' | Lake_ID  == '5698') %>%
  filter(date >= as.Date("2018-06-01") & date <= as.Date("2018-09-30")) %>%
  filter(waterArea != 0)


ggplot(Z, aes(x=date, y=waterArea, group = 1)) + 
  geom_point() +
  facet_wrap(~Lake_ID) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "2 weeks", expand = c(0, 0)) +
  #scale_x_discrete(guide = guide_axis(angle = 45)) +
  ggtitle("Outlier Removal Test - Z")
