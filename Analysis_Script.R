# Summer TEMPEST soil analysis
# Gabby Gonzalez 2026

#load in all the packages that will be needed 
library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_minimal())
library(lubridate)
library(tidyverse)
library(patchwork)
library(naniar)
library(compasstools)

# --------------------------------------------------------
# ------ READ IN SOIL TEMP, VWC, AND EC AND PLOT/SUMMARIZE
# --------------------------------------------------------
# --------Plot--------------------------------------------

# We use the "compasstools" package (loaded above) to load data
# Specifically, compasstools::read_L2_variable()
soil_temp15 <- read_L2_variable("soil-temp-15cm", path = "L2_data/")
soil_temp5 <- read_L2_variable("soil-temp-5cm",  path = "L2_data/")
soil_temp30 <- read_L2_variable("soil-temp-30cm",  path = "L2_data/")

# Using bind_rows() to bring them together 
Soil_temp <- bind_rows(soil_temp15, soil_temp5, soil_temp30)

#Summarizing soil temp file 
head(soil_temp15)
names(soil_temp15)
dim(soil_temp15)
str(soil_temp15)
print(colnames(soil_temp15))
summary(soil_temp15)

# Select just the columns we want to work with 
soil_temp15 <- soil_temp15 |> 
  select (Plot, TIMESTAMP, Instrument_ID, Sensor_ID, Location, Value)

# Line Chart of all the data
# Randomly sub_sample the data to make plotting faster
soil_temp15 |>
  # this dataset is VERY large so just select 1000 random rows to plot
  slice_sample(n = 1000) |>
  ggplot(aes(x = TIMESTAMP, y = Value, color = Plot)) +
  geom_point() + 
  # add a 'smoother' -- a smooth line that follows the data to help
  # audience see trend
  geom_smooth(linetype = 2) +
  labs( title = "Soil Temperature Over Time", x = "TimeStamp", y = "Temperature")

# Goup by month and sensor ID
# Take the mean of the value column
soil_temp15 |> 
  mutate(Month = month(TIMESTAMP)) |> 
  group_by(Month, Sensor_ID, Plot) |>                             
  summarize(mean_temp = mean(Value, na.rm = TRUE)) |>       
  ggplot(aes(x = Month, y = mean_temp, group = Sensor_ID, color = Plot)) +  
  geom_line() + 
  geom_point() + labs( title = "Soil Temperature by Sensor ", x = "Month", y = "Average Temp")


# Convert to date type and extract the month
soil_temp15<- soil_temp15 |> 
  mutate(
    month_column = as.Date(TIMESTAMP))
soil_temp15 <- soil_temp15 |> 
  mutate(
    month = month(month_column, label = TRUE))

# Box plot of temp and month (overall data)
ggplot(
  soil_temp15, aes(x = month, y = Value, group = month, fill = as.factor(month))) + 
  geom_boxplot() + 
  labs(title = "Temperature by Month", x = "Month", y = "Temperature") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust= 1), 
  legend.position = "none")

# Graph of temp and month 
# Same data as above but different graph 
soil_temp15 |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x = month, y = Value, color = Value)) + 
  geom_point(na.rm = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid ="blue", midpoint = 15, limits = c(0, 30)) +
  labs(title = "By Month", x = "Month", y = "Temperature") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust= 1))

# Temperature by month of each plot 
ggplot(
  soil_temp15, aes(x = month, y = Value, group = Plot, color = Plot)) + 
  geom_boxplot() + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "Temperature by Month", y = "Temperature") + 
  theme_minimal()

# Per-month variability 
soil_temp15 |> 
  group_by(Plot, month) |>
  # compute mean and s.d. for each month
  summarise(mean_value = mean(Value, na.rm = TRUE), 
            sd_value = sd(Value, na.rm = TRUE)) |>
  ggplot(aes(x = month, y = mean_value, color = Plot)) +
  geom_point() + 
  geom_line(linetype = 2, aes(group = Plot)) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value)) +
  labs(title = "Per-month Variability", y = "Average Value", x = "Month") + 
  theme(axis.text.x = element_text(angle = 45, hjust= 1))

# Soil volumetric Water Content 15 cm 
# Read in data from soil vwc parquet file
soil_vwc15 <- read_L2_variable("soil-vwc-15cm", path = "L2_data/") 
soil_vwc5 <- read_L2_variable("soil-vwc-5cm", path = "L2_data/")
soil_vwc30 <- read_L2_variable("soil-vwc-30cm", path = "L2_data/")

#Using bind_rows() to bring them together 
Soil_vwc <- bind_rows(soil_vwc15, soil_vwc5, soil_vwc30)

# Summarizing swc file 
head(soil_vwc)
names(soil_vwc)
dim(soil_vwc)
str(soil_vwc)
print(colnames(soil_vwc))
summary(soil_vwc)

# Line Chart of all the data
# Randomly sub_sample the data to make plotting faster
soil_vwc |>
  # this dataset is VERY large so just select 1000 random rows to plot
  slice_sample(n = 1000) |>
  ggplot(aes(x = TIMESTAMP, y = Value, color = Plot)) +
  geom_point() + 
  # add a 'smoother' -- a smooth line that follows the data to help
  # audience see trend
  geom_smooth(linetype = 2) +
  labs( title = "Soil VWC Over Time", x = "Month", y = "Volumetric Water content") + 
  theme_minimal()

# Group by month and sensor
# Take the mean of the value column
soil_vwc |> 
  mutate(Month = month(TIMESTAMP)) |> 
  group_by(Month, Sensor_ID, Plot) |>                             
  summarize(mean_vwc = mean(Value, na.rm = TRUE)) |>       
  ggplot(aes(x = Month, y = mean_vwc, group = Sensor_ID, color = Plot)) +  
  geom_line() + 
  geom_point() +
  labs(title = "Soil VWC over time", y = "Average soil VWC")

# Convert to date type and extract the month for wvc
soil_vwc<- soil_vwc |> 
  mutate(
    month_column = as.Date(TIMESTAMP))
soil_vwc <- soil_vwc |> 
  mutate(
    month = month(month_column, label = TRUE))

# Graph of each month for vwc
ggplot(
  soil_vwc, aes(x = month, y = Value, group = Plot, color = Plot)) + 
  geom_boxplot() + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "Moisture by Month", y = "Temperature", x = "Month") + 
  theme_minimal()

# Boxplot with points (choose between the ones w/ or w/out)
soil_vwc |> 
  slice_sample(n = 1000) |> 
ggplot(
 aes(x = month, y = Value)) + 
  geom_boxplot() + geom_jitter(pch = 19, width = 0.2, aes(color = month)) + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "Moisture by Month", y = "Temperature", x = "Month") + 
  theme_minimal() + theme(legend.position = "none")

# VWC by month graph with colors
soil_vwc |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x = month, y = Value, color = Value)) + 
  geom_point(na.rm = TRUE) +
  scale_color_gradient2(low = "brown", high = "blue",mid ="brown", midpoint = 5, limits = c(0, 30)) +
  labs(title = "By Month", x = "Month", y = "Volumetric Water Content") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust= 1))

# SOIL EC 
# Read in data from soil EC parquet file
soil_EC_15 <- read_L2_variable("soil-EC-15cm", path = "L2_data/") 
soil_EC5 <- read_L2_variable("soil-EC-5cm", path = "L2_data/")
soil_EC30 <- read_L2_variable("soil-EC-30cm", path = "L2_data/")

# Use bind_rows() to bring them together 
Soil_EC <- bind_rows(soil_EC_15, soil_EC5, soil_EC30)

#Spliting the column's contents into multiple columns
Soil_temp |> separate(research_name, into = c("first_part", "second_part", "depth")) -> Split_soil_temp
Soil_vwc |>  separate(research_name, into = c("first_part", "second_part", "depth")) -> Split_Soil_vwc
Soil_EC |> separate(research_name, into = c("first_part", "second_part", "depth")) -> Split_Soil_EC


#Summarizing EC file 
head(Soil_EC_15)
names(Soil_EC_15)
dim(Soil_EC_15)
str(Soil_EC_15)
print(colnames(Soil_EC_15))
summary(Soil_EC_15)

# Line Chart of all the data
# Randomly sub_sample the data to make plotting faster
Soil_EC_15 |>
  # this dataset is VERY large so just select 1000 random rows to plot
  slice_sample(n = 1000) |>
  ggplot(aes(x = TIMESTAMP, y = Value, color = Plot)) +
  geom_point() + 
  # add a 'smoother' -- a smooth line that follows the data to help
  # audience see trend
  geom_smooth(linetype = 2) +
  labs( title = "Soil EC Over Time", x = "TimeStamp", y = "EC") + 
  theme_minimal()

#convert to date type and extract the month for EC
Soil_EC_15<- Soil_EC_15 |> 
  mutate(
    month_column = as.Date(TIMESTAMP))
Soil_EC_15 <- Soil_EC_15 |> 
  mutate(
    month = month(month_column, label = TRUE))

# graph of each month for EC
ggplot(
  Soil_EC_15, aes(x = month, y = Value, group = Plot, color = Plot)) + 
  geom_boxplot() + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "EC by Month", y = "EC", x = "Month") + 
  theme_minimal()

# Boxplot with points (choose between the ones w/ or w/out)
Soil_EC_15 |> 
  slice_sample(n = 1000) |> 
  ggplot(
    aes(x = month, y = Value)) + 
  geom_boxplot() + geom_jitter(pch = 19, width = 0.2, aes(color = month)) + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "EC by Month", y = "EC", x = "Month") + 
  theme_minimal()

# graph with colors 
Soil_EC_15 |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x = month, y = Value, color = Value)) + 
  geom_point(na.rm = TRUE) +
  labs(title = "By Month", x = "Month", y = "EC") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust= 1))

# Merge the data
# TEROS sensors are clustered -- a single sensor simultaneously
# measures temperature, water content, and electrical conductivity
soil_temp15 |> 
  # both data frames have 'Value' columns, which we need to be able to 
  # distinguish after merging. Start by renaming the temperature value...
  rename(Value_temp15 = Value) |> 
  # ...do the join...
  left_join(soil_vwc, 
            by = c("Plot", "TIMESTAMP", "Instrument_ID", "Sensor_ID", "Location"), 
            relationship = "one-to-one") |> 
  # ...and now rename the vwc value column
  rename(Value_vwc15 = Value) |> 
  select (-month_column.x, -month_column.y, -month.x, -month.y) ->
  teros_combined

#Adding the soil EC 
teros_combined |> 
  left_join(Soil_EC_15, 
            by = c("Plot", "TIMESTAMP", "Instrument_ID", "Sensor_ID", "Location"), 
            relationship = "one-to-one") |> 
  rename(Value_ec15 = Value) ->
  teros_combined

# ------------------------------------------------------
# ------ ANALYZE RELATIONSHIPS BETWEEN VARIABLES
# ------------------------------------------------------

#plotting relationships 
flood_start <- ymd_hms("2024-06-11 00:00:00")  
flood_end   <- ymd_hms("2024-06-13 23:45:00")  

#Temperature and VWC
teros_combined |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x = TIMESTAMP)) +
  geom_line(aes(y = Value_temp15, color = "Temperature")) +
  geom_line(aes(y = Value_vwc15 * 70, color = "VWC")) +  # rescaled 
  annotate("rect", xmin = flood_start, xmax = flood_end, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "green") +
  facet_wrap(~ Location) +
  labs(y = "Temperature & VWC", color = "Variable") +
  theme(axis.text.x = element_text(angle = 90))

#Temperature and EC
teros_combined |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x = TIMESTAMP)) +
  geom_line(aes(y = Value_temp15, color = "Temperature")) +
  geom_line(aes(y = Value_ec15 *80, color = "EC")) +  # rescaled 
  annotate("rect", xmin = flood_start, xmax = flood_end, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "green") +
  facet_wrap(~ Location) +
  labs(y = "Temperature & EC", color = "Variable") +
  theme(axis.text.x = element_text(angle = 90))

teros_combined <- teros_combined |> 
  mutate(
    loc_row = substr(Location, 1, 1),       # A-J 
    loc_col = as.numeric(substr(Location, 2, 2))
  )

# Tile (heat map) plots of soil temperature
teros_combined |> 
  # compute the quarter of the year, then...
  mutate(Quarter = lubridate::quarter(TIMESTAMP)) |> 
  # ...for each location in each plot...
  group_by(loc_row, loc_col, Quarter, Plot) |> 
  # ...compute the average temperature by quarter
  summarise(Value_temp15 = mean(Value_temp15)) |>
  # ...and plot
  ggplot(aes(x = loc_col, y = loc_row, fill = Value_temp15)) +
  geom_tile() +
  facet_grid(Quarter ~ Plot) +
  labs(x = "Columns", y = "Location", fill = "Temp")

 
#Graphs
 scale_factor <- 42.15
 
 teros_combined |> 
   slice_sample(n = 1000) |> 
   ggplot(aes(x = TIMESTAMP)) +
   geom_point(aes(y = Value_temp15), color = "red", alpha = 0.05, size = 1) +
   geom_point(aes(y = Value_vwc15 * scale_factor), color = "blue", alpha = 0.05, size = 1) +
   scale_y_continuous(name = "Temperature (°C)",
                      sec.axis = sec_axis(~ . / scale_factor, name = "VWC")) +
   labs(title = "Soil Temperature and Water Content Over Time", x = "Timestamp") +
   theme_minimal()
 
 # Use the ranges of the data to compute a scale factor for plotting
 # two axes on the same graph, below
 scale_factor <- with(teros_combined,
                      (max(Value_vwc15, na.rm = TRUE) - min(Value_vwc15, na.rm = TRUE)) /
                        (max(Value_ec15, na.rm = TRUE) - min(Value_ec15, na.rm = TRUE)))
 
 teros_combined |> 
   slice_sample(n = 1000) |> 
   ggplot(aes(x = TIMESTAMP)) +
   # primary (left) axis
   geom_point(aes(y = Value_vwc15), color = "red", alpha = 0.05, size = 1) +
   geom_smooth(aes(y = Value_vwc15), method = "lm", se = FALSE, color = "red") +
   theme(axis.title.y.left = element_text(color = "red")) +
    # secondary (right) axis
    geom_point(aes(y = Value_ec15 * scale_factor), color = "blue", alpha = 0.05, size = 1) +
   geom_smooth(aes(y = Value_ec15 * scale_factor), method = "lm", se = FALSE, color = "blue") +
   scale_y_continuous(name = "VWC",
                       sec.axis = sec_axis(~ . / scale_factor, name = "EC")) +
   labs(title = "Soil EC and Water Content Over Time", x = "Timestamp") +
   theme_minimal()
 
 
ggplot(
    teros_combined, aes(x = month, y = Value_vwc15, group = month, fill = as.factor(month))) + 
    geom_boxplot() + 
    labs(litle = "By Month", x = "Month", y = "Soil Volumetric Water content") + 
    theme_minimal() + theme(asix.text.x = element_text(angle = 45, hjust= 1), 
                            legend.position = "none")

ggplot(
  teros_combined, aes(x = Value_vwc15, y = Value_temp15, fill = as.factor(month))) + 
  geom_boxplot() + 
  labs(litle = "By Month", x = "Soil Volumetric Water content", y = "Soil Temperature") + 
  theme_minimal() + theme(asix.text.x = element_text(angle = 45, hjust= 1), 
                          legend.position = "none")


#########################################
# BEN AND GABBY GOT TO HERE -- 
#########################################

#Temperature and vwc
teros_combined |> 
  slice_sample(n = 1000) |> 
ggplot(aes(x = TIMESTAMP)) +
  geom_line(aes(y = Value_temp15, color = "Temperature")) +
  geom_line(aes(y = Value_vwc15 * 50, color = "VWC")) + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  annotate("rect", xmin = flood_start, xmax = flood_end, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "green") +
  facet_wrap(~ Location) +
  labs(y = "Temperature/VWC", color = "Variable")

#Temperature and EC
teros_combined |> 
  slice_sample(n = 1000) |> 
ggplot( aes(x = TIMESTAMP)) +
  geom_line(aes(y = Value_temp15, color = "Temperature")) +
  geom_line(aes(y = Value_ec15 / 10, color = "EC")) + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  annotate("rect", xmin = flood_start, xmax = flood_end, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "green") +
  facet_wrap(~ Location)  + 
  labs(y = "Temperature/EC", color = "Variable") 

#Tiles
teros_combined |> group_by(Location, Plot) |> 
  summarise(mean_temp15 = mean(Value_temp15, na.rm = TRUE), .groups = "drop") |> 
  separate(Location, into = c("grid_letter", "grid_number"), sep = 1) ->
  teros_temp15_loc_avg

teros_temp15_loc_avg |>
  #filter(Plot == "S") |>
  ggplot(aes(x = grid_letter, y = grid_number, fill = mean_temp15)) + 
  geom_tile() +
  scale_fill_viridis_c(option = "B") +
  theme_bw() +
  labs(title = "Freshwater Plot Soil Temperature - 15cm", fill = "temperature C") +
  facet_wrap(~Plot)

#calculating the coefficient of variability
sd(soil_temp15$Value, na.rm = TRUE) / mean(soil_temp15$Value, na.rm = TRUE) -> Cv

#Freshwater special plot 15cm-5
teros_combined |> group_by(Location) |> summarise(mean_temp5 = mean(Value, na.rm = TRUE)) |> 
  separate(Location, into = c("grid_letter", "grid_number"), sep = 1) |> 
  ggplot(aes(x = grid_letter, y = grid_number, color = mean_temp5)) + 
  geom_point(size = 3) +
  scale_color_viridis_c(option = "H", begin = 0.2) +
  theme_bw() +
  labs(title = "Freshwatwr Plot Soil Temperature - 15cm", color = "temperature C")

#trying with the new split data set
Split_soil_temp |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x = TIMESTAMP, y = Value, group = depth, color = depth)) + 
  geom_point() + geom_smooth(linetype = 2) 

Split_Soil_vwc |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x= Value, y = Plot, group = depth, color = depth)) +
  geom_point() + geom_smooth(linetype = 2)

Split_Soil_EC |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x= TIMESTAMP, y = Value, group = depth, color = depth)) +
  geom_point() + geom_smooth(linetype = 2)

Split_Soil_EC |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x= Value, y = Plot, group = depth, color = depth)) +
  geom_point() + geom_smooth(linetype = 2)

# Convert to date type and extract the month
Split_soil_temp<- Split_soil_temp |> 
  mutate(
    month_column = as.Date(TIMESTAMP))
Split_soil_temp <- Split_soil_temp |> 
  mutate(
    month = month(month_column, label = TRUE))

# Goup by month and sensor ID
# Take the mean of the value column
Soil_temp |> 
  mutate(Month = month(TIMESTAMP)) |> 
  group_by(Month, Sensor_ID, Plot) |>                             
  summarize(mean_temp = mean(Value, na.rm = TRUE)) |>       
  ggplot(aes(x = Month, y = mean_temp, group = Sensor_ID, color = Plot)) +  
  geom_line() + 
  geom_point() + labs( title = "Soil Temperature by Sensor ", x = "Month", y = "Average Temp")

