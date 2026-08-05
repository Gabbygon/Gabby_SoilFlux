# Summer TEMPEST soil analysis
# Gabby Gonzalez 2026

library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(patchwork)
library(naniar)
library(compasstools)

# ------------------------------------------------------
# ------ READ IN SOIL TEMP, VWC, AND EC AND PLOT/SUMMARIZE
# ------------------------------------------------------# --------Plot----------------------------------------------

# We use the "compasstools" package (loaded above) to load data
# Specifically, compasstools::read_L2_variable()
soil_temp15 <- read_L2_variable("soil-temp-15cm", path = "L2_data/")

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
  labs( title = "Soil Temperature", x = "TimeStamp", y = "Temperature") + 
  theme_minimal()

#group by month and sensor
#take the mean of the value column
soil_temp15 |> 
  mutate(Month = month(TIMESTAMP)) |> 
  group_by(Month, Sensor_ID, Plot) |>                             
  summarize(mean_temp = mean(Value, na.rm = TRUE)) |>       
  ggplot(aes(x = Month, y = mean_temp, group = Sensor_ID, color = Plot)) +  
  geom_line() + 
  geom_point()

#convert to date type and extract the month
soil_temp15<- soil_temp15 |> 
  mutate(
    month_column = as.Date(TIMESTAMP))
soil_temp15 <- soil_temp15 |> 
  mutate(
    month = month(month_column, label = TRUE))

#Box plot graph of temp and month 
ggplot(
  soil_temp15, aes(x = month, y = Value, group = month, fill = as.factor(month))) + 
  geom_boxplot() + 
  labs(title = "Temperature by Month", x = "Month", y = "Temperature") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust= 1), 
  legend.position = "none")

#graph of temp and month with colors
soil_temp15 |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x = month, y = Value, color = Value)) + 
  geom_point(na.rm = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid ="blue", midpoint = 15, limits = c(0, 30)) +
  labs(title = "By Month", x = "Month", y = "Temperature") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust= 1))

#Temperature by month   
ggplot(
  soil_temp15, aes(x = month, y = Value)) + 
  geom_boxplot() + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "Temperature by Month", y = "Temperature") + 
  theme_minimal()

#per-month variability 
soil_temp15 |> 
  group_by(Plot, month) |>
  # compute mean and s.d. for each month
  summarise(mean_value = mean(Value, na.rm = TRUE), 
            sd_value = sd(Value, na.rm = TRUE)) |>
  ggplot(aes(x = month, y = mean_value, color = Plot)) +
  geom_point() + 
  geom_line(linetype = 2, aes(group = Plot)) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value))

# SOIL volumetric Water Content 
# Read in data from soil vwc parquet file

soil_vwc <- read_L2_variable("soil-vwc-15cm", path = "L2_data/") |> 
  select (Plot, TIMESTAMP, Instrument_ID, Sensor_ID, Location, Value)

#Summarizing swc file 
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
  labs( title = "Soil VWC", x = "TimeStamp", y = "Volumetric Water content") + 
  theme_minimal()

#convert to date type and extract the month for wvc
soil_vwc<- soil_vwc |> 
  mutate(
    month_column = as.Date(TIMESTAMP))
soil_vwc <- soil_vwc |> 
  mutate(
    month = month(month_column, label = TRUE))

#graph of each month for vwc
ggplot(
  soil_vwc, aes(x = month, y = Value)) + 
  geom_boxplot() + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "Moisture by Month", y = "Temperature", x = "Month") + 
  theme_minimal()

#Boxplot with points (choose between the ones w/ or w/out)
soil_vwc |> 
  slice_sample(n = 1000) |> 
ggplot(
 aes(x = month, y = Value)) + 
  geom_boxplot() + geom_jitter(pch = 19, width = 0.2, aes(color = month)) + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "Moisture by Month", y = "Temperature", x = "Month") + 
  theme_minimal()

#graph with colors 
soil_vwc |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x = month, y = Value, color = Value)) + 
  geom_point(na.rm = TRUE) +
  scale_color_gradient2(low = "brown", high = "blue",mid ="brown", midpoint = 5, limits = c(0, 30)) +
  labs(title = "By Month", x = "Month", y = "VWC") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust= 1))

# SOIL EC 
# Read in data from soil EC parquet file

f_soil_EC_15 <- read_L2_variable("soil-EC-15cm", path = "L2_data/") |> 
  select (Plot, TIMESTAMP, Instrument_ID, Sensor_ID, Location, Value)

#Summarizing EC file 
head(f_soil_EC_15)
names(f_soil_EC_15)
dim(f_soil_EC_15)
str(f_soil_EC_15)
print(colnames(f_soil_EC_15))
summary(f_soil_EC_15)

# Line Chart of all the data
# Randomly sub_sample the data to make plotting faster
f_soil_EC_15 |>
  # this dataset is VERY large so just select 1000 random rows to plot
  slice_sample(n = 1000) |>
  ggplot(aes(x = TIMESTAMP, y = Value, color = Plot)) +
  geom_point() + 
  # add a 'smoother' -- a smooth line that follows the data to help
  # audience see trend
  geom_smooth(linetype = 2) +
  labs( title = "Soil EC", x = "TimeStamp", y = "EC") + 
  theme_minimal()

#convert to date type and extract the month for wvc
f_soil_EC_15<- f_soil_EC_15 |> 
  mutate(
    month_column = as.Date(TIMESTAMP))
f_soil_EC_15 <- f_soil_EC_15 |> 
  mutate(
    month = month(month_column, label = TRUE))

#graph of each month for vwc
ggplot(
  f_soil_EC_15, aes(x = month, y = Value)) + 
  geom_boxplot() + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "EC by Month", y = "EC", x = "Month") + 
  theme_minimal()

#Boxplot with points (choose between the ones w/ or w/out)
f_soil_EC_15 |> 
  slice_sample(n = 1000) |> 
  ggplot(
    aes(x = month, y = Value)) + 
  geom_boxplot() + geom_jitter(pch = 19, width = 0.2, aes(color = month)) + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "EC by Month", y = "EC", x = "Month") + 
  theme_minimal()

#graph with colors 
f_soil_EC_15 |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x = month, y = Value, color = Value)) + 
  geom_point(na.rm = TRUE) +
  labs(title = "By Month", x = "Month", y = "VWC") + 
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
  left_join(f_soil_EC_15, 
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
   geom_smooth(aes(y = Value_temp15), method = "lm", se = FALSE, color = "red") +
   geom_smooth(aes(y = Value_vwc15 * scale_factor), method = "lm", se = FALSE, color = "blue") +
   scale_y_continuous(name = "Temperature (°C)",
                      sec.axis = sec_axis(~ . / scale_factor, name = "VWC")) +
   labs(title = "Soil Temperature and Water Content Over Time", x = "Timestamp") +
   theme_minimal()
 
 
 scale_factor <- 42.15
 
 teros_combined |> 
   slice_sample(n = 1000) |> 
   ggplot(aes(x = TIMESTAMP)) +
   geom_point(aes(y = Value_vwc15), color = "red", alpha = 0.05, size = 1) +
   geom_point(aes(y = Value_ec15 * scale_factor), color = "blue", alpha = 0.05, size = 1) +
   geom_smooth(aes(y = Value_vwc15), method = "lm", se = FALSE, color = "red") +
   geom_smooth(aes(y = Value_ec15 * scale_factor), method = "lm", se = FALSE, color = "blue") +
   scale_y_continuous(name = "Temperature (°C)",
                      sec.axis = sec_axis(~ . / scale_factor, name = "VWC")) +
   labs(title = "Soil Temperature and Water Content Over Time", x = "Timestamp") +
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
teros_combined |> group_by(Location) |> summarise(mean_temp5 = mean(Value, na.rm = TRUE)) |> 
  separate(Location, into = c("grid_letter", "grid_number"), sep = 1) |> 
  ggplot(aes(x = grid_letter, y = grid_number, fill = mean_temp5)) + 
  geom_tile() +
  scale_fill_viridis_c(option = "H", begin = 0.2) +
  theme_bw() +
  labs(title = "Saltwater Plot Soil Temperature - 15cm", fill = "temperature C")


#calculating the coefficient of variability
sd(soil_temp15$Value, na.rm = TRUE) / mean(soil_temp15$Value, na.rm = TRUE) -> Cv

#Saltwater special plot 15cm-5
teros_combined |> group_by(Location) |> summarise(mean_temp5 = mean(Value, na.rm = TRUE)) |> 
  separate(Location, into = c("grid_letter", "grid_number"), sep = 1) |> 
  ggplot(aes(x = grid_letter, y = grid_number, color = mean_temp5)) + 
  geom_point(size = 3) +
  scale_color_viridis_c(option = "H", begin = 0.2) +
  theme_bw() +
  labs(title = "Saltwater Plot Soil Temperature - 15cm", color = "temperature C")





#Other way 
#KAM addition 28.07.2026
f_soil_temp_30 %>%
  rename(Value_temp30 = Value) %>%
  filter(! is.na(Value_temp30)) -> f_soil1_30

f_soil_EC_30 %>%
  rename(Value_ec30 = Value) %>%
  filter(! is.na(Value_ec30)) -> f_soil2_30

f_soil_vwc_30 |> 
  rename(Value_vwc30 = Value)
  filter(! is.na(Value_vwc30)) -> f_soil3_30
  
  
#merging 
s_soil_temp_5 |> 
  rename(Value_temp5 = Value) |> 
  filter(! 
           is.na(Value_temp5)) -> s_soil1 

s_soil_EC_5 |> 
  rename(Value_EC5 = Value) |> 
  filter(! is.na(Value_EC5)) -> s_soil2

s_soil_vwc_5 |> 
  rename(Value_vwc5 = Value) |> 
  filter(! is.na(Value_vwc5)) -> s_soil3

s_soil1 |> 
  left_join(s_soil2,
            by = c("Plot", "TIMESTAMP", "Instrument_ID", "Location")) %>%
  left_join(s_soil3, 
            by = c("Plot", "TIMESTAMP", "Instrument_ID", "Location")) -> s_5_soilcombined

bind_rows(s_soil_temp_5, s_soil_EC_5, s_soil_vwc_5) |>
  filter(!is.na(Value)) |>
  pivot_wider(names_from = research_name, values_from = Value) -> s_5_combined_long
  



#_______
#Trying to make the bar chart more appealing 
soil_temp15 |> 
  mutate(Month = month(TIMESTAMP)) |> 
  group_by(Month) |> 
  summarize(mean_temp = mean(Value, na.rm = TRUE)) |> 
  ggplot(aes(x = Month, y = mean_temp)) +  
  geom_col()


#double check for this code
soil_temp15 |> 
  slice_sample(n = 100) |> 
  ggplot(soil_temp15, aes(x = TIMESTAMP, fill = Plot)) + 
  geom_bar() + xlab('Timeline') + ylab('Value') + ggtitle('Value over time') 

#Temperature by plot 
ggplot(
  soil_temp15, 
  aes(x = Plot, y = Value)) + 
  geom_boxplot() + 
  xlab('Plot') + 
  ylab('Temperature') + 
  ggtitle('Temperature by plot')