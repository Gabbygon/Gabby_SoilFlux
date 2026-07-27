
library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(patchwork)
library(naniar)

#read in data from soil temp 15 parquet file 
soil_temp15 <- read_parquet("L2_data/TMP_F_2025_soil-temp-15cm_L2_v2-1.parquet")


#Summarizing soil temp file 
head(soil_temp15)
names(soil_temp15)
dim(soil_temp15)
str(soil_temp15)
print(colnames(soil_temp15))
summary(soil_temp15)

# Select just the columns we want to work with 
soil_temp15 <- soil_temp15 |> select (Plot, TIMESTAMP, Instrument_ID, Sensor_ID, Location, Value)

# Line Chart of all the data
# Randomly sub_sample the data to make plotting faster
soil_temp15 |>
  slice_sample(n = 10000) |>
  ggplot(aes(x = TIMESTAMP, y = Value)) +
  geom_point(color = "brown") + 
  labs( title = "Soil Temperature", x = "TimeStamp", y = "Temperature") + 
  theme_minimal()

#group by month and sensor
#take the mean of the value column
soil_temp15 |> 
  mutate(Month = month(TIMESTAMP)) |> 
  group_by(Month, Sensor_ID) |>                             
  summarize(mean_temp = mean(Value, na.rm = TRUE)) |>       
  ggplot(aes(x = Month, y = mean_temp, group = Sensor_ID)) +  
  geom_line() + 
  geom_point()

#bar graph of all soil temp data 
soil_temp15 |> 
  mutate(Month = month(TIMESTAMP)) |> 
  group_by(Month) |> 
  summarize(mean_temp = mean(Value, na.rm = TRUE)) |> 
  ggplot(aes(x = Month, y = mean_temp)) +  
  geom_col()

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
  labs(title = "By Month", x = "Month", y = "Temperature") + 
  theme_minimal() + theme(asix.text.x = element_text(angle = 45, hjust= 1), 
  legend.position = "none")

#graph of temp and month with colors
soil_temp15 |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x = month, y = Value, color = Value)) + 
  geom_point(na.rm = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid ="blue", midpoint = 15, limits = c(0, 30)) +
  labs(title = "By Month", x = "Month", y = "Temperature") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust= 1))

#separate  
ggplot(
  soil_temp15, aes(x = month, y = Value)) + 
  geom_boxplot() + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "Temperature by Month", y = "Temperature") + 
  theme_minimal()

#patchwork of each month for box plot 
m1 <- ggplot(
  filter(soil_temp15, month == "Jan"), 
  aes( x = month, y = Value)) + 
  geom_boxplot() + 
  labs(title= "January")

m2 <- ggplot(
  filter(soil_temp15, month == "Feb"), 
  aes( x = month, y = Value)) + 
  geom_boxplot() + 
  labs(title= "February")

m3 <- ggplot(
   filter(soil_temp15, month == "Mar"), 
   aes( x = month, y = Value)) + 
   geom_boxplot() + 
   labs(title= "March")

m4 <- ggplot(
   filter(soil_temp15, month == "Apr"), 
   aes( x = month, y = Value)) + 
   geom_boxplot() + 
   labs(title= "April")

m5 <- ggplot(
    filter(soil_temp15, month == "May"), 
    aes( x = month, y = Value)) + 
    geom_boxplot() + 
    labs(title= "May")

m6 <- ggplot(
    filter(soil_temp15, month == "Jun"), 
    aes( x = month, y = Value)) + 
    geom_boxplot() + 
    labs(title= "June")

m7 <- ggplot(
    filter(soil_temp15, month == "Jul"), 
    aes( x = month, y = Value)) + 
    geom_boxplot() + 
    labs(title= "July")

m8 <- ggplot(
    filter(soil_temp15, month == "Aug"), 
    aes( x = month, y = Value)) + 
    geom_boxplot() + 
    labs(title= "August")

m9 <- ggplot(
    filter(soil_temp15, month == "Sep"), 
    aes( x = month, y = Value)) + 
    geom_boxplot() + 
    labs(title= "September")

m10 <- ggplot(
    filter(soil_temp15, month == "Oct"), 
    aes( x = month, y = Value)) + 
    geom_boxplot() + 
    labs(title= "October")

m11 <- ggplot(
    filter(soil_temp15, month == "Nov"), 
    aes( x = month, y = Value)) + 
    geom_boxplot() + 
    labs(title= "November")

m12 <- ggplot(
    filter(soil_temp15, month == "Dec"), 
    aes( x = month, y = Value)) + 
    geom_boxplot() + 
    labs(title= "December")

#Combining the plots into one singular graph 
m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10 + m11 + m12 + 
  plot_layout(ncol = 6)

#per-month variability 
soil_temp15 |> 
  group_by(month) |>
  # compute mean and s.d. for each month
  summarise(mean_value = mean(Value, na.rm = TRUE), 
            sd_value = sd(Value, na.rm = TRUE)) |>
  ggplot(aes(x = month, y = mean_value)) +
  geom_point() + 
  geom_line(group = 1, linewidth = 1, linetype = 2) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value))

# SOIL volumetric Water Content
# Read in data from soil vwc parquet file
  soil_vwc <- read_parquet("L2_data/TMP_F_2025_soil-vwc-15cm_L2_v2-1.parquet") |> 
  # just like temperature, select only certain columns we care about
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
  slice_sample(n = 10000) |>
  ggplot(aes(x = TIMESTAMP, y = Value)) +
  geom_point(color = "brown") + 
  labs( title = "Soil VWC", x = "TimeStamp", y = "Columetric water content") + 
  theme_minimal()

#Time and value 
soil_vwc |> 
  group_by(TIMESTAMP) |>                             
  summarize(mean_vwc = mean(Value, na.rm = TRUE)) |>       
  ggplot(aes(x = TIMESTAMP, y = mean_vwc, group = TIMESTAMP)) +  
  geom_line() + 
  geom_point()

#convert to date type and extract the month for wvc
soil_vwc<- soil_vwc |> 
  mutate(
    month_column = as.Date(TIMESTAMP))
soil_vwc <- soil_vwc |> 
  mutate(
    month = month(month_column, label = TRUE))

#grpah of each month for vwc
ggplot(
  soil_vwc, aes(x = month, y = Value)) + 
  geom_boxplot() + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "VWC by Month", y = "Temperature") + 
  theme_minimal()

#bar graph of all data vwc 
soil_vwc |> 
  mutate(Month = month(TIMESTAMP)) |> 
  group_by(Month) |> 
  summarize(mean_temp = mean(Value, na.rm = TRUE)) |> 
  ggplot(aes(x = Month, y = mean_temp)) +  
  geom_col()

#graph with colors 
soil_vwc |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x = month, y = Value, color = Value)) + 
  geom_point(na.rm = TRUE) +
  scale_color_gradient2(low = "brown", high = "blue",mid ="brown", midpoint = 5, limits = c(0, 30)) +
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
  rename(Value_vwc15 = Value) ->
  teros_combined

teros_combined <- teros_combined |>  select (-month_column.x, -month_column.y, -month.x, -month.y)

teros_combined |> 
  left_join(f_soil_EC_15, 
            by = c("Plot", "TIMESTAMP", "Instrument_ID", "Sensor_ID", "Location"), 
            relationship = "one-to-one") |> 
  rename(Value_ec15 = Value) ->
  teros_combined

teros_combined <- teros_combined |>  select (-research_name, -N_avg, -N_drop, -Value_MAC)

#plotting relationships 
flood_start <- ymd_hms("2026-1-1 00:00:00")  
flood_end   <- ymd_hms("2026-12-31 23:45:00")  

ggplot(teros_combined, aes(x = TIMESTAMP)) +
  geom_line(aes(y = Value_temp15, color = "Temperature")) +
  geom_line(aes(y = Value_vwc15 * 70, color = "VWC")) +  # rescaled 
  annotate("rect", xmin = flood_start, xmax = flood_end, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "green") +
  facet_wrap(~ Location) +
  labs(y = "Temperature/VWC", color = "Variable")


teros_combined <- teros_combined |> 
  mutate(
    loc_row = substr(Location, 1, 1),       # A-J 
    loc_col = as.numeric(substr(Location, 2, 2))  # number
  )

ggplot(teros_combined, aes(x = loc_col, y = loc_row, fill = Value_temp15)) +
  geom_tile() +
  facet_wrap(~ TIMESTAMP) +
  labs(x = "Columns", y = "Location", fill = "Temp")


 # EXAMPLE
 
teros_combined

 example_temp <- tibble(TS = 1:3, Value = 1:3)
 example_vwc <- tibble(TS = 1:3, Value = 4:6)
 
 # This doesn't do what we want, because the dataframes BOTH have "Value" columns
 example_temp |> left_join(example_vwc, by = "TS")
 
 # Prepare datasets for merge
 example_temp <- rename(example_temp, Value_temp = Value)
 example_vwc <- rename(example_vwc, Value_VWC = Value)
 
 example_temp |>
   left_join(example_vwc, by = "TS")
 

#trying to do graphs
 scale_factor <- 42.15
 
  ggplot(teros_combined, aes(x = TIMESTAMP)) +
   geom_point(aes(y = Value_temp15), color = "red", alpha = 0.05, size = 1) +
   geom_point(aes(y = Value_vwc15 * scale_factor), color = "blue", alpha = 0.05, size = 1) +
   geom_smooth(aes(y = Value_temp15), method = "lm", se = FALSE, color = "red") +
   geom_smooth(aes(y = Value_vwc15 * scale_factor), method = "lm", se = FALSE, color = "blue") +
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

#reading all of the parquet files 2026 
#Control soil plot 
#Soil EC 
C_soil_EC_30 <- read_parquet("L2_data/TMP_C_2026_soil-EC-30cm_L2_v___.parquet")
C_soil_EC_15 <- read_parquet("L2_data/TMP_C_2026_soil-EC-15cm_L2_v___.parquet")
C_soil_EC_5 <- read_parquet("L2_data/TMP_C_2026_soil-EC-5cm_L2_v___.parquet")
#Soil salinity
C_soil_sal_30 <- read_parquet("L2_data/TMP_C_2026_soil-salinity-30cm_L2_v___.parquet")
C_soil_sal_15 <- read_parquet("L2_data/TMP_C_2026_soil-salinity-15cm_L2_v___.parquet")
C_soil_sal_5 <- read_parquet("L2_data/TMP_C_2026_soil-salinity-5cm_L2_v___.parquet")
#Soil Temperature 
C_soil_temp_30 <- read_parquet("L2_data/TMP_C_2026_soil-temp-30cm_L2_v___.parquet")   
C_soil_temp_15 <- read_parquet("L2_data/TMP_C_2026_soil-temp-15cm_L2_v___.parquet")  
C_soil_temp_5 <- read_parquet("L2_data/TMP_C_2026_soil-temp-5cm_L2_v___.parquet")   
#Soil VWC
c_soil_vwc_30 <- read_parquet("L2_data/TMP_C_2026_soil-vwc-30cm_L2_v___.parquet")
c_soil_vwc_15 <- read_parquet("L2_data/TMP_C_2026_soil-vwc-15cm_L2_v___.parquet")
c_soil_vwc_5 <- read_parquet("L2_data/TMP_C_2026_soil-vwc-5cm_L2_v___.parquet")

#Freshwater soil plot
#soil EC
f_soil_EC_30 <- read_parquet ("L2_data/TMP_F_2026_soil-EC-30cm_L2_v___.parquet")
f_soil_EC_15 <- read_parquet("L2_data/TMP_F_2026_soil-EC-15cm_L2_v___.parquet")
f_soil_EC_5 <- read_parquet("L2_data/TMP_F_2026_soil-EC-5cm_L2_v___.parquet")
#Soil salinity 
f_soil_sal_30 <- read_parquet("L2_data/TMP_F_2026_soil-salinity-30cm_L2_v___.parquet")
f_soil_sal_15 <- read_parquet("L2_data/TMP_F_2026_soil-salinity-15cm_L2_v___.parquet")
#Soil Temperature
f_soil_temp_30 <- read_parquet("L2_data/TMP_F_2026_soil-temp-30cm_L2_v___.parquet") 
f_soil_temp_15 <- read_parquet("L2_data/TMP_F_2026_soil-temp-15cm_L2_v___.parquet") 
f_soil_temp_5 <- read_parquet("L2_data/TMP_F_2026_soil-temp-5cm_L2_v___.parquet") 
#Soil VWC
f_soil_vwc_30 <- read_parquet("L2_data/TMP_F_2026_soil-vwc-30cm_L2_v___.parquet")  
f_soil_vwc_15 <- read_parquet("L2_data/TMP_F_2026_soil-vwc-15cm_L2_v___.parquet")  

#Saltwater Plot 
#Soil EC
s_soil_EC_30 <- read_parquet("L2_data/TMP_S_2026_soil-EC-30cm_L2_v___.parquet")      
s_soil_EC_15 <- read_parquet("L2_data/TMP_S_2026_soil-EC-15cm_L2_v___.parquet")   
s_soil_EC_5 <- read_parquet("L2_data/TMP_S_2026_soil-EC-5cm_L2_v___.parquet")  
#Soil salinity
s_soil_sal_30 <- read_parquet("L2_data/TMP_S_2026_soil-salinity-30cm_L2_v___.parquet")
s_soil_sal_15 <- read_parquet("L2_data/TMP_S_2026_soil-salinity-15cm_L2_v___.parquet")
s_soil_sal_5 <- read_parquet("L2_data/TMP_S_2026_soil-salinity-5cm_L2_v___.parquet")
#Soil temp 
s_soil_temp_30 <- read_parquet("L2_data/TMP_S_2026_soil-temp-30cm_L2_v___.parquet")
s_soil_temp_15 <- read_parquet("L2_data/TMP_S_2026_soil-temp-15cm_L2_v___.parquet")
s_soil_temp_5 <- read_parquet("L2_data/TMP_S_2026_soil-temp-5cm_L2_v___.parquet")
#soil VWC 
s_soil_vwc_30 <- read_parquet("L2_data/TMP_S_2026_soil-vwc-30cm_L2_v___.parquet")
s_soil_vwc_15 <- read_parquet("L2_data/TMP_S_2026_soil-vwc-15cm_L2_v___.parquet")
s_soil_vwc_5 <- read_parquet("L2_data/TMP_S_2026_soil-vwc-5cm_L2_v___.parquet")


#Visualizing N/As
vis_miss(s_soil_vwc_5)
s_soil_vwc_5 |> drop_na(Value)

gg_miss_var(C_soil_EC_15)

#Graphing Value without N/A for visualization 
clean_data_soil_vwc5 <- s_soil_vwc_5 |> drop_na(Value)
ggplot(clean_data_soil_vwc5, aes(x = TIMESTAMP, y = Value)) + geom_point()

#trying to graph with the N/A for visualization 
s_soil_vwc_5 |> 
  mutate(Value = fct_na_value_to_level(factor(Value), level = "Missing Data")) |> 
  ggplot(aes(x = Value)) + 
  geom_bar(fill = "steelblue")

s_soil_vwc5 |> 
  mutate(Value = fct_na_value_to_level(factor(Value), level = "Missing Data")) |> 
  ggplot(aes(x = Value)) + geom_bar(fill = "steelblue")

df_clean |> ggplot(aes(x = TIMESTAMP, y = Value)) + geom_point(color = "brown")


#________________
#2026 Soil temp
#read in data from soil temp 15 parquet file 
#Soil Temperature 
C_soil_temp_30 <- read_parquet("L2_data/TMP_C_2026_soil-temp-30cm_L2_v___.parquet")   
C_soil_temp_15 <- read_parquet("L2_data/TMP_C_2026_soil-temp-15cm_L2_v___.parquet")  
C_soil_temp_5 <- read_parquet("L2_data/TMP_C_2026_soil-temp-5cm_L2_v___.parquet") 

head(C_soil_temp_5)
names(C_soil_temp_5)
dim(C_soil_temp_5)
str(C_soil_temp_5)
print(colnames(C_soil_temp_5))
summary(C_soil_temp_5)


# Select just the columns we want to work with 
C_soil_temp_5 <- soil_temp15 |> select (Plot, TIMESTAMP, Instrument_ID, Sensor_ID, Location, Value)
C_soil_temp_15 <- soil_temp15 |> select (Plot, TIMESTAMP, Instrument_ID, Sensor_ID, Location, Value)
C_soil_temp_30 <- soil_temp15 |> select (Plot, TIMESTAMP, Instrument_ID, Sensor_ID, Location, Value)

# Randomly sub_sample the data to make plotting faster
C_soil_temp_15 |>
  slice_sample(n = 10000) |>
  ggplot(aes(x = TIMESTAMP, y = Value)) +
  geom_point(color = "brown") + 
  labs( title = "Soil Temperature", x = "TimeStamp", y = "Temperature") + 
  theme_minimal()

filter(C_soil_temp_15, !is.na(Value))
C_soil_temp_15 |> ggplot(aes(x = TIMESTAMP, y = Value)) + geom_point(color = "red")

C_soil_temp_5$TIMESTAMP <- as.Date(C_soil_temp_5$TIMESTAMP, format = "%Y-%m-%d")
clean_c_soil_temp_5 <- C_soil_temp_5 |> filter(!is.na(Value))

ggplot(
  clean_c_soil_temp_5, aes(
    x = TIMESTAMP, y = Value)) + 
  geom_line() + 
  labs(x = "Date", y = "Value", title = "Tempetature over time") +
  theme_minimal()

ggplot(clean_c_soil_temp_5 , aes(x = TIMESTAMP, y = Value)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") + 
  labs(x = "Date", y = "Value") + theme_minimal()

clean_c_soil_temp_5 |>  
  slice_sample(n = 100) |> 
  ggplot(
    clean_c_soil_temp_5 , aes(x = TIMESTAMP, y = Value)) + 
  geom_line() +
  scale_x_date(
    date_breaks = "1 week", date_labels = "%b %d") + 
  labs(x = "Date", y = "Value") + 
  theme_minimal()

#2026 Soil temp 5cm
f_soil_temp_5 |>
  slice_sample(n = 10000) |>
  ggplot(aes(x = TIMESTAMP, y = Value)) +
  geom_point(color = "brown") + 
  labs( title = "Soil Temperature", x = "TimeStamp", y = "Temperature") + 
  theme_minimal()

#just the ones we want to work with 
f_soil_temp_5 <- f_soil_temp_5 |> select (Plot, TIMESTAMP, Instrument_ID, Sensor_ID, Location, Value)

#convert to date type and extract the month
f_soil_temp_5<- f_soil_temp_5 |> 
  mutate(
    month_column = as.Date(TIMESTAMP))
f_soil_temp_5 <- f_soil_temp_5 |> 
  mutate(
    month = month(month_column, label = TRUE))