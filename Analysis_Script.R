
library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(patchwork)

# Read in data from soil temp parquet file 
file_path <- "TMP_F_2025_soil-temp-15cm_L2_v2-1.parquet"
soil_temp15 <- read_parquet(file_path)

#Summarizing 
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
  geom_point(color = "pink") + 
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


#boxplot for all of data
soil_temp15 |> 
  mutate(Month = month(TIMESTAMP)) |> 
  group_by(Month) |> 
  summarize(mean_temp = mean(Value, na.rm = TRUE)) |> 
  ggplot(aes(x = Month, y = mean_temp)) +  
  geom_boxplot()


soil_temp15 |> 
       mutate(Month = month(TIMESTAMP)) |> 
       group_by(Month) |> 
       summarize(mean_temp = mean(Value, na.rm = TRUE)) |> 
       ggplot(aes(x = Month, y = mean_temp)) +  
       geom_col()

#convert to date type and extract the month
soil_temp15<- soil_temp15 |> 
  mutate(
    month_column = as.Date(TIMESTAMP)
  )
soil_temp15 <- soil_temp15 |> 
  mutate(month = month(month_column, label = TRUE))


#Boxplot
ggplot(
  soil_temp15, aes(x = month, y = Value, group = month, fill = as.factor(month))) + 
  geom_boxplot() + 
  labs(litle = "By Month", x = "Month", y = "Temperature") + 
  theme_minimal() + theme(asix.text.x = element_text(angle = 45, hjust= 1), 
  legend.position = "none")

#Box plot with colors 
soil_temp15 |> 
  slice_sample(n = 1000) |> 
  ggplot(aes(x = month, y = Value, color = Value)) + 
  geom_point(na.rm = TRUE) +
  scale_color_gradient2(low = "blue", high = "red",mid ="blue", midpoint = 15, limits = c(0, 30)) +
  #geom_boxplot() + scale_fill_gradient(low = "blue", high= "red") + 
  labs(title = "By Month", x = "Month", y = "Temperature") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust= 1))

#separate  
ggplot(
  soil_temp15, aes(x = month, y = Value)) + 
  geom_boxplot() + 
  facet_wrap(~ month, scales = "free_x", ncol = 6) + 
  labs(title = "Temperature by Month", y = "Temperature") + 
  theme_minimal()

#patchwork and combining
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

#Combining the plots 
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
  geom_line(group = 1, linewidth = 2, linetype = 2) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value))

#________________________________________

# SOIL volumetric Water Content
# Read in data from soil vwc parquet file
file_path2 <- "TMP_F_2025_soil-vwc-15cm_L2_v2-1.parquet"
soil_vwc <- read_parquet(file_path2) |>  
  # just like temperature, select only certain columns we care about
  select (Plot, TIMESTAMP, Instrument_ID, Sensor_ID, Location, Value)

#Summarizing 
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
  geom_point(color = "Green") + 
  labs( title = "Soil VWC", x = "TimeStamp", y = "Columetric water content") + 
  theme_minimal()

#Time and value 
soil_vwc |> 
  group_by(TIMESTAMP) |>                             
  summarize(mean_vwc = mean(Value, na.rm = TRUE)) |>       
  ggplot(aes(x = TIMESTAMP, y = mean_vwc, group = TIMESTAMP)) +  
  geom_line() + 
  geom_point()

# Merge the data
# TEROS sensors are clustered -- a single sensor simultaneously
# measures temperature, water content, and electrical conductivity
soil_temp15 |> 
  # both data frames have 'Value' columns, which we need to be able to 
  # distinguish after merging. Start by renaming the temperature value...
  rename(Value_temp15 = Value) |> 
  # ...do the join...
  left_join(soil_vwc, by = c("Plot", "TIMESTAMP", "Instrument_ID", "Sensor_ID", "Location"), 
            relationship = "one-to-one") |> 
  # ...and now rename the vwc value column
  rename(Value_vwc15 = Value) ->
  teros_combined
  

 # EXAMPLE
 
 example_temp <- tibble(TS = 1:3, Value = 1:3)
 example_vwc <- tibble(TS = 1:3, Value = 4:6)
 
 # This doesn't do what we want, because the dataframes BOTH have "Value" columns
 example_temp |> left_join(example_vwc, by = "TS")
 
 # Prepare datasets for merge
 example_temp <- rename(example_temp, Value_temp = Value)
 example_vwc <- rename(example_vwc, Value_VWC = Value)
 
 example_temp |>
   left_join(example_vwc, by = "TS")
 
 