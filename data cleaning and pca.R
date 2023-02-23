require(purrr)
require(tidyverse)
require(lubridate)
require(dplyr)
require(fastDummies)
library(tibble)
library(matrixStats)
library(stats)
library(psych)

# read in data ------------------------------------------------------------
file_names <- dir("data/archive/halfhourly_dataset/halfhourly_dataset", full.names = TRUE)

# Df wide
df_wide <- do.call(rbind, lapply(file_names, read.csv)) %>% 
  mutate(tstp = ymd_hms(tstp)) %>% 
  arrange(tstp, ascending = TRUE) %>% # this fix weird final columns in df_wide
  pivot_wider(values_from = energy.kWh.hh., names_from = tstp)

# Selecting the best 100 ids
df_100 <- df_wide %>% 
  mutate(per_NA = rowSums(is.na(.)) / ncol(.), .after = LCLid) %>% # adds columns with percent of NA for each row
  arrange(per_NA) %>%  # Arranges from lowest NA percent to highest
  head(100) # Select 100 households with lowest NA percent

id_100 <- unique(df_100$LCLid)

# Convert into long format
df_long <- df_100 %>% 
  pivot_longer(cols = -c('LCLid','per_NA'), values_to = 'energy_kWh', names_to = 'time') %>%
  mutate(time = ymd_hms(time)) %>% 
  select(LCLid, time, energy_kWh)
#filter(time >= as.Date("2012-01-01 00:30:00") & time <= as.Date("2014-02-28 00:00:00"))

# read and clean data for weather -----------------------------------------

df_weather <- read.csv("data/weather_hourly_darksky.csv") %>%
  mutate(lubridate::ymd_hms(time), .before = visibility, .keep = 'unused') %>%
  rename(date = `lubridate::ymd_hms(time)`) %>%
  select(-summary)
df_weather_dummy <- dummy_cols(df_weather, select_columns=c('precipType', 'icon'),   remove_selected_columns = TRUE)

# Extracting only clean id_246
id_246 <- df_long %>%
  filter(LCLid == "MAC000246" & energy_kWh != "Null") %>%
  drop_na(energy_kWh) %>% 
  rename(date = time) %>%
  mutate(month = lubridate::month(date),
         week = lubridate::week(date),
         wday = lubridate::wday(date),
         yday= lubridate::yday(date),
  )
sum(is.na(id_246$energy_kWh))

# Turn time data cyclical -------------------------------------------------
time <- id_246$date

# Create dictionary, convert each half hour to the corresponding half hour of the day
halfhour_num_list <- c("0 0" = "0",
                       "0 30" = "1",
                       "1 0" = "2",
                       "1 30" = "3",
                       "2 0" = "4",
                       "2 30" = "5",
                       "3 0" = "6",
                       "3 30" = "7",
                       "4 0" = "8",
                       "4 30" = "9",
                       "5 0" = "10",
                       "5 30" = "11",
                       "6 0" = "12",
                       "6 30" = "13",
                       "7 0" = "14",
                       "7 30" = "15",
                       "8 0" = "16",
                       "8 30" = "17",
                       "9 0" = "18",
                       "9 30" = "19",
                       "10 0" = "20",
                       "10 30" = "21",
                       "11 0" = "22",
                       "11 30" = "23",
                       "12 0" = "24",
                       "12 30" = "25",
                       "13 0" = "26",
                       "13 30" = "27",
                       "14 0" = "28",
                       "14 30" = "29",
                       "15 0" = "30",
                       "15 30" = "31",
                       "16 0" = "32",
                       "16 30" = "33",
                       "17 0" = "34",
                       "17 30" = "35",
                       "18 0" = "36",
                       "18 30" = "37",
                       "19 0" = "38",
                       "19 30" = "39",
                       "20 0" = "40",
                       "20 30" = "41",
                       "21 0" = "42",
                       "21 30" = "43",
                       "22 0" = "44",
                       "22 30" = "45",
                       "23 0" = "46",
                       "23 30" = "47")


halfhour_num <- c()

for (i in 1:length(time)) {
  halfhour_num[i] <- halfhour_num_list[paste(lubridate::hour(id_246$date[i]),
                                             lubridate::minute(id_246$date[i]))]
}

id_246_hh <- cbind(id_246, halfhour = as.integer(halfhour_num))

# Merge weather and consumption data together -----------------------------

# weather is hour, so look at hour data
id_246_h <- id_246_hh %>%
  subset(lubridate::minute(date) == "0") %>%
  mutate(hour = lubridate::hour(date))

# function for turning hour data cyclical
# We know the lag is 24, from the ACF plot
prep_df_h <- function(df){
  df %>%
    transmute(LCLid = LCLid,
              date = date,
              energy_kWh = energy_kWh, # Lag is incorporated in the code for RNN
              sinh = sin(2 * pi * hour / 24),
              cosh = cos(2 * pi * hour / 24),
              sinmonth = sin(2 * pi * month / 12),
              cosmonth = cos(2 * pi * month / 12),
              sinwday = sin(2 * pi * wday / 7),
              coswday = cos(2 * pi * wday / 7),
              sinweek = sin(2 * pi * week / 52),
              cosweek = cos(2 * pi * week / 52),
              sinyday = sin(2* pi * yday / 365),
              cosyday = cos(2* pi * yday / 365)) %>%
    na.omit()
}

df_model_merge <- prep_df_h(id_246_h) %>%
  left_join(df_weather_dummy, by=c("date")) %>%
  na.omit()

# To write a file with info
write.csv(df_model_merge, "data/df_merge.csv", row.names = FALSE)


#___________________________________________________________________
#PCA
df <- df_model_merge %>%
  mutate(date = ymd_hms(date),
         energy_kWh = as.numeric(energy_kWh)) %>%
  select(-'LCLid',-'precipType_rain',-'precipType_snow',-'icon_clear-day',
         -'icon_clear-night',-'icon_cloudy',-'icon_fog',-'icon_partly-cloudy-day', 
         -'icon_partly-cloudy-night',-'icon_wind')

# plotting all the time series
ggplot(df, aes(x = date,
               y = energy_kWh)) + 
  geom_line(size = .1)

#Normalization of data previous to PCA
#______________________________________________________________________________
# Convert the data frame to a matrix and remove the first column (target variable)
data <- data.matrix(df[,-1])
# Calculate the mean and standard deviation of the columns
mean <- colMeans(data)
std <- colSds(data)
# Scale the columns using the mean and standard deviation
data <- scale(data, center = mean, scale = std)
# Define a function that normalizes a vector
normalize <- function(x){
  return(( x - min(x)) / (max(x) - min(x)))
}
max <- colMaxs(data)
min <- colMins(data)
# Normalize the columns using the normalize() function
data <- apply(data, 2, normalize)

#______________________________________________________________________________
#Principal Component Analysis with function princomp()
pca <- princomp(data[, -1], cor = TRUE) # Excluding the target variable
# Print the results
print(pca)
summary(pca)
#scree plot
screeplot(pca, type = "barplot", main = "Scree plot", npcs = 16)
#For the scree plot
ev <- pca$sdev^2
ev
# According to the scree plot the elbow point is achieved at the principal component 5.
# According to the Kaiser criterion, only the six first components possess enough variance to make them worth using.
# To achieve 80% (commonly accepted as a good value) of explained variance will use 7 principal components. Important to note the 7th value
# of variance is almost 1, so very close to the kaiser creterion

#So, let's use 7 Principal Components. 
#RECASTING THE DATA TO THE NEW AXES

selected_pc <- data.frame(pca$loadings[, 1:7])
data_transform <- predict(pca, data[, -1])

#create a new data frame with electricity and the 10 principal components

row_nums <- seq_len(nrow(data))
df_pca <- data.frame(
  dates = df[, 1],
  energy = data[, 1],
  pc = data_transform[, 1:7],
  row_num = row_nums) %>% 
  select(-'row_num')