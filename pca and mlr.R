require(tidyverse)
require(tidyr)
require(ggplot2)
require(dplyr)
require(lubridate)
require(MLmetrics) # for MAPE
require(Metrics) # for RMSE

read_data <- function(){
  # Read in data ------------------------------------------------------------
  # This is same df_merge from the 'cleaning_data.R'
  df <- read.csv("data/df_merge.csv")
  
  # get the id of the house, so we can double check which house we are using
  house_id <<- df$LCLid[1]
  
  print(paste("ID: ", house_id))
  df <- df %>%
    select(-'LCLid')
  
  # check if need to deal with NAs
  print(paste("Number of NA: ", sum(is.na(df))))
  
  # first date
  print(paste("first date: ", head(df$date, 1)))
  # last date
  print(paste("last date: ", tail(df$date, 1)))
  
  return(df)
  
}


# Train / Test split ------------------------------------------------------
train_test_split <- function(df, start_date, end_date){
  if (start_date == "auto"){
    train_start_date <- df[1,]$date
  } else {
    train_start_date <- start_date
  }
  
  train_end_date = end_date
  
  
  # split data into training based on the split dates
  pca.train <- df %>%
    filter( train_start_date <= date  & date < train_end_date)
  
  pca.test <- df %>%
    filter(date >= train_end_date)
  
  test_preds_dates <- pca.test$date
  
  print(paste("Number of test hour predictions: ", length(test_preds_dates)))
  return(list(pca.train = pca.train, pca.test = pca.test))
  
}

# PCA ---------------------------------------------------------------------
train_pca <- function(training_df){
  # compute Principal Components based on the training set
  pca.train <- training_df %>%
    select(-'date')
  
  prin_comp <- prcomp(pca.train, scale. = T)
  
  # Want to find variance of each principal component, so we know how many components to select
  std_dev <- prin_comp$sdev # gives standard deviation
  pr_var <- std_dev^2
  prop_varex <- pr_var/sum(pr_var) #find proportion of variance each principal component holds
  
  cum_prop_var <- cumsum(prop_varex)
  print('Cumulative proportion of variance')
  print(cum_prop_var)
  
  print(paste("Chosen num of PCA: ", min(which(cum_prop_var > 0.9))))
  
  return(list(prin_comp = prin_comp,
              num_pca = min(which(cum_prop_var > 0.9)))) # Choose number of PCA coomponents explains 90% of variance
}


get_train_test_pc <- function(num_pc, train_df, prin_comp, pca.test){
  # transforming traing data using principal components calculated on test data
  train.data <- data.frame(energy_kWh = train_df$energy_kWh, prin_comp$x[, 1:num_pc])
  
  # transforming test data using principal components calculated on test data
  pca.test <- pca.test %>%
    select(-'date')
  
  test.prin_comp <- predict(prin_comp, newdata = pca.test)
  test.data <- data.frame(energy_kWh = pca.test$energy_kWh, test.prin_comp[, 1:num_pc])
  
  return(list(train.data = train.data,
              test.data = test.data))
}



# Linear regression -------------------------------------------------------
prediction_models <- function(train.data, test.data){
  
  linear_model <- lm(energy_kWh ~ ., train.data)
  
  linear_train_preds <- train.data %>%
    transmute(actual = energy_kWh,
              linear_model = predict(linear_model, .))
  
  linear_test_preds <- test.data %>%
    transmute(actual = energy_kWh,
              linear_model = predict(linear_model, .))
  
  return(list(l_train_preds = linear_train_preds,
              l_test_preds = linear_test_preds))
  
}


# Evalution metrics -------------------------------------------------------
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  
  RMSE = sqrt(SSE/nrow(df))
  
  MAPE = MAPE(predicted, true)
  
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square,
    MAPE = MAPE
  )
}


# Main --------------------------------------------------------------------
house_id <- 'x'
df <- read_data()
pca_splits <- train_test_split(df, "auto", "2014-02-21")
pca_info <- train_pca(pca_splits$pca.train)
p_components <- get_train_test_pc(pca_info$num_pca+1, pca_splits$pca.train, pca_info$prin_comp, pca_splits$pca.test)
preds <- prediction_models(p_components$train.data, p_components$test.data)

eval_results(preds$l_train_preds$actual, preds$l_train_preds$linear_model, pca_splits$pca.train)
eval_results(preds$l_test_preds$actual, preds$l_test_preds$linear_model, pca_splits$pca.test)

preds$l_test_preds <- preds$l_test_preds %>%
  transmute(date = lubridate::ymd_hms(pca_splits$pca.test$date),
            actual = actual,
            linear_model = linear_model)

write.csv(preds$l_test_preds, paste0("data/pred_", house_id, ".csv"))

ggplot(data=preds$l_test_preds, mapping = aes(x = date, y = actual)) +
  geom_line() +
  geom_point(data=preds$l_test_preds, mapping = aes(x = date, y = linear_model) ,colour='red') +
  labs(title="Household Energy Consumption",
       subtitle=paste("House:", house_id),
       y="Energy Consumption (kWh)",
       x="Date",
       legend="test" )+
  theme(axis.text=element_text(size=15),
        axis.title =element_text(size=15),
        plot.title=element_text(size=25),
  )