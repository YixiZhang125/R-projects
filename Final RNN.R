
library(tibble)
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(keras)
library(tensorflow)

train_data_pc <- read_csv("data/train_data_pc.csv")
val_data_pc <- read_csv("data/val_data_pc.csv")
test_data_pc <- read_csv("data/test_data_pc.csv")
glimpse(train_data_pc)


##Plot to have a rough understanding of the data


library(ggplot2)
q <- ggplot(train_data_pc, aes(x = 1:nrow(train_data_pc), y = `target`)) + geom_line()
q
r <- ggplot(train_data_pc[1:500,], aes(x = 1:500, y = `target`)) + geom_line()
r

##preprocessed 7 PCAs
names <- c('target','V8','V1','V2','V3','V4','V5','V6','V7')

names(train_data_pc) <- names
names(val_data_pc) <- names
names(test_data_pc) <- names

data <- rbind(train_data_pc,val_data_pc,test_data_pc)

data <- as.matrix(data)
train_data_pc <- as.matrix(train_data_pc)
val_data_pc <- as.matrix(val_data_pc)
test_data_pc <- as.matrix(test_data_pc)

##RNN model
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 100, step = 24) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,1]
    }           
    list(samples, targets)
  }
}

lookback <- 2
step <- 1
delay <- 24
batch_size <- 1000

train_gen <- generator(
  train_data_pc,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = NULL,
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  val_data_pc,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = NULL,
  shuffle = FALSE,
  step = step,
  batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- round((nrow(val_data_pc) - lookback) / batch_size)

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- round((nrow(test_data_pc) - lookback) / batch_size)

evaluate_naive_method <- function() {
  batch_maes <- c()
  for (step in 1:val_steps) {
    c(samples, targets) %<-% val_gen()
    preds <- samples[,dim(samples)[[2]],2]
    mae <- mean(abs(preds - targets))
    batch_maes <- c(batch_maes, mae)
  }
  print(mean(batch_maes))
}

evaluate_naive_method()


#kWh_mae <- 0.47 * val_std[[2]]




#LSTM
model <- keras_model_sequential() %>% 
  layer_lstm(units = 64,
             input_shape = list(NULL, dim(train_data_pc)[[-1]])) %>%
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = "adam",
  loss = "mae"
)

history <- model %>% fit(
  train_gen,
  steps_per_epoch = 100,
  epochs = 30,
  validation_data = val_gen,
  validation_steps = val_steps,
) 

#plot
lookback <- 2
step <- 1
delay <- 24
batch_size <- 2813

test_gen <- generator(
  test_data_pc,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = NULL,
  shuffle = FALSE,
  step = step,
  batch_size = batch_size
)

test_dt <- test_gen()

V1 = seq(1, length(test_dt[[2]]))

plot_data <-
  as.data.frame(cbind(V1, test_dt[[2]]))

inputdata <- test_dt[[1]][,,]


pred_out <- model %>%
  predict(inputdata)


plot_data <-
  cbind(plot_data, pred_out)

p <-
  ggplot(plot_data, aes(x = V1, y = V2)) + geom_line(colour = "blue", size = 0.5,alpha=0.4)
p <-
  p + geom_line(aes(x = V1, y = pred_out), colour = "red", size = 0.5 ,alpha=0.4)

p

##result
library(MLmetrics)

pred <- plot_data[,3]
ob <- plot_data[,2]

MAPE(pred,ob)
RMSE(pred,ob)

write.csv(plot_data, "data/house153.csv", row.names = FALSE)

