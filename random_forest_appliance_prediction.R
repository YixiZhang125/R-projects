library(rpart)
library(tidyverse)
library(randomForest)
library(zoo)

# Setting working directory to the input file directory (where the data lives on this session)
setwd('~/Downloads/BENV0091/Predict appliances/DATA')

# Load the data
train <- read_csv('vw_train.csv')
test <- read_csv('vw_test.csv')

#Convert appliances to factors
train$appliances <- as.factor(train$appliances)

#Impute NAs
imp_train <- train %>% mutate(
    `appliances` = na.locf(appliances)
)

# Fit the model
random_forest <- randomForest(appliances ~ . -id,
                              data = imp_train,
                              ntree = 1001,
                              mtry = 20)

# Print training accuracy
predictions <- predict(random_forest, imp_train, type = 'class')
training_accuracy <- mean(predictions == imp_train$appliances, na.rm = TRUE)
print(paste0("Training classification accuracy: ",
             round(100*training_accuracy, 2), '%'))


# Produce the predictions
submission <- test %>%
    transmute(id = id,
              appliances = predict(random_forest,
                                   test,
                                   type = 'class'))

# Save results
write_csv(submission, 'my_submission.csv')
