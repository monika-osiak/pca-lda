library(tidyverse)
library(caret)
theme_set(theme_classic())

# Load the data
spine <- read.csv("dataset-spine.csv")

# Split the data into training (80%) and test set (20%)
set.seed(123)
training.samples <- spine$class %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- spine[training.samples, ]
test.data <- spine[-training.samples, ]

# Estimate preprocessing parameters
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

library(MASS)
# Fit the model
model <- lda(train.transformed$class~., data=train.transformed)
plot(model)
# Make predictions
predictions <- model %>% predict(test.transformed)
# Model accuracy
mean(predictions$class==test.transformed$class)

