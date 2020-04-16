library(tidyverse)
library(caret)
theme_set(theme_classic())

# Load the data
spine <- read.csv("dataset-spine.csv")[c(1:13)]
#spine.pr <- prcomp(spine[c(1:12)], 
                  # center=TRUE, 
                 #  scale=TRUE)
#l1_var <- as.double(summary(spine.pr)$importance[,1][2])
#l2_var <- as.double(summary(spine.pr)$importance[,2][2])
#print(sprintf('Wariancja danych zawarta w pierwszym składniku wiodącym: %s%%', format(round(l1_var, 2), nsmall = 2)))
#print(sprintf('Wariancja danych zawarta w drugim składniku wiodącym: %s%%', format(round(l2_var, 2), nsmall = 2)))

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

require(MASS)
# Fit the model

model <- lda(formula=class~., data=train.transformed)
print(model)

plot(model)

# Make predictions
predictions <- model %>% predict(train.transformed)
# Model accuracy
print(mean(predictions$class==test.transformed$class))

plot_title <- ''
plot(predictions$x[,1],
     ylab="LD1", 
     main=plot_title, 
     label="none")
grid(nx=NULL,
     ny=NULL, 
     col="lightgray", 
     lty="dotted")
text(predictions$x[,1], label="o", col=c(as.numeric(predictions$class)))
abline(v=0, lty="dotted")
abline(h=0, lty="dotted")

plot_title <- 'Współczynniki'
barplot(c(coef(model)), main=plot_title, xlab = "coef", ylab = "LD1", ylim=c(-1,1))
grid(nx=NULL,
     ny=NULL, 
     col="lightgray", 
     lty="dotted")
abline(v=0, lty="dotted", col=c(as.numeric(predictions$class)))

#plot(predictions$x[,1],predictions.class$x[,2])

