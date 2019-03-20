## Multiple linear regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Categorical data
dataset$State <- factor(dataset$State, 
                        levels = c('New York', 'California', 'Florida'), 
                        labels = c(1,2,3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set[, 2:3] = scale(training_set[, 2:3])
# test_set[, 2:3] = scale(test_set[, 2:3])

# Fitting multiple linear regression to the training set
regressor <- lm(formula = Profit ~ ., data = training_set)
summary(regressor)

# Predict the test set result
y_pred <- predict(regressor, newdata = test_set)

# Building the optimal model using the Backward Elimination
regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, 
                data = training_set)
summary(regressor)

regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, 
                data = training_set)
summary(regressor)

regressor <- lm(formula = Profit ~ R.D.Spend + Marketing.Spend, 
                data = training_set)
summary(regressor)

regressor <- lm(formula = Profit ~ R.D.Spend, 
                data = training_set)
summary(regressor)


## Polynomial Regression 

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[, 2:3]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Purchased, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set[, 2:3] = scale(training_set[, 2:3])
# test_set[, 2:3] = scale(test_set[, 2:3])

# Fitting the Linear Regression to the Dataset
lin_reg <- lm(formula = Salary ~ Level, data = dataset)
summary(lin_reg)

# Fitting the Polynomial Regression to the Dataset
dataset$Level2 <- dataset$Level^2
dataset$Level3 <- dataset$Level^3
dataset$Level4 <- dataset$Level^4
poly_reg <- lm(formula = Salary ~ ., data = dataset)
summary(poly_reg)

# Visualizing the Linear Regression results
library(ggplot2)
ggplot() +
  geom_point(data = dataset, aes(x=Level, y=Salary), colour = 'red') +
  geom_line(data = dataset, aes(x=Level, y=predict(lin_reg, newdata = dataset)), colour = 'dark green')


# Visualizing the Polynomial Regression results
ggplot() +
  geom_point(data = dataset, aes(x=Level, y=Salary), 
             colour = 'red') +
  geom_line(data = dataset, aes(x=Level, y=predict(poly_reg, newdata = dataset)), 
            colour = 'dark green')

# Predicting a new result with linear regression
y_pred <- predict(lin_reg, data.frame(Level=6.5))

# Predicting a new result with polynomial regression
y_pred <- predict(poly_reg, data.frame(Level=6.5,
                                      Level2=6.5^2,
                                      Level3=6.5^3,
                                      Level4=6.5^4))

