# Multiple linear regression

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

# Feature Scaling(R???h???^?k???w?g?۱a?S?x?Y??)
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
