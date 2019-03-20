# Apriori

# Data processing
# install.packages('arules')
library(arules)
dataset <- read.csv('Market_Basket_Optimisation.csv', header = F)
dataset <- read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = T)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

# Training Apriori on the dataset
rules <- apriori(data = dataset, parameter = list(support=0.003, confidence= 0.4))

# Visualizing the results
inspect(sort(rules, by = 'lift')[1:10])
