#Simple Linear Regression

dataset = read.csv('Salary_Data.csv')

#75% is considered good for training set

#training and test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 0.75) #We train dependent variable
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Fitting simple linear regression to the training set
regressor = lm(formula = Salary ~ YearsExperience, data = training_set) 
#lm() is a built in function
# lm(formula = dependent variable ~ independent variable (means the dependent
# variable is propotional to independent variable), data on which we want to train
# our linear regression model)
# to view summary of regressor use , summary(regressor_name) in console.
# Summary tells various imp stuffs like statistical significance by stars
# if it have 3 stars then it is highly significant.
# lower the p value the more strong is the rel between dep and independent var
# 5% is good threshold p value i.e. when we are below 5%, independent var is highly significant.

# Predicting the test set results
y_pred = predict(regressor, newdata = test_set)
# predict(model, newdata = test_set (set for which we want to predict)

# Visualizing the Training set results

# install.packages('ggplot2') This library used for plotting 
library(ggplot2)
ggplot() + 
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') + # first observation points
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
              colour = 'blue') + # then regression line
 # we use predict fun to predict the values of y corresponding to each point of training_set
  ggtitle('YearsExperience Vs Salary (training_set') + # then titles
  xlab('YearsExperience') + # then the labels
  ylab('Salary') 

# Visualizing the Test set results
ggplot() + 
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary), colour = 'red') + 
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
              colour = 'blue') +
  ggtitle('YearsExperience Vs Salary (test_set') + 
  xlab('YearsExperience') +
  ylab('Salary') 
  