
# Do not blindly follow these steps check the assumptions first
# Assumption Of Linear Regression
# 1 - Linearity
# 2 - Homoscedasticity
# 3 - Multivariate Normality
# 4 - Independence of errors
# 5 - Lack of multicollinearity

# we cannot include the categorical data in our equation hence we use Dummy variables
# see the concept of it from lectures , dummy variable trap

# Model types (see the video section 5 lecture 41) main focus - backward elimination

#Multiple Linear Regression
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State, 
                         levels = c('New York', 'California', 'Florida'),
                         labels = c(1, 2, 3)) 

#spliting data into training set and test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8) #we train dependent var
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Multiple linear regression to the training set
regressor = lm(formula = Profit ~ ., data = training_set)
# regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State)
# can replace all other column dependency with .
# summary(regressor)
# from summary we can see that on R.D.Spend is of significance hence we can
# convert the profit ~ . to profit ~ R.D.Spend

# Predicting the test set results
y_pred = predict(regressor, newdata = test_set)

# Building the optimal model using backward elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset) # we can choose dataset or training set
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset) #Removing State
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend, 
               data = dataset) #Removing Administration
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend, 
               data = dataset) #Removing Marketing Spend
summary(regressor)
