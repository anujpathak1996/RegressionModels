# Linear polynomial regression is called linear because in regression we talk about
# the cofficients not the x. y = b0 + b1(x^2). 
# polynomial linear regression is special case of Multiple linear regression

# Polynomial Regression
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

#Fitting linear regresssion model
lin_reg = lm(formula = Salary ~ ., data = dataset)

#Fitting polynomial regression model
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ ., data = dataset)

#Visualizing the linear regression model
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (linear regression') + 
  xlab('Levels') + 
  ylab('Salary')

#Visualizing the polynomial regression model
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') + 
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial regression') + 
  xlab('Levels') + 
  ylab('Salary')

#Predicting a new set results with Linear Regression
#Now we want to compute salary on a level which is not given
y_pred_lr = predict(lin_reg, data.frame(Level = 6.5))

#Predicting a new set results with Polynomial Regression
y_pred_pr = predict(poly_reg, data.frame(Level = 6.5,
                                         Level2 = 6.5^2,
                                         Level3 = 6.5^3,
                                         Level4 = 6.5^4))

#data.frame(column in which you want to add = value)

# Visualising the Regression Model results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Regression Model)') +
  xlab('Level') +
  ylab('Salary')