# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('Main_Dataset1.csv',na.strings=c("","na"))


#first set the current directory as the pwd
#files->select desired folder->More->Set as working directory


#Taking care of missing data

  #is.na=>checks if missing
  #returns true if value in the column Age is missing else, false
  #ifelse(test,yes,no)
  #na.rm => whether NA values should be removed before computation proceeds
dataset$Occupied = ifelse(is.na(dataset$Occupied ),
                     mean(dataset$Occupied,na.rm = TRUE),
                     dataset$Occupied)


#Encoding categorical data
#c: Basically an array, combine
#factor(col to encode, what to replace in column, what to replace it with)
#G:1,R:2,V:3,M:4,Q:5
dataset$Lot = factor(dataset$Lot,
                         levels = c('G','R','V','M','Q'),
                         labels = c(1, 2, 3,4,5))

dataset$Day = factor(dataset$Day,
                     levels = c('Monday','Tuesday','Wednesday','Thursday'),
                     labels = c(1, 2, 3,4))
# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools) #we can either select an installed package like this, or by simply checking it in the
#packages section
set.seed(123) #same as random state in python, so that we get same actual results
split= sample.split(dataset$Occupied,SplitRatio = 0.8)
split

#SplitRatio: How much do we want to put in the TRAINING SET
#Split will return TRUE for 8 entries i.e. the elements that is to be put in the training set according to the split ratio

#Inserting all the 'TRUE' splits into the training set
training_set = subset(dataset,split==TRUE)

#Inserting all false entries into test set
test_set = subset(dataset,split==FALSE)

View(training_set)
View(test_set)


# install.packages('caTools')

# Feature Scaling

#training_set = scale(training_set)
# test_set = scale(test_set)
#The following two will return an error as the datasets are not numeric, but insted
#they are 'factors(), that we used to convert the string to numbers
#So, we will exclude those categories from feature scaling

training_set[,3:4] = scale(training_set[,3:4])
test_set[,3:4] = scale(test_set[,3:4])
dataset[,3:4]=scale(dataset[,3:4])

plot(dataset)

#Simple Linear Regression
regressor = lm(formula = Occupied ~ Time,data=training_set)
summary(regressor)
ggplot()+
  geom_point(aes(x = training_set$Time, y = training_set$Occupied),
             colour = 'red') +
  geom_line(aes(x = training_set$Time, y = predict(regressor,newdata = training_set)),
            colour = 'blue') +
  ggtitle('Time vs Occupance(Simple Linear Regression)')+
  xlab('Time')+
  ylab('Occupance')

regressorday = lm(formula = Occupied ~ Day,data=training_set)
summary(regressorday)
ggplot()+
  geom_point(aes(x = training_set$Day, y = training_set$Occupied),
             colour = 'red') +
  geom_line(aes(x = training_set$Day, y = predict(regressorday,newdata = training_set)),
            colour = 'blue') +
  ggtitle('Day vs Occupance(Simple Linear Regression)')+
  xlab('Day')+
  ylab('Occupance')

regressorlot = lm(formula = Occupied ~ Lot,data=training_set)
summary(regressorlot)

ggplot()+
  geom_point(aes(x = training_set$Lot, y = training_set$Occupied),
             colour = 'red') +
  geom_line(aes(x = training_set$Lot, y = predict(regressorlot,newdata = training_set)),
            colour = 'blue') +
  ggtitle('Lot vs Occupance(Simple Linear Regression)')+
  xlab('Lot')+
  ylab('Occupance')

plot(dataset$Occupied,regressor)
#Multiple Linear Regression
multiregressor = lm(formula = Occupied ~ . ,
               data = training_set)

summary(multiregressor)
plot(multiregressor)
predict.lm(multiregressor)



library(ggplot2)
ggplot()+
  geom_point(aes(x = training_set$Time, y = training_set$Occupied),
             colour = 'red') +
  geom_line(aes(x = training_set$Time, y = predict(multiregressor,newdata = training_set)),
            colour = 'blue') +
  ggtitle('Time vs Occupied')+
  xlab('Time')+
  ylab('Occupancy')



#Polynomial Regression
polyregressor<-lm(Occupied~Lot+Day+Time+I(Lot^2)+I(Day^2)+I(Time^2),dataset)

y_pred = predict(multiregressor,newdata = test_set)
ggplot()+
  geom_point((dataset$Occupied,
             colour = 'red') +
  geom_line(aes(x = training_set$Time, y = predict(regressor,newdata = training_set)),
            colour = 'blue') +
  ggtitle('Time vs Occupance(Linear Regression)')+
  xlab('Time')+
  ylab('Occupancy')

install.packages("e1071")
#Load Library
library(e1071)

#Scatter Plot
plot(dataset$Occupied)
ggplot()+
  geom_point(aes(x = training_set$Day, y = training_set$Occupied),
             colour = 'red') +
  ggtitle('Day vs Occupance(General)')+
  xlab('Day')+
  ylab('Occupancy')


#Regression with SVM
modelsvm = svm(Occupied~.,dataset)

#Predict using SVM regression
predYsvm = predict(modelsvm, dataset)

#Overlay SVM Predictions on Scatter Plot
points(dataset$Day, predYsvm, col = "red", pch=16)

##Calculate parameters of the SVR model

#Find value of W
W = t(modelsvm$coefs) %*% modelsvm$SV

#Find value of b
b = modelsvm$rho

## RMSE for SVR Model

#Calculate RMSE
install.packages("hydroGOF")

#Load Library
library(hydroGOF)
RMSEsvm= rmse(predYsvm,dataset$Occupied)

## Tuning SVR model by varying values of maximum allowable error and cost parameter

#Tune the SVM model
OptModelsvm=tune(svm, Occupied~Day, data=dataset,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)

plot(dataset, pch=16)
points(dataset$Day, predYsvm, col = "blue", pch=3)

points(dataset$Day, predYsvm, col = "blue", pch=3, type="l")
