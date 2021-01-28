dat<-read.csv("airline.csv")
library(tidyverse)

#DATA PREPARATION
colSums(is.na(dat))

name<-unique(dat$City1)
dat$City1=factor(dat$City1,
                 levels =name,
                 labels =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
                 )
name2<-unique(dat$City2)
dat$City2=factor(dat$City2,
                 levels = name2,
                 labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)
                )


airline<-dat %>%select(3:12)
airl<-cor(airline)

library(corrplot)
corrplot(airl)
cor(dat$RPKs,dat$Aircraft_Trips)
cor(dat$RPKs,dat$Passenger_Trips)
cor(dat$RPKs,dat$Seats)
cor(dat$RPKs,dat$ASKs)
#SPLITTING THE DATASET

library(caTools)
set.seed(123)
split<-sample.split(dat$RPKs,SplitRatio=0.8)
training_set<-subset(dat,split=TRUE)
test_set<-subset(dat,split=FALSE)




#REGRESSION 
regressor<-lm(formula = RPKs~Passenger_Trips+Aircraft_Trips+Seats+ASKs,data = training_set)
summary(regressor)
#residual vs fitted values
plot(regressor)
#checks for valid regression model
mean(regressor$residuals)

library(olsrr)
ols_plot_resid_hist(regressor)


y_pred<-predict(regressor,newdata = test_set)
y_pred
# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$Aircraft_Trips+training_set$Passenger_Trips+training_set$Seats+training_set$ASKs, y = training_set$RPKs),
             colour = 'red') +
  geom_line(aes(x = training_set$Aircraft_Trips+training_set$Passenger_Trips+training_set$Seats+training_set$ASKs, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Predictora vs Revenue Per KM (Training set)') +
  xlab('Predictors') +
  ylab('Revenue Per KM')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$Aircraft_Trips+test_set$Passenger_Trips+test_set$Seats+test_set$ASKs, y = test_set$RPKs),
             colour = 'red') +
  geom_line(aes(x = training_set$Aircraft_Trips+training_set$Passenger_Trips+training_set$Seats+training_set$ASKs, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('predictors vs Revenue per KM(Test set)') +
  xlab('predictors') +
  ylab('Revenue Per KM')

