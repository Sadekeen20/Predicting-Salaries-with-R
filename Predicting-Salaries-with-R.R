#building simple linear regression model

#import the dataset
dataset= read.csv('Predicting_salaries.csv')

#splitting our dataset into test and training sets

library("caTools")
set.seed(123)
split=sample.split(dataset$AnnualSalary,SplitRatio = 3/4) #75% to train

training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)


#Fitting simple linear regression to train and test set
linearregessor=lm(formula = AnnualSalary ~YearsOfExperience, data = training_set)

summary(linearregessor)

#predicting testing set result
Y_pred=predict(linearregessor, newdata = testing_set)


summary(Y_pred)



#visualizing the training set results

library(ggplot2)
library(scales)

ggplot()+
    geom_point(aes (x=training_set$YearsOfExperience,y=training_set$AnnualSalary),colour='red')+
    geom_line(aes(x=training_set$YearsOfExperience,y=predict(linearregessor, newdata = training_set)),colour='navy')+
             ggtitle('Annual Salaries of Data Scientist vs Experience in Years')+
    xlab('Years of Experience')+
    ylab('Annual Salary')+
    scale_x_continuous(limits=c(0,12))+
    scale_y_continuous(limits = c(0,150000))

#visualizing the test set results

ggplot()+
    geom_point(aes (x=testing_set$YearsOfExperience,y=testing_set$AnnualSalary),colour='red')+
    geom_line(aes(x=training_set$YearsOfExperience,y=predict(linearregessor, newdata = training_set)),colour='navy')+
    ggtitle('Annual Salaries of Data Scientist vs Experience in Years')+
    xlab('Years of Experience')+
    ylab('Annual Salary')+
    scale_x_continuous(limits=c(0,12))+
    scale_y_continuous(limits = c(0,150000))
