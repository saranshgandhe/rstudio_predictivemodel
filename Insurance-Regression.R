library(dplyr)          # data wrangling
library(ggplot2)        # graphing
library(caret)          # machine learning functions
library(MLmetrics)      # machine learning metrics
library(car)            # VIF calculation
library(lmtest)         # linear regression model testing
library(GGally)         # correlation plot

insurance <- read.csv(file = "insurance.csv")

#Checking for NA Values in dataset
summary(is.na(insurance))


summary(insurance)

#Boxplot visualizations for independent variables relation with the dependent variable, Charges
boxplot(charges~sex, data = insurance)
boxplot(charges~region, data = insurance)
boxplot(charges~children, data = insurance)
boxplot(charges~smoker, data = insurance)
#Scatter Plot Visualization for BMI with charges
plot(insurance$bmi,insurance$charges, xlab = "Body Mass Index", ylab = "Insurance Charges", col = ifelse(insurance$sex == "female", "black", "pink"))
legend("topleft", pch = c(1,1), c("female","male"), col = c("black", "pink"))


#Using dummy variables to convert categorical variables to numerical
attach(insurance)
insurance$sex <- ifelse(insurance$sex == 'male', 1, 0)
insurance$smoker <- ifelse(insurance$smoker == 'yes', 1, 0)

insurance$southeast <- ifelse(insurance$region == 'southeast', 1, 0)
insurance$southwest <- ifelse(insurance$region == 'southwest', 1, 0)
insurance$northeast <- ifelse(insurance$region == 'northeast', 1, 0)


#Scaling/Normalizing variables to (0,1) for accuracy
scalpm1 = function(x){(x-(min(x)+max(x))/2)/(.5*(max(x)-min(x)))}
sage = scalpm1(age)
insurance$sage = sage

scalpm1 = function(x){(x-(min(x)+max(x))/2)/(.5*(max(x)-min(x)))}
sbmi = scalpm1(bmi)
insurance$sbmi = sbmi
schildren = scalpm1(children)
insurance$schildren= schildren

#detach(insurance)
#detach(insurance1)


#Running the Multiple Linear Regression Model
regModel1 = lm(charges ~ sage + sex + sbmi + 
                schildren + smoker + southwest + 
                southeast + northeast, data = insurance)

summary(regModel1)
plot(regModel1)
#removing insignificant variable 'sex'
regModel2 = lm(charges ~ sage + sbmi + 
                schildren + smoker + southwest + 
                southeast + northeast, data = insurance)

summary(regModel2)
plot(regModel2)
#removing insignificant variable 'northeast'
regModel3 = lm(charges ~ sage + sbmi + 
                 schildren + smoker + southwest + 
                 southeast, data = insurance)
summary(regModel3)

plot(regModel3)

#Running Gaussian Process Regression Model
library(caret)
Sinsurance= data.frame(charges,sage,sbmi,schildren,smoker,insurance$southwest,
                       insurance$southeast,insurance$northeast)

trainMeth = trainControl(method = "cv", number = 10)
CVgpModel <- train(charges ~ ., data = Sinsurance, 
                   method = "gaussprRadial",
                   sigma = 2.83,
                   trControl = trainMeth)
predict_charges = predict(CVgpModel,Sinsurance[2:7])
plot(predict_charges,Sinsurance$charges)
print(CVgpModel)


#Running Neural Net Regression Model
library(caret)
set.seed(123)

trainMethNN = trainControl(method = "cv", number = 10)
library(tictoc)
tic()


CVnnModel <- train(charges ~ ., data = insurance, 
method = "nnet",
trControl = trainMethNN,
linout = TRUE)

CVnnModel <- train(charges ~ ., data = insurance, 
method = "neuralnet",
trControl = trainMethNN,
linear.output = TRUE,
threshold = 0.1)

CVnnModel <- train(charges ~ ., data = insurance, 
                   method = "avNNet",
                   trControl = trainMeth,
                   linout = TRUE)
# end time monitoring
toc()





