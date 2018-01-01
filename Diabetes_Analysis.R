data <- read.csv(file ="C:/Users/Pranay Anand/Desktop/Diabetes.csv", head=TRUE, sep = ",")
View(data)
library(caTools)
split <- sample.split(data,SplitRatio = 0.8)#spliting dataset into 80:20 ratio and taking 80
split
training_data <- subset(data,split="TRUE")
testing_data <- subset(data, split = "FALSE")
# glm = Generalized Linear Model
# type(Y value) vs all the X , training data , family of Y is Binomial i.e. Yes or No
logit_model <- glm(type~.,training_data,family = binomial)

summary(logit_model)

# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -9.263621   1.240838  -7.466 8.29e-14 ***
# these are beta0 , beta1 , .. values 
# *** - 99.9% Confidence
# **  - 99% Confidence
# *   - 95% Confidence
# .   - 90% Confidence
# AIC should be minimum

# Null deviance: 420.30  on 331  degrees of freedom
# Null Deviance shows how well the response variable is 
# predicted by model that includes only intercepts
# Residual deviance: 285.17  on 324  degrees of freedom
# Residual Deviance shows how well the response variable is 
# predicted with inclusion of independent Variables 

# Removing the Non Significant variable AIC should decrese & Residual should be decresing or stable
#logit_model <- glm(type~.-age,training_data,family = binomial)

summary(logit_model)
res <- predict(logit_model,training_data,type = "response")
res <- predict(logit_model,testing_data,type = "response")

print(res)

# Accuracy of the model
# Creting a Confusing Matrix 199
# 0.5 is the thresold value
# table(output, predicted values = res)
table(Actual_value = testing_data$type, Predicted_value = res > 0.5)

#
library(ROCR)
ROCRPred <- prediction(res,training_data$type)
ROCRPref <- performance(ROCRPred,"tpr","fpr")

# Receiver Operating Characteristic Curve(ROC)
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1))

table(Actual_value = testing_data$type, Predicted_value = res > 0.3)
