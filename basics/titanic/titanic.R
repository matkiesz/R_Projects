install.packages('ROCR')
install.packages('titanic')
library(ROCR)
library(titanic)

data("titanic_train")
?titanic_train

head(titanic_train)
str(titanic_train)
summary(titanic_train)

titanic_data <- titanic_train[c("Survived","Sex","Age","Pclass")]

mapply(anyNA,titanic_data) # Sprawdzam czy sa puste wiersze

titanic_data <-na.omit(titanic_data) #omijam wiersze z lukami

titanic_data$Sex <- factor(titanic_data$Sex) # zminiam zmienna kategoryczna na liczbe
titanic_data$Pclass <- factor(titanic_data$Pclass)

glm_model <-glm(Survived~ Sex + Age +Pclass, data = titanic_data, family = "binomial")

surv_prob <-predict(glm_model, titanic_data, type = "response")

titanic_results <-cbind(titanic_data, surv_prob)

table(titanic_data$Survived, surv_prob > 0.7) # tabela kondyngencji

ROCRpred <- prediction(surv_prob,titanic_data$Survived)
ROCRperf <- performance(ROCRpred, "tpr","fpr")

par(mfrow = c(1,1))
plot(ROCRperf, colorize = TRUE)

auc <-performance(ROCRpred, measure = "auc")
auc <- auc@y.values
auc
