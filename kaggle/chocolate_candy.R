choclate_competition <-read.csv("candy-data.csv")

library(ROCR)
library(corrplot)
library(glmnet)

# laduje dane i analizuje sklad danych
candy_data <-read.csv("candy-data.csv")
sub_candy_data <-subset(candy_data, select = -c(competitorname)) #subset bez nazw produktów

head(candy_data)
dim(candy_data)
str(candy_data)
summary(candy_data)

corrplot(cor(sub_candy_data), method = "number", type = "upper" , number.cex = 0.55)

#dziele dane na zbiór trenujacy i testujacy
set.seed(2018)
sample <- sample.int(n = nrow(sub_candy_data), size = floor(.70*nrow(sub_candy_data)), replace = F)
candy_train <- sub_candy_data[sample, ]
candy_test  <- sub_candy_data[-sample, ]

#


# tworzymy model regresji logistycznej ze wszystkimi zmiennymi za wyjatkiem nazwy produktu
glm_model <- glm(chocolate ~ caramel + peanutyalmondy , data = candy_train, family = "binomial")   # wynik po testowaniu 0,7
summary(glm_model)
glm_model2 <- glm(chocolate ~ caramel + peanutyalmondy + fruity + nougat, data = candy_train, family = "binomial") # wynik po testowaniu 0.95
glm_model3 <- glm(chocolate ~ caramel + peanutyalmondy + fruity + nougat + sugarpercent  , data = candy_train, family = "binomial") 

chocolate_prob2 <- predict(glm_model2, candy_test, type="response")
chocolate_prob3 <- predict(glm_model3, candy_test, type="response")
#dodaje kolumne z prawdopodobienstwem z modelu glm
chocolate_results2 <- cbind(candy_test, chocolate_prob2)
chocolate_results3 <- cbind(candy_test, chocolate_prob3)
# tworze macierz kontyngencji dla progu 50%( zakladajac prawdopodobienstwo >50% to batonik czekoladowy)
table(chocolate_results2$chocolate, chocolate_prob > 0.5)
table(chocolate_results3$chocolate, chocolate_prob > 0.5)

#szykuje krzywa ROC
ROCRpred2 <- prediction(chocolate_prob2, candy_test$chocolate)
ROCRpred3 <- prediction(chocolate_prob3, candy_test$chocolate)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
par(mfrow = c(1, 1))
plot(ROCRperf, colorize = TRUE)

#obliczam accuracy modelu
auc2 <- performance(ROCRpred2, measure = "auc")
auc3 <- performance(ROCRpred3, measure = "auc")
auc <- auc@y.values[[1]]
auc2
auc3

#wykorzystujac dwie zmiennie dla zadania konkursowego mozna stwierdzic, ze jesli cukierek nie jest owocowy a jego winratio

## 2 wariant 
#buduje model dla istotnie statystycznych zmiennych 
lmodel <- lm(chocolate ~ . -competitorname , data = choclate_competition)
summary(lmodel)
chocolate_prob2 <- predict(lmodel, choclate_competition, type="response")
chocolate_results2 <- cbind(choclate_competition, chocolate_prob2)


ROCRpred2 <- prediction(chocolate_prob2, choclate_competition$chocolate)
ROCRperf2 <- performance(ROCRpred, 'tpr','fpr')
par(mfrow = c(1, 1))
plot(ROCRperf2, colorize = TRUE)

auc2 <- performance(ROCRpred, measure = "auc")
auc2 <- auc2@y.values[[1]]
auc2

checktable <- cbind(chocolate_results$competitorname,chocolate_results$chocolate_prob,chocolate_results$chocolate)
checktable[order(-chocolate_prob),]
