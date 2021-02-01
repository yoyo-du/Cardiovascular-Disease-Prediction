library(ggplot2)
library(corrplot)
library(dplyr)
library(reshape2)
library(epitools)
library(pROC)
library(boot)
library(epitools)
library(tidyverse)
library(caret)

cardio_data <- read.csv("/Users/yuedu/STA542/cardio_train.csv",sep = ";")

cardio_1 <- transform(cardio_data, A = as.factor(alco),
                      S = as.factor(smoke), P = as.factor(active), C = as.factor(cholesterol), 
                      G=as.factor(gluc), Ca=as.factor(cardio))

## clean data
# height
cardio_1 <- cardio_1[-which(cardio_1$height > 230|cardio_1$height < 135),]

# ap_hi
cardio_1 <- cardio_1[-which(cardio_1$ap_hi < 40|cardio_1$ap_hi > 180),]

#ap_lo
cardio_1 <- cardio_1[-which(cardio_1$ap_lo < 40|cardio_1$ap_lo > 120),]

#weight
cardio_1 <- cardio_1[-which(cardio_1$weight < 30),]

#cardio_1$bmi <- weight/(height/100)^2

attach(cardio_1)

write.csv(cardio_1)


## Contengency Table
# smoke
smoke_table <- table(smoke,cardio)
addmargins(smoke_table)

smoke_prop.table <- prop.table(smoke_table)
round(addmargins(smoke_prop.table),4)  

chisq.test(smoke_table)

# alco 
alco_table <- table(alco,cardio)
addmargins(alco_table)

alco_prop.table <- prop.table(alco_table)
round(addmargins(alco_prop.table),4) 

chisq.test(alco_table) ## significant

# cholesterol
chol_table <- table(cholesterol,cardio)
addmargins(chol_table)

chol_prop.table <- prop.table(chol_table)
round(addmargins(chol_prop.table),4)

chisq.test(chol_table) 

# gluc
gluc_table <- table(gluc,cardio)
addmargins(gluc_table)

gluc_prop.table <- prop.table(gluc_table)
round(addmargins(gluc_prop.table),4)

chisq.test(gluc_table)

# activity
acti_table <- table(active,cardio)
addmargins(acti_table)
acti_prop.table <- prop.table(acti_table)
round(addmargins(acti_prop.table),4)

chisq.test(acti_table) 

## model

cardio_fit <- glm(Ca~A+S+P+C+G+ap_hi+ap_lo, family = "binomial", data=cardio_1)
cardio_fit1 <- glm(Ca~A+S+P+C+I(G==3)+ap_hi+ap_lo, family = "binomial", data=cardio_1)
step(cardio_fit1)
summary(cardio_fit1)
AIC(cardio_fit)
AIC(cardio_fit1)
summary(cardio_fit)
anova(cardio_fit1, cardio_fit, test="LRT")

sum(resid(cardio_fit1, type = "pearson")^2)
source("chisqstat.R")
chisqstat(cardio_fit1)
df.residual(cardio_fit1)
pchisq(chisqstat(cardio_fit1), df.residual(cardio_fit1), lower.tail = FALSE)

AIC(cardio_fit)
AIC(cardio_fit1)

anova(cardio_fit, cardio_fit1, test="LRT")
summary(cardio_fit1)

plot(rstandard(cardio_fit1, type="pearson"), type="l", ylab="standardized residuals")
title(main="Plot of standardized residuals")
abline(h=0, col="red")

qqnorm(rstandard(cardio_fit, type="pearson"))
qqline(rstandard(cardio_fit, type="pearson"))

dt = sample(nrow(cardio_1), nrow(cardio_1)*.7)
train <- cardio_1[dt,]
test <- cardio_1[-dt,]

train.mod <- glm(Ca~A+S+P+C+I(G==3)+ap_hi+ap_lo+weight, family = "binomial", data=train)
pred <- predict(train.mod, newdata = test, type = "response")

test <- test  %>% mutate(model_pred = 1*(pred > .5) + 0,
                         cardio_binary = 1*(cardio == "1") + 0)

test <- test %>% mutate(accurate = 1*(model_pred == cardio_binary))
sum(test$accurate)/nrow(test)

## or
pred.ca <- round(pred)
mean(pred.ca==test$cardio)

## ROC curve
# The higher the sensivity for a given specificity, the better, so a model with a higher ROC curve is preferred.
# The area under the ROC curve (AUC) is a measure of predictive power. 
y <- test$cardio
g <- roc(y ~ test$model_pred)
plot(g, grid = TRUE, print.auc = TRUE)
title(main="ROC Curve")





