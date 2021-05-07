#### Import Data ####
wine = read.csv("~/Downloads/winequality-red.csv", header = T, sep = ';')
### Define categories
#How to combine this quality to 
qual.fact = ifelse(wine$quality <= 5, "low", 'high')
table(qual.fact)
wine$quality = qual.fact
#No information rate 
855 / (855+744) #0.5347092
#### Train/Test Split ####
n = nrow(wine)
set.seed(1)
index = sample(1:n, .8*n)

wine.train = wine[index,]
wine.test = wine[-index,]

###Logistic Reg Full
#Logit Link
lr.logit.full = glm(I(quality == "high") ~ ., data = wine.train, family = binomial)
lr.full.pred = predict(lr.logit.full , wine.test, type = 'response')
table(lr.full.pred > 0.5, wine.test$quality == 'high')
mean((lr.full.pred > 0.5) != (wine.test$quality == 'high'))

#Probit Link
lr.probit.full = glm(I(quality == "high") ~ ., data = wine.train, 
                     family = binomial(link = 'probit'))
lr.full.pred = predict(lr.probit.full, wine.test, type = 'response')
table(lr.full.pred > 0.5, wine.test$quality == 'high')
mean((lr.full.pred > 0.5) != (wine.test$quality == 'high'))



#### Best Subsets & Stepwise ####
library(bestglm)
#Get same results as stepwise search
#cannot use regsubsets here
y = 1*(wine.train$qual == 'high')
Xy = data.frame(wine.train[,-12], y=y)

###Test Data
y.tst = 1*(wine.test$qual == 'high')
Xy.test = data.frame(wine.test[,-12], y=y.tst)

### Best Subsets using AIC
#Probit is better than logit here barely
bs.aic = bestglm(Xy, family = binomial(link = 'probit'), IC = "AIC", 
                 method = "exhaustive", nvmax = 12) 
bs.aic$Subsets
#summary(bc.aic$BestModel)
bc.aic.resp = predict(bs.aic$BestModel, Xy.test, type = 'response')
bc.aic.pred = ifelse(bc.aic.resp > 0.5, 'high', 'low')
table(bc.aic.pred, wine.test$quality)
mean(bc.aic.pred != wine.test$quality)

### Best Subsets using BIC
bs.bic = bestglm(Xy, family = binomial(link = 'probit'), IC = "BIC", 
                 method = "exhaustive", nvmax = 12) 
bs.bic$Subsets
#size is 5
#Make Predictions
bc.bic.resp = predict(bs.bic$BestModel, Xy.test, type = 'response')
bc.bic.pred = ifelse(bc.bic.resp > 0.5, 'high', 'low')
table(bc.bic.pred, wine.test$quality)
mean(bc.bic.pred != wine.test$quality)

###LASSO####
library(glmnet)
X = model.matrix(quality~., data = wine.train)[,-1]
Y = 1*(wine.train$qual == 'high')
X.test = model.matrix(quality~., data = wine.test)[,-1]



### Probit Link
lasso.probit <- glmnet(X, Y, family=binomial(link = "probit"), alpha=1)
#Calculate Lambda Value
lasso.cv1 = cv.glmnet(X, Y, alpha = 1, family = binomial(link = "probit"))
#Predict using 1se lambda 
lasso.pred.probit <- predict(lasso.probit, s=lasso.cv1$lambda.1se, 
                             newx=X.test, type = 'response')
table(lasso.pred.probit > 0.5, wine.test$quality == 'high')
mean((lasso.pred.probit > 0.5) != (wine.test$quality == 'high'))

### Logit Link
lasso.logit <- glmnet(X, Y, family=binomial(link = "logit"), alpha=1)
#Calculate Lambda Value
lasso.cv2 = cv.glmnet(X, Y, alpha = 1, family = binomial(link = "logit"))
#Predict 
lasso.pred.logit <- predict(lasso.logit, s=lasso.cv2$lambda.1se, 
                            newx=X.test, type = 'response')
table(lasso.pred.logit > 0.5, wine.test$quality == 'high')
mean((lasso.pred.logit > 0.5) != (wine.test$quality == 'high'))



#### Ridge Regression ####

#Probit Link
ridge.probit <- glmnet(X, Y, family=binomial(link = "probit"), alpha=0)
#Calculate Lambda Value
ridge.cv1 = cv.glmnet(X, Y, alpha = 0, family = binomial(link = "probit"))
#Predict using 1se lambda 
ridge.pred.probit <- predict(ridge.probit, s=ridge.cv1$lambda.1se, 
                             newx=X.test, type = 'response')
table(ridge.pred.probit > 0.5, wine.test$quality == 'high')
mean((ridge.pred.probit > 0.5) != (wine.test$quality == 'high'))

#Logit Link
ridge.logit <- glmnet(X, Y, family=binomial(link = "logit"), alpha=0)
#Calculate Lambda Value
ridge.cv1 = cv.glmnet(X, Y, alpha = 0, family = binomial(link = "logit"))
#Predict using 1se lambda 
ridge.pred.logit <- predict(ridge.logit, s=ridge.cv1$lambda.1se, 
                            newx=X.test, type = 'response')
table(ridge.pred.logit > 0.5, wine.test$quality == 'high')
mean((ridge.pred.logit > 0.5) != (wine.test$quality == 'high'))

#### Assumption Validation ####
#Cooks Distance
par(mfrow = c(1,2))
plot(lr.logit.full, which = 4, id.n = 3, x.lab = "Obs.number", 
     main = "Cook's Distance w/ Logit Link")
plot(lr.probit.full, which = 4, id.n = 3, x.lab = "Obs.number", 
     main = "Cook's Distance w/ Probit Link")

#VIF of models
car::vif(lr.probit.full)
car::vif(lr.logit.full)
car::vif(bs.aic$BestModel)
car::vif(bs.bic$BestModel)





