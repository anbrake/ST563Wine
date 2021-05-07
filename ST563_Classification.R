#### Import Data ####
wine = read.csv("~/Downloads/winequality-red.csv", header = T, sep = ';')

sum(is.na(wine)) #no NA values
str(wine)
table(wine$quality)

#### Make Cor Plot ####
library(GGally)
ggcorr(wine, label = T,label_round = 2)

#### Make Boxplots ####
library(ggplot2)

p <- ggplot(data = wine, aes(x=variable)) + 
  geom_boxplot()

p + facet_wrap( ~ variable, scales="free")


ggplot(wine, aes(x=density)) + 
  geom_boxplot() 

for(i in 1:12){
}


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


#### Error Matrix  #### 
misclass.error = matrix(NA, ncol = 3,20)
colnames(misclass.error) = c("Method","Notes" ,"Error")

#### Logistic Regression ####

###Logistic Reg Full
#Logit Link
lr.logit.full = glm(I(quality == "high") ~ ., data = wine.train, family = binomial)
lr.full.pred = predict(lr.logit.full , wine.test, type = 'response')
table(lr.full.pred > 0.5, wine.test$quality == 'high')
mean((lr.full.pred > 0.5) != (wine.test$quality == 'high'))
misclass.error[1,] = c("Full Logistic Reg", "Logit Link", mean((lr.full.pred > 0.5) != (wine.test$quality == 'high')))

#Probit Link
lr.probit.full = glm(I(quality == "high") ~ ., data = wine.train, 
              family = binomial(link = 'probit'))
lr.full.pred = predict(lr.probit.full, wine.test, type = 'response')
table(lr.full.pred > 0.5, wine.test$quality == 'high')
mean((lr.full.pred > 0.5) != (wine.test$quality == 'high'))
misclass.error[2,] = c("Full Logistic Reg", "Probit Link", mean((lr.full.pred > 0.5) != (wine.test$quality == 'high')))

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
misclass.error[3,] = c("Best Subsets AIC", "Probit Link", 
                       mean(bc.aic.pred != wine.test$quality))


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

misclass.error[4,] = c("Best Subsets BIC", "Probit Link", 
                       mean(bc.bic.pred != wine.test$quality))

### Forward using AIC
fwd.aic = bestglm(Xy, family = binomial(link = 'probit'), IC = "AIC", 
                 method = "forward", nvmax = 12) 
fwd.aic.resp = predict(fwd.aic$BestModel, Xy.test, type = 'response')
fwd.aic.pred = ifelse(fwd.aic.resp > 0.5, 'high', 'low')
table(fwd.aic.pred, wine.test$quality)
mean(fwd.aic.pred != wine.test$quality)

### Forward using BIC 
fwd.bic = bestglm(Xy, family = binomial(link = 'probit'), IC = "BIC", 
                  method = "forward", nvmax = 12) 
fwd.bic.resp = predict(fwd.bic$BestModel, Xy.test, type = 'response')
fwd.bic.pred = ifelse(fwd.bic.resp > 0.5, 'high', 'low')
table(fwd.bic.pred, wine.test$quality)
mean(fwd.bic.pred != wine.test$quality)

### Backwards using AIC

### Backwards using BIC


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

misclass.error[5,] = c("LASSO", "Probit Link", 
        mean((lasso.pred.probit > 0.5) != (wine.test$quality == 'high')))

### Logit Link
lasso.logit <- glmnet(X, Y, family=binomial(link = "logit"), alpha=1)
#Calculate Lambda Value
lasso.cv2 = cv.glmnet(X, Y, alpha = 1, family = binomial(link = "logit"))
#Predict 
lasso.pred.logit <- predict(lasso.logit, s=lasso.cv2$lambda.1se, 
                             newx=X.test, type = 'response')
table(lasso.pred.logit > 0.5, wine.test$quality == 'high')
mean((lasso.pred.logit > 0.5) != (wine.test$quality == 'high'))
misclass.error[6,] = c("LASSO", "Logit Link", 
          mean((lasso.pred.logit > 0.5) != (wine.test$quality == 'high')))

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

misclass.error[7,] = c("RIDGE", "Probit Link", 
         mean((ridge.pred.probit > 0.5) != (wine.test$quality == 'high')))

#Logit Link
ridge.logit <- glmnet(X, Y, family=binomial(link = "logit"), alpha=0)
#Calculate Lambda Value
ridge.cv1 = cv.glmnet(X, Y, alpha = 0, family = binomial(link = "logit"))
#Predict using 1se lambda 
ridge.pred.logit <- predict(ridge.logit, s=ridge.cv1$lambda.1se, 
                             newx=X.test, type = 'response')
table(ridge.pred.logit > 0.5, wine.test$quality == 'high')
mean((ridge.pred.logit > 0.5) != (wine.test$quality == 'high'))

misclass.error[8,] = c("RIDGE", "Logit Link", 
      mean((ridge.pred.logit > 0.5) != (wine.test$quality == 'high')))
                       
#### Elastic Net Regression ####

### Even Mixture (alpha = 0.05) 
#Probit Link
eln.probit <- glmnet(X, Y, family=binomial(link = "probit"), alpha=0.5)
#Calculate Lambda Value
eln.cv1 = cv.glmnet(X, Y, alpha = 0.5, family = binomial(link = "probit"))
#Predict using 1se lambda 
eln.pred.probit <- predict(eln.probit, s=eln.cv1$lambda.1se, 
                             newx=X.test, type = 'response')
table(eln.pred.probit > 0.5, wine.test$quality == 'high')
mean((eln.pred.probit > 0.5) != (wine.test$quality == 'high'))

#Logit Link
eln.logit <- glmnet(X, Y, family=binomial(link = "logit"), alpha=0.5)
#Calculate Lambda Value
eln.cv1 = cv.glmnet(X, Y, alpha = 0.5, family = binomial(link = "logit"))
#Predict using 1se lambda 
eln.pred.logit <- predict(eln.logit, s=eln.cv1$lambda.1se, 
                            newx=X.test, type = 'response')
table(eln.pred.logit > 0.5, wine.test$quality == 'high')
mean((eln.pred.logit > 0.5) != (wine.test$quality == 'high'))

### alpha = 0.2
#Probit Link
eln.probit <- glmnet(X, Y, family=binomial(link = "probit"), alpha=0.2)
#Calculate Lambda Value
eln.cv1 = cv.glmnet(X, Y, alpha = 0.2, family = binomial(link = "probit"))
#Predict using 1se lambda 
eln.pred.probit <- predict(eln.probit, s=eln.cv1$lambda.1se, 
                           newx=X.test, type = 'response')
table(eln.pred.probit > 0.5, wine.test$quality == 'high')
mean((eln.pred.probit > 0.5) != (wine.test$quality == 'high'))

#Logit Link
eln.logit <- glmnet(X, Y, family=binomial(link = "logit"), alpha=0.2)
#Calculate Lambda Value
eln.cv1 = cv.glmnet(X, Y, alpha = 0.2, family = binomial(link = "logit"))
#Predict using 1se lambda 
eln.pred.logit <- predict(eln.logit, s=eln.cv1$lambda.1se, 
                          newx=X.test, type = 'response')
table(eln.pred.logit > 0.5, wine.test$quality == 'high')
mean((eln.pred.logit > 0.5) != (wine.test$quality == 'high'))

#### Dimension Reduction Methods ####

### PLS

### PCR

#### DA Methods ####
library(MASS)
### LDA (full)
lda.full = lda(quality ~ ., data = wine.train)
lda.full.pred = predict(lda.full, wine.test)$class
table(lda.full.pred, wine.test$qual)
mean(lda.full.pred != wine.test$qual)

misclass.error[9,] = c("LDA", NA, 
                       mean(lda.full.pred != wine.test$qual))
                       
### QDA (full)
qda.full = qda(quality ~ ., data = wine.train)
qda.full.pred = predict(qda.full, wine.test)$class
table(qda.full.pred, wine.test$qual)
mean(qda.full.pred != wine.test$qual) #error rate

misclass.error[10,] = c("QDA", NA, 
                       mean(qda.full.pred != wine.test$qual))

#### PCA ####

### PCA with no scale
pca = prcomp(~.-quality, data = wine.train)
summary(pca)

#Seems like one or two pca needed
### Make PCA dataframe 
train.pca = as.data.frame(pca$x)
train.pca$quality = wine.train$quality

#Make sure to center X before doing this stuff
test.pca = as.data.frame(scale(X.test, scale=F)%*% pca$rotation)
test.pca$quality = wine.test$quality

## LDA w/ PCA 
lda.pca = lda(quality ~ PC1+PC2+PC3, data = train.pca)
lda.pca.pred = predict(lda.pca, test.pca)$class
table(lda.pca.pred, test.pca$qual)
mean(lda.pca.pred != wine.test$qual)

misclass.error[11,] = c("LDA", "3 PC's / No Scale", 
                        mean(lda.pca.pred != wine.test$qual))

## Logistic Reg w/ PC (ie PCGLMR)
lr.pca = glm(I(quality == 'high') ~ PC1+PC2+PC3, data = train.pca, family = binomial())
lr.pca.resp = predict(lr.pca, test.pca, type = 'response')
lr.pca.pred = ifelse(lr.pca.resp > 0.5, 'high', 'low')
table(lr.pca.pred, test.pca$quality)
mean(lr.pca.pred != test.pca$quality)

misclass.error[12,] = c("Log Reg", "3 PC's / No Scale", 
                   mean(lr.pca.pred != test.pca$quality))
                        

#These pca with no scaling are realy not great

### PCA with scale 
apply(wine[,-12], 2, sd)

pca.scale =  prcomp(~.-quality, data = wine.train, scale = T)

#Seems like one or two pca needed
### Make PCA dataframe 
train.pca = as.data.frame(pca.scale$x)
train.pca$quality = wine.train$quality

#Make sure to center X before doing this stuff
test.pca = as.data.frame(scale(X.test, scale=F)%*% pca.scale$rotation)
test.pca$quality = wine.test$quality

lda.pca = lda(quality ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7, data = train.pca)
lda.pca.pred = predict(lda.pca, test.pca)$class
table(lda.pca.pred, test.pca$qual)
mean(lda.pca.pred != wine.test$qual)

misclass.error[11,] = c("LDA", "3 PC's / No Scale", 
                        mean(lda.pca.pred != wine.test$qual))

### Log Reg PCA w/ scale
lr.pca = glm(I(quality == 'high') ~ PC1+PC2+PC3+PC4+PC5+PC6+PC7,
             data = train.pca, family = binomial())
lr.pca.resp = predict(lr.pca, test.pca, type = 'response')
lr.pca.pred = ifelse(lr.pca.resp > 0.5, 'high', 'low')
table(lr.pca.pred, test.pca$quality)
mean(lr.pca.pred != test.pca$quality)


#### Assumption Validation ####
#Cooks Distance
par(mfrow = c(1,2))
plot(lr.logit.full, which = 4, id.n = 3, x.lab = "Obs.number", 
     main = "Cook's Distance w/ Logit Link")
plot(lr.probit.full, which = 4, id.n = 3, x.lab = "Obs.number", 
     main = "Cook's Distance w/ Probit Link")
#VIF of models

