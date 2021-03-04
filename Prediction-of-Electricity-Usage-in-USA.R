########################################################### DATA IMPORTING 
library("splitstackshape")
library("nnet")
library("data.table")
library("Hmisc")
library("glmnet")
library("ggplot2")
library("reshape")
library("gamlr")

df1 = read.delim("File1.txt")
data1 = cSplit(df1, "X1392.19503.0.14", " ")
names(data1)<- c("ID","time","electricity")
data1 = as.matrix(data1)
monthly1 = tapply(data1[,3],data1[,1],sum)
monthly1 = as.data.frame(monthly1)
m1 = as.numeric(rownames(monthly1))
n1 = cbind(m1,monthly1)
names(n1)<- c("ID","electricity")

df2 = read.delim("File2.txt")
data2 = cSplit(df2, "X2113.19630.0.038", " ")
names(data2)<- c("ID","time","electricity")
data2 = as.matrix(data2)
monthly2 = tapply(data2[,3],data2[,1],sum)
monthly2 = as.data.frame(monthly2)
m2 = as.numeric(rownames(monthly2))
n2 = cbind(m2,monthly2)
names(n2)<- c("ID","electricity")

df3 = rbind(n1,n2)
covariates <- as.data.frame(read.csv("Smart meters Residential pre-trial survey data.csv"))
final <- merge(df3,covariates)

for (i in 1:ncol(final)){
  colnames(final)[i] = i
}

final[, 28][final[, 28] == 999] <- NA
final[, 38][final[, 38] == 9999] <- 1999
final[, 38][final[, 38] == 29] <- 1929
final[, 38][final[, 38] == 75] <- 1975
final[, 38][final[, 38] == 0] <- 1990
final[, 40][final[, 40] == 999999999] <- NA
######################################################## DATA CLEANING
x=0
for (i in 1:ncol(final)){
  x[i] = sum(is.na(final[,i]))
}
y <- cbind(x,1:length(x))
y = y[y[,1]<1000,] ## dropping variables with missing values more than 1000
y1 <- y[y[,1]==0,] ##index of columns with no missing values
y2 <- y[y[,1]!=0,] ##index of columns with missing values and not dropped

for (i in 1:nrow(y2)){
  newdata = cbind(final[,y2[i,2]],final[,y1[,2]])
  newdata = newdata[order(newdata[,1]),]
  names(newdata)[1] <- "yy"
  newdata1 <- newdata[1:(nrow(newdata)-y2[i,1]),]
  
  
  if (length(unique(newdata1[,1])) == 2){ #1
    
      if (max(newdata1[,1]) == 1){ #2
          reg = glm(yy ~., family = "binomial",newdata1)
          pred = predict(reg,as.data.frame(newdata[(nrow(newdata)+1-y2[i,1]):(nrow(newdata)),2:ncol(newdata)]),type = "response")
          
              for (j in 1:length(pred)){
                
                  if (pred[j]>0.5){pred[j] = 1} else {pred[j] = 0}}#3
               
       } else {     #2
          
              for (o in 1:nrow(newdata1)){
                  if ( newdata1[o,1] ==2 ) {newdata1[o,1] =1} else {newdata1[o,1] =0} #4
              }#4
          
      reg = glm(yy ~., family = "binomial",newdata1)
      pred = predict(reg,as.data.frame(newdata[(nrow(newdata)+1-y2[i,1]):(nrow(newdata)),2:ncol(newdata),]),type = "response")
      
      for (j in 1:length(pred)){
        
        if (pred[j]>0.5){pred[j] = 1} #5
        else {pred [j] = 0}}#5
      
      }} else if (max(newdata1[,1],na.rm = T) > 10){ 
        
        
        reg <- lm(yy ~., data = newdata1)
        pred <- predict(reg, newdata = as.data.frame(newdata[(nrow(newdata)+1-y2[i,1]):(nrow(newdata)),2:ncol(newdata),]))
        pred <- as.numeric(pred)
       
         } else
  
  {
      reg <- multinom(yy ~., data = newdata1)
    pred <- predict(reg, newdata = as.data.frame(newdata[(nrow(newdata)+1-y2[i,1]):(nrow(newdata)),2:ncol(newdata),]), "probs")
    pred <- as.numeric(colnames(pred)[apply(pred,1,which.max)])
  }
  
  newdata[(nrow(newdata)+1-y2[i,1]):(nrow(newdata)),1] <- pred
  
  
  final = final[order(final[,y2[i,2]]),]
  final[,y2[i,2]] <- newdata[,1]
  print(i)
}

final1 = final[,y2[,2]]

table(is.na(final1))  ## make sure there is no NA


mydata <- cbind(final[,2],final[y2[,2]],final[y1[,2]]) ## we will be playing with 134 variables

###### Lasso regression
colnames(mydata)[1]<-"yy"
xx = model.matrix(yy~., mydata)[,-1]
lambdas_to_try <- 10^seq(-3, 5, length.out = 200)

# Setting alpha = 0 implements ridge regression
lasso_reg <- cv.glmnet(xx, as.matrix(mydata[,1]), alpha = 1, lambda = lambdas_to_try, standardize = TRUE)

# Plot cross-validation results
plot(lasso_reg)

coef(lasso_reg)

lm(final[, 2]~.,mydata)

ridge_reg <- cv.glmnet(xx, as.matrix(mydata[,1]), alpha = 0, lambda = lambdas_to_try,standardize = TRUE, nfolds = 10)
coef(ridge_reg)
plot(ridge_reg)


lr = lm(yy~.,mydata)


beta=coef(ridge_reg)
tmp <- as.data.frame(as.matrix(beta))
tmp$coef <- row.names(tmp)
tmp <- reshape::melt(tmp, id = "coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- ridge_reg$lambda[tmp$variable+1] # extract the lambda values

ggplot(tmp[tmp$coef != "(Intercept)",], aes(lambda, value, color = coef, size = 3,group = 1)) + 
  geom_line() + 
  scale_x_log10() + 
  xlab("Lambda (log scale)") + 
  guides(color = guide_legend(title = ""), 
         linetype = guide_legend(title = "")) +
  theme_bw() + 
  theme(legend.key.width = unit(3,"lines"))



t=c()
for (i in 1:ncol(final)){
  t[i]=max(final[,i],na.rm = T)
}

