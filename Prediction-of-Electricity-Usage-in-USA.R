########################################################### DATA IMPORTING 
library("splitstackshape")
library("nnet")
library(data.table)

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
covariates <- as.data.frame(read.csv("Smart meters Residential post-trial survey data.csv"))
final <- merge(df3,covariates)

######################################################## DATA CLEANING
x=0
for (i in 1:ncol(final)){
  x[i] = sum(is.na(final[,i]))
}
x <- cbind(x,1:length(x))
y <- x[x[,1]<500,] ##index of columns we want to keep
y1 <- y[y[,1]==0,] ##index of columns with no missing values
y2 <- y[y[,1]!=0,] ##index of columns with missing values and not dropped
y2 <- y2[c(-29,-31,-32),]

for (i in 1:nrow(y2)){
  newdata = cbind(final[,y2[i,2]],final[,y1[,2]])
  newdata = newdata[order(newdata[,1]),]
  names(newdata)[1] <- "yy"
  newdata1 <- newdata[1:(nrow(newdata)-y2[i,1]),]
  
  
  if (length(unique(newdata1[,1])) == 2){ #1
    
      if (max(newdata1[,1]) == 1){ #2
          reg = glm(yy ~., family = "binomial",newdata1)
          pred = predict(reg,as.data.frame(newdata[(nrow(newdata)+1-y2[i,1]):(nrow(newdata)),2:ncol(newdata),]),type = "response")
          
              for (j in 1:length(pred)){
                
                  if (pred[j]>0.5){pred[i] = 1} #3
                  else {pred [j] = 0}}#3
               
       } else {     #2
          
              for (o in 1:nrow(newdata1)){
                  if ( newdata1[o,1] ==2 ) {newdata1[o,1] =1} else {newdata1[o,1] =0} #4
              }#4
          
      reg = glm(yy ~., family = "binomial",newdata1)
      pred = predict(reg,as.data.frame(newdata[(nrow(newdata)+1-y2[i,1]):(nrow(newdata)),2:ncol(newdata),]),type = "response")
      
      for (j in 1:length(pred)){
        
        if (pred[j]>0.5){pred[i] = 1} #5
        else {pred [j] = 0}}#5
      
      }} else {
  
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



