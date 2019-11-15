---
title: "경경분 과제"
author: "1602108 정유진"
date: "2018년 12월 11일"
output: word_document
---

# VKOSPI & KOSPI 5분 실현변동성 

## 1번

```{r}
mydata<-read.csv("c://Temp/RV_IV_data.csv",header=TRUE)
head(mydata)

y.t = mydata$KOSPI.RV 
x.t = mydata$VKOSPI

yt = y.t[2:length(y.t)]
yt1 = y.t[1:(length(y.t)-1)]
xt = x.t[2:length(x.t)]

lm1<- lm(yt~yt1+xt)
summary(lm1)
```

xt의 t값이 32.891로 95% 수준에서 critical value인 1.96을 훨씬 능가하기 때문에 기각역에 속하고 따라서 추가예측력이 있다고 말할 수 있다.
(H0은 추가예측력 없다. H1은 추가예측력 있다.)

## 2번

```{r}

y_t = y.t[9:length(y.t)]
y_t_1 = y.t[8:(length(y.t)-1)]
y_t_2 = y.t[7:(length(y.t)-2)]
y_t_3 = y.t[6:(length(y.t)-3)]
y_t_4 = y.t[5:(length(y.t)-4)]
y_t_5 = y.t[4:(length(y.t)-5)]
y_t_6 = y.t[3:(length(y.t)-6)]
y_t_7 = y.t[2:(length(y.t)-7)]
y_t_8 = y.t[1:(length(y.t)-8)]

y.data = cbind(y_t_1 = y_t_1, y_t_2= y_t_2, y_t_3 = y_t_3, y_t_4 = y_t_4, y_t_5=y_t_5,y_t_6=y_t_6,y_t_7=y_t_7,y_t_8=y_t_8)

Un_t_1 = x.t[8:(length(x.t)-1)]
Un_t_2 = x.t[7:(length(x.t)-2)]
Un_t_3 = x.t[6:(length(x.t)-3)]
Un_t_4 = x.t[5:(length(x.t)-4)]
Un_t_5 = x.t[4:(length(x.t)-5)]
Un_t_6 = x.t[3:(length(x.t)-6)]
Un_t_7 = x.t[2:(length(x.t)-7)]
Un_t_8 = x.t[1:(length(x.t)-8)]

x.data = cbind(Un_t_1 = Un_t_1, Un_t_2= Un_t_2, Un_t_3 = Un_t_3, Un_t_4 = Un_t_4,Un_t_5 = Un_t_5, Un_t_6= Un_t_6, Un_t_7 = Un_t_7, Un_t_8 = Un_t_8)

library(forecast)

#2번문제 - bic기준으로 p값 구하기
AR.aic = c(); AR.bic = c()
for( p in 1:10){
  AR.fit = Arima(y.t, order = c(p,0,0))
  AR.aic[p] = AR.fit$aic
  AR.bic[p] = AIC(AR.fit,k = log(length(y.t)))
}
par(mfrow=c(1,2))
plot(AR.aic, type = "b", pch =19, main = "AIC", ylab = "", xlab = "p")
abline(v = which.min(AR.aic), col=2, lty = 2)
plot(AR.bic, type = "b", pch =19, main = "BIC", ylab = "", xlab = "p")
abline(v = which.min(AR.bic), col=2, lty = 2)
#bic기준 AR(p)의 적정 p는 8이다.

#ADL
all.case = expand.grid(1:8,1:8)

ADL.aic0 = matrix(0, ncol = 8, nrow = 8); ADL.bic0 = matrix(0, ncol = 8, nrow = 8)
for( k in 1:nrow(all.case)){
  p = all.case[k,1]; q = all.case[k,2]
  ADL.fit = lm(y_t ~ y.data[,1:p] + x.data[,1:q])
  ADL.aic0[p,q] = AIC(ADL.fit)
  ADL.bic0[p,q] = AIC(ADL.fit,k = log(length(y.t)))
}

colnames(ADL.aic0) = c("q=1", "q=2", "q=3", "q=4","q=5","q=6","q=7","q=8")
row.names(ADL.aic0) = c("p=1", "p=2", "p=3", "p=4","p=5","p=6","p=7","p=8")
ADL.aic0

colnames(ADL.bic0) = c("q=1", "q=2", "q=3", "q=4","q=5","q=6","q=7","q=8")
row.names(ADL.bic0) = c("p=1", "p=2", "p=3", "p=4","p=5","p=6","p=7","p=8")
ADL.bic0

# p=5, q=3 일 때 BIC가 168.6774로 가장 낮은 것을 확인할 수 있다. 
library(dplyr)
mydata2<-mydata %>% mutate(year=substr(Date,1,4))
mydata2 %>% filter(year<=2015) %>% summarise(count=n())
# 3106까지가 training data인 것을 확인할 있다.

AR8.fore = c(); ADL.fore1 = c(); 
for(i in 3106:(length(y.t)-1)){
  train.data = data.frame(y.t = y.t[1:i], Unemp = x.t[1:i])
  y_t = train.data[9:nrow(train.data),1]
  y_t_1 = train.data[8:(nrow(train.data)-1),1]
  y_t_2 = train.data[7:(nrow(train.data)-2),1]
  y_t_3 = train.data[6:(nrow(train.data)-3),1]
  y_t_4 = train.data[5:(nrow(train.data)-4),1]
  y_t_5 = train.data[4:(nrow(train.data)-5),1]
  y_t_6 = train.data[3:(nrow(train.data)-6),1]
  y_t_7 = train.data[2:(nrow(train.data)-7),1]
  y_t_8 = train.data[1:(nrow(train.data)-8),1]
  
  Un_t = train.data[9:(nrow(train.data)),2]
  Un_t_1 = train.data[8:(nrow(train.data)-1),2]
  Un_t_2 = train.data[7:(nrow(train.data)-2),2]
  Un_t_3 = train.data[6:(nrow(train.data)-3),2]
  Un_t_4 = train.data[5:(nrow(train.data)-4),2]
  Un_t_5 = train.data[4:(nrow(train.data)-5),2]
  Un_t_6 = train.data[3:(nrow(train.data)-6),2]
  Un_t_7 = train.data[2:(nrow(train.data)-7),2]
  Un_t_8 = train.data[1:(nrow(train.data)-8),2]

  
  # AR(8)
  AR8.fit = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4+y_t_5+y_t_6+y_t_7+y_t_8)
  AR8.fore[i-3105] = sum(AR8.fit$coef*c(1, y_t[length(y_t)], y_t_1[length(y_t)], y_t_2[length(y_t)], y_t_3[length(y_t)],y_t_4[length(y_t)],y_t_5[length(y_t)],y_t_6[length(y_t)],y_t_7[length(y_t)]))
  
  ## ADL(5,3)
  ADL.fit1 = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4+y_t_5 + Un_t_1+Un_t_2+Un_t_3)
  ADL.fore1[i-3105] = sum(ADL.fit1$coef*c(1, y_t[length(y_t)], y_t_1[length(y_t)], y_t_2[length(y_t)], y_t_3[length(y_t)],y_t_4[length(y_t)], Un_t[length(y_t)], Un_t_1[length(y_t)], Un_t_2[length(y_t)]))}
  
AR8.MAE = mean(abs(AR8.fore - y.t[3107:length(y.t)]),na.rm=TRUE)
ADL.MAE = mean(abs(ADL.fore1 - y.t[3107:length(y.t)]),na.rm=TRUE)

AR8.MSE = mean((AR8.fore - y.t[3107:length(y.t)])^2,na.rm=TRUE)
ADL.MSE = mean((ADL.fore1 - y.t[3107:length(y.t)])^2,na.rm=TRUE)

result = matrix(c(AR8.MAE, ADL.MAE, AR8.MSE, ADL.MSE), nrow = 2, byrow = T)
row.names(result) = c("MAE", "MSE")
colnames(result) = c("AR8", "ADL(5,3)")
result
```

## 3번

```{r}
mydata<-read.csv("c://Temp/RV_IV_data.csv",header=TRUE)
head(mydata)

#3번
y.t = mydata$VKOSPI
x.t = mydata$KOSPI.RV

yt = y.t[2:length(y.t)]
yt1 = y.t[1:(length(x.t)-1)]
xt = x.t[2:length(x.t)]

##3번문제
lm2<- lm(yt~yt1+xt)
summary(lm2)
```
xt의 t값이 25.348로 95% 수준에서 critical value인 1.96을 훨씬 능가하기 때문에 기각역에 속하고 따라서 추가예측력이 있다고 말할 수 있다.
(H0은 추가예측력 없다. H1은 추가예측력 있다.)
## 4번

```{r}
y_t = y.t[10:length(y.t)]
y_t_1 = y.t[9:(length(y.t)-1)]
y_t_2 = y.t[8:(length(y.t)-2)]
y_t_3 = y.t[7:(length(y.t)-3)]
y_t_4 = y.t[6:(length(y.t)-4)]
y_t_5 = y.t[5:(length(y.t)-5)]
y_t_6 = y.t[4:(length(y.t)-6)]
y_t_7 = y.t[3:(length(y.t)-7)]
y_t_8 = y.t[2:(length(y.t)-8)]
y_t_9 = y.t[1:(length(y.t)-9)]

y.data = cbind(y_t_1 = y_t_1, y_t_2= y_t_2, y_t_3 = y_t_3, y_t_4 = y_t_4, y_t_5=y_t_5,y_t_6=y_t_6,y_t_7=y_t_7,y_t_8=y_t_8,y_t_9=y_t_9)

Un_t_1 = x.t[9:(length(x.t)-1)]
Un_t_2 = x.t[8:(length(x.t)-2)]
Un_t_3 = x.t[7:(length(x.t)-3)]
Un_t_4 = x.t[6:(length(x.t)-4)]
Un_t_5 = x.t[5:(length(x.t)-5)]
Un_t_6 = x.t[4:(length(x.t)-6)]
Un_t_7 = x.t[3:(length(x.t)-7)]
Un_t_8 = x.t[2:(length(x.t)-8)]
Un_t_9 = x.t[1:(length(x.t)-9)]

x.data = cbind(Un_t_1 = Un_t_1, Un_t_2= Un_t_2, Un_t_3 = Un_t_3, Un_t_4 = Un_t_4,Un_t_5 = Un_t_5, Un_t_6= Un_t_6, Un_t_7 = Un_t_7, Un_t_8 = Un_t_8,Un_t_9 = Un_t_9)

library(forecast)
ar.fit1 = arima(y.t,order=c(1,0,0))
ar.fit1

#2번문제 - bic기준으로 p값 구하기
AR.aic = c(); AR.bic = c()
for( p in 1:10){
  AR.fit = Arima(y.t, order = c(p,0,0))
  AR.aic[p] = AR.fit$aic
  AR.bic[p] = AIC(AR.fit,k = log(length(y.t)))
}
par(mfrow=c(1,2))
plot(AR.aic, type = "b", pch =19, main = "AIC", ylab = "", xlab = "p")
abline(v = which.min(AR.aic), col=2, lty = 2)
plot(AR.bic, type = "b", pch =19, main = "BIC", ylab = "", xlab = "p")
abline(v = which.min(AR.bic), col=2, lty = 2)
#bic기준 AR(p)의 적정 p는 9이다.

#ADL
all.case = expand.grid(1:9,1:9)

ADL.aic0 = matrix(0, ncol = 9, nrow = 9); ADL.bic0 = matrix(0, ncol = 9, nrow = 9)
for( k in 1:nrow(all.case)){
  p = all.case[k,1]; q = all.case[k,2]
  ADL.fit = lm(y_t ~ y.data[,1:p] + x.data[,1:q])
  ADL.aic0[p,q] = AIC(ADL.fit)
  ADL.bic0[p,q] = AIC(ADL.fit,k = log(length(y.t)))
}

colnames(ADL.aic0) = c("q=1", "q=2", "q=3", "q=4","q=5","q=6","q=7","q=8","q=9")
row.names(ADL.aic0) = c("p=1", "p=2", "p=3", "p=4","p=5","p=6","p=7","p=8","p=9")
ADL.aic0

colnames(ADL.bic0) = c("q=1", "q=2", "q=3", "q=4","q=5","q=6","q=7","q=8","q=9")
row.names(ADL.bic0) = c("p=1", "p=2", "p=3", "p=4","p=5","p=6","p=7","p=8","p=9")
ADL.bic0

# p=9, q=1 일 때 BIC가 13146.26로 가장 낮은 것을 확인할 수 있다. 

library(dplyr)
mydata2<-mydata %>% mutate(year=substr(Date,1,4))
mydata2 %>% filter(year<=2015) %>% summarise(count=n())

AR9.fore = c(); ADL.fore1 = c(); 
for(i in 3106:(length(y.t)-1)){
  train.data = data.frame(y.t = y.t[1:i], Unemp = x.t[1:i])
  y_t = train.data[10:nrow(train.data),1]
  y_t_1 = train.data[9:(nrow(train.data)-1),1]
  y_t_2 = train.data[8:(nrow(train.data)-2),1]
  y_t_3 = train.data[7:(nrow(train.data)-3),1]
  y_t_4 = train.data[6:(nrow(train.data)-4),1]
  y_t_5 = train.data[5:(nrow(train.data)-5),1]
  y_t_6 = train.data[4:(nrow(train.data)-6),1]
  y_t_7 = train.data[3:(nrow(train.data)-7),1]
  y_t_8 = train.data[2:(nrow(train.data)-8),1]
  y_t_9 = train.data[1:(nrow(train.data)-9),1]
  
  Un_t = train.data[10:(nrow(train.data)),2]
  Un_t_1 = train.data[9:(nrow(train.data)-1),2]
  Un_t_2 = train.data[8:(nrow(train.data)-2),2]
  Un_t_3 = train.data[7:(nrow(train.data)-3),2]
  Un_t_4 = train.data[6:(nrow(train.data)-4),2]
  Un_t_5 = train.data[5:(nrow(train.data)-5),2]
  Un_t_6 = train.data[4:(nrow(train.data)-6),2]
  Un_t_7 = train.data[3:(nrow(train.data)-7),2]
  Un_t_8 = train.data[2:(nrow(train.data)-8),2]
  Un_t_9 = train.data[1:(nrow(train.data)-9),2]
  
  # AR(9)
  AR9.fit = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4+y_t_5+y_t_6+y_t_7+y_t_8+y_t_9)
  AR9.fore[i-3105] = sum(AR9.fit$coef*c(1, y_t[length(y_t)], y_t_1[length(y_t)], y_t_2[length(y_t)], y_t_3[length(y_t)],y_t_4[length(y_t)],y_t_5[length(y_t)],y_t_6[length(y_t)],y_t_7[length(y_t)],y_t_8[length(y_t)]))
  
  ## ADL(9,1)
  ADL.fit1 = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4+y_t_5 +y_t_6+y_t_7+y_t_8+y_t_9+ Un_t_1)
  ADL.fore1[i-3105] = sum(ADL.fit1$coef*c(1, y_t[length(y_t)], y_t_1[length(y_t)], y_t_2[length(y_t)], y_t_3[length(y_t)],y_t_4[length(y_t)],y_t_5[length(y_t)],y_t_6[length(y_t)],y_t_7[length(y_t)],y_t_8[length(y_t)], Un_t[length(y_t)]))}

AR9.MAE = mean(abs(AR9.fore - y.t[3107:length(y.t)]),na.rm=TRUE)
ADL.MAE = mean(abs(ADL.fore1 - y.t[3107:length(y.t)]),na.rm=TRUE)


AR9.MSE = mean((AR9.fore - y.t[3107:length(y.t)])^2,na.rm=TRUE)
ADL.MSE = mean((ADL.fore1 - y.t[3107:length(y.t)])^2,na.rm=TRUE)

result = matrix(c(AR9.MAE, ADL.MAE, AR9.MSE, ADL.MSE), nrow = 2, byrow = T)
row.names(result) = c("MAE", "MSE")
colnames(result) = c("AR9", "ADL(9,1)")
result
```

# VIX & S&P 500 5분 실현변동성

## 1번

```{r}
mydata<-read.csv("c://Temp/RV_IV_data.csv",header=TRUE)
head(mydata)

#training ,test set 만들어 주기
mydata$Date<-as.Date(mydata$Date)

training<-subset(mydata,Date<"2016-01-04")
test<-subset(mydata,Date>="2016-01-04")

#obs 3107부터 test번

###1번문제
y.t = mydata$SNP.RV
x.t = mydata$VIX

yt = y.t[2:length(y.t)]
yt1 = y.t[1:(length(y.t)-1)]
xt = x.t[2:length(x.t)]

lm1<- lm(yt~yt1+xt)
summary(lm1)
```

xt의 t값이 39.74로 95% 수준에서 critical value인 1.96을 훨씬 능가하기 때문에 기각역에 속하고 따라서 추가예측력이 있다고 말할 수 있다.
(H0은 추가예측력 없다. H1은 추가예측력 있다.)

## 2번

```{r}
###2번문제 - bic기준으로 p값 구하기
#AR모형
AR.bic = c()
for(p in 1:10){
  AR.fit = Arima(y.t, order = c(p,0,0))
  AR.bic[p] = AIC(AR.fit,k = log(length(y.t)))
}
plot(AR.bic, type = "b", pch =19, main = "BIC", ylab = "", xlab = "p")
abline(v = which.min(AR.bic), col=2, lty = 2)
#bic기준 AR(p)의 적정 p는 8이다.

#ADL모형
y_t = y.t[9:length(y.t)]
y_t_1 = y.t[8:(length(y.t)-1)]
y_t_2 = y.t[7:(length(y.t)-2)]
y_t_3 = y.t[6:(length(y.t)-3)]
y_t_4 = y.t[5:(length(y.t)-4)]
y_t_5 = y.t[4:(length(y.t)-5)]
y_t_6 = y.t[3:(length(y.t)-6)]
y_t_7 = y.t[2:(length(y.t)-7)]
y_t_8 = y.t[1:(length(y.t)-8)]

y.data = cbind(y_t_1 = y_t_1, y_t_2= y_t_2, y_t_3 = y_t_3, y_t_4 = y_t_4,y_t_5= y_t_5, y_t_6= y_t_6, y_t_7 = y_t_7, y_t_8 = y_t_8)

Un_t_1 = x.t[8:(length(x.t)-1)]
Un_t_2 = x.t[7:(length(x.t)-2)]
Un_t_3 = x.t[6:(length(x.t)-3)]
Un_t_4 = x.t[5:(length(x.t)-4)]
Un_t_5 = x.t[4:(length(x.t)-5)]
Un_t_6 = x.t[3:(length(x.t)-6)]
Un_t_7 = x.t[2:(length(x.t)-7)]
Un_t_8 = x.t[1:(length(x.t)-8)]

x.data = cbind(Un_t_1 = Un_t_1, Un_t_2= Un_t_2, Un_t_3 = Un_t_3, Un_t_4 = Un_t_4,Un_t_5 = Un_t_5, Un_t_6= Un_t_6, Un_t_7 = Un_t_7, Un_t_8 = Un_t_8)

#ADL
all.case = expand.grid(1:8,1:8)
ADL.bic0 = matrix(0, ncol = 8, nrow = 8)
for( k in 1:nrow(all.case)){
  p = all.case[k,1]; q = all.case[k,2]
  ADL.fit = lm(y_t ~ y.data[,1:p] + x.data[,1:q])
  ADL.bic0[p,q] = AIC(ADL.fit,k = log(length(y.t)))
}

colnames(ADL.bic0) = c("q=1", "q=2", "q=3", "q=4","q=5", "q=6", "q=7", "q=8")
row.names(ADL.bic0) = c("p=1", "p=2", "p=3", "p=4","p=5", "p=6", "p=7", "p=8")
ADL.bic0
#ADL(6,6)이 bic가 -31057.77로 가장 작은 모형이다.


AR8.fore = c(); ADL.fore = c()
for(i in 3106:(length(y.t)-1)){
  train.data = data.frame(y.t = y.t[1:i], Unemp = x.t[1:i])
  y_t = train.data[9:nrow(train.data),1]
  y_t_1 = train.data[8:(nrow(train.data)-1),1]
  y_t_2 = train.data[7:(nrow(train.data)-2),1]
  y_t_3 = train.data[6:(nrow(train.data)-3),1]
  y_t_4 = train.data[5:(nrow(train.data)-4),1]
  y_t_5 = train.data[4:(nrow(train.data)-5),1]
  y_t_6 = train.data[3:(nrow(train.data)-6),1]
  y_t_7 = train.data[2:(nrow(train.data)-7),1]
  y_t_8 = train.data[1:(nrow(train.data)-8),1]
  
  
  Un_t = train.data[9:(nrow(train.data)),2]
  Un_t_1 = train.data[8:(nrow(train.data)-1),2]
  Un_t_2 = train.data[7:(nrow(train.data)-2),2]
  Un_t_3 = train.data[6:(nrow(train.data)-3),2]
  Un_t_4 = train.data[5:(nrow(train.data)-4),2]
  Un_t_5 = train.data[4:(nrow(train.data)-5),2]
  Un_t_6 = train.data[3:(nrow(train.data)-6),2]
  Un_t_7 = train.data[2:(nrow(train.data)-7),2]
  Un_t_8 = train.data[1:(nrow(train.data)-8),2]
  
  # AR(8)
  AR8.fit = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4+y_t_5 + y_t_6 +y_t_7 + y_t_8)
  AR8.fore[i-3105] = sum(AR8.fit$coef*c(1, y_t[length(y_t)], y_t_1[length(y_t)], y_t_2[length(y_t)], y_t_3[length(y_t)],y_t_4[length(y_t)],y_t_5[length(y_t)],y_t_6[length(y_t)],y_t_7[length(y_t)]))
  
  
  ## ADL(6,6)
  ADL.fit = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4 + y_t_5+ y_t_6 + Un_t_1 + Un_t_2 + Un_t_3+ Un_t_4 + Un_t_5 + Un_t_6)
  ADL.fore[i-3105] =sum(ADL.fit$coef*c(1, y_t[length(y_t)], y_t_1[length(y_t)], y_t_2[length(y_t)], y_t_3[length(y_t)], y_t_4[length(y_t)], y_t_5[length(y_t)], 
                                       Un_t[length(y_t)], Un_t_1[length(y_t)], Un_t_2[length(y_t)], Un_t_3[length(y_t)], Un_t_4[length(y_t)], Un_t_5[length(y_t)]))
  
}

#예측력 비교
AR8.MAE = mean(abs(AR8.fore - y.t[3107:length(y.t)]),na.rm=TRUE)
ADL.MAE = mean(abs(ADL.fore - y.t[3107:length(y.t)]),na.rm=TRUE)

AR8.MSE = mean((AR8.fore - y.t[3107:length(y.t)])^2,na.rm=TRUE)
ADL.MSE = mean((ADL.fore - y.t[3107:length(y.t)])^2,na.rm=TRUE)

result = matrix(c(AR8.MAE, ADL.MAE, AR8.MSE, ADL.MSE), nrow = 2, byrow = T)
row.names(result) = c("MAE", "MSE")
colnames(result) = c("AR8", "ADL(6,6)")

result
```

## 3번

```{r}
#install.packages("forecast")
mydata<-read.csv("c://Temp/RV_IV_data.csv",header=TRUE)
head(mydata)

#3번
y.t = mydata$VIX
x.t = mydata$SNP.RV

yt = y.t[2:length(y.t)]
yt1 = y.t[1:(length(x.t)-1)]
xt = x.t[2:length(x.t)]

##3번문제
lm2<- lm(yt~yt1+xt)
summary(lm2)
```

xt의 t값이 22.52로 95% 수준에서 critical value인 1.96을 훨씬 능가하기 때문에 기각역에 속하고 따라서 추가예측력이 있다고 말할 수 있다.
(H0은 추가예측력 없다. H1은 추가예측력 있다.)

## 4번

```{r}
y_t = y.t[6:length(y.t)]
y_t_1 = y.t[5:(length(y.t)-1)]
y_t_2 = y.t[4:(length(y.t)-2)]
y_t_3 = y.t[3:(length(y.t)-3)]
y_t_4 = y.t[2:(length(y.t)-4)]
y_t_5 = y.t[1:(length(y.t)-5)]

y.data = cbind(y_t_1 = y_t_1, y_t_2= y_t_2, y_t_3 = y_t_3, y_t_4 = y_t_4, y_t_5=y_t_5)

Un_t_1 = x.t[5:(length(x.t)-1)]
Un_t_2 = x.t[4:(length(x.t)-2)]
Un_t_3 = x.t[3:(length(x.t)-3)]
Un_t_4 = x.t[2:(length(x.t)-4)]
Un_t_5 = x.t[1:(length(x.t)-5)]

x.data = cbind(Un_t_1 = Un_t_1, Un_t_2= Un_t_2, Un_t_3 = Un_t_3, Un_t_4 = Un_t_4,Un_t_5 = Un_t_5)

library(forecast)

#2번문제 - bic기준으로 p값 구하기
AR.aic = c(); AR.bic = c()
for( p in 1:10){
  AR.fit = Arima(y.t, order = c(p,0,0))
  AR.aic[p] = AR.fit$aic
  AR.bic[p] = AIC(AR.fit,k = log(length(y.t)))
}
par(mfrow=c(1,2))
plot(AR.aic, type = "b", pch =19, main = "AIC", ylab = "", xlab = "p")
abline(v = which.min(AR.aic), col=2, lty = 2)
plot(AR.bic, type = "b", pch =19, main = "BIC", ylab = "", xlab = "p")
abline(v = which.min(AR.bic), col=2, lty = 2)
#bic기준 AR(p)의 적정 p는 9이다.

#ADL
all.case = expand.grid(1:5,1:5)

ADL.aic0 = matrix(0, ncol = 5, nrow = 5); ADL.bic0 = matrix(0, ncol = 5, nrow = 5)
for( k in 1:nrow(all.case)){
  p = all.case[k,1]; q = all.case[k,2]
  ADL.fit = lm(y_t ~ y.data[,1:p] + x.data[,1:q])
  ADL.aic0[p,q] = AIC(ADL.fit)
  ADL.bic0[p,q] = AIC(ADL.fit,k = log(length(y.t)))
}

colnames(ADL.aic0) = c("q=1", "q=2", "q=3", "q=4","q=5")
row.names(ADL.aic0) = c("p=1", "p=2", "p=3", "p=4","p=5")
ADL.aic0

colnames(ADL.bic0) = c("q=1", "q=2", "q=3", "q=4","q=5")
row.names(ADL.bic0) = c("p=1", "p=2", "p=3", "p=4","p=5")
ADL.bic0

# p=5, q=2 일 때 BIC가 13877.14로 가장 낮은 것을 확인할 수 있다. 

library(dplyr)
mydata2<-mydata %>% mutate(year=substr(Date,1,4))
mydata2 %>% filter(year<=2015) %>% summarise(count=n())

AR5.fore = c(); ADL.fore1 = c(); 
for(i in 3106:(length(y.t)-1)){
  train.data = data.frame(y.t = y.t[1:i], Unemp = x.t[1:i])
  y_t = train.data[6:nrow(train.data),1]
  y_t_1 = train.data[5:(nrow(train.data)-1),1]
  y_t_2 = train.data[4:(nrow(train.data)-2),1]
  y_t_3 = train.data[3:(nrow(train.data)-3),1]
  y_t_4 = train.data[2:(nrow(train.data)-4),1]
  y_t_5 = train.data[1:(nrow(train.data)-5),1]
  
  Un_t = train.data[6:(nrow(train.data)),2]
  Un_t_1 = train.data[5:(nrow(train.data)-1),2]
  Un_t_2 = train.data[4:(nrow(train.data)-2),2]
  Un_t_3 = train.data[3:(nrow(train.data)-3),2]
  Un_t_4 = train.data[2:(nrow(train.data)-4),2]
  Un_t_5 = train.data[1:(nrow(train.data)-5),2]
  
  # AR(5)
  AR5.fit = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4+y_t_5)
  AR5.fore[i-3105] = sum(AR9.fit$coef*c(1, y_t[length(y_t)], y_t_1[length(y_t)], y_t_2[length(y_t)], y_t_3[length(y_t)],y_t_4[length(y_t)]))
  
  ## ADL(5,2)
  ADL.fit1 = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4+y_t_5 + Un_t_1+Un_t_2)
  ADL.fore1[i-3105] = sum(ADL.fit1$coef*c(1, y_t[length(y_t)], y_t_1[length(y_t)], y_t_2[length(y_t)], y_t_3[length(y_t)],y_t_4[length(y_t)], Un_t[length(y_t)],Un_t_1[length(y_t)]))}

AR5.MAE = mean(abs(AR5.fore - y.t[3107:length(y.t)]),na.rm=TRUE)
ADL.MAE = mean(abs(ADL.fore1 - y.t[3107:length(y.t)]),na.rm=TRUE)


AR5.MSE = mean((AR5.fore - y.t[3107:length(y.t)])^2,na.rm=TRUE)
ADL.MSE = mean((ADL.fore1 - y.t[3107:length(y.t)])^2,na.rm=TRUE)

result = matrix(c(AR5.MAE, ADL.MAE, AR5.MSE, ADL.MSE), nrow = 2, byrow = T)
row.names(result) = c("MAE", "MSE")
colnames(result) = c("AR9", "ADL(9,1)")
result
```

