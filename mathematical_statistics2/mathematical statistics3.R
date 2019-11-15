#1
## (c)
n<-seq(5,100,5)
length1<-2*qnorm(0.975)/sqrt(n)
length2<-2*sqrt(2)*gamma(n/2)*qt(0.975,n-1)/(sqrt(n)*sqrt(n-1)*gamma((n-1)/2))
plot(length1~n,type='l')
lines(length2~n,add=T,col='red')


#3
data2<-read.csv('http://faculty.etsu.edu/pricejr//MathStat2/BOOK_COST.csv',header=T)
attach(data2)
names(data2)

mean(BOOKS)+qnorm(0.975)*sd(BOOKS)/sqrt(length(BOOKS))
mean(BOOKS)-qnorm(0.975)*sd(BOOKS)/sqrt(length(BOOKS))

#4
## (c)
X<-c(5,7,8,20,17,2,24,8,8,6,4,3,42,10,18,5,7,8,4,5,10)
n1<-length(X)
sum<-sum(X)
2*sum/qchisq(0.975,2*n1)
2*sum/qchisq(0.025,2*n1)

## (d)
theta_hat<-sum/n1
theta_hat-qnorm(0.975)*theta_hat/sqrt(n1)
theta_hat+qnorm(0.975)*theta_hat/sqrt(n1)

