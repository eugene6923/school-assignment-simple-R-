# 1
theta1<-0.198*0.395*0.296*0.099*0.012
theta2<-0.063*0.25*0.375*0.250*0.063
theta3<-0.001*0.015*0.116*0.385*0.482
theta1
theta2
theta3

# 2
## (b)
d=read.csv('http://faculty.etsu.edu/pricejr//MathStat2/BOOK_COST.csv',header=T)
attach(d)
names(d)
n<-length(BOOKS)
xsum<-sum(BOOKS)
product<-prod(BOOKS)
curve((alpha-1)*sum(log(BOOKS))-xsum/(mean(BOOKS)/alpha)-n*log(gamma(alpha))-alpha*n*log(mean(BOOKS)/alpha),0,8,xname='alpha')
alpha<-seq(0.0001,30,0.001)
l<-(alpha-1)*sum(log(BOOKS))-xsum/(mean(BOOKS)/alpha)-n*log(gamma(alpha))-alpha*n*log(mean(BOOKS)/alpha)
which.max(l)
alpha[4192]
theta<-mean(BOOKS)/4.1911
theta

## (c)
hist(BOOKS, freq=FALSE)
curve(dgamma(x,4.1911,1/93.84927),add=T,col="red")

# 3
curve(-log(pi)-0.5*log(x*(1-x)))
abline(h=0,col="red")
(1+sqrt(1-4/(pi^2)))/2
(1-sqrt(1-4/(pi^2)))/2
