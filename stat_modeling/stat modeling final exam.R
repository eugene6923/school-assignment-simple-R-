#Final Exam Data

#Problem 1: Microwave oven sales

year<-seq(1,4)
quarter<-factor(rep(c(1,2,3,4),4))
time<-seq(1,16)
demand<-c(10,31,43,16,11,33,45,17,13,34,48,19,15,37,51,21)


##(b)
demand_ts<-ts(demand,frequency=4,start=1)
plot(decompose(demand_ts))

##(c)
lin.mod1<-lm(demand~quarter+time)
summary(lin.mod1)

##(d)
library(car)
dwt(lin.mod1,alternative="positive")

#2

##(b)
#Problem 2: Bran-Nu Cereal Demand
my.datafile <- tempfile()
cat(file=my.datafile, "
    43 145
    64 140
    62 135
    54 130
    81 125
    93 120
    53 115
    80 110
    87 105
    134 100
    120 95
    166 90
    118 85
    189 80
    174 75
    249 70
    317 65
    217 60
    297 55
    421 50
    488 45
    438 40
    ", sep=" ")

cereal <- read.table(my.datafile, header=FALSE, col.names=c("y","x"))
attach(cereal)
lin.mod2<-lm(log(y)~log(x))
rstandard(lin.mod2)
influence.measures(lin.mod2)

##(c)
shapiro.test(resid(lin.mod2))
library(lmtest)
bptest(lin.mod2,studentize = F)

##(d)
summary(lin.mod2)

##(e)
new<-data.frame(x=125)
predict(lin.mod2,newdata=new,interval = "prediction",level=0.99)



