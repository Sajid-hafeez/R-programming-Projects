# Question 1.1

df <- read.csv("d-3stocks9908.txt", sep="")
View(df)
library(fBasics)
dff = df*100
a =basicStats(dff)
df2 = log(df+1)*100
b =basicStats(df2)

df = log(df+1)
t.test(df$axp)
t.test(df$cat)
t.test(df$sbux)


# Question 1.2
df <- read.csv("m-gm3dx7508.txt", sep="")
dff = df*100
a =basicStats(dff)
df2 = log(df+1)*100
b =basicStats(df2)

df = log(df+1)
t.test(df$gm)
t.test(df$vw)
t.test(df$ew)
t.test(df$sp)

# Question 2.4
library(data.table)
df <- fread("2.4Data.txt")
d2 = df$CAP2RET
d10 = df$CAP10RET
Box.test(d2,lag=12,type='Ljung')

Box.test(d10,lag=12,type='Ljung')
acf(d2 , lag = 10)
fd =ts(d2)
m1=arima(fd,order=c(0,0,1))

m1

predict(m1,12)

# Question 2.6
da=read.table("power6.txt",header=F)
pow=da[,1]
plot(pow, type = 'l')
m1=arima(pow,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
m1

tsdiag(m1,gof=36)
predict(m1,24)

Box.test(da,lag=24,type='Ljung')


# Question 2.10
daaa <- fread("w-Aaa.txt")
names(daaa) <- c("Year", "Month", "Date", "Return")
basicStats(daaa)
dat1 <- daaa$Return
dbaa <- read.table("w-Baa.txt", quote="\"", comment.char="")
names(dbaa) <- c("Year", "Month", "Date", "Return")
sd = data.frame(daaa$Return,dbaa$Return)
a =basicStats(sd)
dat2 <- dbaa$Return
hist(dat1, col="brown", main = "Return Distribution of Aaa", xlab =
       "Return", ylim = c(0,200), breaks = 50)

densitydat1 = dnorm(dat1,mean = mean(dat1), sd = sd(dat1), log = FALSE)
plot(dat1,densitydat1, main = "Probalility Desity Curve of Aaa", xlab =
       "Return", ylab = "Return Probabilty", type="l")

cumdat1 = pnorm(dat1,mean = mean(dat1), sd = sd(dat1), log = FALSE)
plot(dat1,cumdat1, main = "Cumulative Density Curve of Aaa", xlab = "Return",
     ylab = "Cumulative Probability", type="l")

hist(dat2, col="skyblue3", main = "Return Distribution of Baa", xlab =
       "Return", ylim = c(0,400), breaks = 44)

densitydat2 = dnorm(dat2,mean = mean(dat2), sd = sd(dat2), log = FALSE)
plot(dat2,densitydat2, main = "Probalility Desity Curve of Baa", xlab =
       "Return", ylab = "Return Probabilty", type="l")
# Q 2. 11
library(fUnitRoots)
library(TSA)
ts.plot(dat1)
ddat1=diff(dat1)
plot(ddat1, type="l")


max.p=10
max.q=10
model.aic=matrix(,nrow=max.p+1,ncol=max.q+1)
for (p in 0:max.p){
  for (q in 0:max.q){
    model.aic[p+1,q+1]= arima(ddat1,order=c(p,0,q))$aic
  }
}

print(model.aic)
which(model.aic==min(model.aic),arr.ind=T)

mod1 = arima(dat1, order=c(10,1,9))
plot(mod1$residuals)
acf(mod1$residuals)
mod1$aic
tsdiag(mod1)
pacf(ddat1)

adfTest(ddat1, lags=9, type=c("c"))

# Question 12

ts.plot(dat1, type="l", ylim = c(4,18), ylab = "Return", col = "red")
par(new = TRUE)
ts.plot(dat2, type="l", ylim = c(4,18),ylab = "Return", col = "green")

ddat2 = diff(dat2)
ts.plot(ddat2)
mod1 = lm(dat1~dat2)
summary(mod1)
acf(mod1$residuals)
plot(mod1$residuals, type="l")
mod2 = lm(ddat1~ddat2-1)
summary(mod2)
acf(mod2$residuals)
plot(mod2$residuals, type="l")

acf(mod3$residuals)


# Q2.13
df <- read.table("m-ew6299.txt", quote="\"", comment.char="")
View(df)
fd = ts(df$V1)
plot(Aaa$V4, type = 'l')
m1=ar(fd,method='mle')
m1$order
m2=arima(fd,order=c(1,0,0))
m2

m1=arima(fd,order=c(0,0,1))
m1


# Question 15.8

library(tidyverse)
library(haven)
df <- read_dta(file = "nls_panel2.dta")
df1987 = filter(df,year==87)
model = lm(lwage~exper+exper2+south+union,df1987)
summary(model)


df1988 = filter(df,year==88)
model = lm(lwage~exper+exper2+south+union,df1988)
summary(model)


df8788 = filter(df,year==87 | year ==88)
model = lm(lwage~exper+exper2+south+union,df8788)
summary(model)
library(plm)

library(clubSandwich)


model = plm(lwage~exper+exper2+south+union,df8788, model = "within")

library(sandwich)
library(lmtest)
library(miceadds)
model = lm.cluster(lwage~exper+exper2+south+union,df8788)

coeftest(model, vcovHC(model, type = 'HC0', cluster = 'group'))

# Question 15.9

library(tidyverse)
library(haven)
df <- read_dta(file = "nls_panel2.dta")
df1987 = filter(df,year==87)
model = lm(lwage~educ+exper+exper2+black+south+union,df1987)
summary(model)


df1988 = filter(df,year==88)
model = lm(lwage~educ+exper+exper2+black+south+union,df1988)
summary(model)


df8788 = filter(df,year==87 | year ==88)
model = lm(lwage~educ+exper+exper2+black+south+union,df8788)
summary(model)
library(plm)

library(clubSandwich)


model2 = plm(lwage~educ+exper+exper2+black+south+union,df8788, model = "within")
summary(model2)


coeftest(model2, vcovHC(model, type = 'HC0', cluster = 'group'))


# Question 15.10
library(tidyverse)
library(haven)
df <- read_dta(file = "crime.dta")

df8187 = filter(df,year == 81 | year == 87)
model = lm(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lwmfg,df8187)
model = p(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen+lwmfg,df8187, model = "within")
summary(model)


model = lm(lcrmrte~ldensity+lprbarr+lprbconv+lprbpris+lavgsen+lwmfg+d82+d83+d84+d85+d86+d87,df8187)
summary(model)

model = plm(lcrmrte~ldensity+lprbarr+lprbconv+lprbpris+lavgsen+lwmfg+d82+d83+d84+d85+d86+d87,df8187, model = "within")
summary(model)





