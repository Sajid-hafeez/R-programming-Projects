setwd("C:/Users/Sajid/Desktop/R&Python/dataverse_files")
library(readr)
x03 <- read_csv("2003.csv")
x04 <- read_csv("2004.csv")
x05 <- read_csv("2005.csv")
x06 <- read_csv("2006.csv")
pl <- read_csv("plane-data.csv")

df = rbind(x03,x04,x05,x06)


min(x03$DepTime,na.rm = T)
x03$ArrDelay
attach(x03)
library(tidyverse)
ad = x03[order(-CRSArrDelay),]
sd = data.frame(ad$DepTime,ad$ArrDelay)

a = group_by(x03, x03$CRSDepTime) %>%
summarize(arrdl = mean(ArrDelay,na.rm = T),n0flght = n())
as = a[order(a$arrdl),]

a = group_by(x03, x03$DayOfWeek) %>%
  summarize(arrdl = mean(ArrDelay,na.rm = T),n0flght = n())
as = a[order(a$arrdl),]

dff = group_by(x03, TailNum) %>%
  summarize(mnftail = pl$issue_date)
as = a[order(a$arrdl),]
pl$
df = x03
df$tail
df=df%>%rename(tailnum=TailNum)

total <- merge(df,pl,by="tailnum")
a = data.frame(total$year,)

a = group_by(df,year) %>%
  summarize(arrdl = mean(ArrDelay,na.rm = T),n0flght = n())

