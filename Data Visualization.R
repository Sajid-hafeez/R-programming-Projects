library(readr)
df <- read_csv("icecream.csv")


options(scipen = 999)
library(pastecs)
res <- stat.desc(df[,-6:-7])
round(res,2)

require(sjPlot)
library(tidyverse)
library(sjmisc)# load package
sjmisc::frq(df$seasons,title = "Seasons",out = "t")
plot_frq(df$country ,title='Country',type = 'bar',axis.title= 'Country(A & B)'  )

ggplot(df,aes(price))+geom_density()+facet_grid(~country)

ggplot(df,aes(seasons, fill=country) )+geom_bar(position = 'dodge')

library(tidyverse)
saleA = filter(df,country=='A')
saleb = filter(df,country=='B')
t.test(saleA$icecream_sales,saleb$icecream_sales)


library(Hmisc)
hist.data.frame(df,mtitl = "Histograms of data")

library("ggplot2")                     # Load ggplot2 package
library("GGally")
ggpairs(df) 

model = lm(icecream_sales~income+price+temperature+country+seasons , df)
summary(model)

install.packages("Rmisc")
library(Rmisc)
CI(df$ShopID,ci = 0.90)
CI(df$icecream_sales,ci = 0.90)
CI(df$income,ci = 0.90)
CI(df$price,ci = 0.90)
CI(df$temperature,ci = 0.90)

p = data.frame(income = 30000,price = 3, temperature = 23, country='A',seasons ='Spring')
predict(model, newdata = p)