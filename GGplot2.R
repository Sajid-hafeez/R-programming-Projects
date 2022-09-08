library(readr)
df <- read_csv("C:/Users/Sajid/Desktop/final 466/final 466/Shopping_Basket(2).csv")
colSums(df[,-1])


df <- read_csv("C:/Users/Sajid/Desktop/final 466/final 466/Airfoil_Self-Noise.csv")
cor(df)
mdl = lm(V6~.,df)
c = data.frame(V1= 3.15,V2=5.4,V3=15.24,V4=39.6,V5=4.85029)
a = predict(mdl,c)
a


df <- read_csv("C:/Users/Sajid/Desktop/final 466/final 466/Prestige.csv")
library(CORElearn)
df= na.omit(df)
# Feature Selection by Information Gain
res <- attrEval(V6 ~ ., data=df,  estimator = "InfGain")
res


df <- read_csv("C:/Users/Sajid/Desktop/final 466/final 466/Ginzberg.csv")
kf = kmeans(df,3) 
df$cluster = factor(kf$cluster)
df$cluster
centers=as.data.frame(kf$centers)
centers

library(tidyverse)
# Prepare V2 AND V4 clusters for plotting 
ggplot(data=df, aes(x=V2, y=V4, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=V2,y=V4, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)#guide=FALSE)

# Prepare V2 AND V4 clusters for plotting 
ggplot(data=df, aes(x=V2, y=V6, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=V2,y=V6, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)#guide=FALSE)
# Prepare V2 AND V4 clusters for plotting 
ggplot(data=df, aes(x=V2, y=V5, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=V2,y=V5, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)#guide=FALSE)
