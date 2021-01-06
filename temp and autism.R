#read data
url<-"https://raw.githubusercontent.com/norberello/temperament.and.autism/main/pone.0124364.s001.csv"
au.data<-read.csv(url)

library(ggplot2)
library(hrbrthemes)
ggplot(au.data) + 
  geom_density(aes(x = AQ, color = Sex),
               alpha = 0.2)+theme_ipsum()

au.males<-au.data[which(au.data$Sex=="Male"),]
au.females<-au.data[which(au.data$Sex=="Female"),]


t.test(AQ~Sex,au.data)
lm0<-lm(AQ~1,au.data)
lm1<-lm(AQ~Sex,au.data)

summary(lm0)
mean(au.data$AQ)
summary(lm1)

anova(lm0,lm1)



