x<-c(1,4,4,3)
mean(x)
sd(x)

hist(x)
hist(x,densit=T)
rf<-hist(x,probability = T)#rel freq

hist(x,probability = T,xlim=c(0,5))#rel freq
curve(dnorm(x, mean=mean(x),
            sd=sd(x)),
      add=TRUE, col="red")
lines(density(x), lwd = 4,
      col = "blue",add=T)
?density#kernel density


a<-c(1.5,2,2.5,3,4)
b<-c(0.5,1,4,8,9)

mean(a)
mean(b)
sd(a)
sd(b)

summary(a)
summary(b)

boxplot(a,b)

sea<-sd(a)/sqrt(length(a))
seb<-sd(b)/sqrt(length(b))

hist(a,probability = T)
hist(b,probability = T)

t.test(a,b)


media <- mean(a)
s <- sd(a)
n <- length(a)
error <- qnorm(0.975)*s/sqrt(n)
left <- media-error
right <- media+error
left
right
#sample mean 2.6, sd 0.96
#Our level of certainty about the true mean
#is 95% in predicting that the true mean
#is within the interval between 1.75 and
#3.44 assuming that the original random variable
#is normally distributed, and the samples 
#are independent.
#bueno pues al final el CI es la media mas el 
#error standard!!!?? No!

sea<-sd(a)/sqrt(length(a))
#espera
sea*2#no, pero se acerca
sea*1.96
media+sea*1.96#aha, that's it
media-sea*1.96

#######
#######
mediab <- mean(b)
sb <- sd(b)
nb <- length(b)
errorb <- qnorm(0.975)*sb/sqrt(nb)
leftb <- mediab-errorb
rightb <- mediab+errorb
leftb
rightb

seb
ciup<-(1.96*seb)+mediab
cilow<-mediab-(1.96*seb)
mediab


d<-c(1,2)
hist(d)
hist(d,probability = T,xlim=c(0,3))
curve(dnorm(x, mean=mean(d),
            sd=sd(d)),
      add=TRUE, col="red")
lines(density(d), lwd = 4,
      col = "blue",add=T)


hist(a,probability = T)
curve(dnorm(x, mean=mean(a),
            sd=sd(a)),
      add=TRUE, col="red")
lines(density(a), lwd = 4,
      col = "blue",add=T)


hist(b,probability = T)
curve(dnorm(x, mean=mean(b),
            sd=sd(b)),
      add=TRUE, col="red")
lines(density(b), lwd = 4,
      col = "blue",add=T)


wilcox.test(a,b)


#try categorical data
animals <- c("cat", "dog",  "dog", "dog", "dog", "dog", "dog", "dog", "cat", "cat", "bird")
#I turn it into a factor for use with other vectors in my data frame:

table(animals)

animalFactor <- as.factor(animals)
#I now want to create a histogram that shows the frequency of each variable on the y-axis, the name of each factor on the x-axis, and contains one bar for each factor. I attempt this code:
  hist(table(animalFactor), 
       freq=TRUE, xlab = levels(animalFactor),
       ylab = "Frequencies")

  barplot(prop.table(table(animals)))#wonderful

  library(ggplot2)
  # counts
  ggplot(data.frame(animals), aes(x=animals)) +
    geom_bar()    
    
  x=sample(c("Richard", "Minnie", "Albert", "Helen", "Joe", "Kingston"),  
           50, replace=T)
  x=as.factor(x)
  plot(x)

  #toss coin:
#first is the number of random draws
  #size is the number of coins
rbinom(1,size=1,prob=0.5)  
rbinom(1,size=2,prob=0.5)  
  
rbinom(10,size=1,prob=0.5)  #flip 10 coins and the outcome can be 1 of 0
rbinom(10,size=1,prob=0.99) #with prob of 1 outcome being 99
rbinom(10,2,0.99) #ok it is the prob of what i put there
hist(rbinom(10000,6,1/6))




###wonderful
dbinom(x = 4,size = 20,prob = 1/6 ) #probability of getting 4 skulls
#in 20 dice tossess, well or 4 ones
#so, probability of not washing the dishes 7 nights
dbinom(x=7,size=7,prob=3/4)
(3/4)^7
#correct














require(graphics)
# Compute P(45 < X < 55) for X Binomial(100,0.5)
sum(dbinom(46:54, 100, 0.5))

## Using "log = TRUE" for an extended range :
n <- 2000
k <- seq(0, n, by = 20)
plot (k, dbinom(k, n, pi/10, log = TRUE), type = "l", ylab = "log density",
      main = "dbinom(*, log=TRUE) is better than  log(dbinom(*))")
lines(k, log(dbinom(k, n, pi/10)), col = "red", lwd = 2)
## extreme points are omitted since dbinom gives 0.
mtext("dbinom(k, log=TRUE)", adj = 0)
mtext("extended range", adj = 0, line = -1, font = 4)
mtext("log(dbinom(k))", col = "red", adj = 1)

#Binary probability

# Generate 10 separate random flips with probability .3
#10 separate tosses of the same coin
rbinom(10,1,0.3)
#one toss 10 possible outcomes
rbinom(1,10,0.3)

#10 separate tosses from two coins?
rbinom(10,2,0.5)#outcome should be 0 1 or 2

#sheldon not washing 1 or washing 0 in 14 days
rbinom(14,1,3/4)#outcome should be 0 1 or 2
hist(rbinom(14,1,3/4),probability = T)

# Generate 100 occurrences of flipping 10 coins,
#each with 30% probability
rbinom(100,10,0.30)
hist(rbinom(100,10,0.30))

#generate 7 occurrences of 7 nights not washing dishes for sheldon
rbinom(7,1,3/4)
hist(rbinom(7,1,3/4))
hist(rbinom(365,1,3/4),probability = T,breaks = 2)
hist(rbinom(365,7,3/4),probability = T,breaks = 7)


# Calculate the probability that 2 are heads out of a draw of 10 coins using dbinom
dbinom(2,10,0.3)

# Confirm your answer with a simulation using rbinom
mean(rbinom(10000,10,0.3)==2)
hist(rbinom(10000,10,0.3))


#ok, calculate the probability that 7 nights in a row no washing disshes
dbinom(7,7,0.75)
0.75^7
#exactly, what about two weeks
dbinom(14,14,0.75)

mean(rbinom(1000,7,0.75)==1)

dbinom(365,7,0.75)

# Generate 10 separate random flips with probability .3
#10 separate tosses of the same coin
rbinom(10,1,0.3)
#one toss 10 possible outcomes
rbinom(1,10,0.3)

#number of skulls in 20 tosses of dice
teta<-rbinom(20,1,1/6)
hist(teta,breaks=20)
hist(teta,breaks=20,probability = T)

teta<-rbinom(1000,20,1/6)
hist(teta,breaks=20,probability = T,xlim=c(0,20))

rbinom(20,20,1/6)
teta<-rbinom(20,20,1/6)
hist(teta,breaks=20,probability = T,xlim=c(0,20))
hist(teta,probability = T,xlim=c(0,20))

#Two binomial distributions, involving a scenario in which I’m 
#flipping a fair coin, so the underlying success
#probability is θ “ 1{2. In panel (a), 
#we assume I’m flipping the coin N “ 20 times. 
#In panel (b) we assume that the coin is flipped N “ 100 times.


hist(rbinom(20,20,0.5))



#So, for instance, suppose I 
#were to repeat my die rolling experiment
#100 times. I could get R to simulate
#the results of these experiments by
#using the following command:

rbinom( n = 100, size = 20, prob = 1/6 )
hist(rbinom( n = 100, size = 20, prob = 1/6 ))

#Let's say you wanted to simulate rolling a dice 5 times, 
#and you wished to count the number of 3's you observe. 
#You could simulate this experiment using the following code:
#this means through 5 dice and count the number of 3s

### rbinom(number of experiments, number of observations per
#experiment, probability of success)

rbinom(1,5,1/6)  
#draw 5 dice together or one by one (doesnt matter)
#do this twice, and count the number of skulls (any single side)
rbinom(2,5,1/6)  
#so if 10000 times you do this and collect number of skulls
hist(rbinom(10000,5,1/6)) 
#toss a single dice 10000 times and calculate number of ones
hist(rbinom(10000,1,1/6)) 
#so an event has two possible outcomes and the probability of first outcome
#is 1/6


#again:
hist(rbinom(10000,2,1/6)) 
#and event has three possible outcomes and the prob of outcome 2 is 1/6

hist(rbinom(10000,3,1/6)) 
#the question is what is the prob of outcome 2 and 1
#oh, because it is the probability of 3 together

rbinom(1,5,1/6)    ### rbinom(number of experiments, number of observations per experiment, probability of success)
#R OUTPUT
#[1]   2
#Conclusion: The above code simulated rolling a die five times.
#The output of 2 means that there were 
#2 "successes", or 2 observed 3's as rolling a 3 was 
#labeled as a "success".

#simulate not washing the dishes 365 nights
a<-rbinom(365,1,3/4)
hist(a,probability = T)
hist(a)

#cannot be posible becuase this is a discrete variable!!!
curve(dbinom(x, mean=mean(a),
            sd=sd(a)),
      add=TRUE, col="red")
lines(density(a), lwd = 4,
      col = "blue",add=T)




#probability of not washing the dishes 365
dbinom(1,365,3/4)
#correct?
dbinom(1,7,3/4)
0.75^7


#Suppose that 80% of adults with allergies 
#report symptomatic relief with a specific 
#medication. If the medication is given to 10 
#new patients with allergies, what is the probability
#that it is effective in exactly seven?

dbinom(7,10,0.8)

#The likelihood that a patient with a heart attack dies of the attack is 0.04 
#(i.e., 4 of 100 die of the attack).
#Suppose we have 5 patients who suffer a heart attack, what is the probability that all will survive? 
#For this example, we will call a success a fatal attack (p = 0.04).
#We have n=5 patients and want to know the probability 
#that all survive or, in other words, that none are fatal (0 successes).

dbinom(0,5,0.04)

#and the probability 1 person dies
dbinom(1,5,0.04)
dbinom(2,5,0.04)
dbinom(5,5,0.04)


#now generate 10000 experiments with prob that one out  5 person dies
#being 0.04
hist(rbinom(10000,5,0.04))
#now generate 10000 experiments with prob that of surviving
hist(rbinom(10000,5,0.96))






#Suppose there are twelve multiple choice questions
#in an English class quiz.
#Each question has five possible answers, 
#and only one of them is correct. 
#Find the probability of having four or less correct answers
#if a student attempts to answer every question at random.
dbinom(4,12,1/5)#having exactly 4
pbinom(4, size=12, prob=0.2) #having 4 or less

#un ugadodor
#encesta con probabilidad 0.55.
#Calcula la probabilidad de que al tirar 6 veces enceste:
pbinom(1,6,0.55)
#simulo 6 tiros con prob 0.5
rbinom(6,1,0.55)
#ahora simulo 1000 repeticiones de 6 tiros y calculo el numero de aciertos
hist(rbinom(10000,6,0.55))
#Calcula la probabilidad de que al tirar 6 veces no enceste
pbinom(0,6,0.55)

#y esto?
rbinom(6,6,0.55)
#pues seria repite 6 series de 6 tiros


#Un laboratorio afirma que una droga causa efectos secundarios
#en una proporción de 3 de cada 100 pacientes.
#Para contrastar esta afirmación, otro laboratorio elige
#al azar a 5 pacientes a los que aplica la droga.
#probablitdad Ningún paciente tenga efectos secundarios
#Para contrastar esta afirmación, otro laboratorio
#elige al azar a 5 pacientes a los que aplica la droga.

dbinom(0,5,3/100)

#prob Al menos dos tengan efectos secundarios

dbinom(2,5,3/100)

#I’m flipping the coin N “ 20 times.
#and count the number of tails
#or toss 20 coins and count the tails 1000 times
hist(rbinom(1000,20,0.5))
hist(rbinom(1000,100,0.5),breaks=100)



#normal distribution
dnorm( x = 1, mean = 1, sd = 0.1 )
normal.a <- rnorm( n=1000, mean=0, sd=1 )
#1000 numbers with mean 0 and sd 1 that are normally distributed
hist(normal.a)

normal.b <- rnorm( n=1000 )  # another set of normally distributed data
normal.c <- rnorm( n=1000 )  # and another!
#Now that we’ve done that, the theory says we should square these and add them together, like this
chi.sq.3 <- (normal.a)^2 + (normal.b)^2 + (normal.c)^2
hist(chi.sq.3)

#Suppose we “scale” our chi-square data by dividing it by the degrees of freedom, like so
scaled.chi.sq.3 <- chi.sq.3 / 3
#We then take a set of normally distributed variables and divide them by (the square root of) our scaled
#chi-square variable which had df “ 3, and the result is a t distribution with 3 degrees of freedom: > normal.d <- rnorm( n=1000 ) # yet another set of normally distributed data
t.3 <- normal.d / sqrt( scaled.chi.sq.3 )  # divide by square root of scaled chi-square to get t
#If we plot the histogram of t.3, we end up with something that looks very similar to Figure 9.13c. Similarly, we can obtain an F distrib
normal.d <- rnorm( n=1000 ) # yet another set of normally distributed data
t.3 <- normal.d / sqrt( scaled.chi.sq.3 )  # divide by square root of scaled chi-square to get t
hist(t.3)


chi.sq.20 <- rchisq( 1000, 20)  # generate chi square data with df = 20...
scaled.chi.sq.20 <- chi.sq.20 / 20  # scale the chi square variable...
F.3.20 <-  scaled.chi.sq.3  / scaled.chi.sq.20 # take the ratio of the two chi squares...
hist( F.3.20 ) # ... and draw a picture

#####
####

a <- c(0,0,0,1,1,2)
library(lattice)
histogram(a)
#latice does percentage

datax<-rnorm(1000,50,15)#n=100 con mean 50 y sd 15
histogram(datax)

hist(datax,freq=FALSE)
hist(datax,freq=FALSE,right = F)

h<-hist(datax, plot=F)
h$counts <- h$counts / sum(h$counts)
plot(h, freq=TRUE, ylab="Relative Frequency")
boxplot(datax)


#https://www.openintro.org/data/
#many datasets
#install.packages("openintro")
library(openintro)
?openintro
exclusive_relationship
histogram(exclusive_relationship$num)
hist(exclusive_relationship$num)
hist(exclusive_relationship$num,probability = T)




relations<-na.omit(exclusive_relationship)
str(relations)
h<-hist(relations$num, plot=F)
h$counts <- h$counts / sum(h$counts)
plot(h, freq=TRUE, ylab="Relative Frequency")
lines(density(relations$num))
lines(density(relations$num,
              adjust=2), lty="dotted", col="darkgreen", lwd=2) 

hist(relations$num,freq=FALSE)
lines(density(relations$num,
              adjust=0.5), lty="dotted", 
      col="darkgreen", lwd=2) 

par(mfrow=c(2,3))
hist(datax,freq=FALSE,breaks=100)
lines(density(datax,
              adjust=1),
      col="red", lwd=2) 
hist(datax,freq=FALSE,breaks=50)
lines(density(datax,
              adjust=1),
      col="red", lwd=2) 
hist(datax,freq=FALSE,breaks=25)
lines(density(datax,
              adjust=1),
      col="red", lwd=2) 
hist(datax,freq=FALSE,breaks=5)
lines(density(datax,
              adjust=1),
      col="red", lwd=2) 
hist(datax,freq=FALSE,breaks=2)
lines(density(datax,
              adjust=1),
      col="red", lwd=2) 
hist(datax,freq=FALSE,breaks=200)
lines(density(datax,
              adjust=1),
      col="red", lwd=2) 
par(mfrow=c(1,1))


str(babies_crawl)
head(babies_crawl)
plot(babies_crawl$temperature,babies_crawl$avg_crawling_age)
cor(babies_crawl$temperature,babies_crawl$avg_crawling_age)
#it would be challenging to build a datase based on
#mean months and sd months¡ do it!

jan <- data.frame(month=rep(c("Jan"), each=32),
                      age=rnorm(32,29.8,7.08),
                  temp=rnorm(32,66,2))
feb <- data.frame(month=rep(c("Feb"), each=36),
                      age=rnorm(36,30.5,6.96),
                  temp=rnorm(36,73,2))
mar <- data.frame(month=rep(c("Mar"), each=23),
                  age=rnorm(23,29.7,8.33),
                  temp=rnorm(23,72,2))
apr <- data.frame(month=rep(c("Apr"), each=26),
                  age=rnorm(26,31.8,6.21),
                  temp=rnorm(26,63,2))
may <- data.frame(month=rep(c("May"), each=27),
                  age=rnorm(27,28.6,8.07),
                  temp=rnorm(27,52,2))
jun <- data.frame(month=rep(c("Jun"), each=29),
                  age=rnorm(29,31.4,8.1),
                  temp=rnorm(29,39,2))
jul <- data.frame(month=rep(c("Jul"), each=21),
                  age=rnorm(21,33.6,6.91),
                  temp=rnorm(21,33,2))
aug <- data.frame(month=rep(c("Aug"), each=45),
                  age=rnorm(45,32.8,7.61),
                  temp=rnorm(45,37,2))
sep <- data.frame(month=rep(c("Sep"), each=38),
                  age=rnorm(38,33.8,6.93),
                  temp=rnorm(38,33,2))
oct <- data.frame(month=rep(c("Oct"), each=44),
                  age=rnorm(44,33.4,7.29),
                  temp=rnorm(44,37,2))
nov <- data.frame(month=rep(c("Nov"), each=49),
                  age=rnorm(49,33.4,7.42),
                  temp=rnorm(49,48,2))
dec <- data.frame(month=rep(c("Dec"), each=44),
                  age=rnorm(44,32.3,5.71),
                  temp=rnorm(44,57,2))
crawl.ds<-rbind(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)
write.csv(crawl.ds,"datasetcrawl.csv")
dataset<-read.csv("datasetcrawl.csv",header = T)
boxplot(age~month,data=dataset)


dataset$month <- factor(dataset$month, c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                         "Jul","Aug","Sep","Oct","Nov","Dec"))
# plot x axis as ordered
boxplot(age ~ month, outline = FALSE,dataset)

head(dataset)

head(crawl.ds)
tail(crawl.ds)
plot(crawl.ds$temp,crawl.ds$age)
cor(crawl.ds$temp,crawl.ds$age)
boxplot(age~month,crawl.ds)
boxplot(temp~month,crawl.ds)

#reduce range
jan <- data.frame(month=rep(c("Jan"), each=32),
                  age=rnorm(32,29.8,1.5),
                  temp=rnorm(32,66,1.5))
feb <- data.frame(month=rep(c("Feb"), each=36),
                  age=rnorm(36,30.5,1.5),
                  temp=rnorm(36,73,1.5))
mar <- data.frame(month=rep(c("Mar"), each=23),
                  age=rnorm(23,29.7,1.5),
                  temp=rnorm(23,72,1.5))
apr <- data.frame(month=rep(c("Apr"), each=26),
                  age=rnorm(26,31.8,1.5),
                  temp=rnorm(26,63,1.5))
may <- data.frame(month=rep(c("May"), each=27),
                  age=rnorm(27,28.6,1.5),
                  temp=rnorm(27,52,1.5))
jun <- data.frame(month=rep(c("Jun"), each=29),
                  age=rnorm(29,31.4,1.5),
                  temp=rnorm(29,39,1.5))
jul <- data.frame(month=rep(c("Jul"), each=21),
                  age=rnorm(21,33.6,1.5),
                  temp=rnorm(21,33,1.5))
aug <- data.frame(month=rep(c("Aug"), each=45),
                  age=rnorm(45,32.8,1.5),
                  temp=rnorm(45,37,1.5))
sep <- data.frame(month=rep(c("Sep"), each=38),
                  age=rnorm(38,33.8,1.5),
                  temp=rnorm(38,33,1.5))
oct <- data.frame(month=rep(c("Oct"), each=44),
                  age=rnorm(44,33.4,1.5),
                  temp=rnorm(44,37,1.5))
nov <- data.frame(month=rep(c("Nov"), each=49),
                  age=rnorm(49,33.4,1.5),
                  temp=rnorm(49,48,1.5))
dec <- data.frame(month=rep(c("Dec"), each=44),
                  age=rnorm(44,32.3,1.5),
                  temp=rnorm(44,57,1.5))
id<-c(1:413,by=1)#if needed
str(id)

crawl.ds<-rbind(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)
str(crawl.ds)




#default ggplot boxplot
crawl.ds$id<-c(1:413,by=1)#why 414 does not work?
tail(crawl.ds)
ggplot(crawl.ds,
       aes(month, age)) + geom_boxplot()

data_3 <- crawl.ds      
str(data_3)# Replicate example data
data_3 <- cbind(infantID = id,crawl.ds)     # Add new column to data
tail(data_3)
#neet to change infant ID 1 to 414
data_3[414, 1] = 414
tail(data_3)

boxplot(age~month,data=data_3)

names(data_3)
ggplot(data_3, aes(x=month, y=age, group=month)) +
  geom_boxplot(fill="light blue")


data_3<-as.factor(data_3$month)
str(data_3)
table(data_3$month)

#The plot is now ordered !
boxplot(data$value ~ data$names , col=rgb(0.3,0.5,0.4,0.6) , ylab="value" , 
        xlab="names in desired order")



#B. Laeng et al. Why do blue-eyed men prefer women with the same eye color? In: Behavioral Ecology and Sociobiology 61.3 (2007), pp. 371-384.
str(assortive_mating)
head(assortive_mating)
tab.partners<-table(assortive_mating$self_male,assortive_mating$partner_female)
tab.partners
chisq.test(tab.partners)

#The Relationship between Temperament and Autistic Traits in a Non-Clinical
#Students Sample
#https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0124364

url<-"https://raw.githubusercontent.com/norberello/temperament.and.autism/main/pone.0124364.s001.csv"
au.data<-read.csv(url)
head(au.data)
#AQ autistic spectrum quotient
hist(au.data$AQ,breaks=30)

hist(au.data$AQ,breaks=30,probability = T)
lines(density(au.data$AQ))

boxplot(au.data$AQ)
plot(au.data$FCB.Em.reactivity,au.data$AQ,
     pch=18,cex=0.75)



library(ggplot2)
ggplot(au.data) + geom_histogram(aes(x = AQ))
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
ggplot(au.data) +
  geom_histogram(aes(x = AQ),
                 binwidth = 0.8, fill = "grey", color = "black")

plot(density(au.data$AQ, bw = 0.5))
rug(jitter(au.data$AQ))

library(lattice)
densityplot(~ AQ, data = au.data)
densityplot(~ AQ, group = Sex, data = au.data,
            auto.key = TRUE)

ggplot(au.data) + 
  geom_density(aes(x = AQ, color = Sex))

library(hrbrthemes)
ggplot(au.data) + 
  geom_density(aes(x = AQ, color = Sex),
               alpha = 0.2)+theme_ipsum()


ggplot(au.data) +
  geom_density(aes(x = AQ)) +
  facet_wrap(~Sex)+theme_transparent()


ggplot(au.data, aes(AQ)) + 
  stat_ecdf(geom = "point")
ggplot(au.data, aes(AQ)) +
  stat_ecdf(geom = "step")

# Basic ECDF plot
ggplot(au.data, aes(AQ)) + stat_ecdf(geom = "step")+
  labs(title="Empirical Cumulative \n Density Function",
       y = "F(AQ)", x="AQ (Autism quotient)")+
  theme_classic()

# Multiple ECDFs
ggplot(au.data, aes(AQ,colour=Sex)) + 
  stat_ecdf()

# Another option for geom = "point"
ggplot(au.data, aes(AQ)) +
  stat_ecdf(aes(color = Sex,linetype = Sex), 
            geom = "step", size = 1.5) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  labs(y = "f(AQ)")

plot(ecdf(au.data$AQ))       # Create ecdf plot in R

str(au.data)
library(GGally)
ggpairs(au.data, columns = c(2,3,4))
ggpairs(au.data, columns = c("age","Sex","AQ"))
ggpairs(au.data, columns = c(4:15))

ggpairs(au.data, columns = 2:4,
        ggplot2::aes(colour=Sex))

df<-au.data[, c(4:15)]
ggcorr(au.data[, c(4:15)],
       palette = "RdBu", label = TRUE)
ggcorr(au.data[, c(4:15)],
       palette = "RdGn", label = TRUE)

ggcorr(df, palette = "RdYl", 
       label = T, label_color = "black")

library(corrplot)
cor.data = as.matrix(au.data[, c(4:15)])
round(cor(cor.data),2)

corrplot(cor.data)

library(corrplot)
corrplot(cor.data, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


#library(ggplot2)
ggpairs(au.data, mapping = aes(color = Sex),
        columns = c(2,3,4))

str(env_regulation)
table(env_regulation$statement)
barplot(table(env_regulation$statement),las=2)
pie(table(env_regulation$statement))
x.out<-chisq.test(table(env_regulation$statement))
x.out$stdres



str(friday)
boxplot(friday$sixth,friday$thirteenth,ylim=c(0,10000))
wilcox.test(friday$sixth,friday$thirteenth)
wilcox.test(friday$diff)

str(murders)
head(murders)
hist(murders$annual_murders_per_mil)
hist(murders$annual_murders_per_mil,
     probability = T,breaks = 10)
lines(density(murders$annual_murders_per_mil))
abline(v=mean(murders$annual_murders_per_mil),
       col="blue")

plot(density(murders$annual_murders_per_mil))

plot(murders$perc_unemp,murders$annual_murders_per_mil)
plot(murders$perc_pov,murders$annual_murders_per_mil)
plot(murders$population,murders$annual_murders_per_mil)

library(GGally)
ggpairs(murders)
model.m<-lm(annual_murders_per_mil~perc_unemp,data=murders)
plot(murders$perc_unemp,murders$annual_murders_per_mil)
abline(model.m)
summary(model.m)




#Graybill, F.A. & Iyer, H.K., (1994) Regression Analysis: Concepts and Applications, Duxbury, p. 511-6.
summary(gifted)



