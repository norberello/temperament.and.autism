# temperament.and.autism
replicating analyses of the study:
Pisula E, Kawa R, Danielewicz D, Pisula W (2015) The Relationship between Temperament and Autistic Traits in a Non-Clinical Students Sample. PLOS ONE 10(4): e0124364 <https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0124364>
```r
library(ggplot2)
require(gridExtra)
#FCB activity and AQ females (correlation)
a<-ggplot(au.females, aes(x=AQ, y=FCB.Activity))+ 
  geom_point()+ggtitle("females") +
  xlab("autism spectrum quotient")+geom_smooth(method=lm)+theme_bw()
#FCB emotional reactivity and AQ males (correlation)
b<-ggplot(au.males, aes(x=AQ, y=FCB.Em.reactivity))+ 
  geom_point()+
  geom_smooth(method=lm)+theme_bw()+ggtitle("males") +
  xlab("autism spectrum quotient")
grid.arrange(a,b, ncol=2)
```
<img src="figures/fig 2.png">

# total brain volume in children with autism

<img src="two CI bands per group.png">
