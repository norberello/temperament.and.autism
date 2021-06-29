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

Hazlett HC, Poe M, Gerig G, Smith RG, Provenzale J, Ross A, Gilmore J, Piven J. Magnetic resonance imaging and head circumference study of brain size in autism: birth through age 2 years. Arch Gen Psychiatry. 2005 Dec;62(12):1366-76. doi: 10.1001/archpsyc.62.12.1366. PMID: 16330725.

<img src="two CI bands per group.png">
