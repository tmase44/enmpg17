library(tidyverse)
library(ggplot2)
library(readxl)
library(ggpubr)

#read
data <- read.csv("C:/Users/tmaso/OneDrive/Msc Environmental Management/Dissertation/R-Analysis/enmpg17/pitfalldata.csv")
data$Habitat<-as.factor(data$Habitat)
data$TransectID<-as.factor(data$TransectID)
data$TrapID<-as.factor(data$TrapID)


dim(data)

#total species occurence
total<-data %>% 
  group_by(Invertebrate.taxon) %>% 
  summarise(n=sum(individuals)) %>% 
  arrange(desc(n))

# species occurrence by habitat
habitats<-data %>% 
  group_by(Habitat,Invertebrate.taxon) %>% 
  summarise(n=sum(individuals)) %>% 
  arrange(Habitat,desc(n))

### plots---------

# plot habitats total individuals
habitats %>% 
  ggplot(aes(Habitat,n))+
  geom_col()+
  labs(y='number of individuals',
       title='Total individuals per habitat')+
  theme_pubclean()

# plot habitats unique species
habitats %>% 
  group_by(Habitat) %>% 
  summarise('unique_n'=n_distinct(Invertebrate.taxon)) %>% 
  arrange(desc(unique_n)) %>% 
  ggplot(aes(Habitat,unique_n))+
  geom_col()+
  labs(y='number of unique species',
       title = 'Unique species per habitat')+
  theme_pubclean()


# bar species stacked
habitats %>% 
  filter(Invertebrate.taxon!='BLANK'&Invertebrate.taxon!='bug') %>% 
  ggplot(aes(reorder(Invertebrate.taxon,-n),n,fill=Habitat))+
  geom_col(position = 'stack')+
  labs(y='n individuals')+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=12,angle = 90, vjust = 0.5, hjust=1))+
  labs(y='number of individuals',
       x='Species, arranged most-least frequent overall',
       title = 'Species occurrence and distribution')

# boxplot over transects- trap variation
data %>% 
  group_by(Habitat,TransectID,TrapID) %>% 
  summarise(n=sum(individuals)) %>% 
  ggplot(aes(Habitat,n))+
  geom_boxplot(aes(color=Habitat),size=1)+
  geom_point(aes(shape=TrapID),
             alpha=.5,size=3,position=position_dodge(width=.2))+
  theme_pubclean()+
  labs(y='number of individuals',
       title='Sample size variation between plots / habitats')


          