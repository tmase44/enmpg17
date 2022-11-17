# Packages----
library(pacman)
p_load(tidyverse,readxl,ggpubr,gridExtra)

# data import----
pitfall<-read_xlsx('pitfall.xlsx')
woodts<-read_xlsx('woodlandts.xlsx')
qrbias<-read_xlsx('quadratbias.xlsx')

# factors----
pitfall$Type<-as.factor(pitfall$Type) # habitat
woodts$Habitat_FOR<-as.factor(woodts$Habitat_FOR) #habitat
qrbias$group<-as.factor(qrbias$group) #groups
# qrbias$quadrat<-as.factor # might need this later

# pitfall data----

# long transform the pitfall data
pitfalllong<-pitfall %>% 
  select(-`Sum of Individuals`,-`Count of taxa`) %>% 
  pivot_longer(names_to = 'genus',
               values_to = 'count',
               cols = c(-Type,-`Trap line`))
pitfalllong$genus<-as.factor(pitfalllong$genus)
pitfalllong$`Trap line`<-as.factor(pitfalllong$`Trap line`)

#total species occurence
pitfalltotal<-pitfalllong %>% 
  group_by(genus) %>% 
  summarise(n=sum(count)) %>% 
  arrange(desc(n))

# species occurrence by habitat
pitfallhabs<-pitfalllong %>% 
  group_by(Type,genus) %>% 
  summarise(n=sum(count)) %>% 
  arrange(Type,desc(n))

### pitfall plots---------

# plot habitats total individuals
pitfallhabs %>% 
  ggplot(aes(Type,n))+
  geom_col()+
  labs(y='number of individuals',
       x='Habitat',
       title='Total individuals per habitat')+
  theme_pubclean()

# plot habitats unique taxa
pitfallhabs %>% 
  filter(n>0) %>% 
  group_by(Type) %>% 
  summarise('unique_n'=n_distinct(genus)) %>% 
  arrange(desc(unique_n)) %>% 
  ggplot(aes(Type,unique_n))+
  geom_col()+
  labs(y='number of unique taxa',
       x='Habitat',
       title = 'Unique taxa per habitat')+
  theme_pubclean()


# bar species stacked
pitfallhabs %>% 
  ggplot(aes(reorder(genus,-n),n,fill=Type))+
  geom_col(position = 'stack')+
  labs(y='n individuals')+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=12,angle = 90, vjust = 0.5, hjust=1))+
  labs(y='number of individuals',
       x='Species, arranged most-least frequent overall',
       title = 'Species occurrence and distribution')

# boxplot over transects- trap variation
pitbox1<-pitfalllong %>% 
  group_by(Type,`Trap line`) %>% 
  summarise(n=sum(count)) %>% 
  filter(n<20) %>% # remove outliers - optional
  ggplot(aes(Type,n,color=Type))+
  geom_boxplot()+
  #geom_point(aes(shape=`Trap line`),
   #          alpha=.55,size=3,position=position_dodge(width=.05))+
  theme_pubclean()+
  labs(y='n individuals per trap-line',
       x='Habitat',
       title='Sample size variation between plots / habitats')

# 3 main taxa: 
# boxplot for each species, looking at their mean distribution by habitat

# Araneae | spiders
pitbox2<-pitfalllong %>% 
  filter(genus=='Araneae') %>% 
  ggplot(aes(Type,count,color=Type))+
  geom_boxplot()+
  #geom_point(aes(shape=`Trap line`),
  #          alpha=.55,size=3,position=position_dodge(width=.05))+
  theme_pubclean()+
  labs(y='n individuals per trap-line',
       x='Habitat',
       title='Araneae distribution across habitats')

# Diptera | flies, midges
pitbox3<-pitfalllong %>% 
  filter(genus=='Diptera') %>% 
  ggplot(aes(Type,count,color=Type))+
  geom_boxplot()+
  #geom_point(aes(shape=`Trap line`),
  #          alpha=.55,size=3,position=position_dodge(width=.05))+
  theme_pubclean()+
  labs(y='n individuals per trap-line',
       x='Habitat',
       title='Diptera distribution across habitats')

#Collembola | springtails
pitbox4<-pitfalllong %>% 
  filter(genus=='Collembola') %>% 
  ggplot(aes(Type,count,color=Type))+
  geom_boxplot()+
  #geom_point(aes(shape=`Trap line`),
  #          alpha=.55,size=3,position=position_dodge(width=.05))+
  theme_pubclean()+
  labs(y='n individuals per trap-line',
       x='Habitat',
       title='Collembola distribution across habitats')

#### combobox----
ggarrange(pitbox1,pitbox2,pitbox3,pitbox4,
          common.legend = TRUE,
          legend = 'bottom')

# woodland quadrat data
woodts
               
