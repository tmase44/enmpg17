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
p1<-pitfallhabs %>% 
  ggplot(aes(Type,n))+
  geom_col()+
  labs(y='n individuals',
       x='Habitat',
       title='Total individuals per habitat')+
  theme_pubclean()

# plot habitats unique taxa
p2<-pitfallhabs %>% 
  filter(n>0) %>% 
  group_by(Type) %>% 
  summarise('unique_n'=n_distinct(genus)) %>% 
  arrange(desc(unique_n)) %>% 
  ggplot(aes(Type,unique_n))+
  geom_col()+
  labs(y='n unique taxa',
       x='Habitat',
       title = 'Unique taxa per habitat')+
  theme_pubclean()


# bar species stacked
p3<-pitfallhabs %>% 
  ggplot(aes(reorder(genus,-n),n,fill=Type))+
  geom_col(position = 'stack')+
  labs(y='n individuals')+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=12,angle = 90, vjust = 0.5, hjust=1))+
  labs(y='n individuals',
       x='Taxa',
       title = 'Species occurrence and distribution')

#### plots richness----
grid.arrange(p3,
             arrangeGrob(p1,p2,ncol = 2),
             nrow=2)

# boxplot over transects- trap variation
pitbox1<-pitfalllong %>% 
  group_by(Type,`Trap line`) %>% 
  summarise(n=sum(count)) %>% 
  #filter(n<20) %>% # remove outliers - optional
  ggplot(aes(Type,n,color=Type))+
  geom_boxplot(outlier.colour = 'black',outlier.size = 4)+
  stat_summary(fun = 'mean')+
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
  geom_boxplot(outlier.colour = 'black',outlier.size = 4)+
  stat_summary(fun = 'mean')+
  theme_pubclean()+
  labs(y='n individuals per trap-line',
       x='Habitat',
       title='Araneae distribution across habitats')

# Diptera | flies, midges
pitbox3<-pitfalllong %>% 
  filter(genus=='Diptera') %>% 
  ggplot(aes(Type,count,color=Type))+
  geom_boxplot(outlier.colour = 'black',outlier.size = 4)+
  stat_summary(fun = 'mean')+
  theme_pubclean()+
  labs(y='n individuals per trap-line',
       x='Habitat',
       title='Diptera distribution across habitats')

#Collembola | springtails
pitbox4<-pitfalllong %>% 
  filter(genus=='Collembola') %>% 
  ggplot(aes(Type,count,color=Type))+
  geom_boxplot(outlier.colour = 'black',outlier.size = 4)+
  stat_summary(fun = 'mean')+
  theme_pubclean()+
  labs(y='n individuals per trap-line',
       x='Habitat',
       title='Collembola distribution across habitats')

#### combobox----
ggarrange(pitbox1,pitbox2,pitbox3,pitbox4,
          common.legend = TRUE,
          legend = 'bottom')

# woodland quadrats----

# F = frequency, in now many quadrats it appeared
# C = cover, % cover, visual estimate 

# checking calcs against example data
## MEAN, SD & SE
woodts %>% 
  select(Habitat_FOR,Hylocomium_splendens_C) %>% 
  filter(Habitat_FOR=='R') %>% 
  summarise(n=n(),
            mean=mean(Hylocomium_splendens_C),
            SD=sd(Hylocomium_splendens_C),
            SE=SD/sqrt(n))

# CORRECT 53.3, 34.4 & 6.3

woodts %>% 
  select(Habitat_FOR,Hylocomium_splendens_C) %>% 
  filter(Habitat_FOR=='R') %>% 
  summarise(mean=mean(Hylocomium_splendens_C))

# convert names to match pifall habitats
woodts2<-woodts %>% 
  mutate('Habitat'=case_when(Habitat_FOR=='F'~'Mature Woodland (F)',
                             Habitat_FOR=='O'~'Open Habitat (O)',
                             Habitat_FOR=='R'~'Regenerating Woodland (R)'))

# patterns between vegetation density, height, richness and tree density?

woodts2 %>%
  ggplot(aes(Habitat,Height_cm,color=Habitat))+
  geom_boxplot()+
  geom_jitter(aes(Habitat,Height_cm),
              height = .05,width=.05,alpha=.5)+
  theme_pubclean()
# vegetation is taller in O, comparable in F & R

woodts2 %>%
  ggplot(aes(Habitat,tree_qty,color=Habitat))+
  geom_boxplot()+
  geom_jitter(aes(Habitat,tree_qty),
              height = .05,width=.05,alpha=.5)+
  
  theme_pubclean()
# more trees in R --> it is not intentionally planted F - old plantation

woodts2 %>%
  ggplot(aes(Habitat,taxa_unique,color=Habitat))+
  geom_boxplot()+
  geom_jitter(aes(Habitat,taxa_unique),
              height = .05,width=.05,alpha=.5)+
  theme_pubclean()
# need to look at the SD and SE because there is signifcantly more variation in the mature woodland

# MEAN, SD, SE FOR ALL COLS Hab == R
woodts2 %>% 
  group_by(Habitat_FOR) %>% 
  select(c(2:15)) %>% 
  summarise_each(funs(mean))
