# Packages----
library(pacman)
p_load(tidyverse,readxl,ggpubr,gridExtra,reshape2)

# data import----
pitfall<-read_xlsx('pitfall.xlsx')
woodts<-read_xlsx('woodlandts.xlsx')
#qrbias<-read_xlsx('quadratbias.xlsx') # this will helpto support which data is more accurate

# factors----
pitfall$Type<-as.factor(pitfall$Type) # habitat
woodts$Habitat_FOR<-as.factor(woodts$Habitat_FOR) #habitat
#qrbias$group<-as.factor(qrbias$group) #groups
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
  geom_boxplot(outlier.shape=NA)+
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
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = 'mean')+
  theme_pubclean()+
  labs(y='n individuals per trap-line',
       x='Habitat',
       title='Araneae distribution across habitats')

# Diptera | flies, midges
pitbox3<-pitfalllong %>% 
  filter(genus=='Diptera') %>% 
  ggplot(aes(Type,count,color=Type))+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = 'mean')+
  theme_pubclean()+
  labs(y='n individuals per trap-line',
       x='Habitat',
       title='Diptera distribution across habitats')

#Collembola | springtails
pitbox4<-pitfalllong %>% 
  filter(genus=='Collembola') %>% 
  ggplot(aes(Type,count,color=Type))+
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun = 'mean')+
  theme_pubclean()+
  labs(y='n individuals per trap-line',
       x='Habitat',
       title='Collembola distribution across habitats')

#### combobox----
ggarrange(pitbox1,pitbox2,pitbox3,pitbox4,
          common.legend = TRUE,
          legend = 'bottom')

# WOODTS QUADRATS----

# first remove all COVER columns because the variance is too hight
# Frequency only

woodlandqr<-woodts %>% 
  select(-c(1,9:15))
view(woodlandqr)

## vegetation height variance----
woodlandqr %>% 
  ggplot(aes(Habitat_FOR,Height_cm))+
  geom_boxplot()+
  theme_pubclean()
# height does not vary much between plots
# F mean=53.4  SD=17.3  SE=3.06
# O mean=60.7  SD=15.1  SE=2.67
# R mean=46.9  SD=15.7  SE=2.86
woodlandqr %>% 
  select(Habitat_FOR,Height_cm) %>% 
  filter(Habitat_FOR=='R') %>% 
  summarise(n=n(),
            mean=mean(Height_cm),
            SD=sd(Height_cm),
            SE=SD/sqrt(n))

## unique taxa variance----
woodlandqr %>% 
  ggplot(aes(Habitat_FOR,taxa_unique))+
  geom_boxplot()+
  theme_pubclean()
# unique taxa does not vary much between plots
  # HOMOGENOUS!
# F mean=3.97  SD=0.822  SE=0.145
# O mean=3.59  SD=0.979  SE=0.173
# R mean=3.57  SD=0.774  SE=0.141
woodlandqr %>% 
  select(Habitat_FOR,taxa_unique) %>% 
  filter(Habitat_FOR=='F') %>% 
  summarise(n=n(),
            mean=mean(taxa_unique),
            SD=sd(taxa_unique),
            SE=SD/sqrt(n))

## soil temp variance----
woodlandqr %>% 
  ggplot(aes(Habitat_FOR,coil_temp_C))+
  geom_boxplot()+
  theme_pubclean()
#soil temperature also homogenous
# F mean=9.61  SD=0.499  SE=0.088
# O mean=9.14  SD=0.624  SE=0.110
# R mean=9.19  SD=0.388  SE=0.071
woodlandqr %>% 
  select(Habitat_FOR,coil_temp_C) %>% 
  filter(Habitat_FOR=='R') %>% 
  summarise(n=n(),
            mean=mean(coil_temp_C),
            SD=sd(coil_temp_C),
            SE=SD/sqrt(n))

## tree qty variance----
woodlandqr %>% 
  ggplot(aes(Habitat_FOR,tree_qty))+
  geom_boxplot()+
  theme_pubclean()
# trees were more frequent and randomly dispersed in regenerating habitat
  # Open habitat has just a handful of small trees as expected, it is a fire break
  # Mature forest is old plantation with evenly spaces trees and little deviation vs Regen.
# F mean=6.19  SD=2.01  SE=0.355
# O mean=0.406  SD=0.875  SE=0.115
# R mean=20.4  SD=9.08  SE=1.66
woodlandqr %>% 
  select(Habitat_FOR,tree_qty) %>% 
  filter(Habitat_FOR=='R') %>% 
  summarise(n=n(),
            mean=mean(tree_qty),
            SD=sd(tree_qty),
            SE=SD/sqrt(n))

# so how does this affect vegatation?

woodlandqr %>% 
  ggplot(aes(tree_qty,Height_cm))+
  geom_point()+
  geom_smooth(method='lm')+
  theme_pubclean()










# OLD woodland quadrats----

# F = frequency, in now many quadrats it appeared
# C = cover, % cover, visual estimate 

# checking calcs against example data
## MEAN, SD & SE----
woodts %>% 
  select(Habitat_FOR,Hylocomium_splendens_C) %>% 
  filter(Habitat_FOR=='R') %>% 
  summarise(n=n(),
            mean=mean(Hylocomium_splendens_C),
            SD=sd(Hylocomium_splendens_C),
            SE=SD/sqrt(n))

# woodCMSE<-woodts %>% 
#   select(-group) %>% 
#   group_by(Habitat_FOR) %>% 
#   summarise(across(
#     .cols = is.numeric,
#     .fns = list(count=length, mean=mean, sd=sd, se=sd/sqrt(.)),
#     .names = "{col}_{fn}"))


# CORRECT 53.3, 34.4 & 6.3

woodts %>% 
  select(Habitat_FOR,Hylocomium_splendens_C) %>% 
  filter(Habitat_FOR=='R') %>% 
  summarise(mean=mean(Hylocomium_splendens_C))

# convert names to match pifall habitats----
woodts2<-woodts %>% 
  mutate('Habitat'=case_when(Habitat_FOR=='F'~'Mature Woodland (F)',
                             Habitat_FOR=='O'~'Open Habitat (O)',
                             Habitat_FOR=='R'~'Regenerating Woodland (R)'))

# ALL Summary SE----

woodCMSE<-woodts2 %>% 
  select(-group,-Habitat_FOR) %>% 
  group_by(Habitat) %>% 
  summarise_all(list(mean = ~mean(.), sd = ~sd(.), se = ~sd(.x)/sqrt(length(.x))))
# freq
woodmeansF<-woodCMSE %>% 
  select(1:8)

for ( col in 1:ncol(woodmeansF)){
  colnames(woodmeansF)[col] <-  sub("_F.*", "", colnames(woodmeansF)[col])
}

woodmeansF<-woodmeansF %>% 
  pivot_longer(names_to = 'taxa',
               values_to = 'mean freq',
               cols = -Habitat)
  
# cover
woodmeansC<-woodCMSE %>% 
  select(1,9:15)

for ( col in 1:ncol(woodmeansC)){
  colnames(woodmeansC)[col] <-  sub("_C.*", "", colnames(woodmeansC)[col])
}

woodmeansC<-woodmeansC %>% 
  pivot_longer(names_to = 'taxa',
               values_to = 'mean cover',
               cols = -Habitat)
# merge
woodmeanscf<-merge(woodmeansC,woodmeansF, 
                   by=c('Habitat','taxa'),
                   all=T)

# now do this for all data----
x<-woodts2 %>% 
  select(21,16,17,18,19,2:8) #CHECK----
for ( col in 1:ncol(x)){
  colnames(x)[col] <-  sub("_F.*", "", colnames(x)[col])
}
x<-x %>% 
  pivot_longer(names_to = 'taxa',
               values_to = 'frequency',
               cols = -c(1:5))  
#
y<-woodts2 %>% 
  select(21,16,17,18,19,9:15) #CHECK----
for ( col in 1:ncol(y)){
  colnames(y)[col] <-  sub("_C.*", "", colnames(y)[col])
}
y<-y %>% 
  pivot_longer(names_to = 'taxa',
               values_to = 'cover',
               cols = -c(1:5))  
# patterns between vegetation density, height, richness and tree density?
#merge
z<-cbind(x,y[,7])

woodts2 %>%
  ggplot(aes(Habitat,Height_cm,color=Habitat))+
  geom_boxplot()+
  geom_jitter(aes(Habitat,Height_cm),
              height = .05,width=.05,alpha=.5)+
  labs(title = 'Vegetation height across habitats')+
  theme_pubclean()
# vegetation is taller in O, comparable in F & R
# BUT OVERALL = v. little difference. 

woodts2 %>%
  ggplot(aes(Habitat,tree_qty,color=Habitat))+
  geom_boxplot()+
  geom_jitter(aes(Habitat,tree_qty),
              height = .05,width=.05,alpha=.5)+
  labs(title = 'Tree quantity across habitats')+
  theme_pubclean()
# more trees in R --> it is not intentionally planted F - old plantation

woodts2 %>%
  ggplot(aes(Habitat,taxa_unique,color=Habitat))+
  geom_boxplot()+
  geom_jitter(aes(Habitat,taxa_unique),
              height = .05,width=.05,alpha=.5)+
  labs(title = 'Unique taxa across habitats')+
  theme_pubclean()

# need to look at the SD and SE because there is signifcantly more variation in the mature woodland

# next analysis should be:
# linear relationship between taxa and tree density

x %>% 
  filter(frequency!=0) %>% 
  # #filter(tree_qty!=0) %>% 
  ggplot(aes(frequency,Height_cm,color=Habitat))+
  geom_point()+
  geom_smooth(method='lm',se=F)



