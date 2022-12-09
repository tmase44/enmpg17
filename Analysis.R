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

# PITFALL DATA----

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
grid.arrange(p1,p2)
p3

# boxplot over transects- trap variation
pitbox1<-pitfalllong %>% 
  group_by(Type,`Trap line`) %>% 
  summarise(n=sum(count)) %>% 
  filter(n<20) %>% # remove outliers - optional
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

# Mature woodland ie better for invertebrate biodiversity
  # total individuals and unique taxa are greater in Mature woodland
  # Open = more flies. There habitat is open, not ideal for predators (aranea)
    #or collembola which prefer soil, leaf litter and logs. 
  # Collembola are greater in regen where there is more leaf litter as the forest is mixed decidious and broadleaf.


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

w0<-woodlandqr %>% 
  filter(tree_qty>0) %>% 
  ggplot(aes(tree_qty,Height_cm))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(aes(label=..r.label..),color='blue',geom = 'label')+
  theme_pubclean()+
  labs(title='All habitats',x='Tree quantity',y='Vegetation height')
# there is a very week correlation between tree frequency and vegetation height
w1<-woodlandqr %>% 
  filter(tree_qty>0) %>% 
  filter(Habitat_FOR=='F') %>% 
  ggplot(aes(tree_qty,Height_cm))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(aes(label=..r.label..),color='blue',geom = 'label')+
  theme_pubclean()+
  labs(title='Mature Woodland',x='Tree quantity',y='Vegetation height')

w2<-woodlandqr %>% 
  filter(tree_qty>0) %>% 
  filter(Habitat_FOR=='O') %>% 
  ggplot(aes(tree_qty,Height_cm))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(aes(label=..r.label..),color='blue',geom = 'label')+
  theme_pubclean()+
  labs(title='Open Forest',x='Tree quantity',y='Vegetation height')

w3<-woodlandqr %>% 
  filter(tree_qty>0) %>% 
  filter(Habitat_FOR=='R') %>% 
  ggplot(aes(tree_qty,Height_cm))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(aes(label=..r.label..),color='blue',geom = 'label')+
  theme_pubclean()+
  labs(title='Regenerating Forest',x='Tree quantity',y='Vegetation height')

ggarrange(w0,w1,w2,w3)

woodlandqr %>% 
  filter(tree_qty>0) %>% 
  ggplot(aes(tree_qty,Height_cm,color=Habitat_FOR))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(show.legend = F)+
  theme_pubclean()+
  facet_wrap(~Habitat_FOR,
             scales = 'free_x')
# At the habitat level there is more relationship with height and trees
# Mature forest = negative correlation
# Open forest negative but few occurences
# Regen, surprisingly positive, tree qty = greater veg height

# differences: regen is generally shorter, trees are mixed 
# mature has very tall tress with little light penetration from the canopy

# species freq by habitat----

## pivot long
woodlandqrlong <- woodlandqr %>% 
  select(1:7,12) %>% 
  pivot_longer(!Habitat_FOR,
               names_to = 'taxa',
               values_to = 'freq') %>% 
  mutate(taxa=case_when(taxa=='Hylocomium_splendens_F'~'Hylocomium splendens',
                        taxa=='Calluna_vulgaris_F'~'Calluna vulgaris',
                        taxa=='Vaccinium_vitis_idaea_F'~'Vaccinium vitis idaea',
                        taxa=='Vaccinium_myrtilus_F'~'Vaccinium myrtilus',
                        taxa=='Sphagnum_F'~'Sphagnum',
                        taxa=='Molinia_F'~'Molinia',
                        taxa=='Polytrichum_F'~'Polytrichum'))

# most common species total in all habs
woodtaxasum <- woodlandqrlong %>% 
  group_by(taxa,Habitat_FOR) %>% 
  summarise(total=sum(freq)) %>% 
  arrange(desc(total))
print(woodtaxasum)
# 1 Hylocomium_splendens_F   1633
# 2 Calluna_vulgaris_F       1546
# 3 Vaccinium_vitis_idaea_F   734
# 4 Vaccinium_myrtilus_F      712
# 5 Sphagnum_F                162
# 6 Molinia_F                  88
# 7 Polytrichum_F              85

woodtaxasum %>% 
  ggplot(aes(Habitat_FOR,total,fill=Habitat_FOR))+
  geom_col()+
  facet_wrap(~taxa)+
  theme_pubclean()+
  labs(x='Habitat',y='Frequency')

woodtaxasum %>% 
  ggplot(aes(reorder(taxa,-total),total))+
  geom_col()+
  theme_pubclean()+
  labs(x='Habitat',y='Frequency')

woodlandqrlong %>% 
  filter(taxa!='Molinia'&taxa!='Polytrichum'&taxa!='Sphagnum') %>% 
  ggplot(aes(Habitat_FOR,freq,color=Habitat_FOR))+
  geom_boxplot()+
  facet_wrap(~taxa)+
  theme_pubclean()+
  labs(x='Habitat',y='Frequency')

