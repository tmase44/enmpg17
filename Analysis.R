# Packages----
library(pacman)
p_load(tidyverse,readxl,ggpubr,gridExtra,reshape2,kableExtra)
windowsFonts(Times=windowsFont("TT Times New Roman"))

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
  pivot_longer(names_to = 'Taxa',
               values_to = 'count',
               cols = c(-Type,-`Trap line`))
pitfalllong$Taxa<-as.factor(pitfalllong$Taxa)
pitfalllong$`Trap line`<-as.factor(pitfalllong$`Trap line`)

pitfalllong<-pitfalllong %>% 
  mutate(habitat_new=case_when(Type=='Mature Woodland (F)'~'F',
                               Type=='Open Habitat (O)'~'O',
                               Type=='Regenerating Woodland (R)'~'R'))


#total species occurence
pitfalltotal<-pitfalllong %>% 
  group_by(Taxa) %>% 
  summarise(n=sum(count)) %>% 
  arrange(desc(n))

# species occurrence by habitat
pitfallhabs<-pitfalllong %>% 
  group_by(Type,Taxa) %>% 
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
  summarise('unique_n'=n_distinct(Taxa)) %>% 
  arrange(desc(unique_n)) %>% 
  ggplot(aes(Type,unique_n))+
  geom_col()+
  labs(y='n unique taxa',
       x='Habitat',
       title = 'Unique taxa per habitat')+
  theme_pubclean()


# bar species stacked
p3<-pitfallhabs %>% 
  ggplot(aes(reorder(Taxa,-n),n,fill=Type))+
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

pitfallhabs %>% 
  group_by(Type) %>% 
  summarise(total=sum(n))

pitfallhabs %>% 
  group_by(Type) %>% 
  summarise(total=sum(n))


82+62+60
# !!! kable table----
## dont need this in the report.
pitfallhabs %>% 
  pivot_wider(names_from = Type,
              values_from = n) %>% 
  kable(align = 'lccc') %>% 
  add_header_above(header = c(
                              "Table 1: Total number of individuals per habitat"=4),
                   align = 'l') %>% 
  kable_styling(html_font = 'Times', full_width = F)
  
# bigger legend
myTheme <- theme(legend.text = element_text(size = 14), 
                 legend.title = element_text(size = 16), 
                 legend.key.size = unit(1, 'cm'),
                 axis.title.y = element_text(size=14),
                 axis.title.x = element_text(size=14),
                 axis.text.y = element_text(size=16),
                 axis.text.x = element_text(size=16))

# boxplot over transects- trap variation
pitbox1<-pitfalllong %>% 
  group_by(Type,habitat_new,`Trap line`) %>% 
  summarise(n=sum(count)) %>% 
  #filter(n<20) %>% # remove outliers - optional
  ggplot(aes(habitat_new,n,color=Type))+
  geom_boxplot(size=1,outlier.shape=17,outlier.size = 2)+
  stat_summary(fun = 'mean')+
  theme_pubclean()+
  theme(text = element_text(family='Times'))+  
    labs(y='n individuals per trap-line',
       x='Habitat',
       title='4.1: Pitfall samples across habitats')+
  scale_color_manual(name='Habitat',values=c('Mature Woodland (F)'='#009988',
                              'Open Habitat (O)'='#CC3311',
                              'Regenerating Woodland (R)'='#33BBEE'))+
  myTheme+
  coord_cartesian(ylim=c(0,25))

# 3 main taxa: 
# boxplot for each species, looking at their mean distribution by habitat

# Araneae | spiders
pitbox2<-pitfalllong %>% 
  filter(Taxa=='Araneae') %>% 
  ggplot(aes(habitat_new,count,color=Type))+
  geom_boxplot(size=1,outlier.shape=17,outlier.size = 2)+
  stat_summary(fun = 'mean')+
  theme_pubclean()+
  theme(text = element_text(family='Times'))+  
    labs(y='n individuals per trap-line',
       x='Habitat',
       title='4.2: Araneae distribution across habitats')+
  scale_color_manual(name='Habitat',values=c('Mature Woodland (F)'='#009988',
                              'Open Habitat (O)'='#CC3311',
                              'Regenerating Woodland (R)'='#33BBEE'))+
  myTheme+
  scale_y_continuous(limits=c(0,15), breaks = c(0,5,10,15))


# Diptera | flies, midges
pitbox3<-pitfalllong %>% 
  filter(Taxa=='Diptera') %>% 
  ggplot(aes(habitat_new,count,color=Type))+
  geom_boxplot(size=1,outlier.shape=17,outlier.size = 2)+
  stat_summary(fun = 'mean')+
  theme_pubclean()+
  theme(text = element_text(family='Times'))+  
    labs(y='n individuals per trap-line',
       x='Habitat',
       title='4.3: Diptera distribution across habitats')+
  scale_color_manual(name='Habitat',values=c('Mature Woodland (F)'='#009988',
                              'Open Habitat (O)'='#CC3311',
                              'Regenerating Woodland (R)'='#33BBEE'))+
  myTheme+
  scale_y_continuous(limits=c(0,15), breaks = c(0,5,10,15))


#Collembola | springtails
pitbox4<-pitfalllong %>% 
  filter(Taxa=='Collembola') %>% 
  ggplot(aes(habitat_new,count,color=Type))+
  geom_boxplot(size=1,outlier.shape=17,outlier.size = 2)+
  stat_summary(fun = 'mean')+
  theme_pubclean()+
  theme(text = element_text(family='Times'))+  
  labs(y='n individuals per trap-line',
       x='Habitat',
       title='4.4: Collembola distribution across habitats')+
  scale_color_manual(name='Habitat',values=c('Mature Woodland (F)'='#009988',
                              'Open Habitat (O)'='#CC3311',
                              'Regenerating Woodland (R)'='#33BBEE'))+
  myTheme+
  scale_y_continuous(limits=c(0,15), breaks = c(0,5,10,15))



#### combobox----
# SHOW THIS ONE----
ggarrange(pitbox1,pitbox2,pitbox3,pitbox4,
          common.legend = TRUE,
          legend = 'top')



# Mature woodland ie better for invertebrate biodiversity
  # total individuals and unique taxa are greater in Mature woodland
  # Open = more flies. There habitat is open, not ideal for predators (aranea)
    #or collembola which prefer soil, leaf litter and logs. 
  # Collembola are greater in regen where there is more leaf litter as the forest is mixed decidious and broadleaf.

## ALL N = 4 BECAUSE THERE ARE 4 LINES FOR EACH, ALL TRAPS ARE POOLED TOGETHER
options(digits = 5)
# MEANS ALL PITFALL TRAPS----
pitfalllong %>% 
  group_by(habitat_new,`Trap line`) %>% 
  summarise(x=sum(count)) %>% 
  select(habitat_new,x) %>% 
  filter(habitat_new=='F') %>% 
  summarise(n=n(),
            mean=mean(x),
            SD=sd(x),
            SE=SD/sqrt(n))
# F mean=20.5  SD=8.39  SE=4.19
# O mean=15.5  SD=3.32  SE=1.66
# R mean=15.0  SD=4.83  SE=2.42

# MEANS ARANEAE----
pitfalllong %>% 
  select(habitat_new,Taxa,count) %>% 
  filter(Taxa=='Araneae') %>% 
  filter(habitat_new=='O') %>% 
  summarise(n=n(),
            mean=mean(count),
            SD=sd(count),
            SE=SD/sqrt(n))
# F mean=8.5  SD=4.12  SE=2.06
# O mean=2.5  SD=2.38  SE=1.19
# R mean=5.5  SD=2.38  SE=1.19

# MEANS DIPTERA----
pitfalllong %>% 
  select(habitat_new,Taxa,count) %>% 
  filter(Taxa=='Diptera') %>% 
  filter(habitat_new=='R') %>% 
  summarise(n=n(),
            mean=mean(count),
            SD=sd(count),
            SE=SD/sqrt(n))
# F mean=3     SD=5.35  SE=2.68
# O mean=7.25  SD=2.63  SE=1.31
# R mean=1.5   SD=1.29  SE=0.645

# MEANS COLLEMBOLA----
pitfalllong %>% 
  select(habitat_new,Taxa,count) %>% 
  filter(Taxa=='Collembola') %>% 
  filter(habitat_new=='R') %>% 
  summarise(n=n(),
            mean=mean(count),
            SD=sd(count),
            SE=SD/sqrt(n))
# F mean=2.75  SD=3.10  SE=1.55
# O mean=2.75  SD=1.26  SE=0.629
# R mean=4.0   SD=4.90  SE=2.45

#=========================================
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
  geom_smooth(method='lm',color='#0077BB')+
  stat_cor(aes(label=..r.label..),color='#0077BB',geom = 'label')+
  theme_pubclean()+
  theme(text = element_text(family='Times'))+  
    labs(title='3.1: All habitats',x='Tree quantity',y='Vegetation height')+
  myTheme
# there is a very week correlation between tree frequency and vegetation height
w1<-woodlandqr %>% 
  filter(tree_qty>0) %>% 
  filter(Habitat_FOR=='F') %>% 
  ggplot(aes(tree_qty,Height_cm))+
  geom_point()+
  geom_smooth(method='lm',color='#0077BB')+
  stat_cor(aes(label=..r.label..),color='#0077BB',geom = 'label')+
  theme_pubclean()+
  theme(text = element_text(family='Times'))+  
  labs(title='3.2: Mature Woodland (F)',x='Tree quantity',y='Vegetation height')+
  myTheme

w2<-woodlandqr %>% 
  filter(tree_qty>0) %>% 
  filter(Habitat_FOR=='O') %>% 
  ggplot(aes(tree_qty,Height_cm))+
  geom_point()+
  geom_smooth(method='lm',color='#0077BB')+
  stat_cor(aes(label=..r.label..),color='#0077BB',geom = 'label')+
  theme_pubclean()+
  theme(text = element_text(family='Times'))+  
    labs(title='3.3: Open Habitat (O)',x='Tree quantity',y='Vegetation height')+
  myTheme

w3<-woodlandqr %>% 
  filter(tree_qty>0) %>% 
  filter(Habitat_FOR=='R') %>% 
  ggplot(aes(tree_qty,Height_cm))+
  geom_point()+
  geom_smooth(method='lm',color='#0077BB')+
  stat_cor(aes(label=..r.label..),color='#0077BB',geom = 'label')+
  theme_pubclean()+
  theme(text = element_text(family='Times'))+  
    labs(title='3.4: Regenerating Forest (R)',x='Tree quantity',y='Vegetation height')+
  myTheme

# SHOW THIS ONE----
ggarrange(w0,w1,w2,w3)

# other scatterplots----
woodlandqr %>% 
  filter(tree_qty>0) %>% 
  filter(Habitat_FOR=='O') %>% 
    ggplot(aes(tree_qty,Calluna_vulgaris_F))+
  geom_point()+
  geom_smooth(method='lm')+
  stat_cor(aes(label=..r.label..),color='#0077BB',geom = 'label')+
  theme_pubclean()+
  theme(text = element_text(family='Times'))

# pivotlong2 for tree height
woodlandqrlong2 <- woodlandqr %>% 
  select(1:7,10) %>% 
  pivot_longer(!tree_qty,
               names_to = 'taxa',
               values_to = 'freq') %>% 
  mutate(taxa=case_when(taxa=='Hylocomium_splendens_F'~'Hylocomium splendens',
                        taxa=='Calluna_vulgaris_F'~'Calluna vulgaris',
                        taxa=='Vaccinium_vitis_idaea_F'~'Vaccinium vitis idaea',
                        taxa=='Vaccinium_myrtilus_F'~'Vaccinium myrtilus',
                        taxa=='Sphagnum_F'~'Sphagnum',
                        taxa=='Molinia_F'~'Molinia',
                        taxa=='Polytrichum_F'~'Polytrichum'))

woodlandqrlong2 %>% 
  filter(tree_qty>0) %>% 
  filter(taxa=='Vaccinium vitis idaea'|
           taxa=='Vaccinium myrtilus'|
           taxa=='Molinia'|
           taxa=='Hylocomium splendens') %>% 
  ggplot(aes(tree_qty,freq,color=taxa))+
  geom_point()+
  geom_smooth(method='lm', se = FALSE)+
  stat_cor(aes(label=..r.label..),geom = 'label',
           show.legend = F)+
  theme_pubclean()+
  theme(text = element_text(family='Times'))

# tree density and unique species has a weak negative correlation, R=-0.11

?geom_smooth

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
                        taxa=='Polytrichum_F'~'Polytrichum')) %>% 
  mutate(habitat_new=case_when(Habitat_FOR=='F'~'Mature Woodland (F)',
                               Habitat_FOR=='O'~'Open Habitat (O)',
                               Habitat_FOR=='R'~'Regenerating Woodland (R)'))


# most common species total in all habs
woodtaxasum <- woodlandqrlong %>% 
  group_by(taxa,Habitat_FOR) %>% 
  summarise(total=sum(freq)) %>% 
  arrange(Habitat_FOR,desc(total))
View(woodtaxasum)
# 1 Hylocomium_splendens_F   1633
# 2 Calluna_vulgaris_F       1546
# 3 Vaccinium_vitis_idaea_F   734
# 4 Vaccinium_myrtilus_F      712
# 5 Sphagnum_F                162
# 6 Molinia_F                  88
# 7 Polytrichum_F              85
woodtaxasum %>% 
  ggplot(aes(reorder(taxa,-total),total))+
  geom_col()+
  theme_pubclean()+
  labs(x='Habitat',y='Frequency')
# 2 species are dominant: H.splendens and C.vulgaris
# sphagnum, molinia and polytrichium are less common

woodtaxasum %>% 
  ggplot(aes(Habitat_FOR,total,fill=Habitat_FOR))+
  geom_col()+
  facet_wrap(~taxa)+
  theme_pubclean()+
  labs(x='Habitat',y='Frequency')
# sphagnum was found more often in open habitat and hardly at all in mature forest
# V.myrtilus and V.vitis idea were signifcantly more adundant in mature forest

## SHOW THIS ONE----
woodlandqrlong %>% 
  filter(taxa!='Molinia'&taxa!='Polytrichum'&taxa!='Sphagnum') %>% 
  ggplot(aes(Habitat_FOR,freq,color=Habitat_FOR))+
  geom_boxplot(size=1,outlier.shape=17,outlier.size = 2)+
  facet_wrap(~taxa)+
  theme_pubclean()+
  labs(x='Habitat',y='Frequency')
### !! split this up for separate titles
wb2<-woodlandqrlong %>% 
  filter(taxa=='Calluna vulgaris') %>% 
  ggplot(aes(Habitat_FOR,freq,color=habitat_new))+
  geom_boxplot(size=1,outlier.shape=17,outlier.size = 2)+
  stat_summary(fun = 'mean')+
    theme_pubclean()+
  theme(text = element_text(family='Times'))+  
  labs(title = '2.2: Calluna vulgaris',x='Habitat',y='Frequency')+
  scale_color_manual(name='Habitat',values=c('Mature Woodland (F)'='#009988',
                                             'Open Habitat (O)'='#CC3311',
                                             'Regenerating Woodland (R)'='#33BBEE'))+
  myTheme

wb1<-woodlandqrlong %>% 
  filter(taxa=='Hylocomium splendens') %>% 
  ggplot(aes(Habitat_FOR,freq,color=habitat_new))+
  geom_boxplot(size=1,outlier.shape=17,outlier.size = 2)+
  stat_summary(fun = 'mean')+
    theme_pubclean()+
  theme(text = element_text(family='Times'))+  
    labs(title = '2.1: Hylocomium splendens',x='Habitat',y='Frequency')+
  scale_color_manual(name='Habitat',values=c('Mature Woodland (F)'='#009988',
                              'Open Habitat (O)'='#CC3311',
                              'Regenerating Woodland (R)'='#33BBEE'))+
  myTheme

wb3<-woodlandqrlong %>% 
  filter(taxa=='Vaccinium myrtilus') %>% 
  ggplot(aes(Habitat_FOR,freq,color=habitat_new))+
  geom_boxplot(size=1,outlier.shape=17,outlier.size = 2)+
  stat_summary(fun = 'mean')+
  theme_pubclean()+
  theme(text = element_text(family='Times'))+  
  labs(title = '2.3: Vaccinium myrtilus',x='Habitat',y='Frequency')+
  scale_color_manual(name='Habitat',values=c('Mature Woodland (F)'='#009988',
                                             'Open Habitat (O)'='#CC3311',
                                             'Regenerating Woodland (R)'='#33BBEE'))+
  myTheme

wb4<-woodlandqrlong %>% 
  filter(taxa=='Vaccinium vitis idaea') %>% 
  ggplot(aes(Habitat_FOR,freq,color=habitat_new))+
  geom_boxplot(size=1,outlier.shape=17,outlier.size = 2)+
  stat_summary(fun = 'mean')+
  theme_pubclean()+
  theme(text = element_text(family='Times'))+  
  labs(title = '2.4: Vaccinium vitis idaea',x='Habitat',y='Frequency')+
  scale_color_manual(name='Habitat',values=c('Mature Woodland (F)'='#009988',
                                             'Open Habitat (O)'='#CC3311',
                                             'Regenerating Woodland (R)'='#33BBEE'))+
  myTheme

ggarrange(wb1,wb2,wb3,wb4,
          common.legend = TRUE,
          legend = 'top')

# mean,sd,se
woodlandqr %>% 
  select(Habitat_FOR,coil_temp_C) %>% 
  filter(Habitat_FOR=='R') %>% 
  summarise(n=n(),
            mean=mean(coil_temp_C),
            SD=sd(coil_temp_C),
            SE=SD/sqrt(n))

x<-woodlandqr %>% 
  select(1:7,12) %>% 
  group_by(Habitat_FOR) %>% 
  summarise_all(list(mean = ~mean(.), sd = ~sd(.), se = ~sd(.x)/sqrt(length(.x))))

write_excel_csv(x,'qr_summary_se.csv')

woodlandtable<-read_excel('qr_summary_se.xlsx',sheet='Sheet2')
view(woodlandtable)

woodlandtable<-woodlandtable %>% 
  mutate(across(where(is.numeric), round, 3))

colnames(woodlandtable)<-gsub("_m","",colnames(woodlandtable))
colnames(woodlandtable)<-gsub("_o","",colnames(woodlandtable))
colnames(woodlandtable)<-gsub("_r","",colnames(woodlandtable))

# !!! kable table ----
woodlandtable %>%
  kable(align = 'lcccccccccccc') %>% 
  add_header_above(header = c(" "=1,
                              "Mature Forest"=4,
                              "Open Forest"=4,
                              "Regenerating Forest"=4)) %>% 
  kable_styling(html_font = 'Times') %>% 
  column_spec(column = c(2,3,4,5,10,11,12,13), background = '#F2F2F2') %>% 
  row_spec(0, italic = T)
