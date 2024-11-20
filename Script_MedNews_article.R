####################################################################################################################
# PACKAGES
####################################################################################################################
require(stringr)
require(reshape2)
require(ggplot2)
require(readr)
library(readr)
require(dplyr)
require(tidyr)



setwd("C:/Users/aline/OneDrive/Documentos/R/MedNews")

####################################################################################################################
#####DATASET ORGANIZATION

#####FIRST FORMS
mednewsQ1ori<- read_delim("Q1.data.csv", delim = ';', show_col_types = FALSE)#opening the first forms

# The variables in the first forms [mednewsQ1ori[,c(16:20)]] ranged from 1 to 10, changing from 1 to 1. 
# The variables in the second forms [mednewsQ2ori[,c(11:15)]] also ranged from 1 to 10, but changed every 2. 
# Therefore the variables in the first forms will be changed to vary from 2 to 2.

####################################################################################################################
# Loop to change the variables in the first form to vary from 2 to 2.
####################################################################################################################
mednewsQ1Trans<- data.frame(mednewsQ1ori) # saving a dataset

for (i in 1:nrow(mednewsQ1Trans)){
  for (j in 16: ncol(mednewsQ1Trans)){
    if (mednewsQ1Trans[i,j] == 1 | mednewsQ1Trans[i,j] == 2){
      mednewsQ1Trans[i,j]<- 2
    } else if (mednewsQ1Trans[i,j] == 3 | mednewsQ1Trans[i,j] == 4){
      mednewsQ1Trans[i,j]<- 4
    } else if (mednewsQ1Trans[i,j] == 5 | mednewsQ1Trans[i,j] == 6){
      mednewsQ1Trans[i,j]<- 6
    } else if(mednewsQ1Trans[i,j] == 7 | mednewsQ1Trans[i,j] == 8){
      mednewsQ1Trans[i,j]<- 8
    } else if(mednewsQ1Trans[i,j] == 9 | mednewsQ1Trans[i,j] == 10){
      mednewsQ1Trans[i,j]<- 10
    }
  }
}

dim(mednewsQ1Trans)
colnames(mednewsQ1Trans[,16:20])

####################################################################################################################
# Opening and saving the forms that will be used in the analysis
####################################################################################################################
mednewsQ1<- tibble::as_tibble(mednewsQ1Trans) #save the dataset as a tibble
mednewsQ2<- read_delim("Q2.data.csv", show_col_types = FALSE)
mednewsQ3<- read_delim("Q3.data.csv", delim = ';', show_col_types = FALSE)

####################################################################################################################
# Joining the datasets
####################################################################################################################
Q1<- mednewsQ1[,-c(1)] # remove the first column
Q2<- mednewsQ2[,-c(1)] # remove the first column
Q3<- mednewsQ3[,-c(1)] # remove the first column

Q1<- Q1%>%rename_with(~paste0("Q1_", .x), .cols=-1) # add a Q1 before each variable 
Q2<- Q2%>%rename_with(~paste0("Q2_", .x), .cols=-1) # add a Q1 before each variable
Q3<- Q3%>%rename_with(~paste0("Q3_", .x), .cols=-1) # add a Q1 before each variable

joinTables2<-right_join(Q1, Q2, by = "CodeName") #join the Q1 and Q2
joinTables3<-right_join(joinTables2, Q3, by = "CodeName") # join the joinTables2 to Q3

####################################################################################################################
# Data wrangling
####################################################################################################################
joinTables3<- joinTables3[-c(34:37),] # eliminate the patients participated in only one step of the research
sum(is.na(joinTables3)) # Are there NAs in the dataset?
dim(joinTables3)

# Define the categorical variables
colnames(joinTables3)
joinTables3 <- joinTables3 %>% mutate(across(Q1_Trainings:Q3_Training_changed_atittude, factor)) 
joinTables3<- tibble::as_tibble(joinTables3)
str(joinTables3)

####################################################################################################################
### Age evaluation - Figure 2
####################################################################################################################
range(joinTables3$Q1_Age)
sd(joinTables3$Q1_Age)
mean(joinTables3$Q1_Age)

# Barplot of age distribution
ggplot(joinTables3, aes(x=Q1_Age))+
  geom_bar(color = 'blue', fill = 'blue')+
  xlab('Age')+
  ylab('Number of individuals')+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15))

####################################################################################################################
### Graduation time - Figure 3
####################################################################################################################
# Barplot of graduation time
ggplot(joinTables3, aes(x=Q1_Graduation))+
  geom_bar(color = 'blue', fill = 'blue')+
  labs(' ', fill = '', title = 'Time since graduation')+
  xlab('Time since graduation')+
  ylab('Number of individuals')+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15))

####################################################################################################################
# How often do you give bad news? - FIGURE 4
####################################################################################################################
tab_ord_freq<- table(joinTables3$Q1_Frequency)
tab_ord_freq<- prop.table(tab_ord_freq)
tab_ord_freq<- data.frame(tab_ord_freq)

ggplot(tab_ord_freq, aes(x= as.factor(Var1), y=Freq))+
    geom_bar(stat="identity", position = 'dodge', colour = 'blue', fill = 'blue')+
    ylab('Proportion of individuals (%)')+ xlab('')+
    scale_x_discrete(labels = c('Rarely', ' Eventually', 'Frequently'))+
    labs(' ', fill = '', title = ' ')+
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 15),
          plot.margin = margin(t = 10, r= 10, b = 1, l = 30))

####################################################################################################################
# Have you previously received training on how to deliver bad news? - FIGURE 5
####################################################################################################################
tab_ord_tr<- joinTables3 %>% pull(Q1_Trainings) %>% table()
tab_ord_trProp<- prop.table(tab_ord_tr)

ggplot(joinTables3)+
  geom_bar(mapping = aes(x= Q1_Trainings, y = after_stat(prop)*100, group = 1), colour= 'blue', fill = 'blue')+
  xlab(' ')+
  ylab('Proportion of individuals (%)')+
  scale_x_discrete(labels = c('I never participated in training',
                              'I never participated in training. \n I learned from my own professional practice',
                              'I learned by observing my teacher',
                              'I participated in training in graduation',  
                              'I participated in training during residency',
                              'I took courses previously'))+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 10))

####################################################################################################################
# How prepared do you consider yourself to deliver bad news? - Figure 7
####################################################################################################################
prep_tab<- table(joinTables3$Q1_Prepared)
prep_tab<- prop.table(prep_tab)
prep_tab<- data.frame(prep_tab)
prep_tab[,1]<- c('Little prepared', 'Moderately prepared', 'Very prepared')

ggplot(prep_tab)+
  geom_bar(aes(x= factor(prep_tab[,1], level = c('Little prepared', 'Moderately prepared', 'Very prepared')),
               y=Freq), colour= 'blue', fill = 'blue', stat="identity", position = 'dodge')+
  labs(x = '', y = 'Proportion of individuals (%)', fill = ' ')+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 15))

####################################################################################################################
# How satisfied are you with your performance in delivering bad news? - Figure 8
####################################################################################################################
prep_tabPer<- table(joinTables3$Q1_Performance)
prep_tabPer<- prop.table(prep_tabPer)
prep_tabPer<- data.frame(prep_tabPer)
prep_tabPer[,1]<- c('Totally satisfied', 'Little satisfied', 'Moderately satisfied', 'Very satisfied')

ggplot(prep_tabPer)+
  geom_bar(aes(x= factor(prep_tabPer[,1], level = c('Totally satisfied', 'Little satisfied', 'Moderately satisfied', 'Very satisfied')),
               y=Freq), colour= 'blue', fill = 'blue', stat="identity", position = 'dodge')+
  labs(x = '', y = 'Proportion of individuals (%)', fill = ' ')+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 15))

####################################################################################################################
# How uncomfortable are you with giving bad news? - Figure 9
####################################################################################################################
prep_tabDis<- table(joinTables3$Q1_Discomfort)
prep_tabDis<- prop.table(prep_tabDis)
prep_tabDis<- data.frame(prep_tabDis)
prep_tabDis[,1]<- c('Totally uncomfortable', 'Little comfortable', 'Moderately confortable', 'Very confortable', 'Totally confortable')

ggplot(prep_tabDis)+
  geom_bar(aes(x= factor(prep_tabDis[,1], level = c('Totally uncomfortable', 'Little comfortable', 'Moderately confortable', 'Very confortable', 'Totally confortable')),
               y=Freq), colour= 'blue', fill = 'blue', stat="identity", position = 'dodge')+
  labs(x = '', y = 'Proportion of individuals (%)', fill = ' ')+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 15))

####################################################################################################################
# What was the proportion of answers considered as difficult, that is, with a value of 10. - FIGURE 6
####################################################################################################################
# Loop to get the proportion of answer showed the value of 10, the hardest
extratHigVal<- matrix(0, nrow = 7, ncol=1, dimnames = list(colnames(joinTables3[,c(8:14)])))
for (i in 8: ncol(joinTables3)-38){
  hv<- sum(joinTables3[,i] == 10)
  pvp <- (hv*100)/33
  extratHigVal[i-7,]<- pvp 
}

colnames(extratHigVal)<- c('freq')
lab_hist<- c('Inform diagnosis',
             'Discuss treatment strategies',
             'Communicate to the patient \n about onset or recurrence of disease',
             'Talk about ending active treatment \n and starting palliative care',
             'Involve family and friends in the discussion',
             'Dealing with the patient´s reaction',
             'Report death to family')

extratHigVal<- data.frame(cbind(lab_hist,extratHigVal))
extratHigVal$freq<- as.numeric(extratHigVal$freq)

ggplot(extratHigVal)+
  geom_bar(aes(x= reorder(lab_hist, -freq), y = freq), colour= 'blue', fill = 'blue', stat = 'identity')+
  xlab(' ')+
  ylab('Proportion of individuals (%)')+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 12),
        plot.margin = margin(t = 10, r= 10, b = 10, l = 30))

####################################################################################################################
# Generating barplots separately for each variable
####################################################################################################################
myPalette4<- brewer.pal(10, 'Spectral') 

colnames(joinTables3)[8:14] # To generate each separate graph you have to change the variable name in x = as.factor(....)

ggplot(joinTables3)+
  geom_bar(aes(x= as.factor(Q1_Report_diagnosis), y = (..prop..)*100, group = 1), colour = myPalette4, fill = myPalette4)+
  xlab(' ')+
  ylab('Proportion of individuals (%)')+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 15))

####################################################################################################################
# Are there association between age and the scale to deliver bad news?
# The variable course_training were removed because all participants answered 10. Table 1
require(PMCMRplus)
####################################################################################################################
shapiro.test(joinTables3$Q1_Age)# p<0.05 indicate the data are not normally distributed.

## Kruskall Wallis test
mat<- matrix(0, nrow=13, ncol = 1,dimnames = list(colnames(joinTables3[,c(6:18)])))
colnames(mat)<- c('KSpvalue')

mednewsComp<- joinTables3[,c(2, 6:18)]
mednewsComp<- data.frame(mednewsComp)

for (i in 2:ncol(mednewsComp)){
    ks<- kruskal.test(mednewsComp$Q1_Age ~ as.factor(mednewsComp[,i]))
  mat[i-1,1]<- round(ks$p.value,3)
}
ifelse(mat <= 0.05, mat, 'NS')

KS<- kruskal.test(mednewsComp$Q1_Age ~ as.factor(mednewsComp$Q1_Dealing_reaction_patient), data = mednewsComp)
KSNem<- PMCMRplus::kwAllPairsNemenyiTest(mednewsComp$Q1_Age ~ as.factor(mednewsComp$Q1_Dealing_reaction_patient), data = mednewsComp)

####################################################################################################################
## Kendall tau-b correlation between age and how the professional deal with the patient reaction - Figure 10
require(DescTools)
####################################################################################################################
cor.test(mednewsComp$Q1_Age, as.numeric(mednewsComp$Q1_Dealing_reaction_patient), data = mednewsComp, method = 'kendall')
KendallTauB(mednewsQ1$Age, as.numeric(mednewsQ1$Dealing_reaction_patient))

ggplot(mednewsComp, aes(x=as.factor(Q1_Dealing_reaction_patient), y=Q1_Age)) + 
  geom_point(position = 'jitter', colour = 'blue', size = 1)+
  geom_boxplot(width=  0.5, alpha = 0.6, colour = "black", fill = "#4271AE", notch = FALSE, outlier.shape = NA)+
  labs(x = 'Dealing with the patient´s reaction', y = 'Age') +
  annotate('text', x= 0.8, y= 35.5, label = 'Kendall tau-b = 0.40')+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour="black", size = 20),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        axis.text = element_text(size = 15),
        plot.margin = margin(t = 10, r= 10, b = 10, l = 30))

####################################################################################################################
# Are there some correlation between all variables? - Table 2
####################################################################################################################
mednewsComp<- mapply(mednewsComp, FUN = as.numeric)
mednewsComp<- as.matrix(mednewsComp)
medNewsCor<- cor(mednewsComp, mednewsComp, method = 'kendall')
medNewsCor[lower.tri(medNewsCor, diag = TRUE)]<- NA
ResMod <- subset(melt(medNewsCor, na.rm = TRUE), value > 0.5)








############################################################################################
# Comparing the ordinal variables in the first and second forms -  Wilcoxon test
############################################################################################
colnames(joinTables3)
tabWilcox<- matrix(0, nrow=7, ncol=1, dimnames = list(colnames(joinTables3[,c(8:14)])))

for (i in 1: 52){
  tabA<- cbind(joinTables3[,i+7], joinTables3[,i+19])
  colnames(tabA)<- c('antes', 'depois')
  mel<- reshape2::melt(tabA, id.vars = NULL)
  wilc <- wilcox.test(as.numeric(mel$value) ~ mel$variable, paired = TRUE, exact = FALSE)
  tabWilcox[i,]<- round(wilc$p.value,3)
}

############################################################################################
# Comparing the ordinal variables in the first and third forms -  Wilcoxon test - Table 1
############################################################################################
### Wilcoxon entre Q1 e Q3
tabWilcoxQ1Q3<- matrix(0, nrow=4, ncol=1, dimnames = list(colnames(joinTables3[,c(15:18)])))
for (i in 1: 52){
  tabA<- cbind(joinTables3[,i+14], joinTables3[,i+45])
  colnames(tabA)<- c('antes', 'depois')
  mel<- reshape2::melt(tabA, id.vars = NULL)
  wilc <- wilcox.test(as.numeric(mel$value) ~ mel$variable, paired = TRUE, exact = FALSE)
  tabWilcoxQ1Q3[i,]<- round(wilc$p.value,3)
}

tabWilTwoQ1Q3<- matrix(0, nrow=7, ncol=1, dimnames = list(colnames(joinTables3[,c(8:14)])))
for (i in 1: 52){
  tabA<- cbind(joinTables3[,i+7], joinTables3[,i+37])
  colnames(tabA)<- c('antes', 'depois')
  mel<- reshape2::melt(tabA, id.vars = NULL)
  wilc <- wilcox.test(as.numeric(mel$value) ~ mel$variable, paired = TRUE, exact = FALSE)
  tabWilTwoQ1Q3[i,]<- round(wilc$p.value,3)
}

############################################################################################
# Comparing the answers before and after each question: Report_diagnosis, Treatment_strategies, 
# Report_recurrence, Palliative_treatment, Family_involviment, Patient_reaction, Communicate_death 
############################################################################################
##### BEFORE
tabSelOne<-table(joinTables3[,8])
tabSelPropOne<- prop.table(tabSelOne)
tabSelPropMeltOne<- melt(tabSelPropOne)
colnames(tabSelPropMeltOne)<- c('num', colnames(joinTables3[,8]))

colnames(tabSelPropMeltOne)
for (i in 9:14){
  tabSelTwo<-table(joinTables3[,i])
  tabSelPropTwo<- prop.table(tabSelTwo)
  tabSelPropMeltTwo<- melt(tabSelPropTwo)
  colnames(tabSelPropMeltTwo)<- c('num', colnames(joinTables3[,i]))
  tabSelPropMeltOne<- left_join(tabSelPropMeltOne, tabSelPropMeltTwo, by='num')
}

tabSelPropMeltOne<- data.frame(tabSelPropMeltOne)
tabSelPropMeltOne[is.na(tabSelPropMeltOne)] <- 0

##### AFTER
tabSelOneAfter<-table(joinTables3[,38])
tabSelPropOneAfter<- prop.table(tabSelOneAfter)
tabSelPropMeltOneAfter<- melt(tabSelPropOneAfter)
colnames(tabSelPropMeltOneAfter)<- c('num', colnames(joinTables3[,38]))

tabSelPropMeltOneAfter<- rbind(tabSelPropMeltOneAfter[c(1:2),], '0', tabSelPropMeltOneAfter[c(3:9),])
tabSelPropMeltOneAfter[,1]<- c(1:10)

for (i in 39:44){
  tabSelTwoAfter<-table(joinTables3[,i])
  tabSelPropTwoAfter<- prop.table(tabSelTwoAfter)
  tabSelPropMeltTwoAfter<- melt(tabSelPropTwoAfter)
  colnames(tabSelPropMeltTwoAfter)<- c('num', colnames(joinTables3[,i]))
  tabSelPropMeltOneAfter<- left_join(tabSelPropMeltOneAfter, tabSelPropMeltTwoAfter, by='num')
}

tabSelPropMeltOneAfter<- data.frame(tabSelPropMeltOneAfter)
tabSelPropMeltOneAfter[is.na(tabSelPropMeltOneAfter)] <- 0

####Organizing the two datasets of before and after
tabSelPropMeltOne
tabSelPropMeltOneAfter

########
melBefore<- melt(tabSelPropMeltOne, id.vars = 'num')
melBefore<- data.frame(melBefore)
melBefore<- melBefore %>% mutate (variable = str_replace(variable, 'Q1_', ' ')) %>% mutate(Time = 'Before')

melAfter<- melt(tabSelPropMeltOneAfter, id.vars = 'num')
melAfter<- data.frame(melAfter)
melAfter<- melAfter %>% mutate (variable = str_replace(variable, 'Q3_', ' ')) %>% mutate(Time = 'After')

mel<- rbind(melBefore, melAfter)
mel$org<- c(rep('a', 10),rep('b',10),rep('c',10),rep('d',10),rep('e',10),rep('f', 10),rep('g',10),
              rep('h',10),rep('i',10),rep('j',10),rep('k',10),rep('l',10),rep('m',10),rep('n',10))
melOrg<- rbind(mel[c(1:10, 71:80, 11:20, 81:90, 21:30, 91:100, 31:40, 101:110, 41:50, 111:120, 51:60, 121:130, 61:70, 131:140),])
melOrg[,3]<- as.numeric(melOrg[,3])
melOrg[,3]<- round(melOrg[,3], 2)

melOrgA <- melOrg
melOrgA$join<- paste0(melOrgA$variable, '_', melOrgA$Time)
head(melOrgA)
nb.cols <- 10
custom_colors <- c("red", "blue", "green", "orange", "purple", "pink", "cyan", "brown", "yellow", "gray")


changingVar<- c(rep("Inform the diagnosis",20), 
                     rep("Discuss treatment strategies", 20),
                     rep("Communicate to the patient about the onset or recurrence of disease",20),
                     rep("Talk about ending active treatment and starting palliative care",20) ,
                     rep("Involve family and friends in the discussion",20),
                     rep("Dealing with the patien’s reaction",20), 
                     rep("Report death to family",20))
melOrgA$title<- changingVar
melOrgA[c(1:30),]

# THE SAME GRAPH IN VERTICAL
ggplot(melOrgA, aes(fill = factor(num), x= factor(join), y= value))+
  geom_bar(stat="identity")+
  facet_wrap( ~ title, scales = 'free_x', ncol=7, switch = 'x')+
  scale_fill_manual(values = custom_colors)+
  xlab(' ')+  ylab(' ')+
  annotate('text', x= 1.5, y= 0.6, label = ' ')+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8))+
  labs(fill = ' ')+
  scale_x_discrete(labels=c('Before', 'After', 'Before', 'After','Before', 'After','Before', 'After',
                            'Before', 'After', 'Before', 'After','Before', 'After','Before', 'After',
                            'Before', 'After', 'Before', 'After','Before', 'After','Before', 'After',
                            'Before', 'After', 'Before', 'After'))


# THE SAME GRAPH IN HORIZONTAL

## Changing the values
melOrgB<- melOrgA

for (i in 1:nrow(melOrgB)){
  if (melOrgB[i,1] == 1 | melOrgB[i,1] == 2){
    melOrgB[i,1] <- 'I totally disagree'
  } else if (melOrgB[i, 1] == 3 | melOrgB[i, 1] == 4){
    melOrgB[i,1] <- 'I partially disagree'
  } else if (melOrgB[i, 1] == 5 | melOrgB[i, 1] == 6) {
    melOrgB[i,1] <- 'I do not agree nor disagree'
  } else if (melOrgB[i, 1] == 7 | melOrgB[i, 1] == 8) {
    melOrgB[i,1] <- 'I partially agree'
  } else if (melOrgB[i, 1] == 9 | melOrgB[i, 1] == 10) {
    melOrgB[i,1] <- 'I totally agree'
  }
}

ggplot(melOrgB, aes(fill = factor(num), x= factor(Time), y= value))+
  geom_bar(stat="identity")+
  facet_wrap(title ~., scales = 'free_y', ncol=1)+
  coord_flip()+
  scale_fill_grey(start=0.8, end=0.2)+
  xlab(' ')+  ylab(' ')+
  annotate('text', x= 1.5, y= 0.8, label = ' ')+
  theme(panel.grid.major = element_line(colour = "#F0F0F0"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8),
        strip.text = element_text(size = 8),
        strip.background =element_rect(fill="white"))+
  labs(fill = ' ')


############################################################################################
# Comparing the answers before and after each question: 
# Prepared, Performance, Discomfort, Dealing_reaction_patient
############################################################################################
tabSelTwo<-table(joinTables3[,15])
tabSelPropTwo<- prop.table(tabSelTwo)
tabSelPropMeltTwo<- melt(tabSelPropTwo)
colnames(tabSelPropMeltTwo)<- c('num', colnames(joinTables3[,15]))

tabSelPropMeltTwo<- rbind(0,tabSelPropMeltTwo,0)
tabSelPropMeltTwo[,1]<- c(2,4,6,8,10)

for (i in 16:18){
  tabSelThree<-table(joinTables3[,i])
  tabSelPropThree<- prop.table(tabSelThree)
  tabSelPropMeltThree<- melt(tabSelPropThree)
  colnames(tabSelPropMeltThree)<- c('num', colnames(joinTables3[,i]))
  tabSelPropMeltTwo<- left_join(tabSelPropMeltTwo, tabSelPropMeltThree, by='num')
}

tabSelPropMeltTwo<- data.frame(tabSelPropMeltTwo)
tabSelPropMeltTwo[is.na(tabSelPropMeltTwo)] <- 0
colnames(tabSelPropMeltTwo)

##### After
tabSelTwoAfter<-table(joinTables3[,46])
tabSelPropTwoAfter<- prop.table(tabSelTwoAfter)
tabSelPropMeltTwoAfter<- melt(tabSelPropTwoAfter)
colnames(tabSelPropMeltTwoAfter)<- c('num', colnames(joinTables3[,46]))
tabSelPropMeltTwoAfter<- rbind(0,0, tabSelPropMeltTwoAfter, 0)
tabSelPropMeltTwoAfter[,1]<- c(2,4,6,8,10)

for (i in 47:49){
  tabSelThreeAfter<-table(joinTables3[,i])
  tabSelPropThreeAfter<- prop.table(tabSelThreeAfter)
  tabSelPropMeltThreeAfter<- melt(tabSelPropThreeAfter)
  colnames(tabSelPropMeltThreeAfter)<- c('num', colnames(joinTables3[,i]))
  tabSelPropMeltTwoAfter<- left_join(tabSelPropMeltTwoAfter, tabSelPropMeltThreeAfter, by='num')
}

tabSelPropMeltTwoAfter<- data.frame(tabSelPropMeltTwoAfter)
tabSelPropMeltTwoAfter[is.na(tabSelPropMeltTwoAfter)] <- 0

tabSelPropMeltTwo
tabSelPropMeltTwoAfter

melAntesTwo<- melt(tabSelPropMeltTwo, id.vars = 'num')
melAntesTwo<- data.frame(melAntesTwo)
melAntesTwo<- melAntesTwo %>% mutate (variable = str_replace(variable, 'Q1_', ' ')) %>% mutate(Time = 'Before')

melDepoisTwo<- melt(tabSelPropMeltTwoAfter, id.vars = 'num')
melDepoisTwo<- data.frame(melDepoisTwo)
melDepoisTwo<- melDepoisTwo %>% mutate (variable = str_replace(variable, 'Q3_', ' ')) %>% mutate(Time = 'After')

melTwo<- rbind(melAntesTwo, melDepoisTwo)
melTwo$org<- c(rep('a', 5),rep('b',5),rep('c',5),rep('d',5),rep('e',5),rep('f', 5), rep('g',5),rep('h',5))

###
melOrgTwo<- rbind(melTwo[c(1:5, 21:25, 6:10, 26:30,11:15, 31:35,16:20, 36:40),])
melOrgTwo[,3]<- as.numeric(melOrgTwo[,3])
melOrgTwo[,3]<- round(melOrgTwo[,3], 2)

melOrgATwo <- melOrgTwo
melOrgATwo$join<- paste0(melOrgATwo$variable, '_', melOrgATwo$Time)
head(melOrgATwo)
nb.cols <- 5
custom_colors <- c("red", "blue", "green", "orange", "purple")

changingVarTwo<- c(rep("How prepared do you consider yourself to deliver bad news?",10), 
                rep("How satisfied are you with your \n performance in delivering bad news? ", 10),
                rep("How uncomfortable are you \n with giving bad news?",10),
                rep("How prepared are you to deal with the patient´s/ \n relative´s reaction to receiving bad news",10))
melOrgATwo$title<- changingVarTwo

ggplot(melOrgATwo, aes(fill = factor(num), x= factor(join), y= value))+
  geom_bar(stat="identity")+
  facet_wrap( ~ title, scales = 'free_x', ncol=7, switch = 'x')+
  scale_fill_manual(values = custom_colors)+
  xlab(' ')+  ylab(' ')+
  annotate('text', x= 1.5, y= 0.6, label = ' ')+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 8))+
  labs(fill = ' ')+
  scale_x_discrete(labels=c('Before', 'After', 'Before', 'After','Before', 'After','Before', 'After',
                            'Before', 'After', 'Before', 'After','Before', 'After','Before', 'After'))+
  coord_flip()

