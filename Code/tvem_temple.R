### This code runs the analysis for a tvem of steps on daily smoking and tests for mediation via cravings and negative affect. ###

# Set path for data files
path <- "/Users/huffnaglen/ShareFile/Personal Folders/Masters Analysis/Data"
pdf(file = paste(path,'outputs.pdf',sep = '/'))

# Load packages
library(haven) # for read_spss
library(tidyverse) # for data munging/mutating, etc.
library(tvem) # for time varying analyses
library(sjPlot)

# Read in data files
whdata <- read_spss(paste(path,'WSH1_Master_Computed_BaselineAndOutcomes_2021Jan13.sav',sep = '/'))
ped <- read_spss(paste(path,'WSH1_Master_PH_PedDataBaseline_DI_2021Jan13.sav',sep = '/'))
ema <- read_spss(paste(path,'WSH1_Master_PH_BaselineEDData_DI_2021Jan13.sav',sep = '/'))
calendar <- read_spss(paste(path,'WSH1_Master_PH_CalendarY1_DI_2021Jan13.sav',sep = '/'))

# Create demographics dataset
demos <- whdata %>% 
  select(SubjectID,gender,RaceID1,Condition,age) %>% 
  mutate(gender=as.factor(gender),
         RaceID1=as.factor(RaceID1),
         Condition=as.factor(Condition))

# EMA dataset which runs from -7:7, summarises cravings and negative affect variables by summing across day
ema.filtered <- ema %>% 
  filter(DaysFromTQD>=-7 & DaysFromTQD<=7) %>% 
  select(SubjectID,DaysFromTQD,
         urge1,wsws11,
         wsws03,wsws13,wsws12,wsws06,npanas1,npanas2) %>% 
  group_by(SubjectID,DaysFromTQD) %>% 
  summarise(urge=mean(c(urge1,wsws11)),
            naffect=mean(c(wsws03,wsws13,wsws12,wsws06)))

hist(ema.filtered$urge)
hist(ema.filtered$naffect)

# Daily Smoking dataset from -7:7
cig.filtered <- calendar %>% 
  filter(DaysFromTQD>=-7 & DaysFromTQD<=7) %>% 
  select(SubjectID,DaysFromTQD,cignum)

# Create step dataset
ped.steps <- ped %>%
  filter(StartDFQ>=-7 & StartDFQ<=1) %>% 
  select(SubjectID,StartDFQ,contains('Steps')) %>% 
  arrange(SubjectID,StartDFQ)

# flip from wide to long format
ped.steps <- ped.steps %>%
  pivot_longer(cols = 3:9) 

# Get simpler name convention for steps/day and compute a DFQ variable
ped.string <- strsplit(ped.steps$name,'_')
ped.steps$daynum <- as.numeric(sapply(ped.string,FUN = "[[",MARGIN = 2))
ped.steps$daynum_add <- ped.steps$daynum-1
ped.steps$try <- ped.steps$StartDFQ + ped.steps$daynum_add

ped.steps <- ped.steps %>%  
  select(SubjectID,try,value)

names(ped.steps) <- c('SubjectID','DaysFromTQD','Steps')

# Get hours of pedometer wear per day, in same fashion as steps per day
ped.hours <- ped %>%
  filter(StartDFQ>=-7 & StartDFQ<=1) %>% 
  select(SubjectID,StartDFQ,contains('Hours')) %>% 
  arrange(SubjectID,StartDFQ)

ped.hours <- ped.hours %>%
  pivot_longer(cols = 3:9)

ped.string <- strsplit(ped.hours$name,'_')
ped.hours$daynum <- as.numeric(sapply(ped.string,FUN = "[[",MARGIN = 2))
ped.hours$daynum_add <- ped.hours$daynum-1
ped.hours$try <- ped.hours$StartDFQ + ped.hours$daynum_add

ped.hours <- ped.hours %>%  
  select(SubjectID,try,value)

names(ped.hours) <- c('SubjectID','DaysFromTQD','Hours_Worn')

ped4 <- merge(ped.steps,ped.hours,by = c("SubjectID","DaysFromTQD"))

# At this point, exclude any ema or smoking data from subjects who do not appear at least once in the analytical pedomter dataset
ema.filtered <- ema.filtered %>%
  filter(SubjectID %in% ped4$SubjectID)

cig.filtered <- cig.filtered %>%
  filter(SubjectID %in% ped4$SubjectID)

# Merge step, ema, and smoking data by subject ID, by DFQ
final.set <- merge(ped4, ema.filtered,by = c("SubjectID","DaysFromTQD"),all = T)
final.set <- merge(final.set,cig.filtered,by = c("SubjectID","DaysFromTQD"),all = T)

# Merge final.set with demographics
final.wdemos <- merge(final.set,demos,by = "SubjectID")

# Summarize data
summary(final.wdemos)

# Step data cleaning
# exclusion criteria 
# a)	Less than 10 hours of reported pedometer wear time for a day
# b)	A day with > 50,000 steps
# c)	A day with < 500 steps
# d) at least 2 days of wear

nrow(final.wdemos)
cleaned <- final.wdemos %>%
  filter(Hours_Worn>=10)
nrow(cleaned)
cleaned2 <- cleaned %>%
  filter(Steps<50000)
nrow(cleaned2)
cleaned3 <- cleaned2 %>%
  filter(Steps>=500)
nrow(cleaned3)

df.hits <- as.data.frame(table(cleaned3$SubjectID))
df.hits <- df.hits %>% filter(Freq>1) %>% filter(Freq<=14)

cleaned3 <- cleaned3 %>%
  filter(SubjectID %in% df.hits$Var1)
nrow(cleaned3) # 6423 observations
length(unique(cleaned3$SubjectID)) # 786 subjects

# Condition dummy variables
cleaned3$Active_Bupropion <- ifelse(cleaned3$Condition==1,1,0)
cleaned3$Active_Lozenge <- ifelse(cleaned3$Condition==2,1,0)
cleaned3$Active_Patch <- ifelse(cleaned3$Condition==3,1,0)
cleaned3$Active_Bupropion_Lozenge <- ifelse(cleaned3$Condition==4,1,0)
cleaned3$Active_Patch_Lozenge <- ifelse(cleaned3$Condition==5,1,0)
cleaned3$Placebo_Bupropion <- ifelse(cleaned3$Condition==6,1,0)
cleaned3$Placebo_Lozenge <- ifelse(cleaned3$Condition==7,1,0)
cleaned3$Placebo_Patch <- ifelse(cleaned3$Condition==8,1,0)
cleaned3$Placebo_Bupropion_Lozenge <- ifelse(cleaned3$Condition==9,1,0)
cleaned3$Placebo_Patch_Lozenge <- ifelse(cleaned3$Condition==10,1,0)

# Race dummy variables
cleaned3$White <- ifelse(cleaned3$RaceID1==1,1,0)
cleaned3$Black <- ifelse(cleaned3$RaceID1==2,1,0)
cleaned3$American_Indian <- ifelse(cleaned3$RaceID1==3,1,0)
cleaned3$Alaskan_Native <- ifelse(cleaned3$RaceID1==4,1,0)
cleaned3$Asian <- ifelse(cleaned3$RaceID1==5,1,0)
cleaned3$Other <- ifelse(cleaned3$RaceID1==6,1,0)

# Gender dummy variables
cleaned3$Female <- ifelse(cleaned3$gender==0,1,0)
cleaned3$Male <- ifelse(cleaned3$gender==1,1,0)

# Smoke binary and steps 1k
cleaned3$steps1k <- cleaned3$Steps/1000
cleaned3$smoke <- ifelse(cleaned3$cignum>0,1,0)

# Stratify analysis by 10k steps average per day during this time period
under10k<- cleaned3 %>%
  group_by(SubjectID) %>%
  summarise(msteps=mean(Steps)) %>%
  filter(msteps<10000)
setunder10k<- cleaned3 %>%
  filter(SubjectID%in%under10k$SubjectID) %>% 
  mutate(smoke=ifelse(cignum>0,1,0),
         StepGroup='Under 10K')

over10k <- cleaned3 %>%
  group_by(SubjectID) %>%
  summarise(msteps=mean(Steps)) %>%
  filter(msteps>10000)
setover10k<- cleaned3 %>%
  filter(SubjectID%in%over10k$SubjectID) %>% 
  mutate(smoke=ifelse(cignum>0,1,0),
         StepGroup='Over 10K')

cle4 <- rbind(setover10k,setunder10k)
  
cle4 <- cle4 %>%
  select(SubjectID,DaysFromTQD,urge,naffect,smoke,steps1k,StepGroup,12:29)

vecs <- complete.cases(cle4)

cle4 <- cle4[vecs,]

full.sample <- unique(cle4$SubjectID)

under.sample <- cle4 %>% filter(StepGroup=='Under 10K')
under.sample <- unique(under.sample$SubjectID)

over.sample <- cle4 %>% filter(StepGroup=='Over 10K')
over.sample <- unique(over.sample$SubjectID)

setover10k<-setover10k %>% 
  filter(SubjectID %in% over.sample)

setunder10k<-setunder10k %>% 
  filter(SubjectID %in% under.sample)

cleaned3 <- cleaned3 %>% filter(SubjectID%in%full.sample)

# Demographics table
total.set <- cle4

total.set <- total.set %>% 
  select(SubjectID,StepGroup)

demo.merge <- merge(whdata,total.set,by='SubjectID')
demo.merge <- demo.merge %>% group_by(SubjectID) %>% distinct(SubjectID,.keep_all = T)

# n subjects per step group
table(demo.merge$StepGroup)

# % subjects with study condition by step group
prop.table(table(demo.merge$Condition,demo.merge$StepGroup),margin = 2)*100

# total subjects by condition %
prop.table(table(demo.merge$Condition))*100

# % sujects gender by step group
prop.table(table(demo.merge$gender,demo.merge$StepGroup),margin = 2)*100

# total subjects by gender %
prop.table(table(demo.merge$gender))*100

# total subjects by race %
prop.table(table(demo.merge$race_2))*100

# age by step group
demo.merge %>% group_by(StepGroup) %>% summarise(mage=mean(age,na.rm = T))

# Histograms of NA and urge
ggplot(cleaned3,aes(x=log(naffect+1)))+
  geom_histogram(bins = 35,fill='dark gray',color='black')+
  theme_linedraw()+
  ggtitle('Distribution of logged Negative Affect',subtitle = 'All daily NA observations from all subjects')

ggplot(cleaned3,aes(x=naffect))+
  geom_histogram(bins = 35,fill='dark gray',color='black')+
  theme_linedraw()+
  ggtitle('Distribution of Negative Affect',subtitle = 'All daily NA observations from all subjects')

ggplot(cleaned3,aes(x=urge))+
  geom_histogram(bins = 35,fill='dark gray',color='black')+
  theme_linedraw()+
  ggtitle('Distribution of Urge',subtitle = 'All daily urge observations from all subjects')

# Mean plots over time

# Steps
# make a df with daily means for variables of interest with groups for full data, < 10k steps, and > 10k steps

cleaned.un10 <- cleaned3 %>%
  filter(SubjectID %in% under10k$SubjectID) %>% 
  mutate(StepGroup='Under 10K')

cleaned.ov10 <- cleaned3 %>%
  filter(SubjectID %in% over10k$SubjectID) %>% 
  mutate(StepGroup='Over 10K')

cleaned4 <- rbind(cleaned.ov10,cleaned.un10)

cleaned5 <- cleaned4 %>%
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(msteps=mean(Steps))

cleaned6 <- cleaned3 %>% 
  group_by(DaysFromTQD) %>% 
  summarise(msteps=mean(Steps)) %>% 
  mutate(StepGroup='Total Subjects')

cleaned5 <- rbind(cleaned5,cleaned6)

step.p <- ggplot(cleaned5,aes(x=DaysFromTQD,y=msteps,color=StepGroup))+
  geom_line()+
  geom_point()+
  ylim(4000,14000)+
  ylab('Steps')

cleaned5.fem <- cleaned4 %>%
  filter(gender==0) %>% 
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(msteps=mean(Steps)) %>% 
  mutate(gender='Female')

cleaned5.male <- cleaned4 %>%
  filter(gender==1) %>% 
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(msteps=mean(Steps)) %>% 
  mutate(gender='Male')

cleaned5 <- rbind(cleaned5.fem,cleaned5.male)

cleaned6.fem <- cleaned3 %>% 
  filter(gender==0) %>% 
  group_by(DaysFromTQD) %>% 
  summarise(msteps=mean(Steps)) %>% 
  mutate(StepGroup='Total Subjects') %>% 
  mutate(gender='Female')

cleaned6.male <- cleaned3 %>% 
  filter(gender==1) %>% 
  group_by(DaysFromTQD) %>% 
  summarise(msteps=mean(Steps)) %>% 
  mutate(StepGroup='Total Subjects') %>% 
  mutate(gender='Male')

cleaned6 <- rbind(cleaned6.male,cleaned6.fem)

cleaned5 <- rbind(cleaned5,cleaned6)

step.p.gender <- ggplot(cleaned5,aes(x=DaysFromTQD,y=msteps,color=StepGroup))+
  geom_line()+
  geom_point()+
  ylim(4000,14000)+
  ylab('Steps')+
  facet_wrap(~gender)
step.p.gender

# Cravings
# make a df with daily means for variables of interest with groups for full data, < 10k urge, and > 10k urge

cleaned.un10 <- cleaned3 %>%
  filter(SubjectID %in% under10k$SubjectID) %>% 
  mutate(StepGroup='Under 10K')

cleaned.ov10 <- cleaned3 %>%
  filter(SubjectID %in% over10k$SubjectID) %>% 
  mutate(StepGroup='Over 10K')

cleaned4 <- rbind(cleaned.ov10,cleaned.un10)

cleaned5 <- cleaned4 %>%
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(murge=mean(urge,na.rm=T))

cleaned6 <- cleaned3 %>% 
  group_by(DaysFromTQD) %>% 
  summarise(murge=mean(urge,na.rm=T)) %>% 
  mutate(StepGroup='Total Subjects')

cleaned5 <- rbind(cleaned5,cleaned6)

urge.p <- ggplot(cleaned5,aes(x=DaysFromTQD,y=murge,color=StepGroup))+
  geom_line()+
  geom_point()+
  ylab('Cravings')
urge.p

cleaned5.fem <- cleaned4 %>%
  filter(gender==0) %>% 
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(murge=mean(urge,na.rm=T)) %>% 
  mutate(gender='Female')

cleaned5.male <- cleaned4 %>%
  filter(gender==1) %>% 
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(murge=mean(urge,na.rm=T)) %>% 
  mutate(gender='Male')

cleaned5 <- rbind(cleaned5.fem,cleaned5.male)

cleaned6.fem <- cleaned3 %>% 
  filter(gender==0) %>% 
  group_by(DaysFromTQD) %>% 
  summarise(murge=mean(urge,na.rm=T)) %>% 
  mutate(StepGroup='Total Subjects') %>% 
  mutate(gender='Female')

cleaned6.male <- cleaned3 %>% 
  filter(gender==1) %>% 
  group_by(DaysFromTQD) %>% 
  summarise(murge=mean(urge,na.rm=T)) %>% 
  mutate(StepGroup='Total Subjects') %>% 
  mutate(gender='Male')

cleaned6 <- rbind(cleaned6.male,cleaned6.fem)

cleaned5 <- rbind(cleaned5,cleaned6)

urge.p.gender <- ggplot(cleaned5,aes(x=DaysFromTQD,y=murge,color=StepGroup))+
  geom_line()+
  geom_point()+
  ylab('Cravings')+
  facet_wrap(~gender)
urge.p.gender

# Negative Affect
# make a df with daily means for variables of interest with groups for full data, < 10k naffect, and > 10k naffect
cleaned.un10 <- cleaned3 %>%
  filter(SubjectID %in% under10k$SubjectID) %>% 
  mutate(StepGroup='Under 10K')

cleaned.ov10 <- cleaned3 %>%
  filter(SubjectID %in% over10k$SubjectID) %>% 
  mutate(StepGroup='Over 10K')

cleaned4 <- rbind(cleaned.ov10,cleaned.un10)

cleaned5 <- cleaned4 %>%
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(mnaffect=mean(naffect,na.rm=T))

cleaned6 <- cleaned3 %>% 
  group_by(DaysFromTQD) %>% 
  summarise(mnaffect=mean(naffect,na.rm=T)) %>% 
  mutate(StepGroup='Total Subjects')

cleaned5 <- rbind(cleaned5,cleaned6)

naffect.p <- ggplot(cleaned5,aes(x=DaysFromTQD,y=mnaffect,color=StepGroup))+
  geom_line()+
  geom_point()+
  ylab('Negative Affect')
naffect.p

cleaned5.fem <- cleaned4 %>%
  filter(gender==0) %>% 
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(mnaffect=mean(naffect,na.rm=T)) %>% 
  mutate(gender='Female')

cleaned5.male <- cleaned4 %>%
  filter(gender==1) %>% 
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(mnaffect=mean(naffect,na.rm=T)) %>% 
  mutate(gender='Male')

cleaned5 <- rbind(cleaned5.fem,cleaned5.male)

cleaned6.fem <- cleaned3 %>% 
  filter(gender==0) %>% 
  group_by(DaysFromTQD) %>% 
  summarise(mnaffect=mean(naffect,na.rm=T)) %>% 
  mutate(StepGroup='Total Subjects') %>% 
  mutate(gender='Female')

cleaned6.male <- cleaned3 %>% 
  filter(gender==1) %>% 
  group_by(DaysFromTQD) %>% 
  summarise(mnaffect=mean(naffect,na.rm=T)) %>% 
  mutate(StepGroup='Total Subjects') %>% 
  mutate(gender='Male')

cleaned6 <- rbind(cleaned6.male,cleaned6.fem)

cleaned5 <- rbind(cleaned5,cleaned6)

naffect.p.gender <- ggplot(cleaned5,aes(x=DaysFromTQD,y=mnaffect,color=StepGroup))+
  geom_line()+
  geom_point()+
  ylab('Negative Affect')+
  facet_wrap(~gender)
naffect.p.gender

# % Relapse by group
# make a df with daily means for variables of interest with groups for full data, < 10k smoke, and > 10k smoke

cleaned.un10 <- cleaned3 %>%
  filter(SubjectID %in% under10k$SubjectID) %>% 
  mutate(StepGroup='Under 10K')

cleaned.ov10 <- cleaned3 %>%
  filter(SubjectID %in% over10k$SubjectID) %>% 
  mutate(StepGroup='Over 10K')

cleaned4 <- rbind(cleaned.ov10,cleaned.un10)

cleaned5 <- cleaned4 %>%
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(msmoke=mean(smoke,na.rm=T))

cleaned6 <- cleaned3 %>% 
  group_by(DaysFromTQD) %>% 
  summarise(msmoke=mean(smoke,na.rm=T)) %>% 
  mutate(StepGroup='Total Subjects')

cleaned5 <- rbind(cleaned5,cleaned6)

smoke.p <- ggplot(cleaned5,aes(x=DaysFromTQD,y=msmoke,color=StepGroup))+
  geom_line()+
  geom_point()+
  ylab('% Relapse')
smoke.p

cleaned5.fem <- cleaned4 %>%
  filter(gender==0) %>% 
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(msmoke=mean(smoke,na.rm=T)) %>% 
  mutate(gender='Female')

cleaned5.male <- cleaned4 %>%
  filter(gender==1) %>% 
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(msmoke=mean(smoke,na.rm=T)) %>% 
  mutate(gender='Male')

cleaned5 <- rbind(cleaned5.fem,cleaned5.male)

cleaned6.fem <- cleaned3 %>% 
  filter(gender==0) %>% 
  group_by(DaysFromTQD) %>% 
  summarise(msmoke=mean(smoke,na.rm=T)) %>% 
  mutate(StepGroup='Total Subjects') %>% 
  mutate(gender='Female')

cleaned6.male <- cleaned3 %>% 
  filter(gender==1) %>% 
  group_by(DaysFromTQD) %>% 
  summarise(msmoke=mean(smoke,na.rm=T)) %>% 
  mutate(StepGroup='Total Subjects') %>% 
  mutate(gender='Male')

cleaned6 <- rbind(cleaned6.male,cleaned6.fem)

cleaned5 <- rbind(cleaned5,cleaned6)

smoke.p.gender <- ggplot(cleaned5,aes(x=DaysFromTQD,y=msmoke,color=StepGroup))+
  geom_line()+
  geom_point()+
  ylab('% Relapse')+
  facet_wrap(~gender)
smoke.p.gender

# Post quit day relapse rates
# % Relapse by group
# make a df with daily means for variables of interest with groups for full data, < 10k smoke, and > 10k smoke
cleaned.un10 <- cleaned3 %>%
  filter(SubjectID %in% under10k$SubjectID) %>% 
  mutate(StepGroup='Under 10K')

cleaned.ov10 <- cleaned3 %>%
  filter(SubjectID %in% over10k$SubjectID) %>% 
  mutate(StepGroup='Over 10K')

cleaned4 <- rbind(cleaned.ov10,cleaned.un10)

cleaned5 <- cleaned4 %>%
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(msmoke=mean(smoke,na.rm=T))

cleaned6 <- cleaned3 %>% 
  group_by(DaysFromTQD) %>% 
  summarise(msmoke=mean(smoke,na.rm=T)) %>% 
  mutate(StepGroup='Total Subjects')

cleaned5 <- rbind(cleaned5,cleaned6)

smoke.p <- ggplot(cleaned5,aes(x=DaysFromTQD,y=msmoke,color=StepGroup))+
  geom_line()+
  geom_point()+
  ylab('% Relapse')
smoke.p

cleaned5.fem <- cleaned4 %>%
  filter(gender==0) %>% 
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(msmoke=mean(smoke,na.rm=T)) %>% 
  mutate(gender='Female')

cleaned5.male <- cleaned4 %>%
  filter(gender==1) %>% 
  group_by(DaysFromTQD,StepGroup) %>% 
  summarise(msmoke=mean(smoke,na.rm=T)) %>% 
  mutate(gender='Male')

cleaned5 <- rbind(cleaned5.fem,cleaned5.male)

cleaned6.fem <- cleaned3 %>% 
  filter(gender==0) %>% 
  group_by(DaysFromTQD) %>% 
  summarise(msmoke=mean(smoke,na.rm=T)) %>% 
  mutate(StepGroup='Total Subjects') %>% 
  mutate(gender='Female')

cleaned6.male <- cleaned3 %>% 
  filter(gender==1) %>% 
  group_by(DaysFromTQD) %>% 
  summarise(msmoke=mean(smoke,na.rm=T)) %>% 
  mutate(StepGroup='Total Subjects') %>% 
  mutate(gender='Male')

cleaned6 <- rbind(cleaned6.male,cleaned6.fem)

cleaned5 <- rbind(cleaned5,cleaned6)

cleaned5 <- cleaned5 %>% 
  filter(DaysFromTQD>=0)

smoke.p.gender <- ggplot(cleaned5,aes(x=DaysFromTQD,y=msmoke,color=StepGroup))+
  geom_line()+
  geom_point()+
  ylab('% Relapse')+
  facet_wrap(~gender)
smoke.p.gender

# All 4 grid arranged
grid.arrange(step.p.gender,urge.p.gender,naffect.p.gender,smoke.p.gender)

# TVEM MODELS -----
# Smoke ~ Steps1K
tvem.full <- tvem(family = binomial(),cleaned3,formula = smoke~steps1k,id = SubjectID,
                   time = DaysFromTQD,num_knots = 10)
tvem.full

tvem.under <- tvem(family = binomial(),setunder10k,formula = smoke~steps1k,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10)
tvem.under

tvem.over <- tvem(family = binomial(),setover10k,formula = smoke~steps1k,id = SubjectID,
                   time = DaysFromTQD,num_knots = 10)
tvem.over

plot(tvem.full,use_panes = T)
title('Full Sample')
plot(tvem.under,use_panes = T)
title('Under 10K Steps')
plot(tvem.over,use_panes = T)
title('Over 10K Steps')

# Smoke ~ Negative Affect
tvem.full <- tvem(family = binomial(),cleaned3,formula = smoke~naffect,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10)
tvem.full

tvem.under <- tvem(family = binomial(),setunder10k,formula = smoke~naffect,id = SubjectID,
                   time = DaysFromTQD,num_knots = 10)
tvem.under

tvem.over <- tvem(family = binomial(),setover10k,formula = smoke~naffect,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10)
tvem.over

plot(tvem.full,use_panes = F)
title('Full Sample',adj=0)
plot(tvem.under,use_panes = F)
title('Under 10K Steps',adj=0)
plot(tvem.over,use_panes = F)
title('Over 10K Steps',adj=0)


# Smoke ~ Urge
tvem.full <- tvem(family = binomial(),cleaned3,formula = smoke~urge,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10)
tvem.full

tvem.under <- tvem(family = binomial(),setunder10k,formula = smoke~urge,id = SubjectID,
                   time = DaysFromTQD,num_knots = 10)
tvem.under

tvem.over <- tvem(family = binomial(),setover10k,formula = smoke~urge,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10)
tvem.over

plot(tvem.full,use_panes = F)
title('Full Sample',adj=0)
plot(tvem.under,use_panes = F)
title('Under 10K Steps',adj=0)
plot(tvem.over,use_panes = F)
title('Over 10K Steps',adj=0)

# Urge ~ Steps1K
tvem.full <- tvem(family = gaussian(),cleaned3,formula = urge~steps1k,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10)
tvem.full

tvem.under <- tvem(family = gaussian(),setunder10k,formula = urge~steps1k,id = SubjectID,
                   time = DaysFromTQD,num_knots = 10)
tvem.under

tvem.over <- tvem(family = gaussian(),setover10k,formula = urge~steps1k,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10)
tvem.over

plot(tvem.full,use_panes = F)
title('Full Sample',adj=0)
plot(tvem.under,use_panes = F)
title('Under 10K Steps',adj=0)
plot(tvem.over,use_panes = F)
title('Over 10K Steps',adj=0)

# Negative Affect ~ Steps1K
tvem.full <- tvem(family = gaussian(),cleaned3,formula = naffect~steps1k,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10)
tvem.full

tvem.under <- tvem(family = gaussian(),setunder10k,formula = naffect~steps1k,id = SubjectID,
                   time = DaysFromTQD,num_knots = 10)
tvem.under

tvem.over <- tvem(family = gaussian(),setover10k,formula = naffect~steps1k,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10)
tvem.over

plot(tvem.full,use_panes = F)
title('Full Sample',adj=0)
plot(tvem.under,use_panes = F)
title('Under 10K Steps',adj=0)
plot(tvem.over,use_panes = F)
title('Over 10K Steps',adj=0)

# Smoke ~ Steps1K + Urge + Negative Affect 
tvem.full <- tvem(family = binomial(),cleaned3,formula = smoke~steps1k+naffect+urge,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10)
tvem.full

tvem.under <- tvem(family = binomial(),setunder10k,formula = smoke~steps1k+naffect+urge,id = SubjectID,
                   time = DaysFromTQD,num_knots = 10)
tvem.under

tvem.over <- tvem(family = binomial(),setover10k,formula = smoke~steps1k+naffect+urge,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10)
tvem.over

plot(tvem.full)
title('Full Sample')
plot(tvem.under)
title('Under 10K Steps')
plot(tvem.over)
title('Over 10K Steps')

# Smoke ~ Steps1K + Urge + Negative Affect + Invariant Effects
tvem.full <- tvem(family = binomial(),cleaned3,formula = smoke~steps1k+naffect+urge,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10,
                  invar_effects = ~Active_Lozenge+Active_Patch+Active_Bupropion_Lozenge+
                    Active_Patch_Lozenge+Placebo_Bupropion+Placebo_Lozenge+Placebo_Patch+
                    Placebo_Bupropion_Lozenge+Placebo_Patch_Lozenge+Black+American_Indian+
                    Alaskan_Native+Asian+Other+Female)
tvem.full
tvem.under <- tvem(family = binomial(),setunder10k,formula = smoke~steps1k+naffect+urge,id = SubjectID,
                   time = DaysFromTQD,num_knots = 10,
                   invar_effects = ~Active_Lozenge+Active_Patch+Active_Bupropion_Lozenge+
                     Active_Patch_Lozenge+Placebo_Bupropion+Placebo_Lozenge+Placebo_Patch+
                     Placebo_Bupropion_Lozenge+Placebo_Patch_Lozenge+Black+American_Indian+
                     Alaskan_Native+Asian+Other+Female)
tvem.under
tvem.over <- tvem(family = binomial(),setover10k,formula = smoke~steps1k+naffect+urge,id = SubjectID,
                  time = DaysFromTQD,num_knots = 10,
                  invar_effects = ~Active_Lozenge+Active_Patch+Active_Bupropion_Lozenge+
                    Active_Patch_Lozenge+Placebo_Bupropion+Placebo_Lozenge+Placebo_Patch+
                    Placebo_Bupropion_Lozenge+Placebo_Patch_Lozenge+Black+American_Indian+
                    Alaskan_Native+Asian+Other+Female)
tvem.over

plot(tvem.full)
title('Full Sample')
plot(tvem.under)
title('Under 10K Steps')
plot(tvem.over)
title('Over 10K Steps')

# full sample binary
# Compute invariant effects p values
df.full <- as.data.frame(tvem.full$invar_effects_estimates)
df.full$pvalue <- df.full$estimate/df.full$standard_error
write.csv(df.full,file = paste(path,'invar.full.csv',sep = '/'))

# under10K binary
# Compute invariant effects p values
df.under <- as.data.frame(tvem.under$invar_effects_estimates)
df.under$pvalue <- df.under$estimate/df.under$standard_error
write.csv(df.under,file = paste(path,'invar.under10.csv',sep = '/'))

# over10K binary
# Compute invariant effects p values
df.over <- as.data.frame(tvem.over$invar_effects_estimates)
df.over$pvalue <- df.over$estimate/df.over$standard_error
write.csv(df.over,file = paste(path,'invar.over10.csv',sep = '/'))

# save outputs
dev.off()