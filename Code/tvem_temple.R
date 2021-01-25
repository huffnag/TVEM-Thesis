### This code runs the analysis for a tvem of steps on daily smoking and tests for mediators cravings and negative affect. ###
#
#
#

# Set path for data files
path <- "/Users/huffnaglen/ShareFile/Personal Folders/Masters Analysis/Data"

# Load packages
library(haven) # for read_spss
library(tidyverse) # for data munging/mutating, etc.
library(tvem) # for time varying analyses

# Read in data files
whdata <- read_spss(paste(path,'WSH1_Master_Computed_BaselineAndOutcomes_2021Jan13.sav',sep = '/'))
ped <- read_spss(paste(path,'WSH1_Master_PH_PedDataBaseline_DI_2021Jan13.sav',sep = '/'))
ema <- read_spss(paste(path,'WSH1_Master_PH_BaselineEDData_DI_2021Jan13.sav',sep = '/'))
calendar <- read_spss(paste(path,'WSH1_Master_PH_CalendarY1_DI_2021Jan13.sav',sep = '/'))

# Create demographics dataset
demos <- whdata %>% 
  select(SubjectID,gender,RaceID1,Condition) %>% 
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
  summarise(urge=sum(urge1)+sum(wsws11),
            naffect=sum(wsws03)+sum(wsws13)+sum(wsws12)+sum(wsws06)+sum(npanas1)+sum(npanas2)) 

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

# tvem model of steps on cravings
tvem.model <- tvem(final.wdemos,formula = urge~Steps,id = SubjectID,
                   time = DaysFromTQD,num_knots = 25)
plot(tvem.model)
tvem.model
summary(lm(urge~Steps,final.wdemos))

# tvem model of steps on smoking
tvem.model <- tvem(final.wdemos,formula = cignum~Steps,id = SubjectID,
                   time = DaysFromTQD,num_knots = 25)
plot(tvem.model)
tvem.model
summary(lm(cignum~Steps,final.wdemos))

# try step data cleaning
# exclusion criteria 
# a)	Less than 10 hours of reported pedometer wear time for a day
# b)	A day with > 50,000 steps
# c)	A day with < 500 steps

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

# tvem model of steps on cravings
tvem.model <- tvem(cleaned3,formula = urge~Steps,id = SubjectID,
                   time = DaysFromTQD,num_knots = 25)
plot(tvem.model)
tvem.model
summary(lm(urge~Steps,cleaned3))

# tvem model of steps on smoking
tvem.model <- tvem(cleaned3,formula = cignum~Steps,id = SubjectID,
                   time = DaysFromTQD,num_knots = 25)
plot(tvem.model)
tvem.model
summary(lm(cignum~Steps,cleaned3))

# tvem model of steps on negative affect
tvem.model <- tvem(cleaned3,formula = naffect~Steps,id = SubjectID,
                   time = DaysFromTQD,num_knots = 25)
plot(tvem.model)
tvem.model
summary(lm(naffect~Steps,cleaned3))

# tvem model of negative affect on smoking
tvem.model <- tvem(cleaned3,formula = cignum~naffect,id = SubjectID,
                   time = DaysFromTQD,num_knots = 25)
plot(tvem.model)
tvem.model
summary(lm(cignum~naffect,cleaned3))

# w covariates
# tvem model of steps on cravings
tvem.model <- tvem(cleaned3,formula = urge~Steps,id = SubjectID,
                   time = DaysFromTQD,num_knots = 25,
                   invar_effects = ~gender+RaceID1)
plot(tvem.model)
tvem.model
summary(lm(urge~Steps,cleaned3))

# tvem model of steps on smoking
tvem.model <- tvem(cleaned3,formula = cignum~Steps,id = SubjectID,
                   time = DaysFromTQD,num_knots = 25,
                   invar_effects = ~gender+RaceID1)
plot(tvem.model)
tvem.model
summary(lm(cignum~Steps,cleaned3))

# full tvem model of steps on smoking
tvem.model <- tvem(cleaned3,formula = cignum~Steps+naffect+urge,id = SubjectID,
                   time = DaysFromTQD)
plot(tvem.model)
tvem.model
summary(lm(cignum~Steps+naffect+urge,
           cleaned3))

tvem.model <- tvem(cleaned3,formula = cignum~Steps,id = SubjectID,
                   time = DaysFromTQD)
plot(tvem.model)



tvem.model <- tvem(cleaned3,formula = cignum~Steps+naffect+urge,id = SubjectID,
                   time = DaysFromTQD,num_knots = 5)

plot(tvem.model,which_plot = 2) + ylim(c(-10,50))

model2_selected_knots <- select_tvem(data=cleaned3,
                                     formula = Steps~1,
                                     id=SubjectID,
                                     time=DaysFromTQD,
                                     max_knots=10)

model2_selected_knots
plot(model2_selected_knots)


# summary table stuff
summary(cleaned3$Steps)
sd(cleaned3$Steps)

summary(cleaned3$urge)
sd(cleaned3$urge,na.rm = T)

summary(cleaned3$cignum)
sd(cleaned3$cignum,na.rm = T)

summary(cleaned3$naffect)
sd(cleaned3$naffect,na.rm = T)









