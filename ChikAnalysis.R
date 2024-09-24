library(dplyr)
library("FactoMineR")
library("factoextra")
library(extrafont)
library(ggplot2)
library(pastecs)
library(corrplot)
library(ppcor)
library(factoextra)
library(psych)
library(GPArotation)
library(Hmisc)
library(dplyr)
library(ape)
library(psych)
library(psychometric)
require(foreign)
require(MASS)
require(pROC)
require(survey)
require(ResourceSelection)
require(ROCR)
require(car)
require(ggplot2)
require(maptools)

setwd('E:\ResearchProject\Dengue ML')

# Assumptions:
# 
# Odds ratio             =2
# Exposed controls       =10%
# Alpha risk             =5%
# Power                  =95%
# Controls / Case ratio  =2
# 
# Total exposed          =12.7273%
# 
# Estimated sample size:
#   
# Number of cases        =347
# Number of controls     =694
# 
# Total                  =1041



ChikData <- read.csv("Data.csv")
# ChikData <- ChikData[1:435,]

#Age Category
ChikData$age_years
ChikData$age_years_group[ChikData$age_years >= 15 & ChikData$age_years <=  29]  = 1
ChikData$age_years_group[ChikData$age_years >= 30 & ChikData$age_years <= 59] = 2
ChikData$age_years_group[ChikData$age_years >= 60] = 3

ChikData$age_years_group <- factor(ChikData$age_years_group,levels=c(1,2,3),labels = c('15-29','30-59','59+'))
ChikData$age_years_group

#Comorbidities
ChikData$comorbid_conditions___1
ChikData$comorbid_conditions___2
ChikData$comorbid_conditions___3
ChikData$comorbid_conditions___4
ChikData$comorbid_conditions___5
ChikData$comorbid_conditions___6
ChikData$comorbid_conditions___7
ChikData$comorbid_conditions___8
ChikData$comorbid <- ChikData$comorbid_conditions___1 + ChikData$comorbid_conditions___2 + ChikData$comorbid_conditions___3 + ChikData$comorbid_conditions___4 + 
  ChikData$comorbid_conditions___5 + ChikData$comorbid_conditions___6 + ChikData$comorbid_conditions___8
ChikData$comorbid
summary(ChikData$comorbid)

ChikData$comorbid_group[ChikData$comorbid <  1]  = 0
ChikData$comorbid_group[ChikData$comorbid >=  1]  = 1

ChikData$comorbid_group <- factor(ChikData$comorbid_group,levels=c(0,1),labels = c('No','Yes'))
ChikData$comorbid_group
summary(ChikData$comorbid_group)

#hypertesion
ChikData$hypertension <- factor(ChikData$comorbid_conditions___1,levels=c(0,1),labels = c('No','Yes'))
ChikData$hypertension 

#diabetes
ChikData$diabetes <- factor(ChikData$comorbid_conditions___2,levels=c(0,1),labels = c('No','Yes'))
ChikData$diabetes

#Stroke
ChikData$stroke <- factor(ChikData$comorbid_conditions___3,levels=c(0,1),labels = c('No','Yes'))
ChikData$stroke

#heart_disease
ChikData$heart_disease <- factor(ChikData$comorbid_conditions___4,levels=c(0,1),labels = c('No','Yes'))
ChikData$heart_disease

#chronic kidney disease
ChikData$ckd <- factor(ChikData$comorbid_conditions___5,levels=c(0,1),labels = c('No','Yes'))
ChikData$ckd

#COPD
ChikData$copd <- factor(ChikData$comorbid_conditions___6,levels=c(0,1),labels = c('No','Yes'))
ChikData$copd

#other
ChikData$other <- factor(ChikData$comorbid_conditions___8,levels=c(0,1),labels = c('No','Yes'))
ChikData$other

#Gender
ChikData$sex
ChikData$sex_group <- factor(ChikData$sex,levels=c(1,2),labels = c('Male','Female'))
ChikData$sex_group

#Hopitalization
ChikData$is_hospitalized
ChikData$is_hospitalized_group <- factor(ChikData$is_hospitalized,levels=c(0,1),labels = c('No','Yes'))
ChikData$is_hospitalized_group

#joint pain before fever
ChikData$is_joint_muscle_pain
ChikData$is_joint_muscle_pain_group <- factor(ChikData$is_joint_muscle_pain,levels=c(0,1),labels = c('No','Yes'))
ChikData$is_joint_muscle_pain_group

#Age Category
ChikData$arth_pain_rating
ChikData$arth_pain_rating_group[ChikData$arth_pain_rating <=  6]  = 1
ChikData$arth_pain_rating_group[ChikData$arth_pain_rating >= 7] = 2
ChikData$arth_pain_rating_group

ChikData$arth_pain_rating_group <- factor(ChikData$arth_pain_rating_group,levels=c(1,2),labels = c('Mild_Moderate','Severe'))
ChikData$arth_pain_rating_group

#joint pain days
ChikData$jointpain_days
ChikData$jointpain_days_new[ChikData$jointpain_days >  1]  = 1
ChikData$jointpain_days_new[ChikData$jointpain_days >  2]  = 2
ChikData$jointpain_days_new[ChikData$jointpain_days >  3]  = 3
ChikData$jointpain_days_new[ChikData$jointpain_days >  4]  = 4
ChikData$jointpain_days_new[ChikData$jointpain_days >  5]  = 5
ChikData$jointpain_days_new[ChikData$jointpain_days >  6]  = 6
ChikData$jointpain_days_new[ChikData$jointpain_days >  7]  = 7
ChikData$jointpain_days_new[ChikData$jointpain_days >  8]  = 8
ChikData$jointpain_days_new[ChikData$jointpain_days >  9]  = 9
ChikData$jointpain_days_new[ChikData$jointpain_days >  10]  = 10
ChikData$jointpain_days_new[ChikData$jointpain_days >  11]  = 11
ChikData$jointpain_days_new[ChikData$jointpain_days >  12]  = 12
ChikData$jointpain_days_new[ChikData$jointpain_days >  13]  = 13
ChikData$jointpain_days_new[ChikData$jointpain_days >=  14]  = 14
ChikData$jointpain_days_new

#sowlen_joint
ChikData$sowlen_joint
ChikData$sowlen_joint_group <- factor(ChikData$sowlen_joint,levels=c(0,1),labels = c('No','Yes'))
ChikData$sowlen_joint_group

#joint pain morning
ChikData$jointpain_morning
ChikData$jointpain_morning_group <- factor(ChikData$jointpain_morning,levels=c(0,1),labels = c('No','Yes'))
ChikData$jointpain_morning_group


#pain_location
ChikData$pain_location___1
ChikData$pain_location___2
ChikData$pain_location___3
ChikData$pain_location___4
ChikData$pain_location___5
ChikData$pain_location___6
ChikData$pain_location___7
ChikData$pain_location___8
ChikData$pain_location___9

ChikData$pain_location <- ChikData$pain_location___1 + ChikData$pain_location___2 + ChikData$pain_location___3 + ChikData$pain_location___4 + ChikData$pain_location___5 +
  ChikData$pain_location___6 + ChikData$pain_location___7 + ChikData$pain_location___8 + ChikData$pain_location___9
summary(ChikData$pain_location)

ChikData$pain_location_group[ChikData$pain_location <=  4]  = 1
ChikData$pain_location_group[ChikData$pain_location >=  5]  = 2

ChikData$pain_location_group <- factor(ChikData$pain_location_group,levels=c(1,2),labels = c('Oligoarthralgia','Polyarthralgia'))
ChikData$pain_location_group
summary(ChikData$pain_location_group)

#finger
ChikData$finger <- factor(ChikData$pain_location___1,levels=c(0,1),labels = c('No','Yes'))
ChikData$finger 

#wrist
ChikData$wrist <- factor(ChikData$pain_location___2,levels=c(0,1),labels = c('No','Yes'))
ChikData$wrist

#spine
ChikData$spine <- factor(ChikData$pain_location___3,levels=c(0,1),labels = c('No','Yes'))
ChikData$spine

#knee
ChikData$knee <- factor(ChikData$pain_location___4,levels=c(0,1),labels = c('No','Yes'))
ChikData$knee

#ankle
ChikData$ankle <- factor(ChikData$pain_location___5,levels=c(0,1),labels = c('No','Yes'))
ChikData$ankle

#feet
ChikData$feet <- factor(ChikData$pain_location___6,levels=c(0,1),labels = c('No','Yes'))
ChikData$feet

#shoulder
ChikData$shoulder <- factor(ChikData$pain_location___7,levels=c(0,1),labels = c('No','Yes'))
ChikData$shoulder


#sowlen_which_joint
ChikData$sowlen_which_joint___1
ChikData$sowlen_which_joint___2
ChikData$sowlen_which_joint___3
ChikData$sowlen_which_joint___4
ChikData$sowlen_which_joint___5

ChikData$sowlen_which_joint <- ChikData$sowlen_which_joint___1 + ChikData$sowlen_which_joint___2 + 
  ChikData$sowlen_which_joint___3 +ChikData$sowlen_which_joint___4 + ChikData$sowlen_which_joint___5
summary(ChikData$sowlen_which_joint)

#redish joint
ChikData$redish_joint_group <- factor(ChikData$redish_joint,levels=c(0,1),labels = c('No','Yes'))
ChikData$redish_joint_group

#Walking
ChikData$Walking <- factor(ChikData$jointpain_reason_list___1,levels=c(0,1),labels = c('No','Yes'))
ChikData$Walking

#sitting
ChikData$sitting <- factor(ChikData$jointpain_reason_list___2,levels=c(0,1),labels = c('No','Yes'))
ChikData$sitting

#standing up
ChikData$standing <- factor(ChikData$jointpain_reason_list___3,levels=c(0,1),labels = c('No','Yes'))
ChikData$standing

#is_rash
ChikData$skin_rash <- factor(ChikData$is_rash,levels=c(0,1),labels = c('No','Yes'))
ChikData$skin_rash

#is_itchy
ChikData$itching <- factor(ChikData$is_itchy,levels=c(0,1),labels = c('No','Yes'))
ChikData$itching

#is_headache
ChikData$headache <- factor(ChikData$is_headache,levels=c(0,1),labels = c('No','Yes'))
ChikData$headache

#is_muscle_ache
ChikData$myalgia <- factor(ChikData$is_muscle_ache,levels=c(0,1),labels = c('No','Yes'))
ChikData$myalgia

#is_red_eye
ChikData$red_eye <- factor(ChikData$is_red_eye,levels=c(0,1),labels = c('No','Yes'))
ChikData$red_eye

#is_eye_pain
ChikData$retroorbitalpain <- factor(ChikData$is_eye_pain,levels=c(0,1),labels = c('No','Yes'))
ChikData$retroorbitalpain

#body_water
ChikData$edema <- factor(ChikData$body_water,levels=c(0,1),labels = c('No','Yes'))
ChikData$edema




#Loss appetite 
ChikData$lossappetite  <- factor(ChikData$symptoms_when_fever___1,levels=c(0,1),labels = c('No','Yes'))
ChikData$lossappetite

#Nausea
ChikData$nausea <- factor(ChikData$symptoms_when_fever___2,levels=c(0,1),labels = c('No','Yes'))
ChikData$nausea

#Diarrhea
ChikData$diarrhea <- factor(ChikData$symptoms_when_fever___3,levels=c(0,1),labels = c('No','Yes'))
ChikData$diarrhea

#Abdominal cramp
ChikData$abdominalcramp <- factor(ChikData$symptoms_when_fever___4,levels=c(0,1),labels = c('No','Yes'))
ChikData$abdominalcramp

#Irregular bowel movement
ChikData$Irregularbowelmovement <- factor(ChikData$symptoms_when_fever___5,levels=c(0,1),labels = c('No','Yes'))
ChikData$Irregularbowelmovement



#chestpain
ChikData$chestpain <- factor(ChikData$other_symptom___1,levels=c(0,1),labels = c('No','Yes'))
ChikData$chestpain

#blurredvision
ChikData$blurredvision <- factor(ChikData$other_symptom___2,levels=c(0,1),labels = c('No','Yes'))
ChikData$blurredvision

#memoryloss
ChikData$memoryloss <- factor(ChikData$other_symptom___3,levels=c(0,1),labels = c('No','Yes'))
ChikData$memoryloss

#Pigmentation
ChikData$Pigmentation <- factor(ChikData$is_black_spot,levels=c(0,1),labels = c('No','Yes'))
ChikData$Pigmentation

#Oralulcer 
ChikData$Oralulcer  <- factor(ChikData$mouth_wound,levels=c(0,1),labels = c('No','Yes'))
ChikData$Oralulcer 

#Dropbloodpressure 
ChikData$Dropbloodpressure  <- factor(ChikData$history_pain,levels=c(0,1),labels = c('No','Yes'))
ChikData$Dropbloodpressure 

#Maculopapular_eruption 
ChikData$Maculopapular_eruption  <- factor(ChikData$is_large_rash,levels=c(0,1),labels = c('No','Yes'))
ChikData$Maculopapular_eruption 

#Bleeding manifestations 
ChikData$Bleeding_manifestations  <- factor(ChikData$is_bleeding,levels=c(0,1),labels = c('No','Yes'))
ChikData$Bleeding_manifestations


c <- table(ChikData$arth_pain_rating_group)
c
prop.table(c)*100
ChikData

#Results

#Crosstab arth_pain_rating_group and Age
c <- table(ChikData$age_years_group)
c
prop.table(c)*100

c <- table(ChikData$age_years_group,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab arth_pain_rating_group and sex
c <- table(ChikData$sex_group)
c
prop.table(c)*100

c <- table(ChikData$sex_group,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab arth_pain_rating_group and marital_status
c <- table(ChikData$marital_status)
c
prop.table(c)*100

c <- table(ChikData$marital_status,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab arth_pain_rating_group and highest_edu
c <- table(ChikData$highest_edu)
c
prop.table(c)*100

c <- table(ChikData$highest_edu,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab arth_pain_rating_group and month_income
c <- table(ChikData$month_income)
c
prop.table(c)*100

c <- table(ChikData$month_income,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab arth_pain_rating_group and Hypertension
c <- table(ChikData$hypertension)
c
prop.table(c)*100

c <- table(ChikData$hypertension ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab arth_pain_rating_group and Diabetes

c <- table(ChikData$diabetes)
c
prop.table(c)*100

c <- table(ChikData$diabetes ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab arth_pain_rating_group and heart_disease
c <- table(ChikData$heart_disease)
c
prop.table(c)*100

c <- table(ChikData$heart_disease ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab arth_pain_rating_group and copd
c <- table(ChikData$other)
c
prop.table(c)*100

c <- table(ChikData$other ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Sign and symptoms

#Crosstab  arth_pain_rating_group and Abdominal Pain
c <- table(ChikData$sowlen_joint_group)
c
prop.table(c)*100

c <- table(ChikData$sowlen_joint_group, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)


#Crosstab Diarrhea and arth_pain_rating_group
c <- table(ChikData$diarrhea)
c
prop.table(c)*100

c <- table(ChikData$diarrhea, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Vomiting and arth_pain_rating_group
c <- table(ChikData$Irregularbowelmovement)
c
prop.table(c)*100

c <- table(ChikData$Irregularbowelmovement, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab lethargy and arth_pain_rating_group
c <- table(ChikData$Dropbloodpressure)
c
prop.table(c)*100

c <- table(ChikData$Dropbloodpressure, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab headache and  arth_pain_rating_group
c <- table(ChikData$headache)
c
prop.table(c)*100

c <- table(ChikData$headache, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab rash and arth_pain_rating_group
c <- table(ChikData$skin_rash)
c
prop.table(c)*100

c <- table(ChikData$skin_rash, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)



#Crosstab URTI and arth_pain_rating_group
c <- table(ChikData$chestpain)
c
prop.table(c)*100

c <- table(ChikData$chestpain, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab Hemorrhagic and arth_pain_rating_group
c <- table(ChikData$Bleeding_manifestations)
c
prop.table(c)*100

c <- table(ChikData$Bleeding_manifestations, ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)

#Crosstab MSK Symptoms and arth_pain_rating_group
c <- table(ChikData$is_joint_muscle_pain_group)
c
prop.table(c)*100

c <- table(ChikData$is_joint_muscle_pain_group ,ChikData$arth_pain_rating_group)
c
prop.table(c,2)*100
summary(c)


###############################################################################

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$age_years_group),ref = "59+"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$sex_group),ref = "Female"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$marital_status),ref = "2"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$highest_edu),ref = "4"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$month_income),ref = "4"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$hypertension),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$diabetes),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$heart_disease),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$other),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$sowlen_joint_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$diarrhea),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$Irregularbowelmovement),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$Dropbloodpressure),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$headache),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$skin_rash),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$chestpain),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$Bleeding_manifestations),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), 
                      ref = "Mild_Moderate")~ relevel(factor(ChikData$is_joint_muscle_pain_group),ref = "No"), 
              family=binomial(link='logit'), data=ChikData)
summary(model)
exp(cbind(coef(model), confint(model)))

#########################################################################################
#logistic odds ratio

DengueData <- subset(ChikData, select=c('arth_pain_rating_group','age_years_group','sex_group','marital_status','highest_edu','month_income','hypertension',
                          'diabetes','heart_disease','other','sowlen_joint_group','diarrhea','Irregularbowelmovement',
                          'Dropbloodpressure','headache','skin_rash','chestpain','Bleeding_manifestations','is_joint_muscle_pain_group'))

write.csv(DengueData, "DengueData.csv")

model <- glm( relevel(factor(ChikData$arth_pain_rating_group), ref = "Mild_Moderate")~ relevel(factor(ChikData$age_years_group),ref = "59+")+
              relevel(factor(ChikData$sex_group),ref = "Female")+
              relevel(factor(ChikData$marital_status),ref = "2")+
              relevel(factor(ChikData$highest_edu),ref = "4")+
              relevel(factor(ChikData$month_income),ref = "4")+
              relevel(factor(ChikData$hypertension),ref = "No")+ 
              relevel(factor(ChikData$diabetes),ref = "No")+
              relevel(factor(ChikData$heart_disease),ref = "No")+ 
              relevel(factor(ChikData$other),ref = "No")+
              relevel(factor(ChikData$sowlen_joint_group),ref = "No")+
              relevel(factor(ChikData$diarrhea),ref = "No")+
              relevel(factor(ChikData$Irregularbowelmovement),ref = "No")+
              relevel(factor(ChikData$Dropbloodpressure),ref = "No")+
              relevel(factor(ChikData$headache),ref = "No")+
              relevel(factor(ChikData$skin_rash),ref = "No")+ 
              relevel(factor(ChikData$chestpain),ref = "No")+
              relevel(factor(ChikData$Bleeding_manifestations),ref = "No")+
              relevel(factor(ChikData$is_joint_muscle_pain_group),ref = "No"),
              family=binomial(link='logit'), data=ChikData)
summary(model)

round(exp(cbind(coef(model), confint(model))),2)

#multivariable logistic


hoslem.test(model$y, fitted(model), g=10) #hosmer and lemeshow goodness of fit  test

#auc value

prob <- predict(model,type="response")
pred <- prediction(as.numeric(prob),as.numeric(model$y))
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
auc

#roc curve

plot(perf, main="ROC Curve ", xlab="specificity",  ylab="sensitivity")  
grid()
abline(0,1, col="blue", lty=2)

