# DANCEProject
## Introduction

### About this project
Our study is designed as a cross sectional study. All the baseline data were collected from DANCE (Diabetes ANd CarohydratEs) study which is a randomized control trial aiming to contribute to an evidence based dietary recommendation for type-1 Diabetes patients. DANCE study compares the effect of traditional diabetic diet, moderately low carbohydrate diet and very low carbohydrate diet (not ketogenic diet) on insulin requirements in the patients with T1DM also glycemic variability and metabolic control. With the data from DANCE study, we will describe the baseline dietary habits of a group of adults with type 1 diabetes and investigate the association between quality and quantity of carbohydrates and baseline glycemic markers. We have followed a validated FFQ to get the information on the quality and quantity of the carbohydrate and the association with glycemic marker was assessed by blood sample of the adults with T1DM.

### My research aim
Investigate the association between quality of carbohydrates include glycemic index, and baseline glycemic markers in Stockholm T1DM population
Three sub areas of this aim
1. Create the baseline characteristics of the study population
2. Investigate the quality of carbohyrdates and T1DM patients by using different statistical models
3. Find the potential correlation/ models in terms of the quantity of carbohyrdates in T1DM patients

## Study Population
A total of 65 type 1 diabetes patients were recruited from Akademiskt specialistcentrum, Solna, Stockholm 
In total of (1)	55/65 participants were selected based on the active CGM time 28 days (more than 60% per day)
The following code and graph shows the distribution of year of born and duration of diabetes in terms of gender. The blue dots also refer to the BMI, lighter blue means higher BMI.

```R
ggplot(Table1_Basic_information, aes( `Year of Born`, `Duration of Diabetes/yrs`))+
  geom_jitter( aes(colour= BMI))+
  facet_wrap(.~Gender)+
  geom_smooth( method=lm, size=0.5, colour="black")+
  labs(title = "Year of born and duration of diabetes in terms of gender")
```
![](Image/yearandduration.jpeg)
