# DANCE Project

## About this project

Type 1 diabetes mellitus (T1DM) is a type of autoimmune disease characterized by increased blood glucose level due to the inability of producing insulin. Patients need regular insulin injections and usually monitor their blood glucose level continuously. Glucose intake significantly affects the blood glucose level related to the daily dietary intake. This cross-sectional study compared dietary intake between T1DM patients participating in the DANCE study at Center for Diabetes in Stockholm and the general Swedish population. A validated FFQ was filled by 65 participants to investigate the food habits. The mean intake of different food groups was compared with the Swedish national dietary report, Riksmaten-vuxna 2010-11. This comparison gave an overview of the diabetic dietary pattern. Study participants chose less refined carbohydrates, fruits, red meat, and more vegetables, poultry, and fish in comparison to the Swedish general population. We also evaluated the quality of carbohydrate intake and its relationship to glycemic variations. There is a positive correlation between HbA1c and sugar intake, and time in range and energy intake. There is a negative correlation between time in range and sugar and wholegrain intake, standard deviation of mean sensor glucose and energy intake.

## My research aim

(1) Describe the baseline dietary habits of a group of adults with T1DM participating in a clinical trial in Stockholm and compare them with the habits of the general Swedish population 

(2) Investigate the association between quality of carbohydrates including glycemic index, and baseline glycemic markers in the same population. 

### Study Population
A total of 65 type 1 diabetes patients were recruited from Akademiskt specialistcentrum, Solna, Stockholm and other region.
In total of 55/65 participants were selected based on the active CGM time 28 days (more than 60% per day)

# Anthropometric measurement
  Weight/kg
  
  Height/cm
  
  Waist circumference (cm)
  
  Systolic blood pressure (mmHg)
  
  Diastolic blood pressure (mmHg)
  
# Criteria
  Over or equal to 20 years old.
  BMI> 18.5 kg/m2.
  No sever diseases such as kidney failures and cardiovascular diseases.
  No pregnant female or lactate female.
  Have been diagnosis T1DM for over 1 year.
  C-peptide less than 0.3 ng/ml.
  
### R packages used

```library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(bruceR)```

```R
ggplot(Table1_Basic_information, aes( `Year of Born`, `Duration of Diabetes/yrs`))+
  geom_jitter( aes(colour= BMI))+
  facet_wrap(.~Gender)+
  geom_smooth( method=lm, size=0.5, colour="black")+
  labs(title = "Year of born and duration of diabetes in terms of gender")
```

![](Image/yearandduration.jpeg)

```R
ggplot(Table1_Basic_information, aes( `Duration of Diabetes/yrs`, `Does the patient use any tobacco?`))+ 
  geom_jitter( aes(col=Gender))+ 
  facet_wrap(.~Employment)+
  labs(title = "Tobacco use and duration of type 1 diabetes")
```

![](Image/tobaccoandduration.jpeg)

```R
ggplot(Table1_Basic_information, aes(`Any severe hypoglycemias during the last year?`, `Does the patient administer insulin by injection or pump?`))+
  geom_jitter( aes(colour=Gender))+ 
  facet_wrap(.~`Has the patient CGM  or FGM equipment?`)+
  labs(title = "Sever hypoglycemias and the methods of insulin injection")
  ```
  
  ![](Image/methods.jpeg)
  
### Food Intake and biomarkers
```R
ggplot(Excl_Quality, aes (`HbA1c (mmol/mol)`, `Wholegrain total (g)`,)) + 
  geom_boxplot(aes(group=1))+
  geom_point( aes(colour= BMI))+ 
  facet_wrap(.~Gender)+ 
  labs(title = "HbA1c VS whole grain intake in terms of BMI and Gender")
```

  ![](Image/hba1cwholegrain.jpeg)
  
  ```R
ggplot(Excl_Quality, aes (`Mean sensor glucose 1 (mmol/l)`, `Wholegrain total (g)`))+
  geom_boxplot(  aes(group=1))+
  geom_point( aes(colour= BMI), size=2)+
  facet_wrap(.~Gender)+ 
  labs(title = "Blood gluocse level 28 days VS whole grain intake in terms of BMI and gender")
```

![](Image/bgandwholegrain.jpeg)

 ```R
TimeandHbformula= y~poly(x,2)
ggplot(Excl_Quality, aes(`HbA1c (mmol/mol)`, `Time in range 1 (%)`))+ 
  geom_point()+ geom_abline( linetype=3)+
  geom_smooth( formula = y~poly(x,2), method="lm", se=T, level=0.95)+
  stat_poly_eq(formula = TimeandHbformula, parse=T, size=3, colour="blue")+
  labs(title = "Time in Range for 28 days and HbA1c")
```
![](Image/tirandhba1c.jpeg)

 ```R
ggplot(Excl_Quality, aes(`HbA1c (mmol/mol)`, `Sugar/g`))+
  geom_boxplot( aes(group=1))+geom_point( aes(colour= BMI))+
  facet_wrap(.~Gender)+
  labs(title = "HbA1c and simple sugar intake based on BMI and gender")
```
![](Image/sugarandhba1c.jpeg)

## Statistical Analysis (28 days result from CGM period 1 to evaluate the quality of carbs)
### Methods of statistical analysis
Multiple Linear Regression (95% confidence level)
 
Dependent Variables (y): HbA1c/ Mean blood glucose level, Coefficient of variations of mean blood glucose level, Standard Deviation of mean blood glucose level.

Independent Variables (x): Sugar/g, wholegrain total (g), energy (kcal), Duration of diabetes/years, gender, BMI as correction

```R
Excl_Quality_Male<- Excl_Quality[Excl_Quality$Gender=="Male",]
Excl_Quality_Female<- Excl_Quality[Excl_Quality$Gender=="Female",]
```

This is the basic linear regression model for hba1c and selected variables
```R
lmhb<-lm( `HbA1c (mmol/mol)`~ `Duration of Diabetes/yrs`+
            Gender+ BMI+`Sugar/g`+ `Wholegrain total (g)`+`Energy (kcal)`,
          data= Excl_Quality)
summary(lmhb)

vif (lmhb)
confint(lmhb)
coef(lmhb)
predict(lmhb)

plot(lmhb,1)
plot(lmhb,2) 
plot(lmhb,3)
plot(lmhb,4)
```

![](Image/lmhbres.jpeg)


This part look at is there any gender differences

 ```R
lmhbfemale<-lm( `HbA1c (mmol/mol)`~  `Sugar/g`+BMI+ `Energy (kcal)` , data= Excl_Quality_Female)
summary(lmhbfemale)

lmhbmale<-lm( `HbA1c (mmol/mol)`~ `Sugar/g`
              +BMI+ `Energy (kcal)` , data= Excl_Quality_Male)
summary(lmhbmale)
```

Linear Regression model for time in range (%) 28 days and selected variables

```R
lmtime<-lm( `Time in range 1 (%)`~ `Duration of Diabetes/yrs`+
              Gender+ BMI+`Sugar/g`+ `Wholegrain total (g)`+`Energy (kcal)`, data= Excl_Quality)
summary(lmtime)

vif (lmtime)
confint(lmtime)
coef(lmtime)
predict(lmtime)

plot(lmtime,1)
plot(lmtime,2) 
plot(lmtime,3)
plot(lmtime,4)
```

![](Image/lmtimeqq.jpeg)


I also look at other independent factors (y) such as means sensor gluocse 28 days and its SD and CV with the same format


### We mainly focus on 28 days result but for the comprehensive purpose we also look at 14 days result. There is no significant at 14 days result. 

# Discussion 

The Diabetes and Nutrition Study Group (DNSG) of the European Association for the Study of Diabetes (EASD) recommends carbohydrate intake to be ranging from 45% to 60% of total energy intake. It also recommends including more wholegrain alternatives, vegetables and legumes in the diet as a part of T1DM management. The food habit of our participants really reflected most of the recommendations. Patients with T1DM are suggested to limit some food items to keep a stable blood glucose level. Quality of carbohydrate also plays an important role in good diabetic management. From the result we can see most of the participants have avoided sweetened beverages and high energy density foods i.e., sweets, chocolates, and bakery items. Despite of all the recommendations there is still a lot of controversies about the low and high carbohydrate containing diet, usefulness of low glycemic index (GI) foods and following a special meal plan. So far, no study has provided clear evidence to prove the effectivity of any of these. As whatever the food content is, it directly affects the blood glucose level. 

Carbohydrates has three types of subcategories which are sugar, starch, and fiber. Sugar can be found in candy, desserts, processed foods etc. Another main resource of sugar come from natural products such as milk and fruits. Added sugar play an important role in the management of diabetes. The current guidelines recommend reducing the intake of added sugar as much as possible. Starch is a form of complex carbohydrates where our body need to breakdown starch into the energy form to support daily activities. Common starches are bread, potato, and corns. Fiber is another form of complex carbohydrates. Fiber could help prevent stomach or intestinal problems. They may also help to lower cholesterol and blood glucose level. Nuts, seeds, beans, and whole grains are good resources of fiber. 
High fiber diet is an important component of diabetes management, resulting in improvements in measure of glycemic variation, blood lipids, body weight and inflammation. Another study suggested to increase dietary fiber from 15 g/day to 35g/day for diabetic patients, and also suggested that increase fiber intake by replace refined grains products with wholegrain (21). Other studies suggested that risk reduction of non-communicable diseases association with a range of critical outcomes was great when daily intake of fiber was between 25g to 29g. However, in our study, we showed a negative correlation (estimation= -0.16, p= 0.006) between wholegrain intake and time in range. This could be explained by the fact that wholegrain is a long-term dietary goal and we do not have follow-up yet to investigate the long-term effects.

Findings from epidemiological studies suggest that the benefits of wholegrain intake on human health are related to improved body weight, insulin sensitivity, lipid metabolism, inflammation and antioxidant activity. Among the clinical trials with positive findings on insulin metabolism, a study in hyper insulinemic overweight individuals showed that a 6-week period with a wholegrain rich diet, composed of 80% wheat, reduced fasting plasma insulin levels and improved insulin resistance as compared with a refined cereal diet. Another two other studies reported that high fiber bread consumption compared with refined wheat bread significantly increased the first phase of insulin secretion, suggesting an improved beta cell function reduced fasting insulin levels and 24-h urinary C-peptide excretion.

In another aspect of carbohydrates, we also look at the effects of glycemic index (GI) and glycemic variations (GV). The glycemic index is a ranking of carbohydrates on a scale from 0 to 100 according to the extent to which they raise blood glucose levels after eating (27). Foods with a high GI are those which are rapidly digested, absorbed, and metabolized and result in marked fluctuations in blood glucose levels. Low GI carbohydrates – the ones that produce smaller fluctuations in your blood glucose and insulin levels – is one of the secrets to long-term health, reducing your risk of type 2 diabetes and heart disease. We designed three GI categories based on the agreements. High GI group refers to FFQ listed food has over 70 while medium GI group has a range of 56~69. Low GI food group is those food had lower than 55.  We did not find any correlation between GI and glycemic variation within our study no matter gender. In a similar study related to GI and type 2 diabetes, they found that major non-dietary factors were unable to explain the strength of association between T2DM and GI. Another meta-analysis of intervention studies finds that the relations with GI and dietary fiber is independent of one another, and additive, for the reversal of T2DM risk factors: fasting blood glucose and glycated protein.


