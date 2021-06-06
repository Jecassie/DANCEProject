####Basic
### 55 population, excluded due to less active CGM time
par(mfrow=c(2,2))
attach(Table1_Basic_information)
theme_update(plot.title=element_text(hjust = 0.5))
Table1_Basic_information$Gender= 
  factor(Table1_Basic_information$Gender, levels=c(0,1), labels = c("Male", "Female"))
Table1_Basic_information$`Does the patient use any tobacco?`= 
  factor(Table1_Basic_information$`Does the patient use any tobacco?`, levels=c(0,1), labels = c("No", "Yes"))
Table1_Basic_information$`Highest level of education`= 
  factor(Table1_Basic_information$`Highest level of education`, levels=c(0,1,2,3), labels = c("Elementary school", "College","University","Others"))
Table1_Basic_information$Employment= 
  factor(Table1_Basic_information$Employment, levels=c(0,1,2,3,4,5), labels = c("Employee", "Retired","On leave","Student","Unemployed","Other"))
Table1_Basic_information$`Marital Status`= 
  factor(Table1_Basic_information$`Marital Status`, levels=
           c(0,1,2,3,4,5,6), labels = c("Single", "Married","Living with partner","Living Apart","Divorced","Widowed","Other"))
Table1_Basic_information$`Has the patient CGM  or FGM equipment?`= 
  factor(Table1_Basic_information$`Has the patient CGM  or FGM equipment?`, levels=c(0,1,2), labels = c("Has an own CGM", "Has an own FGM", "Will receive a CGM / FGM after the screening visit"))
Table1_Basic_information$`Is the patient taking any other medication than insulin?`=
  factor(Table1_Basic_information$`Is the patient taking any other medication than insulin?`, levels=c(0,1), labels = c("No", "Yes"))
Table1_Basic_information$`Does the patient administer insulin by injection or pump?`=
  factor(Table1_Basic_information$`Does the patient administer insulin by injection or pump?`, levels=c(0,1), labels = c("Injection", "Pump"))
Table1_Basic_information$`Any non serious hypoglycemias during the last year?`= 
  factor(Table1_Basic_information$`Any non serious hypoglycemias during the last year?`, levels=c(0,1), labels = c("No", "Yes"))
Table1_Basic_information$`Any severe hypoglycemias during the last year?`= 
  factor(Table1_Basic_information$`Any severe hypoglycemias during the last year?`, levels=c(0,1), labels = c("No", "Yes"))
Table1_Basic_information$`Has the patient been diagnosed with retinopathy?`= 
  factor(Table1_Basic_information$`Has the patient been diagnosed with retinopathy?`, levels=c(0,1), labels = c("No", "Yes"))
Table1_Basic_information$`Has the patient any ongoing medical history except for diabetes mellitus type 1 and retinopathy?`= 
  factor(Table1_Basic_information$`Has the patient any ongoing medical history except for diabetes mellitus type 1 and retinopathy?`, levels=c(0,1), labels = c("No", "Yes"))


###start of the graph
Summary(Table1_Basic_information)
###Distribution of duration of diabetes and Gender
ggplot(Table1_Basic_information, aes( `Year of Born`, `Duration of Diabetes/yrs`))+
  geom_jitter( aes(colour= BMI))+
  facet_wrap(.~Gender)+
  labs(title = "Year of born and duration of diabetes in terms of gender")
####Distribution of education and employment based on marital status
ggplot(Table1_Basic_information, aes(`Highest level of education`, Employment))+ 
  geom_point( aes(col= `Marital Status`), size=3)+
  facet_wrap(.~Gender)+
  labs(title = "Level of Education and Employment Status based on Gender")
###Distribution of tabocco use in terms of gender
ggplot(Table1_Basic_information, aes( `Duration of Diabetes/yrs`, `Does the patient use any tobacco?`))+ 
  geom_jitter( aes(col=Gender))+ 
  facet_wrap(.~Employment)+
  labs(title = "Tobacco use and duration of type 1 diabetes")
###Relatives Diabets
ggplot(Table1_Basic_information, aes(`Have any of the relatives diabetes mellitus?`, BMI))+
  geom_jitter( aes(col=Gender))+ 
  facet_wrap(.~`Marital Status`)+
  labs(title = "Number of relatives have diabetes and BMI")
###Hypoglycemia
ggplot(Table1_Basic_information, aes(`Any severe hypoglycemias during the last year?`, `Does the patient administer insulin by injection or pump?`))+
  geom_jitter( aes(colour=Gender))+ 
  facet_wrap(.~`Has the patient CGM  or FGM equipment?`)+
  labs(title = "Sever hypoglycemias and the methods of insulin injection")
ggplot(Table1_Basic_information, aes( `Any non serious hypoglycemias during the last year?`, `Has the patient been diagnosed with retinopathy?`))+
  geom_jitter( aes(colour=Gender))+
  facet_wrap(Table1_Basic_information$`Is the patient taking any other medication than insulin?`~. )+
  labs(title = "Retinopathy VS Non serious hypoglycemia based on medications rather than insulin")
ggplot(Table1_Basic_information, aes( `Total dose of basal insulin /day`, BMI))+
  geom_jitter( aes(colour=Gender))+
  labs(title = "Total dose of basal insulin/day VS BMI")+
  geom_abline()
ggplot(Table1_Basic_information, aes( `Total dose of mealtime insulin /day`, BMI))+
  geom_jitter( aes(colour=Gender))+
  labs(title = "Total dose of mealtime insulin/day VS BMI")+
  geom_abline()



table1(~ Gender + `Duration of Diabetes/yrs` + `Has the patient been diagnosed with retinopathy?` + `Has the patient CGM  or FGM equipment?`+ `Have any of the relatives diabetes mellitus?` + `Highest level of education` +
         Employment + `Does the patient use any tobacco?`+ `Any severe hypoglycemias during the last year?`, data= Table1_Basic_information)





       