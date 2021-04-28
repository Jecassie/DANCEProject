attach (Excl_activecgm2)
Excl_activecgm2$Gender= factor( Excl_activecgm2$Gender, levels=c(0,1), 
                            labels = c("Male", "Female"))
Excl_activecgm2_Male<- Excl_activecgm2[Excl_activecgm2$Gender=="Male",]
Excl_activecgm2_Female<- Excl_activecgm2[Excl_activecgm2$Gender=="Female",]

ggplot( Excl_activecgm2, aes(`Active CGM time 2 (%)`))+geom_histogram()


allsutset14<-regsubsets( `HbA1c (mmol/mol)`~ Gender+ `Duration of Diabetes/yrs`+
                        `Energy (kcal)`+`Sugar/g`+
                        `Fat(g)`+ `Protein (g)`+ `Fibre (g)`+ `Wholegrain total (g)` , nbest = 3, data = Excl_activecgm2 )
plot(allsutset14, scale = "adjr2")


###HbA1c VS whole grain intake in terms of BMI and Gender
ggplot(Excl_activecgm2, aes (`HbA1c (mmol/mol)`, `Wholegrain total (g)`,)) + 
  geom_boxplot(aes(group=1))+
  geom_point( aes(colour= BMI))+ 
  facet_wrap(.~Gender)+ 
  labs(title = "HbA1c VS whole grain intake in terms of BMI and Gender")

###HbA1c VS sugar intake in terms of BMI and Gender
ggplot(Excl_activecgm2, aes (`HbA1c (mmol/mol)`, `Sugar/g`)) + 
  geom_boxplot(aes(group=1))+
  geom_point( aes(colour= BMI))+ 
  facet_wrap(.~Gender)+ 
  labs(title = "HbA1c VS sugar intake in terms of BMI and Gender")


###HbA1c-sig
lm2hb<-lm( `HbA1c (mmol/mol)`~`Duration of Diabetes/yrs`+ Gender+BMI+ `Sugar/g`+`Wholegrain total (g)`+
             `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2hb)

lm2hbfemale<-lm( `HbA1c (mmol/mol)`~ 
                  `Wholegrain total (g)`
                +BMI+ `Energy (kcal)` , data= Excl_activecgm2_Female)
summary(lm2hbfemale)

lm2hbmale<-lm( `HbA1c (mmol/mol)`~ `Wholegrain total (g)`
              +BMI+ `Energy (kcal)` , data= Excl_activecgm2_Male)
summary(lm2hbmale)

###time in range-sig
lm2time<-lm( `Time in Range 2 (%)`~`Duration of Diabetes/yrs`+ Gender+BMI+ `Sugar/g`+`Wholegrain total (g)`+
             `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2time)

lm2timefemale<-lm( `Time in Range 2 (%)`~ 
                   `Wholegrain total (g)`
                 +BMI+ `Energy (kcal)` , data= Excl_activecgm2_Female)
summary(lm2timefemale)

lm2timemale<-lm( `Time in Range 2 (%)`~ `Wholegrain total (g)`
               +BMI+ `Energy (kcal)` , data= Excl_activecgm2_Male)
summary(lm2timemale)

###coefficient
lm2coef<-lm( `Coefficient of variation 2 (%) (SD*100/mean sensor glucose)`~`Duration of Diabetes/yrs`+ 
               Gender+BMI+ `Sugar/g`+`Wholegrain total (g)`+
               `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2coef)

###SD
lm2sd<-lm( `Standard deviation of mean sensor glucose 2 (mmol/l)`~`Duration of Diabetes/yrs`+ 
             Gender+BMI+ `Sugar/g`+`Wholegrain total (g)`+
               `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2sd)

lmsdfemale<-lm( `Standard deviation of mean sensor glucose 2 (mmol/l)`~ 
                  `Fibre (g)`
                +BMI+ `Energy (kcal)` , data= Excl_activecgm2_Female)
summary(lmsdfemale)

lmsdmale<-lm( `Standard deviation of mean sensor glucose 2 (mmol/l)`~ `Fibre (g)`
              +BMI+ `Energy (kcal)` , data= Excl_activecgm2_Male)
summary(lmsdmale)


###Mean glucose level 14 days-sig
lm2mean<-lm( `Mean sensor glucose 2 (mmol/l)`~`Duration of Diabetes/yrs`+ Gender+BMI+
               `Sugar/g`+`Wholegrain total (g)`+
             `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2mean)

lm2meanfemale<-lm( `Mean sensor glucose 2 (mmol/l)`~ 
                     `Wholegrain total (g)`
                   +BMI+ `Energy (kcal)` , data= Excl_activecgm2_Female)
summary(lm2meanfemale)

lm2meanmale<-lm( `Mean sensor glucose 2 (mmol/l)`~ `Wholegrain total (g)`
                 +BMI+ `Energy (kcal)` , data= Excl_activecgm2_Male)
summary(lm2meanmale)


###Hb and GI----NOT APPLICABLE TO DEFINATION OF HB-sig
lm2hbhighgi<-lm( `HbA1c (mmol/mol)`~`Duration of Diabetes/yrs`+
                   Gender+BMI+ `Sugar/g`+ `High GI carbs/g`+
             `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2hbhighgi)

lm2hbmediumgi<-lm( `HbA1c (mmol/mol)`~`Duration of Diabetes/yrs`+ 
                     Gender+BMI+ `Sugar/g`+ `Medium GI carbs/g`+
                   `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2hbmediumgi)

lm2hblowgi<-lm( `HbA1c (mmol/mol)`~`Duration of Diabetes/yrs`+ 
                  Gender+BMI+ `Sugar/g`+ `Low GI carbs/g`+
                     `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2hblowgi)

###Time in range and GI-sig
lm2timehighgi<-lm( `Time in Range 2 (%)`~`Duration of Diabetes/yrs`+
                     Gender+BMI+ `Sugar/g`+ `High GI carbs/g`+
               `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2timehighgi)

lm2timemediumgi<-lm( `Time in Range 2 (%)`~`Duration of Diabetes/yrs`+
                       Gender+BMI+ `Sugar/g`+ `Medium GI carbs/g`+
                     `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2timemediumgi)

lm2timelowgi<-lm( `Time in Range 2 (%)`~`Duration of Diabetes/yrs`+ 
                    Gender+BMI+ `Sugar/g`+ `Low GI carbs/g`+
                       `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2timelowgi)

###Coefficient and GI
lm2coefhighgi<-lm( `Coefficient of variation 2 (%) (SD*100/mean sensor glucose)`~`Duration of Diabetes/yrs`+
                     Gender+BMI+ `Sugar/g`+ `High GI carbs/g`+
                     `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2coefhighgi)

lm2coefmediumgi<-lm( `Coefficient of variation 2 (%) (SD*100/mean sensor glucose)`~
                       `Duration of Diabetes/yrs`+ Gender+BMI+ `Sugar/g`+ `Medium GI carbs/g`+
                     `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2coefmediumgi)

lm2coeflowgi<-lm( `Coefficient of variation 2 (%) (SD*100/mean sensor glucose)`~
                    `Duration of Diabetes/yrs`+ Gender+BMI+ `Sugar/g`+ `Low GI carbs/g`+
                       `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2coeflowgi)

###SD and GI
lm2sdhighgi<-lm( `Standard deviation of mean sensor glucose 2 (mmol/l)`~
                   `Duration of Diabetes/yrs`+ Gender+BMI+ `Sugar/g`+ `High GI carbs/g`+
                     `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2sdhighgi)

lm2sdmediumgi<-lm( `Standard deviation of mean sensor glucose 2 (mmol/l)`~
                     `Duration of Diabetes/yrs`+ Gender+BMI+ `Sugar/g`+ `Medium GI carbs/g`+
                    `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2sdmediumgi)

lm2sdlowgi<-lm( `Standard deviation of mean sensor glucose 2 (mmol/l)`~
                  `Duration of Diabetes/yrs`+ Gender+BMI+ `Sugar/g`+ `Low GI carbs/g`+
                      `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2sdlowgi)

###mean glucose 14 days and GI
lm2meanhighgi<-lm( `Mean sensor glucose 2 (mmol/l)`~`Duration of Diabetes/yrs`+
                     Gender+BMI+ `Sugar/g`+ `High GI carbs/g`+
                   `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2meanhighgi)

lm2meanmediumgi<-lm( `Mean sensor glucose 2 (mmol/l)`~`Duration of Diabetes/yrs`+
                       Gender+BMI+ `Sugar/g`+ `Medium GI carbs/g`+
                     `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2meanmediumgi)

lm2meanlowgi<-lm( `Mean sensor glucose 2 (mmol/l)`~`Duration of Diabetes/yrs`+ 
                    Gender+BMI+ `Sugar/g`+ `Low GI carbs/g`+
                       `Energy (kcal)`, data= Excl_activecgm2)
summary(lm2meanlowgi)


str(Excl_activecgm2)

exclheat<- data.frame( Age
        , Gender, `Duration of Diabetes/yrs`,
                    BMI, `HbA1c (mmol/mol)`,
                 `Glucose management indicator 2 (mmol/mol)`)

excl.long <- pivot_longer(data = exclheat, 
                          cols = -c(1:2), 
                          names_to = "Class", 
                          values_to = "Abundance")
head( excl.long)

ggplot(data = excl.long, mapping = aes(x = Gender,
                                       y = Class,
                                       fill = Abundance)) +
  geom_tile() +
  xlab(label = "Gender")
