attach (Excl_Quality)
Excl_Quality$Gender= factor(Excl_Quality$Gender, levels=c(0,1), 
                            labels = c("Male", "Female"))

Excl_Quality_Male<- Excl_Quality[Excl_Quality$Gender=="Male",]
Excl_Quality_Female<- Excl_Quality[Excl_Quality$Gender=="Female",]

describeBy(Excl_Quality$`Wholegrain total (g)`, group=Excl_Quality$Gender)

shapiro.test(Excl_Quality$`P-Creatinine (mikromol/L)`)
shapiro.test(Excl_Quality$`Active CGM time 1 (%)`)
shapiro.test(Excl_Quality$`CGM period 1 (days)`)
hist(Excl_Quality$`P-Creatinine (mikromol/L)`)
qqplot(Excl_Quality$`P-Creatinine (mikromol/L)`, Excl_Quality$`HbA1c (mmol/mol)`)

###graph
theme_update(plot.title=element_text(hjust = 0.5))
####Histogram of active CGM
ggplot(Excl_Quality, aes(`Active CGM time 1 (%)`))+geom_boxplot( aes(colour= Gender), vertical=TRUE)
###HbA1c VS whole grain intake in terms of BMI and Gender
ggplot(Excl_Quality, aes (`HbA1c (mmol/mol)`, `Wholegrain total (g)`,)) + 
  geom_boxplot(aes(group=1))+
  geom_point( aes(colour= BMI))+ 
  facet_wrap(.~Gender)+ 
  labs(title = "HbA1c VS whole grain intake in terms of BMI and Gender")
###Blood gluocse level VS whole grain intake in terms of BMI and gender
ggplot(Excl_Quality, aes (`Mean sensor glucose 1 (mmol/l)`, `Wholegrain total (g)`))+
  geom_boxplot(  aes(group=1))+
  geom_point( aes(colour= BMI), size=2)+
  facet_wrap(.~Gender)+ 
  labs(title = "Blood gluocse level VS whole grain intake in terms of BMI and gender")

ggplot(Excl_Quality, aes(`HbA1c (mmol/mol)`, `Sugar/g`))+
  geom_point( aes(colour=BMI))+stat_summary(geom= "errorbar", fun.min = min, fun.max = max)


ggplot(Excl_Quality, aes (BMI, `HbA1c (mmol/mol)`))+
  geom_point( aes(colour= Gender))

ggplot(Excl_Quality, aes(`HbA1c (mmol/mol)`, `Sugar/g`))+
  geom_point()+geom_abline()

ggplot(Excl_Quality, aes(`HbA1c (mmol/mol)`, `Fibre (g)`))+
  geom_point( aes(colour=BMI))+geom_abline()+
  facet_wrap(.~Gender)+
  labs(title = "Fiber intake and HbA1c Level")

TimeandHbformula= y~poly(x,2)
ggplot(Excl_Quality, aes(`HbA1c (mmol/mol)`, `Time in range 1 (%)`))+ 
  geom_point()+ geom_abline( linetype=3)+
  geom_smooth( formula = y~poly(x,2), method="lm", se=T, level=0.95)+
  stat_poly_eq(formula = TimeandHbformula, parse=T, size=3, colour="blue")+
  labs(title = "Time in Range and HbA1c")

####STAT
describe(Excl_Quality$`HbA1c (mmol/mol)`)
describe(Excl_Quality$`Carbohydrate (g)`)

###Multiple linear regression for hba1c-sig
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

lmhbfemale<-lm( `HbA1c (mmol/mol)`~  `Sugar/g`+BMI+ `Energy (kcal)` , data= Excl_Quality_Female)
summary(lmhbfemale)

lmhbmale<-lm( `HbA1c (mmol/mol)`~ `Sugar/g`
              +BMI+ `Energy (kcal)` , data= Excl_Quality_Male)
summary(lmhbmale)

###Multiple linear regression for time in range 1
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

###Multiple linear regression for Blood glucose level 28 days- sig
tlmbg<-lm( `Mean sensor glucose 1 (mmol/l)`~ `Duration of Diabetes/yrs`+
             Gender+ BMI+`Sugar/g`+ `Wholegrain total (g)`+`Energy (kcal)`
           ,data= Excl_Quality)
summary(tlmbg)

vif (tlmbg)
confint(tlmbg)
coef(tlmbg)
predict(tlmbg)

plot(tlmbg,1)
plot(tlmbg,2)
plot(tlmbg,3)
plot(tlmbg,4)

lmbgfemale<-lm( `Mean sensor glucose 1 (mmol/l)`~ 
                 `Sugar/g`
                +BMI+ `Energy (kcal)` , data= Excl_Quality_Female)
summary(lmbgfemale)

lmbgmale<-lm( `Mean sensor glucose 1 (mmol/l)`~ `Sugar/g`
              +BMI+ `Energy (kcal)` , data= Excl_Quality_Male)
summary(lmbgmale)

###Coefficient of mean glucose sensor 
tlmcoef<-lm(  `Coefficient of variation 1 (%) (SD*100/mean sensor glucose)`~ `Duration of Diabetes/yrs`+
                Gender+ BMI+`Sugar/g`+ `Wholegrain total (g)`+`Energy (kcal)`
           ,data= Excl_Quality)
summary(tlmcoef)

vif (tlmcoef)
confint(tlmcoef)
coef(tlmcoef)
predict(tlmcoef)

plot(tlmcoef,1)
plot(tlmcoef,2)
plot(tlmcoef,3)
plot(tlmcoef,4)

###SD of mean glucose sensor-sig
tlmsd<-lm( `Standard deviation of mean sensor glucose 1 (mmol/l)`~ `Duration of Diabetes/yrs`+
             Gender+ 
             BMI+`Sugar/g`+ `Wholegrain total (g)`+
             `Energy (kcal)`,data= Excl_Quality)
summary(tlmsd)

vif (tlmsd)
confint(tlmsd)
coef(tlmsd)
predict(tlmsd)

plot(tlmsd,1)
plot(tlmsd,2)
plot(tlmsd,3)
plot(tlmsd,4)

lmsdfemale<-lm( `Standard deviation of mean sensor glucose 1 (mmol/l)`~ 
               `Fibre (g)`
                +BMI+ `Energy (kcal)` , data= Excl_Quality_Female)
summary(lmsdfemale)

lmsdmale<-lm( `Standard deviation of mean sensor glucose 1 (mmol/l)`~ `Fibre (g)`
              +BMI+ `Energy (kcal)` , data= Excl_Quality_Male)
summary(lmsdmale)

###High GI and hba1c
lmhbhighGI<-lm( `HbA1c (mmol/mol)`~ `Duration of Diabetes/yrs`+
                  Gender+ BMI+`Sugar/g`+ `High GI carbs/g`+`Energy (kcal)`,
                data= Excl_Quality)
summary(lmhbhighGI)

vif (lmhbhighGI)
confint(lmhbhighGI)
coef(lmhbhighGI)
predict(lmhbhighGI)

plot(lmhbhighGI,1)
plot(lmhbhighGI,2)
plot(lmhbhighGI,3)
plot(lmhbhighGI,4)


####Medium GI and hb1ac
lmhbhmediumGI<-lm( `HbA1c (mmol/mol)`~ `Duration of Diabetes/yrs`+
                     Gender+ BMI+`Sugar/g`+ `Medium GI carbs/g`+`Energy (kcal)`,
                   data= Excl_Quality)
summary(lmhbhmediumGI)

vif (lmhbhmediumGI)
confint(lmhbhmediumGI)
coef(lmhbhmediumGI)
predict(lmhbhmediumGI)

plot(lmhbhmediumGI,1)
plot(lmhbhmediumGI,2)
plot(lmhbhmediumGI,3)
plot(lmhbhmediumGI,4)

###Low GI and hba1c
lmhbhlowGI<-lm( `HbA1c (mmol/mol)`~ `Duration of Diabetes/yrs`+
                  Gender+ BMI+`Sugar/g`+ `Low GI carbs/g`+`Energy (kcal)`,
                data= Excl_Quality)
summary(lmhbhlowGI)

vif (lmhbhlowGI)
confint(lmhbhlowGI)
coef(lmhbhlowGI)
predict(lmhbhlowGI)

plot(lmhbhlowGI,1)
plot(lmhbhlowGI,2)
plot(lmhbhlowGI,3)
plot(lmhbhlowGI,4)

###High GI and time in range
lmtimehighGI<-lm( `Time in range 1 (%)`~ `Duration of Diabetes/yrs`+
                    Gender+ BMI+`Sugar/g`+ `High GI carbs/g`+`Energy (kcal)`,
                  data= Excl_Quality)
summary(lmtimehighGI)

vif (lmtimehighGI)
confint(lmtimehighGI)
coef(lmtimehighGI)
predict(lmtimehighGI)

plot(lmtimehighGI,1)
plot(lmtimehighGI,2)
plot(lmtimehighGI,3)
plot(lmtimehighGI,4)


###Medium GI and time in range
lmtimemediumGI<-lm( `Time in range 1 (%)`~ `Duration of Diabetes/yrs`+
                      Gender+ BMI+`Sugar/g`+ `Medium GI carbs/g`+`Energy (kcal)`,
                    data= Excl_Quality)
summary(lmtimemediumGI)

vif (lmtimemediumGI)
confint(lmtimemediumGI)
coef(lmtimemediumGI)
predict(lmtimemediumGI)

plot(lmtimemediumGI,1)
plot(lmtimemediumGI,2)
plot(lmtimemediumGI,3)
plot(lmtimemediumGI,4)

###Low GI and time in range
lmtimelowGI<-lm( `Time in range 1 (%)`~ `Duration of Diabetes/yrs`+
                   Gender+ BMI+`Sugar/g`+ `Low GI carbs/g`+`Energy (kcal)`
                 , data= Excl_Quality)
summary(lmtimelowGI)

vif (lmtimelowGI)
confint(lmtimelowGI)
coef(lmtimelowGI)
predict(lmtimelowGI)

plot(lmtimelowGI,1)
plot(lmtimelowGI,2)
plot(lmtimelowGI,3)
plot(lmtimelowGI,4)

lmtimelowgifemale<-lm( `Time in range 1 (%)`~ 
                 `Low GI carbs/g`
                +BMI+ `Energy (kcal)` , data= Excl_Quality_Female)
summary(lmtimelowgifemale)

lmtimelowgimale<-lm(  `Time in range 1 (%)`~ `Low GI carbs/g`
              +BMI+ `Energy (kcal)` , data= Excl_Quality_Male)
summary(lmtimelowgimale)

###Coefficient of mean glucose 1 and High GI
tlmcoefhighgi<-lm(  `Coefficient of variation 1 (%) (SD*100/mean sensor glucose)`~ 
                      `Duration of Diabetes/yrs`+
                      Gender+ BMI+`Sugar/g`+ `High GI carbs/g`+`Energy (kcal)`
                    ,data= Excl_Quality)
summary(tlmcoefhighgi)

###Coefficient of mean glucose 1 and medium GI
tlmcoefmediumgi<-lm(  `Coefficient of variation 1 (%) (SD*100/mean sensor glucose)`~ 
                        `Duration of Diabetes/yrs`+
                        Gender+ BMI+`Sugar/g`+ `Medium GI carbs/g`+`Energy (kcal)`
                      ,data= Excl_Quality)
summary(tlmcoefmediumgi)

###Coefficient of mean glucose 1 and low GI
tlmcoeflowgi<-lm(  `Coefficient of variation 1 (%) (SD*100/mean sensor glucose)`~ 
                     `Duration of Diabetes/yrs`+
                     Gender+ BMI+`Sugar/g`+ `Low GI carbs/g`+`Energy (kcal)`
                   ,data= Excl_Quality)
summary(tlmcoeflowgi)

###SD of mean glucose 1 and High GI
tlmsdhighgi<-lm(`Standard deviation of mean sensor glucose 1 (mmol/l)`~ 
                  `Duration of Diabetes/yrs`+
                  Gender+ BMI+`Sugar/g`+ `High GI carbs/g`+`Energy (kcal)`
                ,data= Excl_Quality)
summary(tlmsdhighgi)

###SD of mean glucose 1 and medium GI
tlmsdmediumgi<-lm(`Standard deviation of mean sensor glucose 1 (mmol/l)`~ 
                    `Duration of Diabetes/yrs`+
                    Gender+ BMI+`Sugar/g`+ `Medium GI carbs/g`+`Energy (kcal)`
                  ,data= Excl_Quality)
summary(tlmsdmediumgi)

###SD of mean glucose 1 and low GI
tlmsdlowgi<-lm(`Standard deviation of mean sensor glucose 1 (mmol/l)`~ 
                 `Duration of Diabetes/yrs`+
                 Gender+ BMI+`Sugar/g`+ `Low GI carbs/g`+`Energy (kcal)`
               ,data= Excl_Quality)
summary(tlmsdlowgi)

###all subset regression
model.hb<-regsubsets( `HbA1c (mmol/mol)`~ Gender+ `Duration of Diabetes/yrs`+
                          `Energy (kcal)`+`Sugar/g`+
                          `Fat(g)`+ `Protein (g)`+ `Fibre (g)`+ `Wholegrain total (g)`, nbest = 3, data = Excl_Quality )
plot(model.hb, scale = "adjr2")

##Comparision
anova(lmtimehighGI, lmtimelowGI)
AIC(lmtimelowGI, lmtimemediumGI)

ggplot( Excl_Quality, aes(`Active CGM time 1 (%)`))+geom_histogram(bins = 35)





