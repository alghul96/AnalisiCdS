library(epicalc)
library(foreign)

# load dataset
Dataset <- read.dta("renal_failure.dta")


use(Dataset)
# descriptive statistics
des() #getting a descriptive information of the variables inside the dataset


# transform ren.fail  and  diabetes  as qualitative variables 
ren_fail <- factor (ren_fail, labels = c("ren.fail absent ", "ren.fail present"))
diabetes <- factor (diabetes, labels = c("diabetes absent", "diabetes present"))

# estimate total incidence rate
table0.pyears <-sum(p_years)
table0.ric <- sum(cv_hosp) 
table0.inc1000 <- table0.ric/table0.pyears*1000; table0.inc1000

# we can estimate this rate also from the following model
model0<-glm(cv_hosp~1, offset=log(p_years), family=poisson)

coeff <- t(t(coef(model0))); coeff

#Remember that 't(t(vector))' gives a single column matrix.
coeff.95ci <- cbind(coeff, t(confint(model0)))

#Note that 'confint(model)' provides a 95% confidence interval of the coefficients.
IDR.95ci <- round(exp(coeff.95ci),3)*1000;IDR.95ci



# estimate incidence rate for each level of  ren.fail
table1.pyears <-tapply(p_years, list( ren_fail), sum)
table1.ric <- tapply(cv_hosp, list(ren_fail), sum) 
table1.inc1000 <- table1.ric/table1.pyears*1000;table1.inc1000
#we observe about 25 events for the renal failur absent, while er observe
# 90 events while the renal failure is present

# We know make a model with renal failure as explanatory variable

############### UNIVARIATE MODEL with ren.fail as indipendent variable ################

model1<-glm(cv_hosp~ren_fail, offset=log(p_years), family=poisson)
summary(model1)
# The coefficient is positive, the model is statistically significant 
# so we can say that renal failure is a exp variable

# Giving a look to the estimates of the rates

#rate for renal failure absent
raterenfabs=exp(-3.66996)*1000

#rate for renal failure present
raterenfpres=exp(-3.66996+1.26903)*1000

raterenfabs
raterenfpres

# The estimates confirm what we've seen in the previos commands

idr.display(model1)

# 3.56 is the idr for when renal failur is present while the renal failure is 
# absent. So when the renal failure is present, the risk of cardiovascular 
# disease is 3.56 times higher that ehrn it is absent (confirmed by lrt with a low p-value)


# evaluate goodness of fit for the estimated model 
# comparing observed and predicted events 
poisgof(model1)

# We can see that this model is bad for our data, since if we look at p value hp is rejected)
# (deg of freedom given by number of obs - numb of parameters (2))

expected=predict.glm(model1,type="response")
expected=round(predict.glm(model1,type="response"),3)
# comparing the expected with the observed value!
cbind(expected,Dataset$cv_hosp)
# We see that the model, for same cases, predict very bad our obs variables

# some example (calculating directly the previous command)
#calculate the number of expected events for the first covariate pattern
101.1589041*exp(-3.66996)
# this is equal to expected[1]
# now for the row n. 70 where renal failure is present
# we multiply each time the p_years for the estimated rate
58.2219178*exp(-3.66996+1.26903)
# this is equal to expected[70]
  
coeff <- t(t(coef(model1))); coeff
#Remember that 't(t(vector))' gives a single column matrix.
coeff.95ci <- cbind(coeff, confint(model1))
#Note that 'confint(model)' provides a 95% confidence interval of the coefficients.
IDR.95ci <- round(exp(coeff.95ci),2)[-1,]
# we get the same result for what we've seen from the idr command

# estimate incidence rate for each level of  age.group
table.pyears <-tapply(p_years, list(age_group, ren_fail), sum)
table.ric <- tapply(cv_hosp, list(age_group, ren_fail), sum) 
table.inc1000 <-log( table.ric/table.pyears*1000); table.inc1000


plot.ts(table.inc1000, plot.type="single", xlab=" ", ylab="#/10000 person years", xaxt="n",
        col=c("black", "red"), lty=c(2,1), las=1)
points(rep(1:6,2), table.inc1000, pch=22, cex=table.pyears/sum(table.pyears)*20)
title(main = "Incidence by age and renal failure")
axis(side = 1, at = 1:6, labels = levels(age_group))
legend(3.2,150, legend=levels(ren_fail)[1:2], col=c("red", "black"), 
bg = "white", lty=c(2, 1))

# We can see that in the first group there's a difference, but if we look
# at the final groups it is more similar


#################### UNIVARIATE MODELS ######################

model2<-glm(cv_hosp~age_group, offset=log(p_years), family=poisson)
summary(model2)
idr.display(model2)

model3<-glm(cv_hosp~diabetes, offset=log(p_years), family=poisson)
summary(model3)
idr.display(model3)
# Not statistically significant

model4<-glm(cv_hosp~sex, offset=log(p_years), family=poisson)
summary(model4)
idr.display(model4)
# we see that being a female is protective related to male (infact it is less than one)

model5<-glm(cv_hosp~sys_hyper, offset=log(p_years), family=poisson)
summary(model5)
idr.display(model5)
# Not statistically significant


###### CONFOUNDERS

# now we evaluate the confounding effect of diabetes on ren.fail
modeldiab<-glm(cv_hosp~ren_fail+diabetes, offset=log(p_years), family=poisson)
summary(modeldiab)
idr.display(modeldiab)
# we can say that diabites on ren.fail is not a counfounder, to confirm
# we compare regression coeffcients obtained for ren.fail in the two models: modeldiab and model1

coef(model1)[2]
coef(modeldiab)[2]
# %  change in the regression coefficient for ren.fail
(coef(model1)[2]-coef(modeldiab)[2])/coef(model1)[2]*100

#Not significant


# we evaluate the confounding effects of age.group on ren.fail
modelage<-glm(cv_hosp~ren_fail+age_group, offset=log(p_years), family=poisson)
summary(modelage)
idr.display(modelage)

# We can say that age is a confounder (COMPARING THE CRUDE AND THE ADJUSTED)
# The crude is 3.56 while the adjsuted is 2.77

# we compare regression coeffcients obtained for ?ren.fail? in the two models: modeldiab and model 1
coef(model1)[2]
coef(modelage)[2]
# %  change in the regression coefficient for ren.fail
(coef(model1)[2]-coef(modelage)[2])/coef(model1)[2]*100


################ MODEL WITH THE AGE AND RENAL FAILURE ##############
# we will evaluate if age is an effect modifier
modelageint<-glm(cv_hosp~ren_fail*age_group, offset=log(p_years), family=poisson)
summary(modelageint)
idr.display(modelageint)

# Whit the asterix in the formula we can have bot the interaction that
# the main effect
# We can see from the output that all the interaziton test are not 
# statistically significant, but when we have several coefficient
# Lo si vede dal modello intero dove abbiamoi test di wald dove 
# non tutti sono significativi
# Se facciamo un test del rapporto di verosimiglianza (confronta quello con
# le interazioni senza quello con le interazioni)
# Alla fine di quest output possiamo vedere i p value P(LR-test) che ? quasi
# ad uno, mentre l'altro ? a 0.104. 
# Possiamo quindi dire che l'et? ? un confondente per renal failure
# ma non ? un modificatore di effetto. 


# complete multivariate model
modelall<-glm(cv_hosp~ren_fail+age_group+sex+diabetes+sys_hyper, offset=log(p_years), family=poisson)
summary(modelall)
idr.display(modelall)
# From this output we see that renal failur is still significant to the outcome
# while the age just on the higher classes, while the sex is still significant
# and at the end the sys hypertention is no significant. 
# From the idr.display we can see that the age is still a confounder 
# Look at the result file to see un confronto tra l'univariata e il multivartiato




#reduced model

modelred<-glm(cv_hosp~ren_fail+age_group+sex, offset=log(p_years), family=poisson)
summary(modelred)
idr.display(modelred)
# We see that the result are similar to the complete


anova(modelred,modelall,test="Chisq")
# So we see that the two model are not significally different, so we do not need those two variables

poisgof(modelred)
# This time from the poisgof we see that now we cannot reject the null hp and it means that our model is good as fuck


############ OBTAIN AN ESTIMATE OF THE NUMBER OF EXPECTED EVENTS ##############################

# we estimate the number of expected events for the following characteristics:
# ren.fail="ren.fail present", age.group="[50-59]",sex="m"
newdata <- as.data.frame(list(ren_fail="ren.fail present",
                age_group="[50-59]",sex="m",p_years=1000))
# Set di varibaili con valori predefiniti, ovvero maschi con renal failure, con et? compresa tra 50 e 59
# Visto che il nostro modello ? buono ci pu? dire il numero di eventi attesi (Il numero 1000 sarebbero il numero di persone esposte a rischio in un anno)

predict.glm(modelred,newdata=newdata,type="response")
# now we have that for a group of people of this characteristic we can estimate 30 events per year over 1000 subjects
# Ogni 100 soggetti che sono esposti al richio (con queste caratteristiche) in un anno allora sono attesi 3 casi di attacchi di cuore


# we calculate the total exposure time for this group of people
texp<-sum(Dataset$p_years[ren_fail=="ren.fail present"&age_group=="[50-59]"&sex=="m"])
# Texp va a sommare person year per tutti i record che soddisfano queste caratteristiche
# Sono stati esposti a rischio per un totale di 78 anni, per quanto concerne questo dataset
# Newdata ? come il dataset 

Dataset$p_years[ren_fail=="ren.fail present"&age_group=="[50-59]"&sex=="m"]
#[1] 36.112329 28.317808  8.558904  5.073973

texp
#[1] 78.06301

newdata <- as.data.frame(list(ren_fail="ren.fail present", age_group="[50-59]",sex="m",p_years=78.06301))


predict.glm(modelred,newdata=newdata,type="response");

# expected number of events
# 2.371754  (ora solamente per i 78 soggetti di prima)

Dataset$cv_hosp[ren.fail=="ren_fail present"&age_group=="[50-59]"&sex=="m"]
#[1] 3 2 0 0

sum(Dataset$cv.hosp[ren.fail=="ren.fail present"&age.group=="[50-59]"&sex=="m"])
# observed number of events
#[1] 5
# Noi per queste caratteristiche ne avevamo osservati cinque, anche se il modello ne ha previsti 2.37



# Is it necessary to introduce an interaction term between  ren.fail and age.group?

modelint<-glm(cv_hosp~ren_fail+age_group+sex+ren_fail:age_group, offset=log(p_years), family=poisson)
summary(modelint)
idr.display(modelint)
anova(modelint,modelred,test="Chisq")



################################# ESERCIZIO ###################################


# Con il modello modelred proviamo a
1 valutare un'interazione statisticamente significativa tra renalfailure e il sesso
2 valutare sia il tasso che il numero di eventi attesi per i seguenti individui 
3 calcolare il tasso per 1000 anni di eventi a richio che per soggetti con quelle caratteristiche con

RENAL FAILURE PRESENTE
ETA' DA 70 A 79 ANNI
SESSO FEMMINILE 

e ripetere per il sesso maschile

E ripetere anche quando renal failure ? assente

RENAL FAIL		AGEGROUP		SEX
PRES			70-79			F
PRES			70-79			M
ABS			70-79			F
ABS			70-79			M
###########################################################################

modelsexint<-glm(cv_hosp~ren_fail*sex, offset=log(p_years), family=poisson)
summary(modelsexint)
idr.display(modelsexint)
# apparentemente non ? significativa l'interazione tra sesso e renal failure
# Altrimenti potevamo fare un modello togliendo il sesso e confrontandolo con il completo tramite l'ANOVA

### PUNTO DUE E TRE
modelred<-glm(cv_hosp~ren_fail+age_group+sex, offset=log(p_years), family=poisson)


#PRESENT			70-79			F

newdata1a <- as.data.frame(list(ren_fail="ren.fail present", age_group="[70-79]",sex="f",p_years=1000))
texp<-sum(Dataset$p_years[ren_fail=="ren.fail present"&age_group=="[70-79]"&sex=="f"])
newdata1b <- as.data.frame(list(ren_fail="ren.fail present", age_group="[70-79]",sex="f",p_years=texp))

predict.glm(modelred,newdata=newdata1a,type="response") # Per 1000
predict.glm(modelred,newdata=newdata1b,type="response") # Per gli osservati nel dataset

#PRESENT			70-79			M

newdata1a <- as.data.frame(list(ren_fail="ren.fail present", age_group="[70-79]",sex="m",p_years=1000))
texp<-sum(Dataset$p_years[ren_fail=="ren.fail present"&age_group=="[70-79]"&sex=="m"])
newdata1b <- as.data.frame(list(ren_fail="ren.fail present", age_group="[70-79]",sex="m",p_years=texp))

predict.glm(modelred,newdata=newdata1a,type="response") # Per 1000
predict.glm(modelred,newdata=newdata1b,type="response") # Per gli osservati nel dataset

#ABSENT			70-79			F

newdata1a <- as.data.frame(list(ren_fail="ren.fail absent ", age_group="[70-79]",sex="f",p_years=1000))
texp<-sum(Dataset$p_years[ren_fail=="ren.fail absent "&age_group=="[70-79]"&sex=="f"])
newdata1b <- as.data.frame(list(ren_fail="ren.fail absent ", age_group="[70-79]",sex="f",p_years=texp))

predict.glm(modelred,newdata=newdata1a,type="response") # Per 1000
predict.glm(modelred,newdata=newdata1b,type="response") # Per gli osservati nel dataset


#ABSENT			70-79			M

newdata1a <- as.data.frame(list(ren_fail="ren.fail absent ", age_group="[70-79]",sex="m",p_years=1000))
texp<-sum(Dataset$p_years[ren_fail=="ren.fail absent "&age_group=="[70-79]"&sex=="m"])
newdata1b <- as.data.frame(list(ren_fail="ren.fail absent ", age_group="[70-79]",sex="m",p_years=texp))

predict.glm(modelred,newdata=newdata1a,type="response") # Per 1000
predict.glm(modelred,newdata=newdata1b,type="response") # Per gli osservati nel dataset


