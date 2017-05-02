library(epicalc)

# load dataset

whas <- read.dta("R:\\Miglio\\biostatistics 2016\\lab 12th may\\whas2.dta", 
 convert.dates=TRUE, convert.factors=TRUE, missing.type=TRUE, 
 convert.underscore=TRUE, warn.missing.labels=TRUE)


use(whas)
# information on variables
des()
# descriptive statistics
summ()

#note the asymmetry of cpk variable

hist(whas$cpk)

# use log transformation
hist(log(whas$cpk))

# load library for survival analysis
library(survival)



# Create the survival data object
msurv<- Surv(lenfol, fstat == "Dead")
# descriptive analysis and Kaplan-Meier estimate
mfit <- survfit((msurv)~1)
options(survfit.print.mean = TRUE)
mfit
summary(mfit)

#Moltiplicando il valore precedente per la stima della probabilità condizionata
# otteniamo il valore della stima incondizionata del blablabla (cazzo devi seguire)
# per vedere il tempo mediano di sopravvivenza vedo quando più o meno la funzione di sopravvivenza va al di sotto del valore di 0.5
# (nel nostro caso si tratta di 2335)
# Allo stesso modo per quanto riguarda il venticinquesimo percentile dobbiamo vedrre quello con 0.75 (ovver il 205)
# mentre quello del 75esimo non arriviamo a stimarlo in quanto ci fermiamo allo 0.38

plot(mfit)

###############################################################
######### Stima della funzione di rischio cumulata ############
###############################################################

# estimate the cumulative hazard function

my.fit <- summary(mfit) #questo oggetto vciene creato dal summary di mfit contiene l'elemento surv
H.hat <- -log(my.fit$surv); # che è proprio lo stimatore della funzione di kaplan mayer
H.hat <- c(H.hat, H.hat[length(H.hat)]) # a questo oggetto si aggiunge un ulteriore elemento per rappresentarlo graficamrente

plot(c(my.fit$time, my.fit$time[length(my.fit$time)]+1),H.hat,type="s") # e quindi si plotta
plot(mfit, fun="cumhaz") #oppure questa è la funzione automatica 

# plot KM estimate

###########################################
# FUN option of plot.survfit
###########################################
#an arbitrary function defining a transformation of the survival curve. 
#For example fun=log is an alternative way to draw a log-survival curve (but with the axis labeled with log(S) values), 
#and fun=sqrt would generate a curve on square root scale. Four often used transformations can be specified with a character argument instead: 
#"log" is the same as using the log=T option, 
#"event" plots cumulative events (f(y) = 1-y), 
#"cumhaz" plots the cumulative hazard function (f(y) = -log(y)), 
#and "cloglog" creates a complimentary log-log survival plot (f(y) = log(-log(y)) along with log scale for the x-axis)----"log minus log".

plot(mfit, fun="cloglog")

#####################################################################

###############################################
### plot of KM estimate for some covariates ###
###############################################
# Going bak to our analysis

# Create KM estimates broken out by miord
mfit.bymiord<-survfit((msurv)~miord)
# nell'output è possibile vedere che il secondo intervallo di confidenza non viene riportato 
plot(mfit.bymiord)
# in questo grafico vediamo la stima delle due curve, una con miord recurrent e l'altra meno
# la differenza tra le due curve comunque si nota!
# with confidence band
plot(mfit.bymiord, conf.int = TRUE, col = c("black", "red"), lty = 1:2)
legend(locator(1), legend=c("First","Recurrent"), lty=1:2)
# vediamo che in questo grafico la curva nera è quella del gruppo di coloro che sono 
# al primo infarto, mentre quella in rosso è la sopravvivenza stimata di coloro che sono al 
# secondo o ad un superiore infarto
 

#plot of log-minus-log function

plot(mfit.bymiord, fun="cloglog", col = c("black", "red"), 
lty = 1:2)




# Create KM estimates broken out by mitype


mfit.bymitype<-survfit((msurv)~mitype)
plot(mfit.bymitype)
# with confidence band
plot(mfit.bymitype, conf.int = TRUE, col = c("black", "red"), lty = 1:2)
legend(locator(1), legend=c("Q-wave","non-Q-wave"), lty=1:2)

#plot of log-minus-log function

plot(mfit.bymitype, fun="cloglog", col = c("black", "red"), 
lty = 1:2)


################################
# passiamo quindi i test


# Log rank test for miord
survdiff(msurv~miord, rho=0)
# Questo test ci restituisce gli eventi osservati, quelli attesi e 
# altre mirabolanti cose che sarebbero gli osservati meno gli attesi. 
# il p-value è piccolo e quindi si rifiuta l'ipotesi nulla che le due curve
# siano uguali tra di loro. 

# Peto test 
survdiff(msurv~miord, rho=1)
# Anche se applichiamo il test di peto anche questo infulisce sugli attesi
# e anche questo test ci riporta a rifiutare l'hp nulla. 

# altre variabili 
# Log rank test for mitype
survdiff(msurv~mitype, rho=0)
# Peto test 
survdiff(msurv~mitype, rho=1)
# Se valutiamo il risultato qui ci accorgiamo che il log-rank test è pari a 0.13
# ma questa variabile non ha rischi proporzionali, quindi in teoria
# siam portati a non rifiutare l'hp nulla anche se alla fine sembra 
# da rifiutare. 
# se vediamo il test di peto, invece, che è capace di valutare la situazione anche 
# quando è rifiutato l'assunto di rischi proporzionali, siamo sicurdi di poter
# accettare l'ipotesi.


# TEST DEL LOG RANK SU VARIABILE CONTINUA
# Log rank test for agecl
survdiff(msurv~agecl, rho=0)
mfit.agecl<-survfit((msurv)~agecl)
plot(mfit.agecl)



#########################
# univariate COX regression model
#########################
cox.bymiord<-coxph(msurv ~ miord , iter.max=20)
cox.bymitype<-coxph(msurv ~ mitype , iter.max=20)
cox.bysex<-coxph(msurv ~ sex, iter.max=20)

cox.byage<-coxph(msurv ~ age , iter.max=20)
cox.byagecl<-coxph(msurv ~ factor(agecl) , iter.max=20)

cox.bysho<-coxph(msurv ~ sho , iter.max=20)
cox.bychf<-coxph(msurv ~ chf , iter.max=20)

cox.bycpk<-coxph(msurv ~ cpk , iter.max=20)
cox.bycpkcl<-coxph(msurv ~ factor(cpkcl) , iter.max=20)

summary(cox.bymiord)
summary(cox.bymitype)
summary(cox.bysex)

summary(cox.byage)
summary(cox.byagecl)

summary(cox.bysho)
summary(cox.bychf)

summary(cox.bycpk)
summary(cox.bycpkcl)


# complete model
cox.comp <- coxph(msurv ~ factor(agecl) + miord+mitype + chf+sho+sex+factor(cpkcl), iter.max=12)

# Assess PH by estimating log relative hazard over time
# we could save scaled shoenfeld residual and plot them versus time or log(time)

# Andiamo a vedere se l'ipotesi del modello di cox è stata mantenuta
# quella dei rischi proprozionali

r <- resid(cox.comp, "scaledsch")
# more easy

# Andiamo a vedere se l'ipotesi del modello di cox è stata mantenuta
# quella dei rischi proprozionali
# Ci sono tre tipi di residui di schenfel. 
# Quelli scalati, quelli non scalati e un terzo che non ricordo come si chiama

# Questa funzione permette di stimare questi residui, permette di fare test
# su questi residui e creare dei grafici di questi residui nel tempo 

plot(cox.zph(cox.comp))    # invokes plot.cox.zph

# In questo grafico son messi i grafici uno dopo l'altro, non si capisce un cazzi

zph.comp<- cox.zph(cox.comp, transform = 'log')
par(mfrow=c(3,3))
plot(zph.comp[1])
abline(h=0, lty=3)
plot(zph.comp[2])
abline(h=0, lty=3)
plot(zph.comp[3])
abline(h=0, lty=3)
plot(zph.comp[4])
abline(h=0, lty=3)
plot(zph.comp[5])
abline(h=0, lty=3)
plot(zph.comp[6])
abline(h=0, lty=3)
plot(zph.comp[7])
abline(h=0, lty=3)
plot(zph.comp[8])
abline(h=0, lty=3)
plot(zph.comp[9])
abline(h=0, lty=3)

# Su alcuni grafici, tolte le età, si può rilevar eun andamento
# allora si crea un test che dovrebbe cercare il coefficiente di 
# correlazione linerare tra ciascuno di questi residui e il tempo. 

print(zph.comp)


# vediamo che mitype e cpkcl dell'ultima classe può dare noia 
# Quindi in teoria l'uniche correlazioni lineari potrebbero essere quelle lì