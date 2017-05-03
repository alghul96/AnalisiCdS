library(epicalc)
library(survival)

###########################
#### OVERALL SURVIVAL #####


# Creazione dell'oggetto survivial
use(survival)


msurv<- Surv(`OVERALL SURVIVAL (mesi)`, Morto == 1)

mfit <- survfit((msurv)~1)
options(survfit.print.mean = TRUE)
my.fit = summary(mfit)

#### PLOT DELLA CURVA DELLA SOPRAVVIVENZA ####

plot(mfit, main = "Funzione di sopravvivenza", xlab = "Mesi dall'operazione")


### STIMA DELLA CUMULATIVE HAZARD FUNCTION ####

plot(mfit, fun="cumhaz", main = "Funzione cumulativa del Rischio (-logS(t))") # funzione cumulativa di azardo 


#### ANALISI DELLE COVARIATE: LNR ####

#### Analisi della sopravvivenza

mfit.LNR = survfit(msurv ~ survival$`LNR CLASSE`)

mfit.LNR

par(mfrow = c(1,2))
plot(mfit.LNR, conf.int = FALSE, col = c("red", "green", "blue"), main = "Sopravvivenza per LNR")
plot(mfit.LNR, conf.int = TRUE, col = c("red", "green", "blue"))
legend("topright", legend = c("Classe 0","Classe 1", "Classe 2"), col = c("red", "green", "blue"), lty = c(1,1,1))

#### Log Rank test

survdiff(msurv ~ survival$`LNR CLASSE`, rho = 0) # rifiutiamo l'hp che le curve di sopravvivenza sono uguali



#### ANALISI DELLE COVARIATE: LODDS ####

#### Analisi della sopravvivenza

mfit.LODDS = survfit(msurv ~ survival$`LODDS CLASSE`)

mfit.LODDS

par(mfrow = c(1,2))
plot(mfit.LODDS, conf.int = FALSE, col = c("red", "green", "blue"), main = "Sopravvivenza per LODDS")
plot(mfit.LODDS, conf.int = TRUE, col = c("red", "green", "blue"))
legend("topright", legend = c("Classe 0","Classe 1", "Classe 2"), col = c("red", "green", "blue"), lty = c(1,1,1))

#### Log Rank test

survdiff(msurv ~ survival$`LODDS CLASSE`, rho = 0) # rifiutiamo l'hp che le curve di sopravvivenza sono uguali
