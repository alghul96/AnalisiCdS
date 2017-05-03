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


# Confronto tra le due curve

par(mfrow = c(1,2))
plot(mfit.LODDS, conf.int = FALSE, col = c("red", "green", "blue"), main = "Sopravvivenza per LODDS")
plot(mfit.LNR, conf.int = FALSE, col = c("red", "green", "blue"), main = "Sopravvivenza per LNR")

# Kaplan–Meier survival curves of the patients staged by log odds of positive lymph nodes (LODDS) and lymph node ratio.

################################
#### DISEASE-FREE SURVIVAL #####


disurv<- Surv(`DISEASE- FREE SURVIVAL (mesi)`, Morto == 1)

difit <- survfit((disurv)~1)
options(survfit.print.mean = TRUE)
my.fit = summary(difit)

#### PLOT DELLA CURVA DELLA SOPRAVVIVENZA ####

plot(difit, main = "Funzione di sopravvivenza Disease-Free", xlab = "Mesi dall'operazione")


### STIMA DELLA CUMULATIVE HAZARD FUNCTION ####

plot(difit, fun="cumhaz", main = "Funzione cumulativa del Rischio (-logS(t))") # funzione cumulativa di azardo 


#### ANALISI DELLE COVARIATE: LNR ####

#### Analisi della sopravvivenza

difit.LNR = survfit(disurv ~ survival$`LNR CLASSE`)

difit.LNR

par(mfrow = c(1,2))
plot(difit.LNR, conf.int = FALSE, col = c("red", "green", "blue"), main = "Disease-Free per LNR")
plot(difit.LNR, conf.int = TRUE, col = c("red", "green", "blue"))
legend("topright", legend = c("Classe 0","Classe 1", "Classe 2"), col = c("red", "green", "blue"), lty = c(1,1,1))

#### Log Rank test

survdiff(disurv ~ survival$`LNR CLASSE`, rho = 0) # rifiutiamo l'hp che le curve di sopravvivenza sono uguali



#### ANALISI DELLE COVARIATE: LODDS ####

#### Analisi della sopravvivenza

difit.LODDS = survfit(disurv ~ survival$`LODDS CLASSE`)

difit.LODDS

par(mfrow = c(1,2))
plot(difit.LODDS, conf.int = FALSE, col = c("red", "green", "blue"), main = "Disease-Free per LODDS")
plot(difit.LODDS, conf.int = TRUE, col = c("red", "green", "blue"))
legend("topright", legend = c("Classe 0","Classe 1", "Classe 2"), col = c("red", "green", "blue"), lty = c(1,1,1))

#### Log Rank test

survdiff(disurv ~ survival$`LODDS CLASSE`, rho = 0) # rifiutiamo l'hp che le curve di sopravvivenza sono uguali


######################################################
### CONFRONTO TRA DISEASE FREE E OVERALL SURVIVAL ####


windows()
par(mfrow = c(2,2))

plot(mfit.LNR, conf.int = FALSE, col = c("red", "green", "blue"), main = "Sopravvivenza Totale per LNR")
plot(difit.LNR, conf.int = FALSE, col = c("red", "green", "blue"), main = "Sopravvivenza Disease Free per LNR")
legend("topright", legend = c("Classe 0","Classe 1", "Classe 2"), col = c("red", "green", "blue"), lty = c(1,1,1))


plot(mfit.LODDS, conf.int = FALSE, col = c("red", "green", "blue"), main = "Sopravvivenza Totale per LODDS")
plot(difit.LODDS, conf.int = FALSE, col = c("red", "green", "blue"), main = "Sopravvivenza Disease Free per LODDS")
legend("topright", legend = c("Classe 0","Classe 1", "Classe 2"), col = c("red", "green", "blue"), lty = c(1,1,1))

