load(".RData")
library(epicalc)
library(survival)
library(ggfortify)
library(gridExtra)
library(ggplot2)
## http://rpubs.com/sinhrks/plot_surv



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
ov_surv_curv = autoplot(mfit, main = "Funzione di sopravvivenza", xlab = "Mesi dall'operazione", surv.colour = "orange", censor.colour = "red", ylim=c(0,1))
 

### STIMA DELLA CUMULATIVE HAZARD FUNCTION ####

plot(mfit, fun="cumhaz", main = "Funzione cumulativa di rischio (-logS(t))") # funzione cumulativa di rischio 
ov_cum_haz<-autoplot(mfit,fun="cumhaz",main = "Funzione cumulativa di rischio (-log S(t))",surv.colour = "skyblue1",censor.colour = "blue")

#### ANALISI DELLE COVARIATE: LNR ####

#### Analisi della sopravvivenza

mfit.LNR = survfit(msurv ~ survival$`LNR CLASSE`)

mfit.LNR

par(mfrow = c(1,2))
plot(mfit.LNR, conf.int = FALSE, col = c("red", "green", "blue"), main = "Sopravvivenza per LNR")
plot(mfit.LNR, conf.int = TRUE, col = c("red", "green", "blue"))
legend("topright", legend = c("Classe 0","Classe 1", "Classe 2"), col = c("red", "green", "blue"), lty = c(1,1,1))

ov_lnr <- autoplot(mfit.LNR, conf.int = F, col = c("red", "green", "blue"), main = "Sopravvivenza per LNR", ylim=c(0,1))
ov_lnr_conf <- autoplot(mfit.LNR, col = c("red", "green", "blue"), main = "Sopravvivenza per LNR", ylim=c(0,1))

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

ov_lodds<-autoplot(mfit.LODDS,conf.int = F, main = "Sopravvivenza per LODDS", ylim=c(0,1))
ov_lodds_conf <- autoplot(mfit.LODDS, main = "Sopravvivenza per LODDS",ylim=c(0,1))

#### Log Rank test

survdiff(msurv ~ survival$`LODDS CLASSE`, rho = 0) # rifiutiamo l'hp che le curve di sopravvivenza sono uguali


# Confronto tra le due curve

par(mfrow = c(1,2))
plot(mfit.LODDS, conf.int = FALSE, col = c("red", "green", "blue"), main = "Sopravvivenza per LODDS")
plot(mfit.LNR, conf.int = FALSE, col = c("red", "green", "blue"), main = "Sopravvivenza per LNR")

#grid.arrange(ov_lodds, ov_lnr, ncol=2,nrow = 1)

# Kaplan–Meier survival curves of the patients staged by log odds of positive lymph nodes (LODDS) and lymph node ratio.






################################
#### DISEASE-FREE SURVIVAL #####


disurv <- Surv(`DISEASE- FREE SURVIVAL (mesi)`, Malattia == 1)

difit <- survfit((disurv)~1)
options(survfit.print.mean = TRUE)
my.fit = summary(difit)

#### PLOT DELLA CURVA DELLA SOPRAVVIVENZA ####

plot(difit, main = "Funzione di sopravvivenza Disease-Free", xlab = "Mesi dall'operazione")
df_surv_curv <-
    autoplot(
    difit,
    surv.colour = "orange",
    censor = TRUE,
    censor.colour = "red",
    ylim = c(0, 1),
    main = "Funzione di sopravvivenza Disease-Free"
  )

### STIMA DELLA CUMULATIVE HAZARD FUNCTION ####

plot(difit, fun="cumhaz", main = "Funzione cumulativa del Rischio (-logS(t))") # funzione cumulativa di rischio 
df_cum_haz <-
  autoplot(
    difit,
    fun = "cumhaz",
    surv.colour = "skyblue1",
    censor = TRUE,
    main = "Funzione cumulativa di rischio (-log S(t)) per Disease-Free"
  )

#### ANALISI DELLE COVARIATE: LNR ####

#### Analisi della sopravvivenza

difit.LNR = survfit(disurv ~ survival$`LNR CLASSE`)

difit.LNR

par(mfrow = c(1,2))
plot(difit.LNR, conf.int = FALSE, col = c("red", "green", "blue"), main = "Disease-Free per LNR")
plot(difit.LNR, conf.int = TRUE, col = c("red", "green", "blue"))
legend("topright", legend = c("Classe 0","Classe 1", "Classe 2"), col = c("red", "green", "blue"), lty = c(1,1,1))

df_lnr <- autoplot(difit.LNR, conf.int = F)
df_lnr_conf <- autoplot(difit.LNR,
                        censor = TRUE,
                        main = "Disease-Free per LNR")

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

df_lodds_conf<-autoplot(difit.LODDS,  main = "Disease-Free per LODDS")
df_lodds <- autoplot(difit.LODDS, 
                     conf.int = TRUE,
                     main = "Disease-Free per LODDS",
                     censor = TRUE)

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

#grid.arrange(ov_lnr,df_lnr,ov_lodds,df_lodds,ncol=2,nrow = 2)

##### Grafici #####

ov_surv_curv # overall 
ov_cum_haz # cumulative hazards
ov_lnr # con lnr
ov_lnr_conf # con intervalli di confidenza per lnr
ov_lodds # con LODDS
ov_lodds_conf # con intervalli di confidenza per LODDS
ov_loods_lnr<-grid.arrange(ov_lodds, ov_lnr, ncol=2, nrow=1)

df_surv_curv
df_cum_haz
df_lnr
df_lnr_conf
df_lodds_conf
df_lodds

ov_df <- grid.arrange(ov_lnr,
                      df_lnr,
                      ov_lodds,
                      df_lodds,
                      ncol = 2,
                      nrow = 2)