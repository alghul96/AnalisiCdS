# Importazione delle librerie
library(epicalc)
library(survival)


# Plot della log minus log, per vedere l'hp di rischi proporzionali

prop.lnr = autoplot(
  mfit.LNR,
  fun = "cloglog",
  #ylim = c(-2, 1),
  conf.int = FALSE,
  main = "Log minus log per LNR"
)

prop.lodds = autoplot(
  mfit.LODDS,
  fun = "cloglog",
  #ylim = c(-2, 1),
  conf.int = FALSE,
  main = "Log minus log per LODDS"
)

grid.arrange(prop.lnr,
             prop.lodds,
             ncol = 2,
             nrow = 1)

# Vediamo che l'ipotesi di rischi proporzionali sembra non essere rispettata

# recoding delle variabili

modeldata = as.data.frame(rep(0, 202))

modeldata[, "Asa"] = factor(survival$ASA)
modeldata[,"nLinf12"] = factor(survival$`>12`)
modeldata[, "stadio"] = factor(survival$STADIO)
modeldata[, "ricN"] = factor(survival$Ric_N)
modeldata[, "LNR"] = factor(survival$`LNR CLASSE`)
modeldata[, "LODDS"] = factor(survival$`LODDS CLASSE`)
modeldata[,"Age"] = (survival$`ETA'`)
modeldata = modeldata[,-1]
str(modeldata)

use(modeldata)


#### MODELLI UNIVARIATI ####

summary(coxph(msurv ~ LNR))

# Come già visto, il coefficiente riferito alla classe 1 di LNR non è significativo.
# Il coefficiente relativo alla classe 2, invece, risulta essere significativo. L'hazard ratio risulta essere, infatti, di 3.055
# con valori che oscillano tra 1.89 e 4.9 con intervalli di confidenza del 95%

summary(coxph(msurv ~ LODDS))

# Situazione analoga per LODDS. La prima classe non risulta significativa pe run intervallo di confidenza del 95%.
# La classe 2, invece, è significativa, con un HR di 3.8, con intervalli di confidenza al 95% che oscillano
# da 2.257 a 6.396



#### Modello di Cox multivariato  ####

model0 = coxph(msurv ~ Asa + nLinf12 + stadio + ricN + LNR + LODDS)
summary(model0)

summary(coxph(msurv ~ Age + Asa + nLinf12 + stadio + LNR))

summary(coxph(msurv ~ Age + nLinf12 + LNR))

summary(coxph(msurv ~ Age + LNR + Asa))

recode(Asa, old.value = 1, new.value = NA)

Asanew = factor(as.numeric(Asa))

summary(coxph(disurv ~ Age + LNR + Asanew))
