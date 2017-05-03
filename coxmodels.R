# Importazione delle librerie
library(epicalc)
library(survival)

# recoding delle variabili

modeldata = as.data.frame(rep(0, 202))

modeldata[, "Asa"] = factor(survival$ASA)
modeldata[,"nLinf12"] = factor(survival$`>12`)
modeldata[, "stadio"] = factor(survival$STADIO)
modeldata[, "ricN"] = factor(survival$Ric_N)
modeldata[, "LNR"] = factor(survival$`LNR CLASSE`)
modeldata[, "LODDS"] = factor(survival$`LODDS CLASSE`)
modeldata = modeldata[,-1]
str(modeldata)

use(modeldata)

#### Modello di Cox multivariato ####

model0 = coxph(msurv ~ Asa + nLinf12 + stadio + ricN + LNR + LODDS)
summary(model0)

table(msurv, LNR)

# Valutando la significatività

summary(coxph(msurv ~ ricN + LNR))
summary(coxph(msurv ~ LODDS + LNR))
summary(coxph(msurv ~ LNR + LODDS))


summary(co)