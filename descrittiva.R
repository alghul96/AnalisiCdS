# load(".RData")
# download.file(url = "http://cran.r-project.org/src/contrib/Archive/epicalc/epicalc_2.15.1.0.tar.gz",
#              destfile = "epicalc_2.15.1.0.tar.gz")
# install.packages(pkgs="epicalc_2.15.1.0.tar.gz", type="source", repos=NULL)
# unlink("epicalc_2.15.1.0.tar.gz")

library(epicalc)
library(ggplot2)

surv<-survival

str(surv)

#use(surv)
#des()
#summ()
#table(SESSO)

summary(surv)
table(surv$`LNR CLASSE`)
table(surv$`LODDS CLASSE`)


classe_lnr<-ggplot(data=surv, aes(`LNR CLASSE`,fill=factor(`LNR CLASSE`))) +
  geom_bar() +
  ggtitle("Diagramma a barre: Classe LNR") +
  xlab("Classe LNR") + ylab("Conteggio") +
  scale_fill_discrete(name="Classe LNR") +
  geom_text(stat="count",aes(label=..count..),vjust=-0.5,size=4.2)

classe_lodds<-ggplot(data=surv, aes(`LODDS CLASSE`,fill=factor(`LODDS CLASSE`))) +
  geom_bar()+
  ggtitle("Diagramma a barre: Classe LODDS") +
  xlab("Classe LODDS") + ylab("Conteggio") +
  scale_fill_discrete(name="Classe LODDS") +
  geom_text(stat="count",aes(label=..count..),vjust=-0.5,size=4.2)

eta<-ggplot(data=surv, aes(`ETA'`)) +
  geom_histogram(bins=20,fill = "red",colour="black") +
  labs(title="Diagramma a barre: età",x="Classe età", y="Conteggio")

shapiro.test(surv$`ETA'`)


sesso<-ggplot(data=surv, aes(`SESSO`,fill=factor(`SESSO`))) +
  geom_bar() +
  labs(title="Diagramma a barre: sesso",x="Sesso", y="Conteggio")

morto<-ggplot(data=surv, aes(`Morto`,fill=factor(`Morto`))) +
  geom_bar() +
  labs(title="Diagramma a barre: morti",x="Decesso", y="Conteggio")

malato<-ggplot(data=surv, aes(`Malattia`,fill=factor(`Malattia`))) +
  geom_bar() +
  labs(title="Diagramma a barre: morti",x="Ha avuto una ricaduta", y="Conteggio")



barlines <- "#1F3552"
overall_surv<-ggplot(data=surv, aes(`OVERALL SURVIVAL (mesi)`,fill=as.factor(Morto))) +
  geom_histogram(bins=20,colour = barlines) +
  labs(title="Sopravvivenza totale",x="Mesi", y="Conteggio")


sesso
eta

morto
malato

classe_lnr
classe_lodds


# overall_surv
