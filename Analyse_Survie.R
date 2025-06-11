# Analyse de survie

library(survival)
library(ggplot2)

mydata.surv=Surv(time=c(rep(10,50),round(runif(n=30,min=1,max=8))),
                 event=c(rep(1,50),rep(2,30)),
                 origin=0,
                 type="right")
summary(survfit(mydata.surv~1, conf.type="plain"))

# Tracer courbe de Kaplan-Meier
plot(survfit(mydata.surv~1, conf.type="plain"), col="blue", lwd=2)




# On compare 2 traitements


Temp<-c(rep(10,50),round(runif(n=30,min=1,max=8)),
        rep(10,30),round(runif(n=50,min=1,max=8)))
Statut<-c(rep(1,50),rep(2,30),
          rep(1,30),rep(2,50))
Groupe<-as.factor(c(rep("Trait",80),rep("Placebo",80)))

Data<-as.data.frame(Temp)
Data$Statut<-Statut
Data$Groupe<-Groupe
plot(survfit(Surv(Data$Temp, Data$Statut)~Data$Groupe),col=c("red","blue"))
# Première couleur = niveau 1 (ici Placebo)

T1<-c(rep(10,50),round(runif(n=30,min=0,max=8)))
S1<-c(rep(0,50),rep(1,30))
T2<-c(rep(10,30),round(runif(n=50,min=1,max=8)))
S2<-c(rep(1,30),rep(2,50))

plot(survfit(Surv(T1,S1)~1),col="red")
lines(survfit(Surv(T2,S2)~1),col="blue")

survfit(Surv(T1,S1)~1)

survfit(Surv(T2,S2)~1)




survfit(Surv(Data$Temp,Data$Statut)~1)
# Si median=Na -> + de 50% de Survie
survfit(Surv(Data$Temp,Data$Statut)~Groupe)

survdiff(Surv(Temp,Statut)~Groupe)


# Cas avec des censures

Temp<-c(rep(10,50),round(runif(n=30,min=1,max=8)),runif(n=30,min=1,max=8),
        rep(10,30),round(runif(n=50,min=1,max=8)))
  Statut<-c(rep(0,50),rep(1,30),rep(0,30),
          rep(0,30),rep(1,50))
Groupe<-as.factor(c(rep("Trait",110),rep("Placebo",80)))

Data<-as.data.frame(Temp)
Data$Statut<-Statut
Data$Groupe<-Groupe
plot(survfit(Surv(Temp, Statut)~Groupe,data=Data, conf.type="plain"),
     col=c("red","blue"))

plot(survfit(Surv(Temp, Statut)~1,data=Data, conf.type="plain"),
     col=c("blue"),mark.time = T)
