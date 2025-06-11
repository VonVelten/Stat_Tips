#============================================================================================================ #
#================================================== PROG ID ================================================= #
#                                                                                                             #
# PROJET..................= VAGABOND                                                                          #
# NOM SCRIPT.............= 2020_FormationPG_XGB.R                                                             #
# AUTEUR...................= L. RIABOFF                                                                       #
# version R................= R version 4.0.2                                                                  #
# DEPENDANCES.............= ""                                                                                #
# ENTREES....................=                                                                                #
#                                                                                                             #
# SORTIES...................= Matrice de confusion, accuracy, précision, sensibilité, spécificité, Fscore,    #
# taux d'erreur et importance des variables                                                                   #
#                                                                                                             #
#================================================ DESCRIPTION =============================================== #
#                                                                                                             #
# OBJECTIFS: - Trouver les critères qui optimisent XGB                                                        #
#            - Evaluer la qualité du modèle sur la base d'un jeu de données test                              #
#                                                                                                             #                                                                       
# PRINCIPALES ETAPES DU PROGRAMME:                                                                            #
#                                                                                                             #
#     -  IMPORTER LE JEU DE DONNEES                                                                           #
#     -  PREPARATION DE LA TABLE DE DONNEES POUR FORET ALEATOIRE                                              #                                                                                                                                    
#     -  RECHERCHE DES PARAMETRES OPTIMAUX                                                                    #
#     -  EVALUATION DE LA QUALITE DU MODELE VIA LE JEU DE DONNEES TEST                                        #
#                                                                                                             #
#================================================= REVISIONS ================================================ #
#---------|-------------|----------------|------------------------------------------------------------------- #
# VERSION |     DATE    |     AUTHOR     |                               COMMENTS                             #
#---------|-------------|----------------|------------------------------------------------------------------- #
# def     |  08/10/2020 | L. RIABOFF     | Code réadapté pour la formation à l'ESA avec Pierre Gaignon.       #
#         |             |                |                                                                    #                                                                           
#         |             |                |                                                                    #
#         |             |                |                                                                    #
#         |             |                |                                                                    #
#============================================================================================================ #
#============================================================================================================ #

rm(list=ls())
library(caret)
library(xgboost)

# 1. IMPORTER LES JEUX DE DONNEES
setwd("~/Formation P. Gaignon/ValidationModeleML")
load("2020_AJUSTSET.RData")
load("2020_TESTSET.RData")

setwd("~/Formation P. Gaignon/AlgorithmesML/PartieApplications/XGB")
source("useMulticore.R")

# 3.PREPARATION DE LA TABLE DE DONNEES POUR LA FORET ALEATOIRE
traindata <- training_select
testdata <- test_select

rm(training_select)
rm(test_select)

# 3.1 SUPPRESSION DES VARIABLES INUTILES POUR PREDICTION
## Jeu de données d'entraînement
traindata <- traindata[,-c(which(colnames(traindata)=='sensor'),which(colnames(traindata)=='farm'),which(colnames(traindata)=='paddock'),which(colnames(traindata)=='id_cow'), which(colnames(traindata)=='date'), which(colnames(traindata)=='h_begin'),which(colnames(traindata)=='h_end'))]

## Jeu de données test
testdata <- testdata[,-c(which(colnames(testdata)=='sensor'),which(colnames(testdata)=='farm'),which(colnames(testdata)=='paddock'),which(colnames(testdata)=='id_cow'), which(colnames(testdata)=='date'), which(colnames(testdata)=='h_begin'),which(colnames(testdata)=='h_end'))]

# 3.2 CONVERSION DE LA COLONNE BEHAVIOUR AU FORMAT FACTEUR
traindata$behaviour<-as.factor(traindata$behaviour)
testdata$behaviour<-as.factor(testdata$behaviour)

# 3.3 RENOMMER LA VARIABLE BEHAVIOUR "y"

names(traindata)[1]="y"
names(testdata)[1]="y"

x=traindata[2:ncol(traindata)]
y=traindata$y

# 4. RECHERCHE DES PARAMETRES OPTIMAUX

multicore=TRUE
if (multicore==TRUE){cl=useMulticore()}

## 4.1 CREATION DE LA GRILLE DE PARAMETRISATION ET METHODE POUR TROUVER LES PARAMETRES OPTIMAUX 
set.seed(111)
metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid", allowParallel=TRUE)
tunegrid <- expand.grid(nrounds=c(150,200,250), max_depth=c(2:6), eta=c(0.1,0.2,0.3,0.4), gamma=0, colsample_bytree=c(0.6,0.7,0.8), 
                      min_child_weight=1, subsample=c(0.50,0.75,0.9,1))

## 4.3 AJUSTEMENT DU MODELE DE RF
ptm <- proc.time()
xgb.fit <- train(y~., data=traindata, method="xgbTree", metric=metric, tuneGrid=tunegrid, trControl=control)
xgb.fit.time <- proc.time()-ptm
print(paste0("Execution time = ",xgb.fit.time,"s")) 

# setwd("~/Formation P. Gaignon/AlgorithmesML/PartieApplications/XGB")
# load(file = "2019-04-29_model_XGB_TFEN10RECV90NORM.RData")

#4.4 PARAMETRES DU MODELE AJUSTE
xgb.fit$bestTune
x11()
plot(xgb.fit)

# 5. EVALUATION DES PERFORMANCES DE ML 

summary(xgb.fit)

## 5.1 PERFORMANCE DU MODELE SUR JEU DE DONNEES D'ENTRAINEMENT
xgb.predictions=predict(xgb.fit, newdata=traindata)
perf_custom_xgb_traindata <- confusionMatrix(data=xgb.predictions,reference=traindata$y,positive=NULL)

## 5.2 PERFORMANCE DES DIFFERENTS MODELES TESTES PAR VALIDATION CROISEE

perf_xgb_cv <- subset(xgb.fit$results, nrounds == xgb.fit$bestTune$nrounds & max_depth == xgb.fit$bestTune$max_depth & eta == xgb.fit$bestTune$eta & gamma == xgb.fit$bestTune$gamma &  colsample_bytree == xgb.fit$bestTune$colsample_bytree & min_child_weight == xgb.fit$bestTune$colsample_bytree & subsample == xgb.fit$bestTune$subsample, c("Accuracy", "Kappa"))

## 5.3 EVALUATION DE LA QUALITE DU MODELE AVEC LE JEU DE DONNEES TEST
xgb.predictions=predict(xgb.fit, newdata=testdata)
perf_xgb_testdata <- confusionMatrix(data=xgb.predictions,reference=testdata$y,positive=NULL)

mat_conf_xgb<- t(perf_custom_xgb_testdata$table)
metric_overall_xgb <- perf_custom_xgb_testdata$overall
metric_byClass_xgb <- perf_custom_xgb_testdata$byClass

# 6. ANALYSE DE L'IMPORTANCE DES VARIABLES

xgb.varImp <- varImp(xgb.fit, scale=TRUE)
## Classement par ordre d'importance global
imp_globale <- sort(apply(xgb.varImp$importance, 1,function(x){sqrt(sum(x^2))}), decreasing = TRUE)
## Variables les plus discriminantes pour les comportements les plus durs à discriminer 
imp_standing_none <- data.frame(name = rownames(xgb.varImp$importance), imp = xgb.varImp$importance$Standing...None)
imp_standing_none <- imp_standing_none[order(imp_standing_none$imp, decreasing = TRUE),]
top_20_standing_none  <- imp_standing_none[1:20,"name"]

imp_walking <- data.frame(name = rownames(xgb.varImp$importance), imp = xgb.varImp$importance$Walking)
imp_walking <- imp_walking[order(imp_walking$imp, decreasing = TRUE),]
top_20_walking  <- imp_walking[1:20,"name"]

# 7. GENERER LES RESULTATS
setwd("~/Formation P. Gaignon/AlgorithmesML/PartieApplications/Resultats")
save(mat_conf_xgb, file = "mat_conf_xgb.RData")
save(metric_overall_xgb, file = "metric_overall_xgb.RData")
save(metric_byClass_xgb, file = "metric_byClass_xgb.RData")
write.csv2(xgb.varImp$importance, file = "varImp.xgb.csv", dec = ".", sep = ";")


