#============================================================================================================ #
#================================================== PROG ID ================================================= #
#                                                                                                             #
# PROJET..................= VAGABOND                                                                          #
# NOM SCRIPT.............= 2020_FormationPG_AdaBoost.R                                                        #
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
# OBJECTIFS: - Trouver les critères mtry et ntree qui optimisent AdaBoost                                     #
#            - Evaluer la qualité du modèle sur la base d'un jeu de données test                              #
#                                                                                                             #                                                                       
# PRINCIPALES ETAPES DU PROGRAMME:                                                                            #
#                                                                                                             #
#     -  IMPORTER LE JEU DE DONNEES                                                                           #
#     -  PREPARATION DE LA TABLE DE DONNEES POUR ADABOOST                                                     #                                                                                                                                    
#     -  RECHERCHE DES PARAMETRES OPTIMAUX                                                                    #
#     -  EVALUATION DE LA QUALITE DU MODELE VIA LE JEU DE DONNEES TEST                                        #
#                                                                                                             #
#================================================= REVISIONS ================================================ #
#---------|-------------|----------------|------------------------------------------------------------------- #
# VERSION |     DATE    |     AUTHOR     |                               COMMENTS                             #
#---------|-------------|----------------|------------------------------------------------------------------- #
# def     |  07/10/2020 | L. RIABOFF     | Code réadapté pour la formation à l'ESA avec Pierre Gaignon.       #
#         |             |                |                                                                    #                                                                           
#         |             |                |                                                                    #
#         |             |                |                                                                    #
#         |             |                |                                                                    #
#============================================================================================================ #
#============================================================================================================ #

rm(list=ls())
library(caret)
library(adabag)

# 1. IMPORTER LES JEUX DE DONNEES
setwd("~/Formation P. Gaignon/ValidationModeleML")
load("2020_AJUSTSET.RData")
load("2020_TESTSET.RData")

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

## 4.1 CREATION DE LA GRILLE DE PARAMETRISATION ET METHODE POUR TROUVER LES PARAMETRES OPTIMAUX 
set.seed(111)
metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid", allowParallel=TRUE)
tunegrid <- expand.grid(mfinal=c(100,150), maxdepth=c(10:30))

## 4.2 AJUSTEMENT DU MODELE ADABOOST
# ptm <- proc.time()
# adaboost.fit <- train(y~., data=traindata, method="AdaBag", metric=metric, tuneGrid=tunegrid, trControl=control)
# adaboost.fit <- proc.time()-ptm
# print(paste0("Execution time = ",adaboost.fit[1],"s")) #12603s

setwd("~/Formation P. Gaignon/AlgorithmesML/PartieApplications/Ada")
load(file = "2019-04-22_model_ADABOOST_TFEN10RECV90NORM.RData")

## 4.4 PARAMETRES DU MODELE AJUSTE
adaboost.fit$bestTune
x11()
plot(adaboost.fit)

# 5. EVALUATION DES PERFORMANCES DE ML 

summary(adaboost.fit)

## 5.1 PERFORMANCE DU MODELE SUR JEU DE DONNEES D'ENTRAINEMENT
ada.predictions=predict(adaboost.fit, newdata=traindata)
perf_adaboost_fit_traindata <- confusionMatrix(data=ada.predictions,reference=traindata$y,positive=NULL)

## 5.2 PERFORMANCE DES DIFFERENTS MODELES TESTES PAR VALIDATION CROISEE

perf_ada_fit_cv <- subset(adaboost.fit$results, mfinal == adaboost.fit$bestTune$mfinal & maxdepth == adaboost.fit$bestTune$maxdepth, c("Accuracy", "Kappa"))

## 5.3 EVALUATION DE LA QUALITE DU MODELE AVEC LE JEU DE DONNEES TEST
ada.predictions=predict(adaboost.fit, newdata=testdata)
perf_adaboost_fit_testdata <- confusionMatrix(data=ada.predictions,reference=testdata$y,positive=NULL)

mat_conf_ada <- t(perf_adaboost_fit_testdata$table)
metric_overall_ada <- perf_adaboost_fit_testdata$overall
metric_byClass_ada <- perf_adaboost_fit_testdata$byClass

# 6. ANALYSE DE L'IMPORTANCE DES VARIABLES

ada.varImp <- varImp(adaboost.fit, scale=TRUE)
## Classement par ordre d'importance global
imp_globale <- sort(apply(ada.varImp$importance, 1,function(x){sqrt(sum(x^2))}), decreasing = TRUE)

# 7. GENERER LES RESULTATS
setwd("~/Formation P. Gaignon/AlgorithmesML/PartieApplications/Resultats")
save(mat_conf_ada, file = "mat_conf_ada.RData")
save(metric_overall_ada, file = "metric_overall_ada.RData")
save(metric_byClass_ada, file = "metric_byClass_ada.RData")
write.csv2(ada.varImp$importance, file = "varImp.ada.csv", dec = ".", sep = ";")


