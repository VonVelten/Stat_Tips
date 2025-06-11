#============================================================================================================ #
#================================================== PROG ID ================================================= #
#                                                                                                             #
# PROJET..................= VAGABOND                                                                          #
# NOM SCRIPT.............= 2020_FormationPG_RandomForest.R                                                    #
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
# OBJECTIFS: - Trouver les critères mtry et ntree qui optimisent random forest                                #
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
# def     |  07/10/2020 |L. RIABOFF      | Code réadapté pour la formation à l'ESA avec Pierre Gaignon.       #
#         |             |                |                                                                    #                                                                           
#         |             |                |                                                                    #
#         |             |                |                                                                    #
#         |             |                |                                                                    #
#============================================================================================================ #
#============================================================================================================ #

rm(list=ls())
library(caret)
library(randomForest)


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

## 4.1 CREATION D'UNE PARAMETRISATION EXTENSION DE CARET (CODE SP)
customRF <- list(type="Classification", library="randomForest", loop=NULL)
customRF$parameters <- data.frame(parameter=c("mtry", "ntree"), class=rep("numeric", 2), label=c("mtry", "ntree"))
customRF$grid <- function(x, y, len=NULL, search="grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

## 4.2 CREATION DE LA GRILLE DE PARAMETRISATION ET METHODE POUR TROUVER LES PARAMETRES OPTIMAUX 
set.seed(111)
metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=10, repeats=3, search = "grid")
tunegrid <- expand.grid(.mtry=c(7,10,15,20,25), .ntree=c(500,1000,1500,2000))

## 4.3 AJUSTEMENT DU MODELE DE RF
# ptm <- proc.time()
# custom_rf <- train(y~., data=traindata, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
# custom_rf.time <- proc.time()-ptm
# print(paste0("Execution time = ",custom_rf.time[1],"s")) #12603s

setwd("~/Formation P. Gaignon/AlgorithmesML/PartieApplications/RF")
load(file = "2019-04-29_model_RF_TFEN10RECV90NORM.RData")

#4.4 PARAMETRES DU MODELE AJUSTE
custom_rf$bestTune
x11()
plot(custom_rf)

# 5. EVALUATION DES PERFORMANCES DE ML 

summary(custom_rf)

## 5.1 PERFORMANCE DU MODELE SUR JEU DE DONNEES D'ENTRAINEMENT
rf.predictions=predict(custom_rf, newdata=traindata)
perf_custom_rf_traindata <- confusionMatrix(data=rf.predictions,reference=traindata$y,positive=NULL)

## 5.2 PERFORMANCE DES DIFFERENTS MODELES TESTES PAR VALIDATION CROISEE

perf_custom_rf_cv <- subset(custom_rf$results, mtry == custom_rf$bestTune$mtry & ntree == custom_rf$bestTune$ntree, c("Accuracy", "Kappa"))

## 5.3 EVALUATION DE LA QUALITE DU MODELE AVEC LE JEU DE DONNEES TEST
rf.predictions=predict(custom_rf, newdata=testdata)
perf_custom_rf_testdata <- confusionMatrix(data=rf.predictions,reference=testdata$y,positive=NULL)

mat_conf_rf <- t(perf_custom_rf_testdata$table)
metric_overall_rf <- perf_custom_rf_testdata$overall
metric_byClass_rf <- perf_custom_rf_testdata$byClass

# 6. ANALYSE DE L'IMPORTANCE DES VARIABLES

rf.varImp <- varImp(custom_rf, scale=TRUE)
## Classement par ordre d'importance global
imp_globale <- sort(apply(rf.varImp$importance, 1,function(x){sqrt(sum(x^2))}), decreasing = TRUE)
## Variables les plus discriminantes pour les comportements les plus durs à discriminer 
imp_standing_none <- data.frame(name = rownames(rf.varImp$importance), imp = rf.varImp$importance$Standing...None)
imp_standing_none <- imp_standing_none[order(imp_standing_none$imp, decreasing = TRUE),]
top_20_standing_none  <- imp_standing_none[1:20,"name"]

imp_walking <- data.frame(name = rownames(rf.varImp$importance), imp = rf.varImp$importance$Walking)
imp_walking <- imp_walking[order(imp_walking$imp, decreasing = TRUE),]
top_20_walking  <- imp_walking[1:20,"name"]

# 7. GENERER LES RESULTATS
setwd("~/Formation P. Gaignon/AlgorithmesML/PartieApplications/Resultats")
save(mat_conf_rf, file = "mat_conf_rf.RData")
save(metric_overall_rf, file = "metric_overall_rf.RData")
save(metric_byClass_rf, file = "metric_byClass_rf.RData")
write.csv2(rf.varImp$importance, file = "varImp.rf.csv", dec = ".", sep = ";")


