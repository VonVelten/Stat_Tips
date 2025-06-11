#============================================================================================================ #
#================================================== PROG ID ================================================= #
#                                                                                                             #
# PROJET..................= VAGABOND                                                                          #
# NOM SCRIPT.............= 2020_FormationPG_SVM.R                                                             #
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
# OBJECTIFS: - Trouver les critères C et sigma qui optimisent le SVM (fonction de noyau radiale)              #
#            - Evaluer la qualité du modèle sur la base d'un jeu de données test                              #
#                                                                                                             #                                                                       
# PRINCIPALES ETAPES DU PROGRAMME:                                                                            #
#                                                                                                             #
#     -  IMPORTER LE JEU DE DONNEES                                                                           #
#     -  PREPARATION DE LA TABLE DE DONNEES POUR SVM                                                          #                                                                                                                                    
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
library(kernlab)

# 1. IMPORTER LES JEUX DE DONNEES
setwd("~/Formation P. Gaignon/ValidationModeleML")
load("2020_AJUSTSET.RData")
load("2020_TESTSET.RData")

# 3.PREPARATION DE LA TABLE DE DONNEES POUR SVM
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
svm.seed=111
set.seed(svm.seed)
fitControl <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid", allowParallel=TRUE)
mygrid <- expand.grid(sigma=seq(0.01,0.1,by=0.01), C=c(1,2,5,10,50,100,120,140,160,180,200,250,300,350,400,450,500,1000)) # + valeurs de C élevées --> + marge est stricte
metric <- "Accuracy"

## 4.2 AJUSTEMENT DU MODELE SVM
# ptm <- proc.time()
# svm.fit <- train(y ~ ., data=traindata, method="svmRadial", trControl=fitControl,metric=metric,
#                  preProcess=NULL, tuneGrid=mygrid)  
# svm.time <- proc.time()-ptm

setwd("~/Formation P. Gaignon/AlgorithmesML/PartieApplications/SVM")
load(file = "2019-04-21_model_SVM_TFEN10RECV90NORM.RData")

## 4.3 PARAMETRES DU MODELE AJUSTE
svm.fit$bestTune
x11()
plot(svm.fit)

# 5. EVALUATION DES PERFORMANCES DE ML 

## 5.1 PERFORMANCE DU MODELE SUR JEU DE DONNEES D'ENTRAINEMENT
svm.predictions=predict(svm.fit, newdata=traindata)
perf_svm_fit_traindata <- confusionMatrix(data=svm.predictions,reference=traindata$y,positive=NULL)

## 5.2 PERFORMANCE DES DIFFERENTS MODELES TESTES PAR VALIDATION CROISEE

perf_svm_fit_cv <- subset(svm.fit$results, C == svm.fit$bestTune$C & sigma == svm.fit$bestTune$sigma, c("Accuracy", "Kappa"))

## 5.3 EVALUATION DE LA QUALITE DU MODELE AVEC LE JEU DE DONNEES TEST
svm.predictions=predict(svm.fit, newdata=testdata)
perf_svm_fit_testdata <- confusionMatrix(data=svm.predictions,reference=testdata$y,positive=NULL)

mat_conf_svm <- t(perf_svm_fit_testdata$table)
metric_overall_svm <- perf_svm_fit_testdata$overall
metric_byClass_svm <- perf_svm_fit_testdata$byClass

# 6. ANALYSE DE L'IMPORTANCE DES VARIABLES

svm.varImp <- varImp(svm.fit, scale=TRUE)
## Classement par ordre d'importance global
imp_globale <- sort(apply(svm.varImp$importance, 1,function(x){sqrt(sum(x^2))}), decreasing = TRUE)
## Variables les plus discriminantes pour les comportements les plus durs à discriminer 
imp_standing_none <- data.frame(name = rownames(svm.varImp$importance), imp = svm.varImp$importance$Standing...None)
imp_standing_none <- imp_standing_none[order(imp_standing_none$imp, decreasing = TRUE),]
top_20_standing_none  <- imp_standing_none[1:20,"name"]

imp_walking <- data.frame(name = rownames(svm.varImp$importance), imp = svm.varImp$importance$Walking)
imp_walking <- imp_walking[order(imp_walking$imp, decreasing = TRUE),]
top_20_walking  <- imp_walking[1:20,"name"]

# 7. GENERER LES RESULTATS
setwd("~/Formation P. Gaignon/AlgorithmesML/PartieApplications/Resultats")
save(mat_conf_svm, file = "mat_conf_svm.RData")
save(metric_overall_svm, file = "metric_overall_svm.RData")
save(metric_byClass_svm, file = "metric_byClass_svm.RData")
write.csv2(svm.varImp$importance, file = "varImp.svm.csv", dec = ".", sep = ";")
