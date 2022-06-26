#Telechargement et lecture des donnees. Creation de la variable projetstats 


projetstats <- read.csv("data/train.csv",header = TRUE) 

#on regarde la structure des donnees
str(projetstats)


#installation des differents packages necessaires au projet

#install.packages("disk.frame")
library(disk.frame)

#install.packages("parallel")
library(parallel)

#install.packages("dplyr")
library(dplyr)

#install.packages("Amelia")
#library(Amelia)

#consultation de l ensemble des variables
#missmap(train, col = c("black", "grey"))


#install.packages("tidyverse")
library("tidyverse")


#on simplifie les donnees

id <- projetstats$id

step<-projetstats$step

type <- factor(projetstats$type, levels=c(unique(projetstats$type)), labels=c("pay", "tra","deb","casi",'caso'))

amount<-round(projetstats$amount,0) #On arrondi la variable amount

naOrig<-projetstats$nameOrig

oldbalOrig <- round(projetstats$oldbalanceOrg,0) #On arrondi la variable oldbalanceOrg

nbOrig<- round(projetstats$newbalanceOrig,0) #On arrondi la variable newbalanceOrg

nameDest<-projetstats$nameDest

oldbalDest<-round(projetstats$oldbalanceDest,0) #On arrondi la variable oldbalanceDest

nwbalDest<-round(projetstats$newbalanceDest,0) #On arrondi la variable newbalanceDest

isFraud<-factor(projetstats$isFraud,levels=c(unique(projetstats$isFraud)), labels=c("nofraud", "fraud"))

isFlag<-factor(projetstats$isFlaggedFraud,levels=c(unique(projetstats$isFlaggedFraud)), labels=c("non_flag", "flag"))


df_projetdatas<- data.frame(id,step,type,amount,naOrig,oldbalOrig,nbOrig,nameDest,oldbalDest,nwbalDest,isFraud,isFlag)

#verification de df_projetdatas
df_projetdatas

#verification de la structure de df_projetdatas
str(df_projetdatas)



#FALSE permet de ne pas reutiliser les donnees dans les autres echantillons
#Creation de la dataframe train (60% des donnees de df_projetstats en echantillon aleatoire)
df_train <- collect(sample_frac(df_projetdatas, 0.6), replace = FALSE)


#Creation de la dataframe validation (20% des donnees de df_projetstats en echantillon aleatoire)
df_validation <- collect(sample_frac(df_projetdatas, 0.2), replace = FALSE)


#Creation de la dataframe test (20% des donnees de df_projetstats en echantillon aleatoire)
df_test <- collect(sample_frac(df_projetdatas, 0.2), replace = FALSE)


#On enleve la colonne iD car elle ne doit pas entrer en ligne de compte dans nos traitements
#On enleve egalement les colonnes naOrig et nameDest car notre machine ne nous permet pas de les prendre en compte
df_train2 <- select(df_train,step,type,amount,oldbalOrig,nbOrig,oldbalDest,nwbalDest,isFraud,isFlag)

df_validation2 <- select(df_validation,step,type,amount,oldbalOrig,nbOrig,oldbalDest,nwbalDest,isFraud,isFlag)

df_test2 <- select(df_test,step,type,amount,oldbalOrig,nbOrig,oldbalDest,nwbalDest,isFraud,isFlag)


#les valeurs de 'isFraud' et 'isFlaggedFraud' ne sont pas reellement des integer.
#Elles sont des variables qualitatives nominales. On les transforme alors en factor

df_train2$isFraud <- factor(df_train2$isFraud)
df_train2$isFlag <- factor(df_train2$isFlag)

df_validation2$isFraud <- factor(df_validation2$isFraud)
df_validation2$isFlag <- factor(df_validation2$isFlag)

df_test2$isFraud <- factor(df_test2$isFraud)
df_test2$isFlag <- factor(df_test2$isFlag)



#Statistiques descriptives et analyse des données

#afin dafficher la moyenne, le minimum, la maximum, la mediane
summary(df_train2)

#calcul de la variance et de l ecart type

sd(df_train2$amount)
sd(df_train2$oldbalOrig)
sd(df_train2$nbOrig)
sd(df_train2$oldbalDest)
sd(df_train2$nwbalDest)

'
environ

moyenne :
              amount     : 179975
              oldbalorig : 833033
              newbalorig : 854242
              oldbalDest : 1096387
              newbalDest : 1220980
              nofraud    : 2554513
              fraud      : 3261

ecart type :
              amount     : 610966
              oldbalorig : 2888151
              newbalorig : 2923890
              oldbalDest : 3356985
              newbalDest : 3634943
'

#install.packages(ggplot2)
library(ggplot2)
library(GGally)


#Les transactions frauduleuses sont signalees lorsque le montant de transfert est superieur a 200.000.
#Nous effectuons ici une verification dans l ensemble des donnees


verif_isFlag2 <-filter(df_projetdatas, isFlag=="flag")
#verif_isFlag2

verif_isFlag2_sup200 <- filter(df_projetdatas, amount > 200000 & isFlag=="flag")
#verif_isFlag2_sup200

verif_isFlag2_projetdata <-  filter(df_projetdatas, amount > 200000)
#verif_isFlag2_projetdata

'on a plus de 1.000.000 de lignes dans les donnees principales dont le montant de transfert
est superieur a 200.000$ et qui  n ont pas ete signale (isflag = noflag)




#Analyse des correlations entre les classes
ggcorr(df_train2,
       nbreaks = 5,
       label = TRUE,
       label_size = 3,
       color = "grey50")

'Nous observons une corrélation très forte entre oldbalanceDest et newbalanceDest (1)
                                           entre oldbalanceOrg et newbalanceOrig (1)
                une corrélation non négligeable enntre amount et newbalanceDest (0.5)'


#Par soucis de precaution, on analyse les correlations entre classes pour les donnees de validation et de test
'ggcorr(df_validation,
       nbreaks = 5,
       label = TRUE,
       label_size = 3,
       color = "grey50")

ggcorr(df_test,
       nbreaks = 5,
       label = TRUE,
       label_size = 3,
       color = "grey50")

elles sont similaires'



#On affiche un graphique representant le nombre d'operations financieres non frauduleuses et frauduleuses 
ggplot(df_train2, aes(x = isFraud)) +
  geom_bar(width=0.5, fill = "coral") +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  theme_classic()

'il y a 2.554.509 opérations financieres non frauduleuses (99.87% des donnees de df_train2)
      et 3265 opérations frauduleuses (0.13% des donnees de df_train2)'



#on compte le nombre de fraudes en fonction du nombre de fraudes signalées
ggplot(df_train2, aes(x = isFlag, fill = isFraud)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()


#Graphique sur le nombre d'operations non frauduleuses et frauduleuses par type
ggplot(df_train2, aes(x = isFraud, fill = type)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()

'On observe que les plus grand types d operations non frauduleuses sont le Cash_out (35.15%) et le Payment (33.88%)
 Il y a 1620 operations frauduleuses effectuees par Cash_out et 1645 operantions frauduleuses effectuees par Transfert'


'df_validation Il y a 577 operations frauduleuses effectuees par Cash_out et 577 operantions frauduleuses effectuees par Transfert'

'df_test Il y a 564 operations frauduleuses effectuees par Cash_out et 571 operantions frauduleuses effectuees par Transfert'




#test

library(tidyverse)


#graphique representant le nombre de non Fraude par type
df_traingraph <- df_train %>% filter(isFraud=="nofraud")

ggplot(df_traingraph, aes(x = type, fill = isFraud)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()



#graphique representant le nombre de Fraud par type
df_traingraph <- df_train %>% filter(isFraud=="fraud")

ggplot(df_traingraph, aes(x = type, fill = isFraud)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()



#arbre de decision

library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)



set.seed(2022)
dtree <- rpart(isFraud ~ ., data = df_train2, method = 'class')

#visualisation de l arbre de decision
rpart.plot(dtree, extra = 106)

#donnees supplementaire sur l arbre de decision
summary(dtree)


#on trace un graphique qui permet d indiquer le bon nombre de feuilles concernant notre arbre de decision
#ce graphique evalue les performances par validation croisee
plotcp(dtree)


'nous remarquons par lecture graphique que les performances de notre arbre sont bonnes lorsque le nbre de feuilles depasse 8
C est le cas de notre graphique'
'sinon nous aurions cherche l endroit qui minimise l erreur afin de le faire correspondre avec le nombre
de feuilles necessaires a notre arbre pour eviter le surapprentissage'

#correction concernant le nombre de feuilles optimales
dtree_simple <- prune(dtree,cp=0.013)



#install.packages("MLmetrics")
library(MLmetrics)




y_pred <- predict(dtree, df_validation2, type = 'class')
y_true <- df_validation2$isFraud

dtree_precision <- Precision(y_true, y_pred, positive = "fraud")
dtree_recall <- Recall(y_true, y_pred, positive = "fraud")
dtree_f1 <- F1_Score(y_true, y_pred, positive = "fraud")

dtree_auc <- AUC(y_true, y_pred)

paste0("Precision: ", dtree_precision)

paste0("Recall: ", dtree_recall)

paste0("F1 Score: ", dtree_f1)

paste0("AUC: ", dtree_auc)



#install.packages("ROSE")
library(ROSE)


roc.curve(y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)


'Area under the curve (AUC): 0.983'

#on affine notre arbre

set.seed(2022)

control <- rpart.control(minsplit = 9,
                         minbucket = 4,
                         maxdepth = 8,
                         cp = 0)
dtree_tuned_fit <- rpart(isFraud ~ ., data = df_train2, method = 'class', control = control)
summary(dtree_tuned_fit)

y_pred <- predict(dtree_tuned_fit, df_validation2, type = 'class')

dtree_tuned_fit_precision <- Precision(y_true, y_pred, positive = "fraud")
dtree_tuned_fit_recall <- Recall(y_true, y_pred, positive = "fraud")
dtree_tuned_fit_f1 <- F1_Score(y_true, y_pred, positive = "fraud")
dtree_tuned_fit_auc <- AUC(y_true, y_pred)

paste0("Precision: ", dtree_tuned_fit_precision)

paste0("Recall: ", dtree_tuned_fit_recall)

paste0("F1 Score: ", dtree_tuned_fit_f1)

paste0("AUC: ", dtree_tuned_fit_auc)

rpart.plot(dtree_tuned_fit, extra = 106)

roc.curve(y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)

'Area under the curve (AUC): 0.993'


#on trace un graphique afin d evaluer les performances par validation croisee
plotcp(dtree)





#regression logistique


logreg <- glm(formula = isFraud ~ ., data = df_train2, family = "binomial")
summary(logreg)


'toutes nos variables sont significatives'


'Rstudio nous signale qu il na pas converge dans le calcul de la regression logistique'



'Les resultats nous indiquent que toutes les variables sont significatives :
            -plus le montant de la transaction est eleve, moins la transaction a de risque detre frauduleuse
            -plus le solde bancaire du compte d origine est eleve, plus la transaction a de risque detre frauduleuse
            -plus le nouveau solde bancaire du compte d origine est eleve, moins la transaction a de risque detre frauduleuse
            -plus l ancien solde bancaire du compte destinataire est eleve, moins la transaction a de risque detre frauduleuse
            -plus le nouveau solde bancaire du compte destinataire est eleve, plus la transaction a de risque detre frauduleuse
            -lorsque la transaction est signalee, il y a plus de chance pour que l operation soit frauduleuse
            '



LR <- logreg$null.deviance - logreg$deviance
p <- logreg$df.null - logreg$df.residual
pchisq(LR, p, lower.tail = F)


aic_logreg <- AIC(logreg)
aic_logreg

bic_logreg <- BIC(logreg)
bic_logreg




y_true <- df_validation2$isFraud
y_pred <- predict(logreg, df_validation2, type = 'response')
y_pred <- as.factor(ifelse(y_pred > 0.5, "fraud", "nofraud"))



logreg_precision <- Precision(y_true, y_pred, positive = "fraud")
logreg_recall <- Recall(y_true, y_pred, positive = "fraud")
logreg_f1 <- F1_Score(y_true, y_pred, positive = "fraud")
logreg_auc <- AUC(y_true, y_pred)

paste0("Precision: ", logreg_precision)

paste0("Recall: ", logreg_recall)

paste0("F1 Score: ", logreg_f1)

paste0("AUC: ", logreg_auc)

roc.curve(y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)




#Nous decidons alors de standardiser nos variables


set.seed(2022)


df_train2_2 <- mutate_if(df_train2,
                          is.numeric,
                          list(~as.numeric(scale(.))))


df_validation2_2 <- mutate_if(df_validation2,
                                is.numeric,
                                list(~as.numeric(scale(.))))


#les ecart types sont egales a 1
apply(df_train2_2, MARGIN = 2, FUN = sd)



#regression logistique avec les donnees standardisees

logreg <- glm(formula = isFraud ~ ., data = df_train2_2, family = "binomial")
summary(logreg)


#analyse de la significativite globale


LR <- logreg$null.deviance - logreg$deviance
p <- logreg$df.null - logreg$df.residual
pchisq(LR, p, lower.tail = F)


aic_logreg <- AIC(logreg)
aic_logreg

bic_logreg <- BIC(logreg)
bic_logreg




y_true <- df_validation2_2$isFraud
y_pred <- predict(logreg, df_validation2_2, type = 'response')
y_pred <- as.factor(ifelse(y_pred > 0.5, "fraud", "nofraud"))



logreg_fit_precision <- Precision(y_true, y_pred, positive = "fraud")
logreg_fit_recall <- Recall(y_true, y_pred, positive = "fraud")
logreg_fit_f1 <- F1_Score(y_true, y_pred, positive = "fraud")
logreg_fit_auc <- AUC(y_true, y_pred)

paste0("Precision: ", logreg_fit_precision)

paste0("Recall: ", logreg_fit_recall)

paste0("F1 Score: ", logreg_fit_f1)

paste0("AUC: ", logreg_fit_auc)

roc.curve(y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)

'Area under the curve (AUC): 0.912'




#classifieur bayesien naif


#install.packages("e1071")
library(e1071)

set.seed(2022)
nbClassifier <- naiveBayes(isFraud ~ step + type + amount + oldbalOrig + oldbalDest + nwbalDest + isFlag, data = df_train2)
y_pred <- predict(nbClassifier, df_validation2)
y_true <- df_validation2$isFraud

nbClassifier_precision <- Precision(y_true, y_pred, positive = "fraud")
nbClassifier_recall <- Recall(y_true, y_pred, positive = "fraud")
nbClassifier_f1 <- F1_Score(y_true, y_pred, positive = "fraud")
nbClassifier_auc <- AUC(y_true, y_pred)

paste0("Precision: ", nbClassifier_precision)

paste0("Recall: ", nbClassifier_recall)

paste0("F1 Score: ", F1_Score(y_true, y_pred, positive = "fraud"))

paste0("AUC: ", nbClassifier_auc)

roc.curve(y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)

'Area under the curve (AUC): 0.528'

install.packages("mosaic")
library(mosaic)



#random forest

install.packages("randomForest")
library(randomForest)


set.seed(42)

modele_random <- randomForest (isFraud ~ ., ntree = 20, data = df_train2)

#modele_random

summary(modele_random)

#Graphique pour mesure l importance des variables
#varImpPlot(modele_random)




y_true <- df_validation2$isFraud
y_pred <- predict(modele_random, df_validation2, type = 'response')
y_pred <- predict(modele_random, df_validation2)


modele_random_precision <- Precision(y_true, y_pred, positive = "fraud")
modele_random_recall <- Recall(y_true, y_pred, positive = "fraud")
modele_random_f1 <- F1_Score(y_true, y_pred, positive = "fraud")
modele_random_auc <- AUC(y_true, y_pred)

paste0("Precision: ", modele_random_precision)

paste0("Recall: ", modele_random_recall)

paste0("F1 Score: ", F1_Score(y_true, y_pred, positive = "fraud"))

paste0("AUC: ", modele_random_auc)

roc.curve(y_pred, y_true, plotit = TRUE, add.roc = FALSE, 
          n.thresholds=100)

'Area under the curve (AUC): 0.994'





#comparaison et sauvegarde du modele




dtree_perfs <- list(precision = dtree_precision,
                    recall = dtree_recall,
                    f1_score = dtree_f1,
                    auc = dtree_auc)



dtree_tuned_fit_perfs <- list(precision = dtree_tuned_fit_precision,
                              recall = dtree_tuned_fit_recall,
                              f1_score = dtree_tuned_fit_f1,
                              auc = dtree_tuned_fit_auc)


logreg_perfs <- list(precision = logreg_precision,
                     recall = logreg_recall,
                     f1_score = logreg_f1,
                     auc = logreg_auc)


logreg_fit_perfs <- list(precision = logreg_fit_precision,
                     recall = logreg_fit_recall,
                     f1_score = logreg_fit_f1,
                     auc = logreg_fit_auc)


nbClassifier_perfs <- list(precision = nbClassifier_precision,
                           recall = nbClassifier_recall,
                           f1_score = nbClassifier_f1,
                           auc = nbClassifier_auc)


modele_random_perfs <- list(precision = modele_random_precision,
                           recall = modele_random_recall,
                           f1_score = modele_random_f1,
                           auc = modele_random_auc)

perfs <- as.data.frame(t(do.call(rbind, Map(data.frame,
                                            dtree = dtree_perfs,
                                            dtree_tuned = dtree_tuned_fit_perfs,
                                            logreg = logreg_perfs,
                                            logreg_fit = logreg_fit_perfs,
                                            nbClassifier = nbClassifier_perfs,
                                            modele_random = modele_random_perfs))))






attach(perfs)
perfs[order(-f1_score),]







detach(perfs)

#sauvegarde du modele_random

saveRDS(dtree_tuned_fit, "modele_random.rds")
modele_random <- readRDS("modele_random.rds")



#Test du modele

y_pred <- predict(modele_random, df_test2, type = 'class')
y_true <- df_test2$isFraud

modele_random_precision <- Precision(y_true, y_pred, positive = "fraud")
modele_random_recall <- Recall(y_true, y_pred, positive = "fraud")
modele_random_f1 <- F1_Score(y_true, y_pred, positive = "fraud")
modele_random_auc <- AUC(y_true, y_pred)

paste0("Precision: ", modele_random_precision)

paste0("Recall: ", modele_random_recall)

paste0("F1 Score: ", modele_random_f1)

paste0("AUC: ", modele_random_auc)



