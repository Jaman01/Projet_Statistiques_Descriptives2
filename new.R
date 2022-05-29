#Téléchargement et lecture des donnees. Création de la dataframe df_projetstats 

df_projetstats <- read.csv("data/train.csv")
df_projetstats
 


#installation des différents packages nécessaires au projet

#install.packages("disk.frame")
#library(disk.frame)

#install.packages("parallel")
#library(parallel)

#install.packages("Amelia")

#install.packages("tidyverse")
#library("tidyverse")

#library(Amelia)
#missmap(train, col = c("black", "grey"))



#FALSE permet de ne pas réutiliser les donnees dans les autres echantillons
#Creation de la dataframe train (60% des données de df_projetstats en échantillon aléatoire)
df_train <- collect(sample_frac(df_projetstats, 0.6), replace = FALSE)

#Lecture de df_train
df_train

#Creation de la dataframe validation (20% des données de df_projetstats en échantillon aléatoire)
df_validation <- collect(sample_frac(df_projetstats, 0.2), replace = FALSE)

#Lecture de df_validation
df_validation

#Creation de la dataframe test (20% des données de df_projetstats en échantillon aléatoire)
df_test <- collect(sample_frac(df_projetstats, 0.2), replace = FALSE)

#Lecture de df_train
df_test



#library(dplyr)

#On enlève la colonne iD car elle ne nous servira pas dans le cadre de l'étude des données
df_train <- select(df_train, step, type, amount,oldbalanceOrg, newbalanceOrig, oldbalanceDest, newbalanceDest,isFraud,isFlaggedFraud)
df_train

df_validation <- select(df_validation,step, type, amount,oldbalanceOrg, newbalanceOrig, oldbalanceDest, newbalanceDest,isFraud,isFlaggedFraud)
df_validation

df_test <- select(df_test, step, type, amount,oldbalanceOrg, newbalanceOrig, oldbalanceDest, newbalanceDest,isFraud,isFlaggedFraud)
df_test


#On observe la structure des données
str(df_projetstats)

#les valeurs de 'isFraud' et 'isFlaggedFraud' ne sont pas reellement des integer.
#Elles sont des variables qualitatives nominales

df_train$isFraud <- factor(df_train$isFraud)
df_train$isFlaggedFraud <- factor(df_train$isFlaggedFraud)

df_validation$isFraud <- factor(df_validation$isFraud)
df_validation$isFlaggedFraud <- factor(df_validation$isFlaggedFraud)

df_test$isFraud <- factor(df_test$isFraud)
df_test$isFlaggedFraud <- factor(df_test$isFlaggedFraud)






#Début de l'analyse des données

install.packages(ggplot2)
library(ggplot2)
library(GGally)


#Analyse des correlations entre les classes
ggcorr(df_train,
       nbreaks = 5,
       label = TRUE,
       label_size = 3,
       color = "grey50")

'Nous observons une corrélation très forte entre oldbalanceDest et newbalanceDest (1)
                                           entre oldbalanceOrg et newbalanceOrig (1)
                une corrélation non négligeable en amount et newbalanceDest (0.5)'


#Par soucis de précaution, on analyse les correlations entre classes pour les donnees de validation et de test
ggcorr(df_validation,
       nbreaks = 5,
       label = TRUE,
       label_size = 3,
       color = "grey50")



ggcorr(df_test,
       nbreaks = 5,
       label = TRUE,
       label_size = 3,
       color = "grey50")

'elles sont similaires'



#On affiche un graphique représentant le nombre d'operations financieres non frauduleuses et frauduleuses 
ggplot(df_train, aes(x = isFraud)) +
  geom_bar(width=0.5, fill = "coral") +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  theme_classic()

'il y a 2.554.509 opérations financieres non frauduleuses (99.87% des donnees de df_train)
      et 3265 opérations frauduleuses (0.13% des donnees de df_train)'



#Graphique du nombre d'operations financieres non frauduleuses et frauduleuses pour df_validation et df_test
ggplot(df_validation, aes(x = isFraud)) +
  geom_bar(width=0.5, fill = "coral") +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  theme_classic()

'df_validation : opérations financieres non frauduleuses : 99.86%
                 opérations frauduleuses : 0.13%'



ggplot(df_test, aes(x = isFraud)) +
  geom_bar(width=0.5, fill = "coral") +
  geom_text(stat='count', aes(label=stat(count)), vjust=-0.5) +
  theme_classic()

'df_test : opérations financieres non frauduleuses : 99.86%
                 opérations frauduleuses : 0.13%'



#comptage du nombre de fraudes en fonction du nombre de fraudes signalées
ggplot(df_train, aes(x = isFlaggedFraud, fill = isFraud)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()





ggplot(df_validation, aes(x = isFraud, fill = type)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()

ggplot(df_test, aes(x = isFraud, fill = isFlaggedFraud)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()



#Graphique sur le nombre d'operations non frauduleuses et frauduleuses par type
ggplot(df_train, aes(x = isFraud, fill = type)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()

'On observe que les plus grand types d operations non frauduleuses sont le Cash_out (35.15%) et le Payment (33.88%)
 Il y a 1620 operations frauduleuses effectuees par Cash_out et 1645 operantions frauduleuses effectuees par Transfert'



#Graphique du nombre d'operations financieres non frauduleuses et frauduleuses pour df_validation et df_test
ggplot(df_validation, aes(x = isFraud, fill = type)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()

'Il y a 577 operations frauduleuses effectuees par Cash_out et 577 operantions frauduleuses effectuees par Transfert'



ggplot(df_test, aes(x = isFraud, fill = type)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()

'Il y a 564 operations frauduleuses effectuees par Cash_out et 571 operantions frauduleuses effectuees par Transfert'






# Discretisation
'df_train$Discretized.amount <- cut(df_train$amount, c(0, 500000, 1000000, 1500000, 2000000, 2500000, 3000000, 3500000, 4000000, 4500000))

ggplot(df_train, aes(x = Discretized.amount, fill = isFraud)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', aes(label=stat(count)), position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()'





#test

library(tidyverse)

df_train2 <- df_train %>% filter(isFraud==1)

df_train2
ggplot(df_train2, aes(x = type, fill = isFraud)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()
""

df_validation2 <- df_validation %>% filter(isFraud==1)

df_validation2
ggplot(df_validation2, aes(x = type, fill = isFraud)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()
""

df_test2 <- df_test %>% filter(isFraud==1)

df_test2
ggplot(df_test2, aes(x = type, fill = isFraud)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1), vjust=-0.5)+
  theme_classic()



#test par rapport transer et cashout
ggplot(df_test, aes(x = isFraud, fill = type)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count',
            aes(label=stat(count)), 
            position = position_dodge(width=1), 
            vjust=-0.5)+
  theme_classic()




#Filtre par rapport transer et cashout (isFRAUD = 1)

df_train3 <- df_train2 %>% filter(!is.na("CASH_IN"))
df_train3 <- df_train2 %>% filter(!is.na("CASH_OUT"))
df_train3 <- df_train2 %>% filter(!is.na("DEBIT"))

df_train3

ggplot(df_train3, aes(x = isFraud, fill = type)) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count',
            aes(label=stat(count)), 
            position = position_dodge(width=1), 
            vjust=-0.5)+
  theme_classic()


#compter le nombre de fois o? on retrouve un nameOrig

df_train_count <- df_projetstats

data.frame(table(df_train_count$nameOrig))


lien pourKaggle
'https://www.kaggle.com/code/imanelmountasser/d-tection-de-fraude/notebook'












