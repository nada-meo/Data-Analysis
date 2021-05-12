#############################################################################################
################################### Projet Classification ###################################
#############################################################################################

#Lecture des données
library("readxl")
data=read_excel("./data.xlsx")
nrow(data)
str(data)
## Elimination des valeurs manquantes
library(tidyr)
data<-data %>% drop_na()
sum(is.na(data))

## Question 1
# Centrer-Réduire les variables
data_cr<-scale(data, center=T, scale=T)
## Kmeans avec nbr de classes k = 1 à 10
inertie.expl <- rep(0, times=10) # la proportion d'inertie expliquée
for (k in 1:10){
  clus <- kmeans(data_cr, centers=k, nstart=5)
  inertie.expl[k] <- (clus$betweenss/clus$totss)
}

## Question 2
plot(1:10, inertie.expl, type="b", xlab="Nb. de classes k ", ylab="% inertie expliquée")

# Kmeans with k=3
res.kmeans <- kmeans(data_cr, centers=3, nstart=5)

## Question 3
# Les données ont été préalablement centrées et réduites
library(FactoMineR)
library("factoextra")
# CAH
data_cr<-as.data.frame(data_cr)
res.hcpc<-HCPC(data_cr, nb.clust=-1)

# Corrélation entre la variable de classification et les variables quantitatives
# & La description des classes retenues par variables
res.hcpc$desc.var


# Les taux d’inertie : Inertie Inter/Inertie total, avant et après la consolidation de la CAH
# Avant consolidation
res.hcpc$call$bw.before.consol #5.17767
# Après consolidation
res.hcpc$call$bw.after.consol #5.701025


## Question 4 - Comparaison entre kmeans et CAH
# Qualité de partition 
#kmeans : 530.7026 
res.kmeans$betweenss
#CAH : 5.17767
res.hcpc$call$bw.before.consol



