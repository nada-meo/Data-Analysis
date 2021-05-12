#############################################################################################
######################################## Projet ACP #########################################
#############################################################################################

## Lecture des données
library("readxl")
data=read_excel("./houses.xlsx")
data
nrow(data)
str(data)

## Elimination des valeurs manquantes
library(tidyr)
data<-data %>% drop_na()
sum(is.na(data))

## Application de l'ACP
library(factoextra)
library(FactoMineR)
# Centrer et réduire les variables
data_cr<-scale(data, center=T, scale=T)
# ACP normée
res.pca<-PCA(data_cr,ncp=5,axes=c(1,2))
attributes(res.pca)


## Calcul de l'indice KMO et des MSAi
library(psych)
KMO(cor(data)) #KMO=0.64
# Conclusion : Avec un KMO = 0.64, notre échantillon est plutôt « médiocre »

## Les valeurs propres, le pourcentage d’inertie de chaque valeur propre et le cumul 
#des pourcentages d’inertie
res.pca$eig

## Le graphique des valeurs propres.
plot(1:12, res.pca$eig[,1], type="b", ylab="Valeurs propres", xlab="Composantes",
     main="graphique des valeurs propres")
sum(res.pca$eig[,1])

# La dimension du sous espace 
vp<-res.pca$eig[,1]
inertie.expl <- rep(0,times=8)
for (k in 2:8){
  clus <- kmeans(vp,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss
}
#graphique
plot(1:8,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquee")
write.table(vp,"Valeurs propres.csv",sep=",",col.names=TRUE,dec=',', row.names=FALSE)
#Règle inert intra/inert tot <0.05 : 3 axes

#######################################
######### Nuage des variables #########
#######################################

## Qualité de representation des variables : cos2 des variables sur le sous espace de 3 dims
res.pca$var$cos2[,0:3]
# Cos2 total des variables sur Dim.1, Dim.2 et Dim3
fviz_cos2(res.pca, choice = "var", axes = 1:3)


## La contribution des variables dans chaque axe du sous espace
cont.var<-res.pca$var$contrib[,0:3]
cont.var

## CAH au tableau des contributions des variables 
res.hcpc.var<-HCPC(as.data.frame(cont.var))
res.hcpc.var$desc.var


## Nuage des variables sur les 2 premiers axes (1er plan factoriel)
fviz_pca_var(res.pca, col.var = "black")
# ou plot(res.pca, choix = "var", autoLab = "yes")
res.pca$var$cor




#######################################
######### Nuage des individus #########
#######################################

## Cos2 des individus sur le sous espace (les 3 premiers axes)
res.pca$ind$cos2[,0:3]
fviz_cos2(res.pca, choice = "ind", axes = 1:3)



# Les individus bien représentées ayant un cos2 > 0.75
cos.ind <- rowSums(res.pca$ind$cos2[,0:3])
cos.ind[cos.ind>=0.75] 
which(cos.ind>=0.75)
# 3  4  5  6 33 34 35 37 40 41 42 43 44 45 46 47 48 58 59 60 65 66 67 68 72 81 82 83 84 85
# 86 87 88 89 90 92 93 

# Les individus moyennement  représentées ayant un 0.25 < cos2 < 0.75
cos.ind[cos.ind<0.75 & cos.ind>=0.25] 
which(cos.ind<0.75 & cos.ind>=0.25)
# 1  2  7  8  9 10 12 15 17 18 19 20 21 22 23 24 25 27 28 29 30 31 32 36 38 39 49 50 51 52 
# 53 54 55 56 57 61 62 63 69 70 73 74 75 76 77 78 79 80 91

# Les individus faiblement  représentées ayant un cos2 < 0.25
cos.ind[cos.ind<0.25] 
which(cos.ind<0.25)
# 11 13 14 16 26 64 71

## Contribution des individus dans chaque axe du sous espace
cont.ind<-res.pca$ind$contrib[,0:3]
cont.ind

##CAH au tableau des contributions des individus
res.hcpc.ind<-HCPC(as.data.frame(cont.ind))
res.hcpc.ind$desc.var


## Nuage des variables sur les 2 premiers axes (1er plan factoriel)
fviz_pca_ind (res.pca, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE # Évite le chevauchement de texte
)


