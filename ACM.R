#############################################################################################
######################################## Projet ACM #########################################
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

install.packages('matlab')
library(matlab)

## Discrétisation des variables (On a choisi 3 modalités pour toutes les variables)
#vecteur des taux d'inertie expliquée
inertie.expl <- rep(0, times=12)
#matrice contenant les classes de chaques variables
res.kmeans <- zeros (93, 12)

#kmeans
for (var in 1:12){
  clus <- kmeans(data[,var], centers=3, nstart=5)
  res.kmeans[,var] <- clus$cluster
  inertie.expl[var] <- (clus$betweenss/clus$totss)
}
inertie.expl # Toutes les valeurs du taux d'inertie sont >0.5

#create dataframe containing results
new_data <- as.data.frame(res.kmeans)
install.packages('tidyverse')
library(tidyverse)
#rename variables
new_data <- new_data %>% 
  rename(
    CRIM_1 = V1,
    INDUS_1 = V2,
    NOX_1 = V3,
    RM_1 = V4,
    AGE_1 = V5,
    DIS_1 = V6,
    RAD_1 = V7,
    TAX_1 = V8,
    PTRATIO_1 = V9,
    B_1 = V10,
    LSTAT_1 = V11,
    MEDV_1 = V12
  )

data_disc <- cbind(data, new_data)
#Taux d'inertie expliquée dans la derniere ligne
data_disc[94,] = c(rep(0, times=12), inertie.expl)

#Export into Excel file
install.packages('writexl')
library("writexl")
write_xlsx(data_disc, "./data_disc.xlsx")

#Convert variables from numeric to factor
col_names <- names(new_data)
new_data[,col_names] <- lapply(new_data[,col_names] , factor)

## Construction du tableau disjonctif complet
library(FactoMineR)
tableau <- tab.disjonctif(new_data) 
tableau
## Fréquence des modalités
propmod = apply(tableau, 2, sum)/(nrow(tableau))
propmod
#Présence d’une modalité rare (fréquence < 0.01)
propmod[propmod<=0.01] #Absence de modalité rare

## ACM
res.mca <- MCA(new_data,ncp=8,axes=c(1,2))
#nbr d’individus
length(res.mca$call$marge.row) #93
#nbr de variables 
length(res.mca$call$quali) #12
#nbr de modalités
length(res.mca$call$marge.col) #36

## les valeurs propres, le pourcentage d’inertie de chaque valeur propre et le cumul des
# pourcentages d’inertie
res.mca$eig

## Graphique des valeurs propres
plot(1:24,res.mca$eig[,1], type="b", ylab="Valeurs propres", xlab="Composantes",
     main="graphique des valeurs propres")


#######################################
######### Nuage des Modalités #########
#######################################

## Cos2 des modalités sur le sous espace
res.mca$var$cos2[,1:5]
print(t(apply(res.mca$var$cos2[,1:5],1,cumsum)),digit=2)
library("factoextra")
fviz_cos2(res.mca, choice = "var", axes = 1:5)

# Les modalités bien représentées ayant un cos2 > 0.75
cos.mod <- rowSums(res.mca$var$cos2[,1:5])
cos.mod[cos.mod>=0.75] 
which(cos.mod>=0.75)

# CRIM_1_1, CRIM_1_2, CRIM_1_3, NOX_1_1, NOX_1_2, NOX_1_3, DIS_1_1, TAX_1_3, PTRATIO_1_2,
# LSTAT_1_1 

# Les modalités moyennement  représentées ayant un 0.25 < cos2 < 0.75
cos.mod[cos.mod<0.75 & cos.mod>=0.25] 
which(cos.mod<0.75 & cos.mod>=0.25)

# Les modalités faiblement  représentées ayant un cos2 < 0.25
cos.mod[cos.mod<0.25] 
which(cos.mod<0.25)
# AGE_1_3

## Contribution des modalités dans chaque axe du sous espace
cont.mod<-res.mca$var$contrib[,1:5]
cont.mod

##CAH au tableau des contributions des individus
res.hcpc.mod<-HCPC(as.data.frame(cont.mod))
res.hcpc.mod$desc.var



## Nuage des modalités sur les 2 premiers axes (1er plan factoriel)
plot(res.mca, 
     invisible = c("ind", "quali.sup", "quanti.sup"),
     cex = 0.8,
     autoLab = "yes")


#######################################
######### Nuage des individus #########
#######################################

## Cos2 des individus sur le sous espace (les 5 premiers axes)
res.mca$ind$cos2[,1:5]
fviz_cos2(res.mca, choice = "ind", axes = 1:5)


# Les individus bien représentées ayant un cos2 > 0.75
cos.ind.mca <- rowSums(res.mca$ind$cos2[,1:5])
cos.ind.mca[cos.ind.mca>=0.75] 
which(cos.ind.mca>=0.75)
# 3  4  5 15 20 21 22 23 29 34 47 48 49 83 85 89 90 91 92 93

# Les individus moyennement  représentées ayant un 0.25 < cos2 < 0.75
cos.ind.mca[cos.ind.mca<0.75 & cos.ind.mca>=0.25] 
which(cos.ind.mca<0.75 & cos.ind.mca>=0.25)
# 2  6  7  8  9 10 11 12 13 14 16 17 18 19 24 25 26 27 28 30 32 33 35 36 37 38 39 40 41 42
# 43 44 45 46 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75
# 76 77 78 79 80 81 82 84 86 87 88

# Les individus faiblement  représentées ayant un cos2 < 0.25
cos.ind.mca[cos.ind.mca<0.25] 
which(cos.ind.mca<0.25)
# 1 31

## Contribution des individus dans chaque axe du sous espace
cont.ind.mca<-res.mca$ind$contrib[,1:5]
cont.ind.mca

##CAH au tableau des contributions des individus
res.hcpc.ind.mca<-HCPC(as.data.frame(cont.ind.mca))
res.hcpc.ind.mca$desc.var


#######################################
######### Nuage des variables #########
#######################################

## Calcul des coefficients de corrélation des variables avec les projections sur les axes
res.mca$var$eta2
## Graphique des coefficients de corrélation des variables avec les facteurs du 1er plan 
# factoriel
plot(res.mca$var$eta2[,1:2],choix="var")  






