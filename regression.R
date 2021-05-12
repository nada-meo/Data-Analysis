#############################################################################################
################################### Projet regression ###################################
#############################################################################################


## Lecture des données
library("readxl")
library(lmtest)
data=read_excel("data.xlsx")


#calcul du modele de regression
modele<-lm(MEDV~.,data=data)
#variables explicatives non significatives ? + R2 et R_ajus2 + fisher
summary(modele)
#methode de selection step 
mod_step= step(modele)
#modele1<-lm(MEDV ~ CRIM + INDUS + NOX + RM + AGE + TAX + B,data=data)
plot(resid(mod_step),predict(mod_step))

#test d'homoscedasticite
plot(mod_step)
bptest(mod_step)

#Shapiro
residus<-residuals(mod_step)
shapiro.test(residus) 
#ks test
ks.test(resid(mod_step),pnorm)

#valeurs aberrantes
sd=sqrt(deviance(mod_step)/df.residual(mod_step))
#sd = 2.269022
y = as.matrix(data[c("MEDV")])
sd
abr=abs(y-predict(mod_step))/sd
plot(abr)
abline(h=2)


#procédure de sélection des variables explicatives basée sur le coefficient de corrélation du résidu et d’une variable explicative 
lis <- list("CRIM","ZN", "INDUS","CHAS","NOX","AGE", "DIS",	"RAD","TAX","PTRATIO","B","LSTAT")

#nous calculons les coefficients de correlation entre y et chacune des autres variables
curr = NULL
maximu = 0
res =0
for (element in lis) {
  current = as.matrix(data[c(element)])
  #calcul du coeff de correlation entre y et les varibles de la liste
  x = abs(cor(current, y,method = c("pearson")))
  print(x)
  if(x>maximu){
    #choix du max
    maximu = x
    #on garde la variable donc le coeff est le plus grand
    curr = element
    #on fait le test de significativite
    res<-cor.test(current,y, method="pearson")
  }
}
#on affiche la variable qui entrera
print(curr)
#test de significativite
res


#On remarque que LSTAT est la variable donc le coeff de correlation est le plus grand et par suite c'est la variable entrante

reg<-lm(MEDV ~ LSTAT, data=data)
AIC(reg)
reg$coefficients
#calcul des R2 et Rajuste2
summary(reg)
maximum =0
#calcul du residu
res1 = resid(reg)
for (element in lis) {
  current = as.matrix(data[c(element)])
  #calcul du coeff de correlation entre le residu et les varibles explicatives de la liste
  x = abs(cor(current, res1,method = c("pearson")))
  print(x)
  if(x>maximum){
    #choix du max
    maximum = x
    #choix de la colonne qui donne le max
    curr = element
    #test de significativite
    res<-cor.test(~current + res1, method="pearson")
  }
}
print(curr, quote=FALSE)
res
#vu la p-value est toujours<5% donc on ne s'arrete pas,et que la variable explicative dont le coeff le plus grand est PTRATIO donc c'est la prochaine variable a entrer



reg<-lm(MEDV ~ LSTAT+PTRATIO, data=data)
AIC(reg)
reg$coefficients
#calcul des R2 et Rajuste2
summary(reg)
maximum =0
#calcul du residu
res1 = resid(reg)
for (element in lis) {
  current = as.matrix(data[c(element)])
  #calcul du coeff de correlation entre le residu actuel et les varibles explicatives de la liste
  x = abs(cor(current, res1,method = c("pearson")))
  print(x)
  if(x>maximum){
    #choix du max
    maximum = x
    #choix de la colonne qui donne le max
    curr = element
    #test de significativite
    res<-cor.test(~current + res1, method="pearson")
  }
}
print(curr, quote=FALSE)
res
#vu la p-value est toujours<5% donc on ne s'arrete pas,et que la variable explicative dont le coeff le plus grand est RAD donc c'est la prochaine variable a entrer



reg<-lm(MEDV ~ LSTAT+PTRATIO+RAD, data=data)
AIC(reg)
reg$coefficients
#calcul des R2 et Rajuste2
summary(reg)
maximum =0
#calcul du residu
res1 = resid(reg)
for (element in lis) {
  current = as.matrix(data[c(element)])
  #calcul du coeff de correlation entre le residu actuel et les varibles explicatives de la liste
  x = abs(cor(current, res1,method = c("pearson")))
  print(x)
  if(x>maximum){
    #choix du max
    maximum = x
    #choix de la colonne qui donne le max
    curr = element
    #test de significativite
    res<-cor.test(~current + res1, method="pearson")
  }
}
print(curr, quote=FALSE)
res
#vu la p-value est toujours<5% donc on ne s'arrete pas,et que la variable explicative dont le coeff de correlation est le plus grand est TAX donc c'est la prochaine variable a entrer


reg<-lm(MEDV ~ LSTAT+PTRATIO+RAD+TAX, data=data)
#calcul du critere AIC du modele obtenu
AIC(reg)
reg$coefficients
#calcul des R2 et Rajuste2
summary(reg)
maximum =0
#calcul du residu
res1 = resid(reg)
for (element in lis) {
  current = as.matrix(data[c(element)])
  #calcul du coeff de correlation entre le residu actuel et les varibles explicatives de la liste
  x = abs(cor(current, res1,method = c("pearson")))
  print(x)
  if(x>maximum){
    #choix du max
    maximum = x
    #choix de la colonne qui donne le max
    curr = element
    #test de significativite
    res<-cor.test(~current + res1, method="pearson")
  }
}
print(curr, quote=FALSE)
res
#on remarque ici que le test de significative donne une valeur de p_value>5% donc le critere d'arret est verifie et par suite on s'arretera 
#ici et le modele sera tq y=MEDV dependra des variables explicatives LSTAT,PTRATIO,RAD,TAX.


#Test de normalite
shapiro.test(resid(reg))
ks.test(resid(reg),pnorm)

#test d'homoscedastisit?
plot(predict(reg), resid(reg))
abline(h=0)

#valeurs aberrantes
sd=sqrt(deviance(reg)/df.residual(reg))
#sd = 3.700937
sd
abr=abs(y-predict(reg))/sd
plot(abr)
abline(h=2)











