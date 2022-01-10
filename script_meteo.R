##### Projet GLM
##### Predire s il pleuvera le lendemain en fonction des parametres

#chargement des library
#library(FactoMineR)

chemin = getwd()
setwd(chemin)

#chargement des donnees du fichier .csv
meteo.train = read.csv("../donnees/meteo.train.csv")

#### sous jeu de donnees en prevision ACP
sub.meteo.train = data.frame(meteo.train[,7:47])
#acP
# acp.meteo = PCA(sub.meteo.train, quali.sup = 41, graph = F)
# plot(acp.meteo, axes = c(3,4))

################################################## Analyse descriptive
#jeu de donnees sans l heure et le jour et X
meteo.train.min = meteo.train[,-c(1,2,4,5,6)]

#creation du tableau desriptif des variables a optimiser
table.descr = sapply(2:length(meteo.train.min)-1,function(x){

  pluie = meteo.train.min[meteo.train.min$pluie.demain==TRUE,x]
  soleil = meteo.train.min[meteo.train.min$pluie.demain==FALSE,x] 
  
  s.p = summary(pluie)
  s.np = summary(soleil)
  
  test.stat = t.test(pluie,soleil)

  
  r = c(var=colnames(meteo.train.min)[x], 
        iqr.pluie=paste0("[",s.p[1],"-",s.p[6],"]"," ",round(s.p[4],2)),
        iqr.soleil=paste0("[",s.np[1],"-",s.np[6],"]"," ",round(s.np[4],2)),
        p.value = as.numeric(test.stat$p.value))
  
  
  return(r)
})


table.descr = as.data.frame(t(table.descr))
table.descr$p.value = as.numeric(table.descr$p.value)
var.select = table.descr[table.descr$p.value<0.01,]

#recuperation des variables qui ne sont pas dans la selection
var.non.significatif = setdiff(colnames(meteo.train.min),var.select$var)

#je m interesse maintenant au(x) varaibles qualitatives en particulier au mois de l'annee
tab.mois = xtabs(~pluie.demain+Month, data = meteo.train.min)

pluie.demain.num = ifelse(meteo.train.min$pluie.demain==T,1,0)
cor.test(pluie.demain.num, meteo.train.min$Month)
mosaicplot(meteo.train.min$Month ~ meteo.train.min$pluie.demain, col=2:5,
           main = "Repartition par mois des jours de pluie",
           xlab="Mois",
           ylab="Modalités")

t.chi = chisq.test(tab.mois)
t.chi

# Pearson's Chi-squared test
# 
# data:  tab.mois
# X-squared = 41.685, df = 11, p-value = 1.836e-05


#dans la liste des noms de variables non significatives je veux garder le mois
var.non.significatif = var.non.significatif[-1]
var.non.significatif.num = which(colnames(meteo.train) %in% var.non.significatif)

###selection de variable
library(leaps)
regsubsets.formula(pluie.demain ~ . , int = T, nbest = 1, nvmax = 4, method = "exhaustive", data = meteo.train.min)


library(MASS)
m1 = glm(pluie.demain~1,data=meteo.train.min)
m2 = glm(pluie.demain ~ .,data=meteo.train.min)
resume1 = step(m1, scope=list(lower=m1, upper=m2),data=meteo.train.min, direction="forward")
summary(resume1)

m1 = glm(pluie.demain~1,data=meteo.train.min)
m2 = glm(pluie.demain ~ .,data=meteo.train.min)
resume2 = step(m1, scope=list(lower=m1, upper=m2),data=meteo.train.min, direction="backward")
summary(resume2)

m1 = glm(pluie.demain~1,data=meteo.train.min)
m2 = glm(pluie.demain ~ .,data=meteo.train.min)
resume3 = step(m1, scope=list(upper=m2),data=meteo.train.min, direction="both")
summary(resume3)




################################################## Analyse en regression lineaire multiple, a priori pas possible sur variable binaire

#creation du modele 1 regression lineaire multiple
attach(meteo.train.min)
mod.rlm.1 = lm(data = meteo.train.min, pluie.demain ~ .-Relative.Humidity.daily.mean..2.m.above.gnd.
                                                 -Snowfall.amount.raw.daily.sum..sfc.
                                                 -Shortwave.Radiation.daily.sum..sfc.
                                                 -Relative.Humidity.daily.max..2.m.above.gnd.
                                                 -Relative.Humidity.daily.min..2.m.above.gnd.)

summary(mod.rlm.1)

mod.rlm.2 = lm(data = meteo.train, pluie.demain ~ factor(Month)
               +Mean.Sea.Level.Pressure.daily.mean..MSL.
               +Wind.Direction.daily.mean..900.mb.
               +Mean.Sea.Level.Pressure.daily.max..MSL.
               +Mean.Sea.Level.Pressure.daily.min..MSL.
               +Wind.Speed.daily.min..10.m.above.gnd.)

summary(mod.rlm.2)

library(corrplot)
corrplot(cor(meteo.train.min[, c(4,18,24,25)], use="complete"))

mod.rlm.3 = lm(data = meteo.train, pluie.demain ~ Mean.Sea.Level.Pressure.daily.mean..MSL.
               +Wind.Direction.daily.mean..900.mb.
               +Mean.Sea.Level.Pressure.daily.max..MSL.
               +Mean.Sea.Level.Pressure.daily.min..MSL.)
summary(mod.rlm.3)

anova(mod.rlm.2,mod.rlm.3) #garde le modele le plus parcimonieux
# Analysis of Variance Table
# 
# Model 1: pluie.demain ~ Month + Mean.Sea.Level.Pressure.daily.mean..MSL. + 
#   Wind.Direction.daily.mean..900.mb. + Mean.Sea.Level.Pressure.daily.max..MSL. + 
#   Mean.Sea.Level.Pressure.daily.min..MSL. + Wind.Speed.daily.min..10.m.above.gnd.
# Model 2: pluie.demain ~ Mean.Sea.Level.Pressure.daily.mean..MSL. + Wind.Direction.daily.mean..900.mb. + 
#   Mean.Sea.Level.Pressure.daily.max..MSL. + Mean.Sea.Level.Pressure.daily.min..MSL.
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1   1173 238.89                           
# 2   1175 239.71 -2  -0.81548 2.0021 0.1355

confint(mod.rlm.3)

#tentons de predire avec ce modele
result.predict = predict(mod.rlm.3,type = "response", se.fit=T)
summary(result.predict["fit"])

detach(meteo.train.min)


################################################## Analyse en regression logistique

#integration au modele des variables selectionnee dans l etude descriptive
detach(meteo.train.min)

meteo.train.select = data.frame(Month=meteo.train$Month,meteo.train[,var.select$var],pluie.demain=meteo.train$pluie.demain)

##################### Regression logit

reg.glm.1l = glm(pluie.demain ~ ., family = binomial(logit),data=meteo.train.select)
summary(reg.glm.1l)

#conservation des variables significatives
reg.glm.2l = glm(pluie.demain ~ Mean.Sea.Level.Pressure.daily.mean..MSL.+
                               Wind.Direction.daily.mean..900.mb.+
                               Mean.Sea.Level.Pressure.daily.max..MSL.+
                               Mean.Sea.Level.Pressure.daily.min..MSL.+
                               Wind.Speed.daily.min..10.m.above.gnd.,
                               family = binomial(logit),data=meteo.train.select)
summary(reg.glm.2l)
pred.glm.2 = predict(reg.glm.2l, meteo.train.select,type = "response")
mean(abs(pred.glm.2 - meteo.train.select[,"pluie.demain"]), na.rm = T)

reg.glm.3l = glm(pluie.demain ~ Mean.Sea.Level.Pressure.daily.mean..MSL.+
                  Wind.Direction.daily.mean..900.mb.+
                  Mean.Sea.Level.Pressure.daily.max..MSL.+
                  Mean.Sea.Level.Pressure.daily.min..MSL.,
                family = binomial(logit),data=meteo.train.select)
summary(reg.glm.3l)
pred.glm.3 = predict(reg.glm.3l,meteo.train.select, type = "response")

mean(abs(pred.glm.3 - meteo.train.select[,"pluie.demain"]), na.rm = T)

library(questionr)
odds.ratio(reg.glm.3l)

library(gtsummary)
tab.reg = tbl_regression(reg.glm.3l, exponentiate = T)
tab.reg

reg.glm.4l = glm(pluie.demain ~ Mean.Sea.Level.Pressure.daily.mean..MSL.+
                  Wind.Direction.daily.mean..900.mb.+
                  Mean.Sea.Level.Pressure.daily.max..MSL.:Mean.Sea.Level.Pressure.daily.min..MSL.,
                family = binomial(logit),data=meteo.train.select)
summary(reg.glm.4l)
pred.glm.4 = predict(reg.glm.3l,meteo.train.select, type = "response")
mean(abs(pred.glm.4 - meteo.train.select[,"pluie.demain"]), na.rm = T)

table(pred.glm.4 > 0.5, meteo.train.select$pluie.demain)

#matrice de confusion

mat.conf.glm.4 = table(pred.glm.4 > 0.5, meteo.train.select$pluie.demain)
taux.erreur.glm.4 = round(((mat.conf.glm.4[1,2]+mat.conf.glm.4[2,1])/nrow(meteo.train.select))*100,2)

################### 30% d'erreur sur la base avec ce modele 


##################### Regression sans logit
reg.glm.1 = glm(pluie.demain ~ ., family = binomial(logit),data=meteo.train.select)
summary(reg.glm.1)

reg.glm.2 = glm(pluie.demain ~ Mean.Sea.Level.Pressure.daily.mean..MSL.+
                   Wind.Direction.daily.mean..900.mb.+
                   Mean.Sea.Level.Pressure.daily.max..MSL.+
                   Mean.Sea.Level.Pressure.daily.min..MSL.+
                   Wind.Speed.daily.min..10.m.above.gnd.,
                 family = binomial,data=meteo.train.select)
summary(reg.glm.2)
pred.glm.2 = predict(reg.glm.2, meteo.train.select,type = "response")
mean(abs(pred.glm.2 - meteo.train.select[,"pluie.demain"]), na.rm = T)


reg.glm.3 = glm(pluie.demain ~ Mean.Sea.Level.Pressure.daily.mean..MSL.+
                   Wind.Direction.daily.mean..900.mb.+
                   Mean.Sea.Level.Pressure.daily.max..MSL.+
                   Mean.Sea.Level.Pressure.daily.min..MSL.,
                 family = binomial,data=meteo.train.select)
summary(reg.glm.3)
pred.glm.3 = predict(reg.glm.3,meteo.train.select, type = "response")

mean(abs(pred.glm.3 - meteo.train.select[,"pluie.demain"]), na.rm = T)

mat.conf.glm.3 = table(pred.glm.3 > 0.5, meteo.train.select$pluie.demain)
taux.erreur.glm.3 = round(((mat.conf.glm.3[1,2]+mat.conf.glm.3[2,1])/nrow(meteo.train.select))*100,2)




reg.glm.4 = glm(pluie.demain ~ Mean.Sea.Level.Pressure.daily.mean..MSL.+
                   Wind.Direction.daily.mean..900.mb.+
                   Mean.Sea.Level.Pressure.daily.max..MSL.:Mean.Sea.Level.Pressure.daily.min..MSL.,
                 family = binomial,data=meteo.train.select)
summary(reg.glm.4)
pred.glm.4 = predict(reg.glm.4,meteo.train.select, type = "response")
mean(abs(pred.glm.4 - meteo.train.select[,"pluie.demain"]), na.rm = T)

table(pred.glm.4 > 0.5, meteo.train.select$pluie.demain)


##################### Regression probit

reg.glm.1p = glm(pluie.demain ~ ., family = binomial(link="probit"),data=meteo.train.select)
summary(reg.glm.1p)

reg.glm.2p = glm(pluie.demain ~ Mean.Sea.Level.Pressure.daily.mean..MSL.+
                               Wind.Direction.daily.mean..900.mb.+
                               Mean.Sea.Level.Pressure.daily.max..MSL.+
                               Mean.Sea.Level.Pressure.daily.min..MSL.+
                               Medium.Cloud.Cover.daily.max..mid.cld.lay.+
                               Wind.Speed.daily.max..10.m.above.gnd.+
                               Wind.Speed.daily.min..10.m.above.gnd.+
                               Medium.Cloud.Cover.daily.max..mid.cld.lay.+
                               Mean.Sea.Level.Pressure.daily.min..MSL.+
                               Wind.Gust.daily.max..sfc.+
                               Wind.Direction.daily.mean..900.mb.+
                               Temperature.daily.max..2.m.above.gnd.
                   ,
                family = binomial(link="probit"),data=meteo.train.select)
summary(reg.glm.2p)

pred.glm.2p = predict(reg.glm.2p,meteo.train.select, type = "response")
table(pred.glm.2p > 0.5, meteo.train.select$pluie.demain)

mat.conf.glm.2p = table(pred.glm.2p > 0.5, meteo.train.select$pluie.demain)
taux.erreur.glm.2p = round(((mat.conf.glm.2p[1,2]+mat.conf.glm.2p[2,1])/nrow(meteo.train.select))*100,2)

