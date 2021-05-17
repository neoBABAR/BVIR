data<-read.csv(file="Bilan_resultats_BVI.csv",header=TRUE,sep=";",dec=".")
head(data)
data$Stade<-factor(data$Stade)
str(data)
dim(data)
names(data)

#install.packages("prettyR) > enlever le # si le package n'est pas installé

dataetude<-data[,c(2,3,11,13)]
head(dataetude)
summary(dataetude)
table(dataetude$Genotype,dataetude$Stade,useNA='always') 


#Création des subsets pour TMT1, TMT2 et WT

TMT1<-subset(data,Genotype=='TMT135S2', select = c(Stade,Glucose,Malate))
WT<-subset(data,Genotype=='WT', select = c(Stade,Glucose,Malate))

#Statistiques descriptives :

#représentation des WT Malate et GLucose en boxplot
dev.new()
par(mfrow=c(2,2))
hist(TMT1$Glucose, main="Glucose - TMT1", ylab = "Effectifs", xlab = "µmol/gFW",col = "lightblue")
hist(TMT1$Malate, main="Malate - TMT1", ylab = "Effectifs", xlab = "µmol/gFW",col = "lightblue")
hist(WT$Malate, main="Malate - WT", ylab = "Effectifs", xlab = "µmol/gFW",col = "tomato2")
hist(WT$Glucose, main="Glucose - WT", ylab = "Effectifs", xlab = "µmol/gFW",col = "tomato2")

dev.new()
par(mfrow=c(2,2))
boxplot(WT$Malate~WT$Stade,main="Malate WT",xlab="Stade en Jours",ylab = "Malate en µmol/gFW",col='tomato2')
boxplot(WT$Glucose~WT$Stade,main="Glucose WT",xlab="Stade en Jours",ylab = "Glucose en µmol/gFW",col='tomato2')
boxplot(TMT1$Malate~TMT1$Stade,main="Malate TMT1",xlab="Stade en Jours",ylab = "Malate en µmol/gFW",col='lightblue')
boxplot(TMT1$Glucose~TMT1$Stade,main="Glucose TMT1",xlab="Stade en Jours",ylab = "Glucose en µmol/gFW",col='lightblue')


#Test statistique

shapiro.test(TMT1$Malate) #(p-value = 0.0008466) on rejette H0, ne suit pas loi normale 
shapiro.test(TMT1$Glucose) #(p-value = 0.1962) on accepte H0 

shapiro.test(WT$Malate) #(p-value = 0.493) suit la loi normale
shapiro.test(WT$Glucose) #(p-value = 0.02314) ne suit pas la loi normale


#On va faire toutes nos comparaisons de moyennes par du Wilcoxon, malgré une perte de précision
#on veut étudier si les mutations TMT1 ont un effet sur la concentration en Glu et Mal

#Etude du TMT1 Malate et Glucose - WT GLucose et Malate

wilcox.test(TMT1$Malate,WT$Malate) #effet significatif p-value = 5.317e-07

wilcox.test(TMT1$Glucose,WT$Glucose, alternative = c("less")) #effet significatif p-value = 3.189e-15

#TMT1 a donc un effet à la fois sur les concentration en glucose et en Malate

#On veut voir si le changement de concentration en glucose affecte la concentration en malate
#Test de corrélation 
plot(TMT1$Glucose,TMT1$Malate) #on visualise par nuage de point

cor(TMT1$Malate,TMT1$Glucose,use="everything") #corrélation = -0.7851049 
cor.test(TMT1$Glucose,TMT1$Malate) # significatif p-value = 6.313e-08

#on représente le nuage de point avec la droite de régression
modele<-lm(TMT1$Malate~TMT1$Glucose)
plot(TMT1$Glucose,TMT1$Malate)
abline(modele,col='lightblue')

#Test de correlation chez WT
plot(WT$Glucose,WT$Malate)

cor(WT$Malate,WT$Glucose,use="everything")

#on vérifie la corrélation avec spearman car le nuage de point n'est pas linéaire
cor.test(WT$Glucose,WT$Malate) #non-signficatif p-value = 0.1067
cor.test(WT$Glucose,WT$Malate,method = c("spearman")) #non-signficatif p-value = 0.4856

