# Adresse du dossier où vous travaillez
setwd("~/Documents/université/master2/AnalyseDonnees/projet/code")
# Packages utilisés dans la suite
library("FactoMineR")
library(dplyr)
library("readxl")

# Supprimer toutes les variables
rm(list=ls(all=TRUE))
# Supprimer tous les graphiques déjà  présents
graphics.off()
# Chargement des données
poisson_data <- read_excel("../Data/Projets/poissons.xls")

# Remplacer les valeurs manquantes par la moyenne de chaque espèce
data_full <- poisson_data %>%
  group_by(ESPECE) %>%
  mutate(across(c(LONG, POID, MUSC, INTE,ESTO,BRAN,FOIE,REIN), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
data_full <- data_full[complete.cases(data_full),]
# pour effacer les lignes qui contient des données manquantes  
data_rm <- poisson_data[complete.cases(poisson_data),]

# new data avec log10
data_full_log <- data_transformed <- data_full %>%
  mutate(across(3:10, log10))
data_rm_log <- data_transformed <- data_rm %>%
  mutate(across(4:11, log10))


# facteur régimes
data_full$REGIME <- as.factor(data_full$REGIME)
data_full$ESPECE <- as.factor(data_full$ESPECE)

data_full_log$REGIME <- as.factor(data_full_log$REGIME)
data_full_log$ESPECE <- as.factor(data_full_log$ESPECE)

data_rm$ESPECE <- as.factor(data_rm$ESPECE)
data_rm$REGIME <- as.factor(data_rm$REGIME)

data_rm_log$ESPECE <- as.factor(data_rm_log$ESPECE)
data_rm_log$REGIME <- as.factor(data_rm_log$REGIME)


data_full$REGIME_CLASS <- as.numeric(data_full$REGIME)
data_full$ESPECE_CLASS <- as.numeric(data_full$ESPECE)

data_full_log$REGIME_CLASS <- as.numeric(data_full$REGIME)
data_full_log$ESPECE_CLASS <- as.numeric(data_full$ESPECE)

data_rm$ESPECE_CLASS <- as.numeric(data_rm$ESPECE)
data_rm$REGIME_CLASS <- as.numeric(data_rm$REGIME)
data_rm_log$ESPECE_CLASS <- as.numeric(data_rm$ESPECE)
data_rm_log$REGIME_CLASS <- as.numeric(data_rm$REGIME)


# Affichage des données
#print(data,digits=4)
## data$REIN
## data[is.na(data$REIN)==False,]
## na.rm=TRUE
X <- data_full_log
data_full <- X
# Calcul de la moyenne et de l’écart type des variables pour data_full
mean <- apply(X[,4:11],2,mean)
std <- apply(X[,4:11],2,sd) #standard deviation
stat <- rbind(mean,std)

# Calcul de la moyenne et de l’écart type des variables pour data_full_log
mean_full <- apply(data_full[,4:13],2,mean)
std_full <- apply(data_full[,4:13],2,sd) #standard deviation
stat_full <- rbind(mean_full,std_full)

# Calcul de la moyenne et de l’écart type des variables pour data_full_log
mean_full_log <- apply(data_full_log[,4:11],2,mean)
std_full_log <- apply(data_full_log[,4:11],2,sd) #standard deviation
stat_full_log <- rbind(mean_full_log,std_full_log)

# Calcul de la moyenne et de l’écart type des variables pour data_rm
mean_rm <- apply(data_rm[,4:11],2,mean)
std_rm <- apply(data_rm[,4:11],2,sd) #standard deviation
stat_rm <- rbind(mean_rm,std_rm)

# Calcul de la moyenne et de l’écart type des variables pour data_rm_log
mean_rm_log <- apply(data_rm_log[,4:11],2,mean)
std_rm_log <- apply(data_rm_log[,4:11],2,sd) #standard deviation
stat_rm_log <- rbind(mean_rm_log,std_rm_log)


# Reshape the vector of standard deviations
#mean_reshaped <- cbind(mean, mean)
#std_reshaped <- cbind(std, std)
#stat <- rbind(mean_reshaped,std_reshaped)
# Affichage
print(stat_full,digits=4)
print(stat_full_log,digits=4)
print(stat_rm,digits=4)
print(stat_rm_log,digits=4)

# Création des données centrées data_full...
datanorm_full <- sweep(data_full[,4:13],2,mean_full,"-")
# ... et réduites
datanorm_full <- sweep(datanorm_full,2,std_full,"/")
# Affichage des données centrées - réduites
print(datanorm_full,digits=4)



# Nombre de clusters souhaité
numcluster <- 8

## KMEANS
# Algorithme des kmeans (avec affichage) pour data_full
km <- kmeans(data_full[,4:13],numcluster,nstart=50)
print(km)
# Algorithme des kmeans sur données centrées-réduites (avec affichage)
kmnorm <- kmeans(datanorm_full,numcluster,nstart=50)
print(kmnorm)

# Concatenation des données avec leur résultat de cluster
cluster <- as.factor(km$cluster)
clusternorm <- as.factor(kmnorm$cluster)
XplusCluster <- data.frame(data_full[,4:13],cluster=cluster)
XnormplusCluster <- data.frame(datanorm_full,cluster=clusternorm)
colclust <- length(data_full[,4:13])+1
print(XplusCluster)
print(XnormplusCluster)

# ACP sur les données brutes
rPCA <- PCA(XplusCluster,scale.unit=FALSE,graph=FALSE,quali.sup=colclust)
# Nuage des individus et des variables dans le premier plan factoriel
par(mfrow=c(1,2))
plot.PCA(rPCA,axes=c(1,2),choix="ind",habillage=colclust,invisible="quali")
plot.PCA(rPCA,axes=c(1,2),choix="var")
# Nuage des individus et des variables dans le deuxième plan factoriel
par(mfrow=c(1,2))
plot.PCA(rPCA,axes=c(1,3),choix="ind",habillage=colclust,invisible="quali")
plot.PCA(rPCA,axes=c(1,3),choix="var")

# ACP sur les données centrées-réduites
rPCAnorm <- PCA(XnormplusCluster,graph=FALSE,quali.sup=colclust)
# Nuage des individus et des variables dans le premier plan factoriel
par(mfrow=c(1,2))
plot.PCA(rPCAnorm,axes=c(1,2),choix="ind",habillage=colclust,invisible="quali")
plot.PCA(rPCAnorm,axes=c(1,2),choix="var")
# Nuage des individus et des variables dans le deuxième plan factoriel
par(mfrow=c(1,2))
plot.PCA(rPCAnorm,axes=c(1,3),choix="ind",habillage=colclust,invisible="quali")
plot.PCA(rPCAnorm,axes=c(1,3),choix="var")




#Classification hiérarchique de Ward sur données brutes
d <- dist(data_full[,4:13])
tree <- hclust(d^2,method="ward.D2")
par(mfrow=c(1,1))
plot(tree)

#Classification hiérarchique de Ward sur données centrées-réduites
dnorm <- dist(datanorm_full)
treenorm <- hclust(dnorm^2,method="ward.D2")
plot(treenorm)

# Concatenation des données avec leur résultat de cluster
clusterW <- as.factor(cutree(tree,numcluster))
XplusClusterW <- data.frame(data_full[,4:13],cluster=clusterW)
print(XplusClusterW)
clusternormW <- as.factor(cutree(treenorm,numcluster))
XnormplusClustW <- data.frame(datanorm_full,cluster=clusternormW)
print(XnormplusClustW)

# ACP sur les données brutes
rPCAW <- PCA(XplusClusterW,scale.unit=FALSE,graph=FALSE,quali.sup=colclust)
# Nuage des individus et des variables dans le premier plan factoriel
par(mfrow=c(1,2))
plot.PCA(rPCAW,axes=c(1,2),choix="ind",habillage=colclust,invisible="quali")
plot.PCA(rPCAW,axes=c(1,2),choix="var")
# Nuage des individus et des variables dans le deuxième plan factoriel
par(mfrow=c(1,2))
plot.PCA(rPCAW,axes=c(1,3),choix="ind",habillage=colclust,invisible="quali")
plot.PCA(rPCAW,axes=c(1,3),choix="var")

# ACP sur les données centrées-réduites
rPCAnormW <- PCA(XnormplusClustW,scale.unit=FALSE,graph=FALSE,quali.sup=colclust)

# Nuage des individus et des variables dans le premier plan factoriel
par(mfrow=c(1,2))
plot.PCA(rPCAnormW,axes=c(1,2),choix="ind",habillage=colclust,invisible="quali")
plot.PCA(rPCAnormW,axes=c(1,2),choix="var")
# Nuage des individus et des variables dans le deuxième plan factoriel
par(mfrow=c(1,2))
plot.PCA(rPCAnormW,axes=c(1,3),choix="ind",habillage=colclust,invisible="quali")
plot.PCA(rPCAnormW,axes=c(1,3),choix="var")
                 
                 