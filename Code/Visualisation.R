# Adresse du dossier où vous travaillez
setwd("~/school/AnalyseDonnees/ProjetAnalyse/Code")
# Packages utilisés dans la suite
library("FactoMineR")
library(PCAmixdata)
library(GGally)
library("readxl")

# Supprimer toutes les variables
rm(list=ls(all=TRUE))
# Supprimer tous les graphiques déjà  présents
graphics.off()

# Chargement des données
data <- read_excel("../../Data/Projets/poissons.xls")

# Suppression des valeurs non assignées
data <- data[is.na(data$INTE) == FALSE & is.na(data$ESTO)==FALSE & 
                   is.na(data$BRAN)==FALSE & is.na(data$REIN)==FALSE,]

# Classe numérique pour le régime et l'espèce
data$REGIME <- as.numeric(as.factor(data$REGIME))
data$ESPECE <- as.numeric(as.factor(data$ESPECE))

# Affichage des données
print(data, digits=4)

# Calcul de la moyenne et de l’écart type des variables
mean <- apply(data[,4:11], 2, mean)
std <- apply(data[,4:11], 2, sd) #standard deviation
stat <- rbind(mean, std)

# Affichage des statistiques
print(stat, digits=4)

# Création des données centrées et réduites
datanorm <- sweep(data[,4:11], 2, mean, "-")
datanorm <- sweep(datanorm, 2, std, "/")
# Concaténation des données qualitatives
datanorm$REGIME = data$REGIME
datanorm$ESPECE = data$ESPECE
datanorm <- datanorm[, c(10,9,1:8)]

# Affichage des données centrées - réduites
print(datanorm, digits=4)

# Visualisation avec les données brutes
# Description bivariée
pairs(data[,2:11])
# Matrice de corrélation
ggcorr(data[,2:11])
# Aller encore plus loin avec ggpairs
ggpairs(data[,2:11])
# Matrice des distances entre les individus
dist(data[,2:11])
# Corrélation entre les variables
cor(data[,2:11])

# Visualisation avec les données centrées réduites
# Description bivariée
pairs(datanorm[,1:10])
# Matrice de corrélation
ggcorr(datanorm[,1:10])
# Aller encore plus loin avec ggpairs
ggpairs(datanorm[,1:10])
# Matrice des distances entre les individus
dist(datanorm[,1:10])
# Corrélation entre les variables
cor(datanorm[,1:10])

# Analyse en composantes principales sur les données d’origine
# (scale.unit=FALSE)
res <- PCA(data,graph=FALSE,scale.unit=FALSE)
# Figure individus
plot(res,choix="ind",cex=1.5,title="")
# Figure variables
plot(res,choix="var",cex=1.5,title="")

# Analyse en composantes principales sur les données centrées-réduites
# (par défaut: scale.unit=TRUE)
resnorm <- PCA(data,graph=FALSE,quali.sup=3)

# Figure individus
plot(resnorm,choix="ind",cex=1.5,title="")
# Figure variables
plot(resnorm,choix="var",cex=1.5,title="") #, select = c(1))

plot(resnorm$var$coord[1:3,1],resnorm$var$coord[1:3,2],cex=1.5,title="")
print(resnorm$var$coord)

dim(resnorm$var$coord)
# Inertie (variance) des composantes principales

resnorm$eig
barplot(resnorm$eig[,1])

# Projection des individus
resnorm$ind$cos2
# Somme avec les 2 premières
resnorm$ind$cos2[,1]+resnorm$ind$cos2[,2]
# Et les 3 premières ?
resnorm$ind$cos2[,1]+resnorm$ind$cos2[,2]+resnorm$ind$cos2[,3]


# Contribution des individus
resnorm$ind$contrib

# Projection des variables
resnorm$var$cos2
# Somme avec les 2 premières
resnorm$var$cos2[,1]+resnorm$var$cos2[,2]
# Et les 3 ?
resnorm$var$cos2[,1]+resnorm$var$cos2[,2]+resnorm$var$cos2[,3]
# Contribution des variables
resnorm$var$contrib
