# Adresse du dossier où vous travaillez
setwd("~/Documents/université/master2/AnalyseDonnees/projet/code")
# Packages utilisés dans la suite
library("FactoMineR")
library(PCAmixdata)
library(GGally)
install.packages("readxl")
library("readxl")

# Supprimer toutes les variables
rm(list=ls(all=TRUE))
# Supprimer tous les graphiques déjà  présents
graphics.off()
# Chargement des données
poisson_data <- read_excel("../Data/Projets/poissons.xls")
data <- poisson_data
# facteur régimes
data$REGIME <- as.factor(data$REGIME)
data$ESPECE <- as.factor(data$ESPECE)
data$REGIME_CLASS <- as.numeric(data$REGIME)
data$ESPECE_CLASS <- as.numeric(data$ESPECE)


#poisson_data <- poisson_data[is.na(poisson_data$INTE)==FALSE,]
#poisson_data <- poisson_data[is.na(poisson_data$ESTO)==FALSE,]
#poisson_data <- poisson_data[is.na(poisson_data$BRAN)==FALSE,]
#poisson_data <- poisson_data[is.na(poisson_data$REIN)==FALSE,]
#poisson_data
#data <- poisson_data
# Affichage des données
print(data,digits=4)
## data$REIN
## data[is.na(data$REIN)==False,]
# Calcul de la moyenne et de l’écart type des variables
mean <- apply(data[,4:13],2,mean,na.rm=TRUE)
std <- apply(data[,4:11],2,sd,na.rm=TRUE) #standard deviation
stat <- rbind(mean,std)
# Affichage
print(stat,digits=4)

# Création des données centrées ...
datanorm <- sweep(data[,4:11],2,mean,"-")
# ... et réduites
datanorm <- sweep(datanorm,2,std,"/")
# Affichage des données centrées - réduites
print(datanorm,digits=4)

# Visualisation des données en description bivariée
pairs(data[,4:13],na.rm=TRUE)
# Afficher la matrice de corrélation
ggcorr(data[,4:13],na.rm=TRUE)
# Aller encore plus loin avec ggpairs
ggpairs(data[,4:12],na.rm=TRUE)

# Matrice des distances entre les individus
dist(data[,4:11])
# Corrélation entre les variables
cor(data[, 4:11])

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
