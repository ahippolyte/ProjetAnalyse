### Notes intéressantes
- ACP: corellation assez forte entre les contaminations (foie)
- transformations: seuil et log pour les variables très petites/grandes pour avoir des données plus comparables entre elles
- Donnée manquantes:
    - soit on enleve les observations avec les données manquantes
    - supprimer la colonne
    - moyenner les données sur {tout, espece, régime}
 
Clustering en enlevant certaines variables
On peut commencer à aller vers l'apprentissage supervisé en se demandant si le clustering regroupe pas les données ensemble
+ gros + vécu longtemps + contaminé
si il est carnivore -> bout de chaine alimentaire -> contaminé


### Données
- Longueur en centimètre
- Poids en grammes
- Contamination en $$\mu g/g$$

### Visualisation : Points de corrélation intéréssants
- Très forte corrélation entre la longueur et Poids des poissons
- Type de régime corrélé avec la contamination des organes (intestin, muscles et branchies essentiellement) et la taille du poisson
- Très forte corrélation entre la contamination des muscles et la contamination des branchies (les branchies sont des muscles)

