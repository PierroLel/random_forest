lycee <- read.csv2("https://www.data.gouv.fr/s/resources/indicateurs-de-resultat-des-lycees-denseignement-general-et-technologique/20160401-163749/MEN-DEPP-indicateurs-de-resultats-des-LEGT-2015.csv", 
                   sep = ";", header = TRUE, fileEncoding = "ISO-8859-15", na.strings = c(" ", 
                                                                                          "."))

View(lycee)

colnames(lycee)

nometab <- paste(lycee$Etablissement, lycee$Code.Etablissement) # on se fait le nouveau nom
nometab <- gsub("LYCEE ", "", nometab) # Vire "lycee" du nom
row.names(lycee) <- nometab

#On garde pas toutes les colonnes
lycee2 <- select(lycee, Secteur.Public.PU.Privé.PR, Académie, Sructure.pédagogique.en.7.groupes, 
                 Taux.Brut.de.Réussite.Total.séries, Taux.Réussite.Attendu.toutes.séries, 
                 Effectif.de.seconde, Effectif.de.première, Effectif.de.terminale)


for (i in 4:ncol(lycee2)) {
  lycee2[, i] <- as.numeric(as.character(lycee2[, i]))
}


set.seed(123)
# Public ou privé? caractère discriminant?
fit <- randomForest(Secteur.Public.PU.Privé.PR ~ ., data = lycee2, na.action = na.roughfix)


print(fit) # 500 trees pour le bagging
# moyenne des erreur d'approximation, sans prendre en compte l'echantillon utilisé pour def la fonciton de fittage
#Chaque arbre de la forêt est construit sur une fraction ("in bag") des données (c'est la fraction qui sert à l'entraînement de l'algorithme. 
#Alors pour chacun des individus de la fraction restante ("out of bag") l'arbre peut prédire une classe.

# 22% err poour privé et 8% pour public

varImpPlot(fit)
fit$importance
# reussite au bac facteur determinant

plot(Secteur.Public.PU.Privé.PR ~ Taux.Brut.de.Réussite.Total.séries, data = lycee2)
#privé majoritaire au dessus de 95, minoritaire partout ailleurs
plot(Secteur.Public.PU.Privé.PR ~ Effectif.de.seconde, data = lycee2)
#privé ont moins d'eleve en seconde

plot(fit$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")
## décroissance de l'err OOB

## on mets mnt 5000 arbres.
fit <- randomForest(Secteur.Public.PU.Privé.PR ~ ., data = lycee2, ntree = 5000, 
                    mtry = 2, na.action = na.roughfix)
print(fit)


#choix du nombre de split: prender défaut, multiplié par 2 et div ppaar 2  et prend le mmieux
set.seed(123)
fit <- randomForest(Secteur.Public.PU.Privé.PR ~ ., data = lycee2, ntree = 2000, 
                    mtry = 4, na.action = na.roughfix)
print(fit)


par(mfrow = c(1, 1))
partialPlot(fit, lycee2, 4, "PR", main = "Privé", ylab = "Taux.Brut.de.Réussite.Total.séries")
partialPlot(fit, lycee2, 6, "PR", main = "Privé", ylab = "Effectif.de.seconde")



mod <- train(Secteur.Public.PU.Privé.PR ~ ., data = lycee2, method = "rf",na.action=na.omit)
print(mod)
#Plus l'Accuracy et le kappa sont grand plus le modèle est bon.
#mieux à 21 (1 var = 1 modalité dans ce calcul là)

print(mod$finalModel)
varImpPlot(mod$finalModel)
plot(varImp(mod), top = 20)
plotmo(mod, type = "prob", nresponse = "PR",pmethod="apartdep")

############################################################################

modele=randomForest(Secteur.Public.PU.Privé.PR ~ . , data=lycee2,ntree=500,mtry=2,do.trace=100,na.action=na.roughfix)
print(modele)
varImpPlot(modele)
plot(varImpPlot(modele),top=20)
reprtree:::plot.getTree(modele,depth=3,k=3)
plot(modele,type="h")
plot.getTree(modele)
model <- randomForest(Species ~ ., data=iris, importance=TRUE, ntree=500, mtry = 2, do.trace=100)


data<- read.csv2("C:/Users/Pierre/Desktop/ENSAE/S4/Monte Carlo/OnlineNewsPopularity.csv",sep=",");x = data[,3:61];y = data[,61]
x=data.frame(x)
for (i in 1:ncol(x)) {
  x[, i] <- as.numeric(as.character(x[, i]))
}

modele1=randomForest(shares~.,data=x,ntree=500,mtry=2,na.action=na.roughfix)
varImpPlot(modele1)
reprtree:::plot.getTree(modele1,depth=3,k=3)
plotmo(mod, type = "prob", nresponse = "PR",pmethod="apartdep")

############################################################################



for (i in 4:ncol(lycee2)) {
   print(typeof(lycee2[, i]))
}


