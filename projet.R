ls()
getwd()
SerieIndice=read.csv("Data.csv",header=F,dec=".",sep=",")
SerieIndice

SerieIndice = SerieIndice[,c("V2")]
SerieIndice
  ### Transformer "SerieIndice" en un objet du type ts
SerieIndice_ts<- ts(SerieIndice,frequency=12,start = c(1956,1), end =c(1973,12))
SerieIndice_ts

is.ts(SerieIndice_ts) ##Vérifier que "SerieIndice" est bien une série temporelle

plot(SerieIndice_ts)

### L'approche de décomposition sur R 

X=SerieIndice_ts
X

#Quelle fonction R faut-il utiliser pour décomposer la série
decomp <- decompose(X,type="additive")
decomp

#Que représente les valeurs "NA" dans decomp ? valeur manquantte

plot(decomp)

decomp <- decompose(X)
attributes(decomp)
names(decomp)
class(decomp)
plot(decomp)


# On peut ensuite recomposer le signal sans le bruit en additionnant la tendance 
recomp <- decomp$trend + decomp$seasonal
recomp
par(mfcol=c(2,1)) # instruction graphique pour faire deux fenêtres
plot(recomp)
plot(X)


# recomposer la série comme dans le excel c'est trop long en R donc on ne fait pas

#Sur la représentation graphique, quatre courbes sont représentées:
#de haut e bas, figurent
# -la série initiale,
# -la tendance,
# -la composante saisonnière
# -la partie résiduelle
# Ces quatres parties correspondent aux différentes composantes de l'objet créé.
# La décomposition repose sur l'application de moyenne mpobiles
# dont on peut préciser le filtre éventuellement
# Par défaut une moyenne syméétrique est employée


## Comparer les compsantes saisonnières obtenues avec cette et celles obtenues avec le tableur

#Extrait des composantes saisonnières

