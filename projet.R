ls()
getwd()
SerieIndice=read.csv("Data.csv",header=F,dec=".",sep=",")
SerieIndice

SerieIndice = SerieIndice[,c("V2")]
SerieIndice
  ### Transformer "SerieIndice" en un objet du type ts
SerieIndice_ts<- ts(SerieIndice,frequency=12,start = c(1956,1), end =c(1973,12))
SerieIndice_ts

is.ts(SerieIndice_ts) ##V�rifier que "SerieIndice" est bien une s�rie temporelle

plot(SerieIndice_ts)

### L'approche de d�composition sur R 

X=SerieIndice_ts
X

#Quelle fonction R faut-il utiliser pour d�composer la s�rie
decomp <- decompose(X,type="additive")
decomp

#Que repr�sente les valeurs "NA" dans decomp ? valeur manquantte

plot(decomp)

decomp <- decompose(X)
attributes(decomp)
names(decomp)
class(decomp)
plot(decomp)


# On peut ensuite recomposer le signal sans le bruit en additionnant la tendance 
recomp <- decomp$trend + decomp$seasonal
recomp
par(mfcol=c(2,1)) # instruction graphique pour faire deux fen�tres
plot(recomp)
plot(X)


# recomposer la s�rie comme dans le excel c'est trop long en R donc on ne fait pas

#Sur la repr�sentation graphique, quatre courbes sont repr�sent�es:
#de haut e bas, figurent
# -la s�rie initiale,
# -la tendance,
# -la composante saisonni�re
# -la partie r�siduelle
# Ces quatres parties correspondent aux diff�rentes composantes de l'objet cr��.
# La d�composition repose sur l'application de moyenne mpobiles
# dont on peut pr�ciser le filtre �ventuellement
# Par d�faut une moyenne sym��trique est employ�e


## Comparer les compsantes saisonni�res obtenues avec cette et celles obtenues avec le tableur

#Extrait des composantes saisonni�res

