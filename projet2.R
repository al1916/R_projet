install.packages("forecast")
library("forecast")


#------  On désaisonnalise la série 
SNCF=read.csv("Data.csv",header=F,dec=".",sep=",")
SNCF = SNCF[,c("V2")]
serie=ts(data = SNCF, start = c(1953,1), frequency = 12)
serie.des=decompose(serie)$trend
X11() 
par(mfcol=c(2,1)) 
plot(serie,main="1953-1978",ylab="Prodution en Hectolitre",xlab="")
points(serie.des,type="l",col="red")
monthplot(serie,main="1953-1978",ylab="Prodution en Hectolitre",xlab="")

#--------- On a affiché la série et la série désaisonnalisé 


#-------- Lissage exponentiel simple --------
#On prend un échantillon de 53 à 68 pour le lissage
#On refait une désaisonnalisation comme au dessus 
serie.LES=window(serie,start=c(1953,1),end=c(1970,12))
serie.LES.des=decompose(serie)$trend
X11()
par(mfcol=c(2,1))
plot(serie.LES,main="1953-1968",ylab="Prodution en Hectolitre",xlab="")
points(serie.LES.des,type="l",col="red")
monthplot(serie.LES,main="1953-1968",ylab="Prodution en Hectolitre",xlab="")
# On a affiché notre nouvelle série de 53 à 68


serie.LES.app=window(serie,start=c(1953,1),end=c(1967,12))
serie.LES.val=window(serie,start=c(1968,1),end=c(1968,12))
# LES
LES=ets(serie.LES,model="ANN",additive.only=TRUE)
summary(LES)

# Previsions
prev=forecast(LES,h=12)
X11()
plot(prev)
points(serie.LES,type="l",col="red")

#---- Lissage exponentielle double --------

# serie.LED=window(serie,start=c(1968,1),end=c(1978,12))
# serie.LED.des=decompose(serie.LED)$trend
# X11()
# par(mfcol=c(2,1))
# plot(serie.LED,main="1968-1978",ylab="Prodution en Hectolitre",xlab="")
# points(serie.LED.des,type="l",col="red")
# monthplot(serie.LED,main="1968-1978",ylab="Prodution en Hectolitre",xlab="")
# 
# 
# serie.LED.app=window(serie,start=c(1968,1),end=c(1973,12))
# serie.LED.val=window(serie,start=c(1974,1),end=c(1978,12))
# # LED
# LED=ets(serie.LED.app,model="AAN",additive.only=TRUE)
# summary(LED)
# 
# 
# 
# # Previsions
# prev=forecast(LED,h=60)
# #X11()
# plot(prev)
# points(serie.LED,type="l",col="red")



#--------- Holt-Winters-------

HW=ets(serie.LED.app,model="AAA",additive.only=TRUE)
summary(HW)


# Previsions
prev1=forecast(HW,h=60)
X11()
plot(prev1)
points(serie.LED,type="l",col="red")
#Ajouter la vraie courbe pour comparer 

# Modele multiplicatif
HW=ets(serie.LED.app,model="MMM")
prev2=forecast(HW,h=60)

# Modele automatique

HW=ets(serie.LED.app,model="ZZZ")
prev3=forecast(HW,h=60)
X11()
par(mfcol=c(3,1))
plot(prev1)
points(serie.LED,type="l",col="red")
plot(prev2)
points(serie.LED,type="l",col="red")
plot(prev3)
points(serie.LED,type="l",col="red")

#erreur il me semble dans les prev
plot(serie.LED,ylim=c(50,300))
points(prev$fitted,col=2,type='l')
points(prev$mean,col=2,type='l')
points(prev1$fitted,col=3,type='l')
points(prev1$mean,col=3,type='l')
points(prev3$fitted,col=4,type='l')
points(prev3$mean,col=4,type='l')
legend("topleft",c("Série","LES","AAA","ZZZ"),col=c(1,2,3,4),lty =c(1,1,1,1))
abline(v=1976,lty=3)

