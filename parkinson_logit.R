#librerie
library(tidyverse)
library(ggplot2)
library(MASS)
library(ResourceSelection)
library(PRROC)
library(corrplot)

#carico il dataset
string = "Data/parkinson_logit.csv"
pd = read.csv(string)

#regressione logistica pre-stepwise
mod.pd = glm( pd$status ~ pd$MDVP.Fo.Hz.+pd$MDVP.Fhi.Hz.+pd$MDVP.Flo.Hz.+
                pd$MDVP.Jitter...+pd$MDVP.Jitter.Abs.+pd$MDVP.RAP+
                pd$MDVP.PPQ+pd$Jitter.DDP+pd$MDVP.Shimmer+pd$MDVP.Shimmer.dB.+
                pd$Shimmer.APQ3+pd$Shimmer.APQ5+pd$MDVP.APQ+pd$Shimmer.DDA+
                pd$NHR+pd$HNR+pd$RPDE+pd$DFA+pd$spread1+pd$spread2+pd$D2+
                pd$PPE, family = binomial( link = logit ) , data = pd)
summary( mod.pd)

#studiamo le correlazioni
pd<-select(pd,-name)
m=cor(pd)
corrplot(m,method='color')

#usiamo stepAIC bilatero per minimizzare l'AIC
mod2.pd<-stepAIC(mod.pd,direction="both")
summary(mod2.pd)
AIC(mod2.pd)

#la scrematura del dataset è buona, ma non ottima:
#la variabile "pd$MDVP.Jitter" non viene eliminata nonostante un
#p-value relativamente alto (bassa significatività)

#ricorriamo quindi allo stepBIC bilatero per minimizzare il BIC
#(viene attribuita maggiore importanza alla riduzione del numero di variabili)
n = dim(pd)[1]
mod3.pd<-stepAIC(mod.pd,direction="both",k=log(n))
summary(mod3.pd)
BIC(mod3.pd)

#nonostante la scrematura, la devianza è ancora ottimale
anova( mod3.pd, mod.pd, test = "Chisq" )

#calcoliamo uno pseudo-R^2 con il metodo di McFadden
ll.null<-mod3.pd$null.deviance/-2
ll.proposed<-mod3.pd$deviance/-2
(ll.null-ll.proposed)/ll.null

#calcoliamo il suo p-value basato su una chi quadro
1-pchisq(2*(ll.proposed-ll.null),df=(length(mod3.pd$coefficients)-1))


#creiamo i dati
predicted.data <- data.frame(
  probability.of.pd=mod3.pd$fitted.values,
  pd=pd$status)

#ordiniamoli
predicted.data <- predicted.data[
  order(predicted.data$probability.of.pd, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

#plottiamo le probabilità
#in azzuro i veri positivi, in blu i veri negativi
ggplot(data=predicted.data, aes(x=rank, y=probability.of.pd)) +
  geom_point(aes(color=pd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting pd")


#facciamo un check sul GOF del modello.
hoslem.test( mod3.pd$y, fitted( mod3.pd ), g = 6 )


#tabelle di (mis-)classificazione

soglia = 0.5
valori.reali  = pd$status

#applichiamo la soglia
# 1 se > soglia, 0 se < = soglia
valori.predetti = as.numeric( mod3.pd$fitted.values > soglia )
valori.predetti

#creiamo la tabella
tab = table( valori.reali, valori.predetti )
tab

#La tabella riportata è detta matrice di confusione, e riporta le osservazioni dette 
#Veri Positivi (True Positive o TP, osservazioni 1 classificate come 1), 
#Veri Negativi (True Negative o TN, osservazioni 0 classificate come 0), 
#Falsi Positivi (False Positive o FP, osservazioni 0 classificati come 1), 
#Falsi Negativi (Falsi Negativi o FN, osservazioni 1 classificati come 0). 

#Ci sono numerose metriche che permettono di valutare le performance del modello, a seconda delle esigenze:
#Accuracy, Sensitivity, Specificity

#Accuracy

# % di casi classificati correttamente:
Corretti_pct=round( sum( diag( tab ) ) / sum( tab ), 2 )
Corretti_pct

# % di casi misclassificati:
Errati_pct=round( ( tab [ 1, 2 ] + tab [ 2, 1 ] ) / sum( tab ), 2 )
Errati_pct

#Sensitivity
sensitivity =  tab [ 2, 2 ] /( tab [ 2, 1 ] + tab [ 2, 2 ] ) 
sensitivity

#Specificity 
specificity = tab[ 1, 1 ] /( tab [ 1, 2 ] + tab [ 1, 1 ] )
specificity


#Curva ROC

fit2 = mod3.pd$fitted

#media campionaria della prob di sopravvivenza nel campione
soglie_roc  = seq( 0, 1, length.out = 2e2 )
lens = length( soglie_roc )-1
ascissa_roc  = rep( NA, lens )
ordinata_roc = rep( NA, lens )

for ( k in 1 : lens )
{
  soglia = soglie_roc [ k ]
  
  classification = as.numeric( sapply( fit2, function( x ) ifelse( x < soglia, 0, 1 ) ) )
  
  #ATTENZIONE, voglio sulle righe il vero e sulle colonne il predetto
  #t.misc = table( pd$status, classification )
  
  ordinata_roc[ k ] = sum( classification[ which( pd$status == 1 ) ] == 1 ) /
    length( which( pd$status == 1 ) )
  
  ascissa_roc[ k ] = sum( classification[ which(pd$status == 0 ) ] == 1 ) /
    length( which( pd$status == 0 ) )
  
  # ordinata_roc [ k ]  = t.misc [ 1, 1 ] /( t.misc [ 1, 1 ] + t.misc [ 1, 2 ] )
  #
  # ascissa_roc [ k ]  = t.misc [ 2, 1 ] /( t.misc [ 2, 1 ] + t.misc [ 2, 2 ] )
}


#Visualizziamo la curva ROC.

plot( ascissa_roc, ordinata_roc, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity",
      main = "Curva ROC", lwd = 2, col = 'darkblue', ylim = c( 0, 1 ), xlim = c( 0, 1 ) )
abline( h = c( 0, 1 ), v = c( 0, 1 ), lwd = 1, lty = 2, col = 'red' )
abline( a = 0, b = 1, lty = 2, col = 'black' )

#plottiamo il punto relativo alla soglia da noi scelta
abline( v = 1 - specificity,  h = sensitivity, lty = 3, col = 'blue' )
points( 1 - specificity, sensitivity, pch = 4, lwd = 3, cex = 1.5, col = 'blue' )


#facciamo stampare direttamente a R con il suo comando apposito
PRROC_obj <- roc.curve(scores.class0 = fit2, weights.class0=as.numeric(paste(pd$status)),
                       curve=TRUE)
x11()
plot(PRROC_obj)

