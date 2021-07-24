#librerie necessarie
library(tidyverse)
library(RColorBrewer)
library(MASS)

#carichiamo il dataset
string = "Data/parkinson_anova.csv"
pd <- read_csv(string, col_types = cols(`Participant  code` = col_number()))

#creiamo il boxplot
my_colors = brewer.pal( 3, 'Set1')
x11()
par(mfrow=c(1,1))
boxplot( pd$`Rate  of  speech  timing  (-/min)` ~ pd$`Participant  code`, xlab = 'class', ylab = 'RST',
         main = 'Rate of speech timing according to class',
         col = my_colors )
abline( h = mean( pd$`Rate  of  speech  timing  (-/min)` ) )

#verifichiamo che siano soddisfatte le ipotesi dell'ANOVA:

#normalità con Shapiro
PvalueShapiro = c( shapiro.test( pd$`Rate  of  speech  timing  (-/min)` [pd$`Participant  code`==1] )$p,
        shapiro.test( pd$`Rate  of  speech  timing  (-/min)` [pd$`Participant  code`==2] )$p,
        shapiro.test( pd$`Rate  of  speech  timing  (-/min)` [pd$`Participant  code`==3] )$p)

#guardiamo i valori
PvalueShapiro

#trasformazione Box-Cox
g = lm(pd$`Rate  of  speech  timing  (-/min)` ~ pd$`Participant  code`)
b = boxcox( g, lambda = seq(-3,3,by=0.01) )

#troviamo il lambda che massimizza la likelihood
best_lambda = b$x[ which.max( b$y ) ]
best_lambda

#applichiamo la trasformazione
rst_b=((pd$`Rate  of  speech  timing  (-/min)`)^best_lambda-1)/best_lambda

#verifichiamo di nuovo le ipotesi dell'ANOVA
PvalueShapiro2=tapply( rst_b, pd$`Participant  code`, function( x ) shapiro.test( x )$p )
PvalueShapiro2

#gli alti p-values ci permettono di accettare H0 e quindi la normalità dei dati

#omoschedasticità

#scriviamoci le varianze per ogni classe
Var = c( var( rst_b [pd$`Participant  code`==1] ),
         var( rst_b [pd$`Participant  code`==2] ),
         var( rst_b [pd$`Participant  code`==3] ))
Var

#per verificare l'omogeneità tra le varianze abbiamo diverse possibilità. 

#test di Bartlett
#con ipotesi H_0: sigma_1 = sigma_2 = ... = sigma_g  vs H_1: H_0^C
#Il test di Bartlett assume che le osservazioni appartenenti ai vari gruppi siano iid da una Normale.

#applichiamolo

#prima di Box-Cox
bartlett.test( pd$`Rate  of  speech  timing  (-/min)`, pd$`Participant  code`)
#dopo Box-Cox
bartlett.test( rst_b, pd$`Participant  code`)

#Il test di Bartlett in entrambi i casi accetta H_0.

#test di Levene
#anche il test di Levene serve per valutare l'omogeneità delle varianze di una variabile calcolato per due o 
#più gruppi. Questo test è un'alternativa a quello di Bartlett, meno sensibile alla non normalità dei dati.

#prima di Box-Cox
leveneTest( pd$`Rate  of  speech  timing  (-/min)`, pd$`Participant  code` )
#dopo Box-Cox
leveneTest( rst_b, pd$`Participant  code` )

#anche il test di Levene è concorde nell'accettare l'ipotesi nulla.

#ora che abbiamo verificato che le ipotesi sono soddisfatte possiamo procedere con una One-Way ANOVA.

#one-way ANOVA

fit = aov( rst_b ~ pd$`Participant  code` )
summary( fit )

#affermiamo quindi che c'è differenza delle medie fra i gruppi.
