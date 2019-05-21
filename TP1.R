
#####################TP1 NDIAYE Laity ###############

#-----------Calcul des cooefficients de correlation-----------------------------------------

data1 = read.table("data1TP1.txt", header = TRUE)

#1 Tracé du nuage de 15 points pour chaque variable.

par(mfrow=c(2,3))

plot(d$A,d$Y,pch=16, col='red')
plot(d$B,d$Y,pch=16, col='blue')
plot(d$C,d$Y,pch=16,col='green')
plot(d$D,d$Y,pch=16,col='brown')
plot(d$E,d$Y,pch=16,col='yellow')

#2 Coefficient r de Pearson

r <- function(X, Y){
  return (cov(X, Y)/(sd(X)*sd(Y)))
}

#Test de notre fonction
paste("coefficient de r de Pearson avec A et Y (fonction) :", r(data1$A, data1$Y) )

#Vérification avec la fonction cor
paste("coefficient de r de Pearson avec A et Y (cor) :", cor(data1$A, data1$Y) )

#Test
paste("coefficient de r de Pearson avec B et Y (fonction) :", r(data1$B, data1$Y) )
paste("coefficient de r de Pearson avec C et Y (fonction) :", r(data1$C, data1$Y) )
paste("coefficient de r de Pearson avec D et Y (fonction) :", r(data1$D, data1$Y) )
paste("coefficient de r de Pearson avec E et Y (fonction) :", r(data1$E, data1$Y) )

#Analyse de nos résultats
#En valeur absolue, La variable E a la plus petite correlation.
#Sinon c'est La variable A qui a la plus petite correlation.

#3 Fonction du coefficient de Spearman

N = 15
rho <- function(X, Y){
  r = 6*sum((rank(X) - rank(Y))^2)
  return (1 - r/(N^3 - N))
}

#Test de notre fonction
paste("coefficient de Spearman avec A et Y (fonction) :", rho(data1$A, data1$Y) )

#Vérification avec la fonction cor
paste("coefficient de Spearman avec A et Y (cor) :", cor(data1$A, data1$Y, method="spearman") )

#Test
paste("coefficient de Spearman avec B et Y (fonction) :", rho(data1$B, data1$Y) )
paste("coefficient de Spearman avec C et Y (fonction) :", rho(data1$C, data1$Y) )
paste("coefficient de Spearman avec D et Y (fonction) :", rho(data1$D, data1$Y) )
paste("coefficient de Spearman avec E et Y (fonction) :", rho(data1$E, data1$Y) )



#-----------Test de validation d'hyphotèses---------------------------


#-----------Test paramétrique-----------------------------------------


#5 Test d'indépendance pour une variable quantitative
data2=read.table("data2TP1.txt", header = TRUE)



#H0 = l'inflation n'a pas affecté le coût de la vie.
#degré de liberté = 14
#alpha = 5%
#score < t-distribution => on accepte H.
#score > t-distribution => H fausse, l'inflation a affecté le coût de la vie.

score_t <- function(X){
  abs( mean(X)-19 ) / ( sd(X) / sqrt(length(X)) )
}

paste('Score de t 2010-2019 : ', score_t(data2$Marseille))

#Dans notre cas-ci, on se rencontre que l'inflation a affecté le cout de la vie.


#6 Test d'indépendance pour deux variables quantitatives

#degré de liberté = 29
#H1 = dépendance significative entre Marseille et Aix.
score_t_2_variable <- function(X,Y){
  abs( mean(X) - mean(Y) ) / sqrt( sd(X)^2/length(X)+sd(Y)^2/length(Y) )
}

paste('Score de t pour 2 variables: ', score_t_2_variable(data2$Marseille, data2$Aix) )
#avec alpha = 5%, H0 fausse => il n'y a pas de dépendance.
#Cépendant avec 2%, H0 vraie => il y a une dépendance.

#----------Test non paramétrique--------------------

#7 Test d'indépendance pour une variable qualitative

#Ho = le ratio est 9:3:3:1
#alpha = 5% 
#degré de liberté = 4 - 1
#Khi-distribution = 7.81
#score < Khi-distribution => on accepte H.
#score > Khi-distribution => H fausse, l'inflation a affecté le coût de la vie.

m_phe <- matrix(c(9, 1528, 3, 106, 3, 117, 1, 381), nrow = 2, ncol = 4)
somme_ratio = sum(m_phe[1, ])
V_th_phe = m_phe[1, ]/somme_ratio * sum(m_phe[2, ])
paste("les valeurs théoriques pour chaque phonétique est: ", V_th_phe)

#Créons notre fonction Khi

Khi_deux <- function(val_obs, val_th){
  res = (val_obs - val_th)^2/val_th
  res = sum(res)
  res
}

paste("Khi_deux : ", Khi_deux(m_phe[2,], V_th_phe))

#Ho faux, nous pouvons conclure que 9:3:3:1 n'est pas le vrai ratio.


#8 Test d'indépendance pour les variables qualitatives

#Ho : deux variables sont indépendantes      ---- alpha = 5% 
#degré de liberté FORM = 3 - 1               ---- Khi-distribution FORM = 5.99
#degré de liberté COLOR = 2 - 1              ---- Khi-distribution COLOR = 3.84

m_form <- matrix(c(29, 40, 18, 5, 32, 22, 46, 8, 0) , nrow = 3, ncol = 3)
V_th_form <- matrix(nrow = 3, ncol = 3)
m_color <- matrix(c(20, 29, 12, 60, 51, 28), nrow=3 , ncol = 2)
V_th_color <- matrix(nrow=3 , ncol = 2)

for (i in 1:3){
  for (j in 1:3){
    V_th_form[i,j] = sum(m_form[i, ])*sum(m_form[, j])/sum(m_form)
  }
}

for (i in 1:3){
  for (j in 1:2){
    V_th_color[i, j] = sum(m_color[i, ])*sum(m_color[, j])/sum(m_color)
  }
}

paste("Khi_deux form :", Khi_deux(m_form, V_th_form))
paste("Khi_deux color :", Khi_deux(m_color, V_th_color))

#HO est faux pour le test du Khi_deux entre diagnostic-forme
#HO est vrai pour le test du Khi_deux entre diagnostic-couleur
#la variable couleur est importante pour detecter un melanome

#Q9 
#
# Nous pouvons appliquer les tests Student/t sur les données qualitatives uniquement 
# sur 2 échantillons.
#

#Q10
#
# Nous ne pouvons pas appliquer le coéfficient de Pearson et de Spearman sur les
# données qualitatives, c'est réservé uniquement pour les données quantitatives
#
