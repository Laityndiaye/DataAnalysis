

#####################TP2 NDIAYE Laity ###############


library("plot3D")
library("rgl")

my_data2 <- read.delim("data1TP2.txt")

#1 Tracez en dimension 3 le nuage de 10 points
scatter3D(my_data2$Stature, my_data2$Poids,my_data2$Taille, colvar = my_data2$Taille, col = NULL, add = FALSE)


#2 Écrivez le tableau centré B, et la matrice de covariance V

moyStature <- mean(my_data2$Stature) 
moyPoids <- mean(my_data2$Poids) 
moyTaille <- mean(my_data2$Taille) 
tabStature <- c()
tabPoids <- c()
tabTaille <- c()

for (i in 1:length( my_data2$Stature)) {
  tabStature[i] <- my_data2$Stature[i]- moyStature
  
}
for (i in 1:length( my_data2$Poids)) {
  tabPoids[i] <- my_data2$Poids[i]- moyPoids
 
}
for (i in 1:length( my_data2$Taille)) {
  tabTaille[i] <- my_data2$Taille[i]- moyTaille
 
}
B<-matrix( c(tabStature,tabPoids,tabTaille), # the data elements 
   nrow=10,              # number of rows 
  ncol=3,              # number of columns 
  byrow = FALSE
)

print(B)
V <- cov(B)
print(V)


#3 Déterminez la représentation spectrale (valeurs propres et vecteurs propres de V). 
x = eigen(V)
print("les valeurs propres ")
print(x$values)
print(x$vectors)

#4 Indiquez les axes principaux (dans l'ordre). 
# l'axe principale est le premier axe des vecteurs propres 

#5 Générez le tableau C en multipliant B par les vecteurs propres de V. 

C <- B%*%x$vectors
print(C)
princomp(my_data2)$scores

#6 Observez en dimension 3 le nuage de points avec tracé du premier axe principal. 

scatter3D(x =c (0,-300*x$vectors[1,1]), y =c(0,-300*x$vectors[2,1]), z = c(0,-300*x$vectors[3,1]), add = TRUE, type ="l")
plot()
#7 Représentez le nuage de points en dimension 2
plot(C[,1],C[,2])


