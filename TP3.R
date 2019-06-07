# TP3 Analyse de données
# NDIAYE Laity

# 1.--------------------- On genere un nuage de 300 points avec 3 modes de generation differents----------------------

# a. Generation de x et y uniformes sur [0,1]
x <- runif(100,0,1)
y <- runif(100,0,1)
nuage <- cbind(x,y) 

# b. Generation de x et y gaussiennes de variance 1, avec x de moyenne 4 et y centree
x <- rnorm(100,4,1)
y <- rnorm(100)
nuage <- rbind(nuage, cbind(x, y) ) 

# c. Generation de x et y gaussiennes de variance 2, avec x de moyenne 0.5 et y de moyenne 6
x <- rnorm(100,0.5,sqrt(2))
y <- rnorm(100,6,sqrt(2))
nuage <- rbind(nuage, cbind(x, y) ) 

colnames(nuage) <- c("X","Y")

# Visualisation points obtenus des points obtenus
plot(nuage[,1:2], pch = 4)

# 2----------------------- On ecrit une fontion de classification ascendante hierarchique------------------------------


classificationAscendanteHierarchique <- function(K, m, M){
  # On cree une matrice D contenant les distances entre tous les points de la matrice
  D<-dist(M, method = "euclidean")
  D<-as.matrix(D)
  
  # Passage de D en matrice triangulaire
  D[upper.tri(D)] <- NA
  diag(D) <- NA
  
  # Recherche de la distance minimum
  temp <- which(D == min(D, na.rm = T), arr.ind = T)
  tempX <- temp[,1]
  tempY <- temp[,2]
  
  # Calcul de la moyenne
  moyX <- (M[tempX, 1] + M[tempY, 1]) / 2 
  moyY <- (M[tempX, 2] + M[tempY, 2]) / 2 
  

  while(nrow(D) > K){
    temp <- which(D == min(D, na.rm = T), arr.ind = T)
    tempX <- temp[1,1]
    tempY <- temp[1,2]
    
    moyX <- (M[tempX, 1] + M[tempY, 1]) / 2 
    moyY <- (M[tempX, 2] + M[tempY, 2]) / 2
    
    M[tempX, 1] <- moyX
    M[tempX, 2] <- moyY
    
    M <- M[-tempY,]
    
    D <- D[-tempY,]
    D <- D[,-tempY] 
    
    # Et on recommence en redimensionnant la matrice D
    D<-dist(M, method = "euclidean")
    D<-as.matrix(D)
    
    D[upper.tri(D)] <- NA
    diag(D) <- NA
  }
  
  return (M)
}

# Calcul des centres de classes
centreNuage <- classificationAscendanteHierarchique(3,300, nuage[,1:2])

# Vérification résultats
D <- dist(nuage, method = "euclidian")
AscHierarchique <- hclust(D, method = "complete")
plot(AscHierarchique, cex = 0.6, hang = -1)
cluster = cutree(AscHierarchique,3)


# 3.
# Affichage des donnees colorees
couleur<-c("#f079f2","orange","darkgreen")
plot(nuage[,1:2], pch = 4, col = couleur[cluster])

# Affichage des centres de classes
points(centreNuage, pch = 15, col = "red")

# Inertie relative intra-classe
inertie <- sort(AscHierarchique$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 4, 5), inertie[c(2, 4, 5)], col = c("#f079f2", "orange", 
                                                 "darkgreen"), cex = 2, lwd = 3)


