#TP2 - Analyse de Données
#INFO 4A
#Ghofrane Guizani
#Bastien Roure


#Analyse en Composantes Principales
#1

d <- read.delim("/amuhome/r17006296/Bureau/DataAnalysis-master/data1TP2.txt", header=TRUE, sep="\t")
library(scatterplot3d)
library(plot3D)
library(rgl)
A <- subset(d, select = c(Stature, Poids, Taille))

nuage3d <- scatterplot3d(A)

#2
B<-scale(A, center = TRUE, scale = FALSE)
V<-cov(A)

#3
x = eigen(V)
valP=x$values # valeurs propres
vectP=x$vectors # vecteurs propres

#4
#Les axes principaux sont l'ordre suivants (classés selon leur valeur propre) : Stature, Poids et Taille

pca <- prcomp(A)
pca$rotation
ecartType <- pca$sdev
variance <- 100 * pca$sdev^2 / sum(pca$sdev^2)  
varianceTotale <- sum(100 * (pca$sdev^2)[1:2] / sum(pca$sdev^2)) 
print(ecartType)
print(variance)
print(varianceTotale)
plot(pca)

#5
c<-B %*% vectP

princomp(A)$scores

#6
pca = prcomp(A)

pc1 <- pca$rotation[,1]

pc1 <- as.vector(pc1)
a <- c(pca[2]$rotation[1,1],0)
b <- c(pca[2]$rotation[2,1],0)
c <- c(pca[2]$rotation[3,1],0)

plot3d(a,b,c, type="l")
scatter(pc2,pc3)


D <- matrix(nrow = 10,ncol=1);
for( i in 1:10){
  D[i,1] <- pc1[1] * A[i,1] + pc1[2] * A[i,2] + pc1[3] * A[i,3]
  }


plt<-scatterplot3d(pca$rotation)
plt$points3d(pc1, type="l", col="blue", lwd=2)

# 7. 

P = matrix(c(C[,1],C[,2]), ncol=2)
plot(P[,1], P[,2], main="Nuage", xlab="Stature", ylab="Poids", pch=19)

plot(C[,1],C[,2])

# 8.
#Les résultats obtenus sont cohérents, en effet on remarque que la stature posséde les valeurs les plus important,

