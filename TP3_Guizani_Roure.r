#Guizani Ghofrane
#Roure Bastien

#TP3 

#generation des matrices

generationTabunif<- function(m,mini,maxi){
  mat <- matrix(nrow=m, ncol=2)
  for (i in 1:m) {
    mat[i,1]=runif(1,mini,maxi)
    mat[i,2]=runif(1,mini,maxi)
    
  }
  return (mat) ;
}

T1=generationTabunif(100,0,1)
#plot(T1[,1],T[,2])

generationTabGauss<- function(m,moyenx,moyeny,variance){
  mat <- matrix(nrow=m, ncol=2)
  for (i in 1:m) {
    mat[i,1]=rnorm(1,moyenx,variance)
    mat[i,2]=rnorm(1,moyeny,variance)
    
  }
  return (mat) ;
}

T2=generationTabGauss(100,4,0,1)
#plot(T2[,1],T2[,2])

T3=generationTabGauss(100,0.5,6,2)
#plot(T3[,1],T3[,2])


copy<-function(m,T1,T2,T3){
  mat <- matrix(nrow=3*m, ncol=2)
  for (i in 1:m) {
    mat[i,1]=T1[i,1]
    mat[i,2]=T1[i,2]
  }
  for (i in 1:m) {
    mat[m+i,1]=T2[i,1]
    mat[m+i,2]=T2[i,2]
  }
  for (i in 1:m) {
    mat[2*m+i,1]=T3[i,1]
    mat[2*m+i,2]=T3[i,2]
  }
  return(mat)
}

mat<-copy(100,T1,T2,T3)
plot(mat[,1],mat[,2])
#¢lassification non supervisée 

classnonsup<-function(m,mat){
  d=matrix(nrow = m, ncol = m )
  d=dist(mat, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  return (d)
  
}

classnonsup(300,mat)

ex2 <- function(nuage, K){
  count = 1
  m = nrow(nuage)
  #C = diag(m)
  C <- diag(nrow = m, ncol = m)
  D = matrix(numeric(0),m,m)
  for (i in 1:m){
    for(j in 1:m){
      D[j,i] = sqrt((nuage[i,1]-nuage[j,1])^2 + (nuage[i,2]-nuage[j,2])^2)
    }
  }
  while (count <= m-K){
    cntmin = D[2,1]
    pi = 2
    pj = 1
    for(i in 2:(m-count-1)){
      for(j in 1:(i-1)){
        if(D[i,j] < cntmin){
          cntmin = D[i,j]
          pi = i
          pj = j
        }
      }
    }
    C[pj,] = C[pj,] + C[pi,]
    
    for(i in 1:m-count+1){
      nitmp = nuage[C[i,]==1,]
      gitmp = c(mean(nitmp$X),mean(nitmp$Y))
      dtmp = sqrt((gtmp[1]-gitmp[1])^2 + (gtmp[2]-gitmp[2])^2)
      if(i < pj){
        D[pj, i] = dtmp
      } else {
        D[i, pj] = dtmp
      }
    }
    C = C[-pi,]
    ntmp = nuage[C[pj,]==1,]
    gtmp = c(mean(ntmp$X), mean(ntmp$Y))
    D = D[-pi,]
    D = D[,-pi]
    count = count+1
  }
  return(t(C))
}

names(mat) <- c("X", "Y")
mat <- as.data.frame(mat)
C = ex2(mat,3)
D = dist(nuage, method = "euclidean")
AscHierarchique = hclust(D, method="complete")
plot(AscHierarchique, cex=0.6, hang=-1)
cluster = cutree(AscHierarchique, 3)




# affichage du cluster
plot(cluster)

# justifier le choix du nombre de classes# diagramme inertie en fonction du nombre de classes pour 

inertie <- sort(AscHierarchique$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 4, 5), inertieD[c(2, 4, 5)], col = c("green3", "red3", 
                                                 "blue3"), cex = 2, lwd = 3)


plot(AscHierarchique, labels = FALSE, main = "Partition en 2, 4 ou 5 classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(AscHierarchique, 2, border = "green3")
rect.hclust(AscHierarchique, 4, border = "red3")
rect.hclust(AscHierarchique, 5, border = "blue3")

# autre fonction de calcul des barycentres
barycentre2 <- function(dfxy, fac, wt = rep(1, length(fac))){
  fun1 <- function(cl) {
    n <- length(cl)
    cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1:n) + n * (unclass(cl) - 1)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    data.frame(x)
  }
    
  dfxy <- data.frame(dfxy)
  h=fun1(fac)
  dfdistri <- h * wt
  w1 <- unlist(lapply(dfdistri, sum))
  dfdistri <- t(t(dfdistri)/w1)
  coo2 <- t(dfdistri) %*% as.matrix(dfxy)
  rownames(coo2) <- levels(fac)
  coo2
}

ba1 <- barycentre2(matrice, cluster)

# la distance entre le barycentre du nuage de points et les barycentres des classes :
db1 <- (t(ba1)-colMeans(matrice))^2
db1 <- colSums(db1)

# multiplier par le nombre d'individus par groupe :
db1 <- db1*table(cluster)
db1
# l'inertie inter (%de l'inertie totale) :
iner.inter <- sum(db1)/sum((t(matrice)-colMeans(matrice))^2)
iner.inter

# inertie intra :
iner.intra <- 1-iner.inter
iner.intra




