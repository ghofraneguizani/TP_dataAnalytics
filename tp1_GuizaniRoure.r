#TP1 - Analyse de Données
#INFO 4A
#Ghofrane Guizani
#Bastien Roure

#Le calcul des Coefficients de Corrélation
d <- read.delim("/amuhome/r17006296/Bureau/DataAnalysis-master/data1TP1.txt", header=TRUE, sep="\t")

attach(d)
par(mfrow=c(2,3))
  plot(d$A,d$Y,col="red", pch=16)
  plot(d$B,d$Y, pch=16,col="blue")
  plot(d$C,d$Y, pch=16,col="magenta")
  plot(d$D,d$Y, pch=16, col="green")
  plot(d$E,d$Y, pch=16)
#1
#nous remarquons que les figures A B et D sont lineaires et monotones alors que les deux autres ne sont pas monotones(D) ni lineaires (C)
 
#2 fonction pearson 
  pearson <- function(X,Y) {
    sX <- sqrt(var(X))
    sY <- sqrt(var(Y)) 
    
    p <- cov(X,Y)/(sX*sY)
    return(p)
  }
  
#pearson 
print("pearson A=")
print(pearson(d$A,d$Y))
print("pearson cor A=")
print(cor(d$A,d$Y, method="pearson"))

print("pearson B= ")
print(pearson(d$B,d$Y))
print("pearson cor B= ")
print(cor(d$B,d$Y, method="pearson"))

print("pearson C= ")
print(pearson(d$C,d$Y))
print("pearson cor C= ")
print(cor(d$C,d$Y, method="pearson"))


print("pearson D= ")
print(pearson(d$D,d$Y))

print("pearson cor D= ")
print(cor(d$D,d$Y, method="pearson"))

print("pearson cor E= ")
print(pearson(d$E,d$Y))
print(cor(d$E,d$Y, method="pearson"))

#la variable qui a le coefficient de correlation le plus petit est la figure E parce que elle est la courbe la plus non linéaire 
  
spearman <- function(X,Y,N) {
    Xi <- rank(X)
    Yi <- rank(Y)
    somme <- 0
    for (i in 1:N) {
      somme <- somme + (Xi[i]-Yi[i])^2
}
  coeff <- 1- ((6*somme)/(N^3 - N))
    return(coeff)
}
print("spearman")
spearman(d$A,d$Y,15)
cor(d$A,d$Y, method="spearman")

print("pspearman A=")
print(spearman(d$A,d$Y,15))
print("spearman cor A=")
print(cor(d$A,d$Y, method="spearman"))

print("spearman B= ")
print(spearman(d$B,d$Y,15))
print("pearson cor B= ")
print(cor(d$B,d$Y, method="spearman"))

print("spearman C= ")
print(spearman(d$C,d$Y,15))
print("pearson cor C= ")
print(cor(d$C,d$Y, method="spearman"))


print("spearman D= ")
print(spearman(d$D,d$Y,15))

print("spearman cor D= ")
print(cor(d$D,d$Y, method="spearman"))

print("spearman cor E= ")
print(spearman(d$E,d$Y,15))
print(cor(d$E,d$Y, method="spearman"))
  

#La différence c'est que la fonction de spearman calcule la linearite entre les variables et Y  alors que 
#la foncion de pearson calcule la monotonie des variables

#4 - on peut utiliser en premier lieu pearson pour la non linearite et aprés spearman pour la monotonie 

#Test de validation d'hypothèses
#Test Paramétrique
etudiant <- read.delim("/amuhome/r17006296/Bureau/DataAnalysis-master/data2TP1.txt", header=TRUE, sep="\t")


#5  
testinde <- function(X,moyth,n) {
    m <- mean(X)
   
    sigma <- sqrt(var(X))
    testinde <- abs(m - moyth)/(sigma/sqrt(n))
    return(testinde)
    }
#notre hypothese c'est que l'inflation affecte le cout de vie 
#on prend h0=l'inflatiohn n'affecte pas le cout de vie 
#puis on calcule la valeur critique=2.145 et le score ici =2.177369
#on remarque que la valeur critique est plus grande que le score donc notre hypothese H0 est rejeté .
#nous pouvons pas conclure si l'inflation affecte le cout de la vie ou non .
print("score=")
print(testinde(etudiant$Marseille,19,15))
  

#6
  
testinde2 <- function(X,Y) {
    m1 <- mean(X)
    m2 <- mean(Y)
    testinde <- abs(m1 - m2)/sqrt(((var(X)/length(X))+(var(Y)/length(Y))))
    return(testinde)
}
  

#notre hypothese correspond a une dépendance significative entre Marseille et Aix-en-Provence
#on prend h0=il existe une dépendance significative entre Marseille et Aix-en-Provence 
#puis on calcule la valeur critique=20.07 et le score ici =2.321494
#on remarque que la valeur critique est plus petite que le score donc notre hypothese H0 est accepté .
#nous pouvons qu'il existe une dépendance significative entre Marseille et Aix-en-Provence .
print(testinde2(etudiant$Marseille,etudiant$Aix))
  
#test non parametrique:
n<-1526+106+117+381
VL<-(n*9)/16
VR<-(n*3)/16
RL<-(n*3)/16
RR<-n/16
l<-data.frame(vioLong = c(9, 1528, VL), VoiRond = c(3 ,106,VR),RouLong = c(3,117,RL) ,RouRond=c(1,381,RR))

khiDeux<- function(l){
  somme = 0
  for (i in 1:length(l)){
    O = l[2,i]
    E = l[3,i]
    somme = somme + (((O-E)^2)/E)
  }
  return(somme)
}
print("khideux=")
print(khiDeux(l))
#notre hypothese est "le vrai ratio est 9:3:3:1"
#on prend h0="le vrai ratio est 9:3:3:1"
#puis on regarde la valeur de khi deux dans le tableau =2.73 et le khi deux calculé ici =967.5195
#on remarque que la valeur critique est plus grande que le score donc notre hypothese H0 est rejetée .
#nous pouvons que le vrai ratio n est pas 9:3:3:1.


#8
l2f<-data.frame(dotCNA = c(29,34.8), dotCNat = c(5,23.6), dotCNT = c(46,21.6), dotANA = c(5,34.8), dotANat = c(32,23.6), dotANT = c(22,21.6), dotMA = c(46,17.4), dotMat = c(8,11.8), dotMT = c(0,10.8))
l2c<-data.frame(lightCNA = c(20,24.4), lightCNP = c(60,55.6), lightANA = c(29,24.4), lightANP = c(51,55.6), lightMA= c(12,12.2), lightMP = c(28,27.8))

khiDeux2<- function(l){
  somme = 0
  for (i in 1:length(l)){
    O = l[1,i]
    E = l[2,i]
    somme = somme + (((O-E)^2)/E)
  }   
  return(somme)
}

print(khiDeux2(l2f))
print(khiDeux2(l2c))
#notre hypothese est "les deux variables sont indepandantes"
#on prend h0="les deux variables sont indepandantes"
#puis on regarde les valeurs de khi deux dans le tableau =2.73 et 1.15 et les khi deux calculé ici =130.7375 et 2.39415
#on remarque que les valeurs critiques sont plus grandes que les scores corresponants donc notre hypothese H0 est rejetée .
#nous pouvons que lles deux variables sont indepandantes.

#9
#Le test de Student est basé sur un échantillon qui suit une distribution normale et le test du Khi deux suit une distribution expérimentale et pas statistique.
#Le test de Student est appliqué aux données quantitatives sinon il n est pas fiable.

#10
#Le coefficient de Pearson et le coefficient de Spearman sont appliqués aux données quantitatives sinon ils ne sont pas fiables.
