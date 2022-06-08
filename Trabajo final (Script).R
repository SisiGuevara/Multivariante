
#_____ Dendrograma____

# Exploración de la matriz 

library(cluster.datasets)
library(datasets)
library(knitr)

data("longley")
datos=longley[,-6]
kable(datos)


head(datos)
names(datos)
dim(datos)
str(datos)
anyNA(datos)
?longley

# Calculo de la matriz de distancia de Mahalonobis
dist.datos<-dist(datos)

# Proyección de la matriz de la distancia redondeada
round(as.matrix(dist.datos),3)

# Gráfico de la matriz de distancias
library(factoextra)
fviz_dist(dist.obj = dist.datos, lab_size = 10)

# Calculo del dendrograma
dend.datos<-as.dendrogram(hclust(dist.datos))

# Generacion del dendrograma
plot(dend.datos)

# Dendrograma con tres particiones 
library(factoextra)
library(ggplot2)
hc <- hclust(dist.datos)
fviz_dend(as.dendrogram(hc), 
          k = 3)







