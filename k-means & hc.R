### Agrupamiento de k medias ###
## funcion kmeans()
## ejemplo simulado, en el que hay dos grupos en los datos:
## las primeras 25 observaciones tienen un cambio medio relativo 
## a las siguientes 25 observaciones
set.seed(2)
x <- matrix(rnorm(50 * 2), ncol = 2)
View(x)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
## realizamos el agrupamiento de k-medias con k=2
km.out <- kmeans(x, 2, nstart = 20)
## las asignaciones (cluster) de las 50 observaciones estan contenidas en 
km.out$cluster
## el agrupamiento de k-medias separo perfectamente las observaciones
## en dos grupos a pesar de que no proporcionamos ninguna informacion de grupo a kmeans()
## Graficamos los datos, con cada observacion coloreada segun su asignacion de grupo
par(mfrow = c(1,2))
plot(x, col =  (km.out$cluster + 1), main = "Resultados de agrupamiento de k-medias con k=2"
     , xlab = "", ylab = "", pch = 20, cex = 2)
## en este ejemplo sabiamos que realmente habia dos clusters porque generamos los datos
## para datos reales no conocemos el verdadero numero de conglomerados
## k-means con k=3
set.seed(4)
km.out <- kmeans(x, 3, nstart = 20)
km.out
plot(x, col = (km.out$cluster + 1), main = "Resultados de agrupamiento de k-medias con k=3",
     xlab = "", ylab = "", pch = 20, cex = 2 )
## cuando k=3, el agrupamiento de k-medias divide los dos grupos
## para ejecutar la funcion kmeans() con varias asignaciones de clusters iniciales, usamos el
## argumento nstart
## si se usa un valor de nstart mayor que uno,
## entonces el agrupamiento de k-medias se realizara usando multiples
## asignaciones aleatorias en el Paso 1 del Algoritmo 
## y la funcion kmeans() informara solo los mejores resultados
## comparamos usando nstart=1 con nstart=20
set.seed(4)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss
## tenga en cuenta que km.out$tot.withinss es la suma de cuadrados
## total dentro del cluster
## las sumas de cuadrados individuales dentro del grupo estan contenidas en el vector
## km.out$withinss
## se recomienda ejecutar siempre el agrupamiento de k-means con nstar grande, como 20 o 50,
## de lo contrario se puede obtener un minimo local no deseado

## Hierarchical Clustering
## trazar el dendrograma de agrupamiento jerárquico utilizando el agrupamiento de vínculos
## completo, único y promedio, con la distancia euclidiana como medida de disimilitud

## Comenzamos agrupando las observaciones utilizando un enlace completo
# La función dist() se utiliza para calcular la matriz de distancia euclidiana 
# de interobservación de 50 × 50.

hc.complete <- hclust(dist(x), method = "complete")

##agrupamiento jerarquico con un enlace promedio o unico

hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")

## graficamos los dendogramas 
#  Los números en la parte inferior de la gráfica identifican cada observación.
par(mfrow = c(1, 3))
plot(hc.complete, main = "Enlace completo", xlab = "", sub = "", cex = 0.9)
plot(hc.average, main = "Enlace promedio", xlab = "", sub = "", cex = 0.9)
plot(hc.single, main = "Enlace unico", xlab = "", sub = "", cex = 0.9)

## Para determinar las etiquetas de conglomerados para cada observación 
## asociada con un corte dado del dendrograma, podemos usar la función cutree():
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

# Para escalar las variables antes de realizar el agrupamiento 
# jerárquico de las observaciones, usamos la función scale():

xsc <- scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Clustering jerarquico con caracteristicas escaladas")
cutree(hclust(dist(xsc), method = "complete"),2)

##  falta distancia basada en la correlacion ### 

## Analisis de Datos genomicos ###
## Ilustramos estas tecnicas (PCA y HC) en los datos de microarrays de la linea
## de celulas cancerosas NCI60, que consta de 6,830 mediciones de expresion
## genica en 64 lineas celulares de cancer

install.packages("ISLR2")
library(ISLR2)
nci.labs <- NCI60$labs  ## Cada linea celular esta etiquetada con un tipo de cáncer, dado en nci.labs
nci.data <- NCI60$data

# No hacemos uso de los tipos de cáncer al realizar PCA y agrupamiento, 
# ya que estas son técnicas no supervisadas
# Pero después de realizar PCA y agrupamiento, verificaremos hast
# a qué punto estos tipos de cáncer concuerdan con los resultados de estas técnicas no supervisadas

dim(nci.data)
##64 renglones y 6830 columnas

##Comenzamos examinando los tipos de cáncer para las líneas celulares
## estandarizamos las variables media=0, y desv esta=1
## cada gen estara en la misma escala
sd.data <- scale(nci.data)

par(mfrow = c(1, 3))
data.dist <- dist(sd.data)
plot(hclust(data.dist, method = "complete"), main = "Enlace completo", xlab = "", sub = "", ylab = "", labels = nci.labs)
plot(hclust(data.dist, method = "average"), main = "Enlace promedio", xlab = "", sub = "", ylab = "", labels = nci.labs)
plot(hclust(data.dist, method = "single"), main = "Enlace unico", xlab = "", sub = "", ylab = "", labels = nci.labs)


