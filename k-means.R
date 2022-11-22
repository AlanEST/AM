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