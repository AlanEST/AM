## Conjunto de datos NCI60, 
## 6830 mediciones de expresión génica
## para cada una de las 64 líneas celulares
## de cáncer (hay 14 tipos de cáncerc)
## en lugar de predecir una variable de salida particular
## estamos interesados en determinar si hay grupos
## o grupos entre líneas celulares en función
## de sus mediciones de expresión génica
## hay miles de medicines de expresión génica
## por línea celular, lo que dificulta la visualización

library(ISLR2)
nci.labs <- NCI60$labs #cada línea celular está etiquetada con un tipo de cancer
nci.data <- NCI60$data

## No hacemos uso de los tipos de cáncer al realizar PCA y agrupamiento, 
## ya que estas son técnicas no supervisadas.
## Pero después de realizar PCA y agrupamiento,
## verificaremos hasta qué punto estos tipos de cáncer concuerdan
## con los resultados de estas técnicas no supervisadas.

dim(nci.data) ## 64 renglones y 6,830 columnas

## Comenzamos examinando los tipos de cáncer para las líneas celulares.

nci.labs[1:4]
table(nci.labs)

## PCA en los datos NCI60
## Primero realizamos PCA en los datos después de escalar las variables (genes)

pr.out <- prcomp(nci.data, scale = TRUE)

## graficamos los scores de los componentes principales
## Las observaciones (líneas celulares) correspondientes 
## a un tipo de cáncer dado se trazarán en el mismo color,
## para que podamos ver en qué medida las observaciones dentro 
## de un tipo de cáncer son similares entre sí.

Cols <- function(vec) {
    cols <- rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])}

## funcion que asigna un color distinto a cada elemento de un vector numerico
## asignará un color a cada una de las 64 líneas celulares, segun el tipo
## de cáncer al que corresponda.

par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, c(1, 3)], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = "Z3")

## en estos gráficos observamos que, en general,
## las líneas celulares correspondientes a un solo tipo de cáncer
## tienden a tener valores similares en los primeros vectores
## de puntuación de componentes principales.
## Esto indica que las líneas celulares del mismo tipo de cáncer
## tienden a tener niveles de expresión génica bastante similiares.

## Ahora obtenemos un resumen de la proporción de la varianza explicada
## para los primeros componentes principales.

summary(pr.out)

## Ahora graficamos la varianza explicada por los primeros componentes principales

plot(pr.out)

## para observar con mayor claridad, trazamos un gráfico de sedimentación (scree plot) y la proporción
## de la varianza explicada acumulada de cada componente principal

pve <- 100 * pr.out$sdev^2 / sum(pr.out$sdev^2)
par(mfrow = c(1, 2))
plot(pve, type = "o", ylab = "Proporción de la Varianza Explicada",
     xlab = "Componente Principal", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Proporción de la Varianza Explicada Acumulada",
     xlab = "Componente Principal", col = "brown3")

## vemos que mientras cada uno de los primeros siete componentes principales explica 
## una cantidad sustancial de varianza, hay una marcada disminución en la varianza explicada 
## por otros componentes principales.
## Es decir, hay un codo en la gráfica después de aproximadamente el séptimo componente principal.
## Esto sugiere que puede haber poco beneficio en examinar más de siete o más componentes principales 
## (aunque incluso examinar siete componentes principales puede ser difícil).

## Clustering Jerárquico de las observaciones de los datos NCI60
## Ahora procedemos a agrupar jerárquicamente las líneas celulares en los datos del NCI60, 
## con el objetivo de averiguar si las observaciones se agrupan o no en distintos tipos de cáncer.

## estandarizamos las variables
sd.data <- scale(nci.data)

## Ahora realizamos un agrupamiento jerárquico de las observaciones utilizando un enlace completo,
## único y promedio. La distancia euclidiana se utiliza como medida de disimilitud.

par(mfrow = c(1, 3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "",labels = nci.labs, main = "Vínculo Completo")
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Vínculo Promedio", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Vínculo Único", xlab = "", sub = "", ylab = "")

## la elección del enlace afecta los resultados obtenidos.
## Por lo general, el enlace único tenderá a producir grupos de seguimiento:
## grupos muy grandes a los que se unen observaciones individuales 
## una por una.
## Por otro lado, los vínculos completo y promedio, tienden a producir
## conglomerados más equilibrados y atractivos (por esto, preferimos esos vínculos)
## Las líneas celulares dentro de un solo tipo de cáncer tienden a agruparse,
## aunque la agrupación no es perfecta.

## Continuamos el estudio con el vínculo completo

## Cortamos el dendograma a una altura particular
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)

## Todas las líneas celulares de leucemia caen en el grupo 3, 
## mientras que las líneas celulares de cáncer de mama se distribuyen en tres grupos diferentes.

## Trazamos el corte en el dendrograma que produce estos cuatro grupos:
par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")

## Imprimimos un resumen de hclust
hc.out

## ¿Cómo se comparan estos resultados de agrupación jerárquica de NCI60 
## con lo que obtenemos si realizamos una agrupación de K-medias con K = 4?

set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)

## El grupo 4 en el agrupamiento de K-medias es idéntico al grupo 3 en el agrupamiento jerárquico.
## Sin embargo, los otros conglomerados difieren: por ejemplo,
## el conglomerado 2 en el conglomerado de K-medias contiene una parte de las observaciones asignadas al conglomerado 1 por conglomerado jerárquico,
## así como todas las observaciones asignadas al conglomerado 2 por conglomerado jerárquico.

## Podríamos un agrupamiento jerárquico en los primeros vectores de puntuación de los componentes principales, de la siguiente manera:

hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs, main = "Clust. Jerarq. en los Primeros Cinco Vectores de Puntuación")
table(cutree(hc.out, 4), nci.labs)