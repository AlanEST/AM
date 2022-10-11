library("MVA")
ldemo("Ch-PCA")
# Extracción de Sigma vs R (ro)
blood_corr
blood_sd
#Hay diferencias considerables entre estas desviaciones #estándar.
blood_pcacov <- princomp(covmat = blood_cov)
summary(blood_pcacov, loading = TRUE)
blood_pcacor <- princomp(covmat = blood_corr)
summary(blood_pcacor, loading = TRUE)

## Examen:
## Los "espacios en blanco" en esta salida representan valores muy pequeños
## cada uno de los componentes principales de la matriz
## de covarianza está dominado en gran medida por una 
## sola variable
## la varianza de la variable placa es aproximadamente 400 veces mayor
## el primer PC de la matriz de correlación pondera a las 8 variables

## Las longitudes y anchuras de cabeza (en milímetros) ##para cada uno de los dos primeros hijos adultos en 25 ##familias. Aquí usaremos solo las longitudes de la ##cabeza; los anchos de cabeza se utilizarán más ##adelante en el capítulo. El vector medio y la matriz ##de covarianza de las medidas de la longitud de la 
##cabeza se encuentran usando

head_dat <- headsize[,c("head1", "head2")]
colMeans(head_dat)
cov(head_dat)

##PCs de sigma
head_pca <- princomp(x = head_dat)
head_pca
print(summary(head_pca), loadings = TRUE)

## Analizar varios aspectos de la contaminación, 
## y utilizarlo para abordar factores determinantes de la contaminación

#SO2: contenido de SO2 del aire en microgramos por metro cúbico;
#temp: temperatura media anual en grados Fahrenheit;
#manu: número de empresas manufactureras que emplean a 20 o más trabajadores;
#popul: tamaño de la población (censo de 1970) en miles;
#wind: velocidad media anual del viento en millas por hora;
#precip: precipitación media anual en pulgadas;
#predays: promedio de días con precipitación al año.

## Para comenzar ignoramos SO2 y nos fijamos en las variables restantes
## 2 (manu, popul) se relacionan con la ecología humana y 4 (temp, wind, precip, predays) con el clima
## 1. Antes de hacer el PCA de los datos de contaminación del aire, contruimos 
## la matriz de dispersión de las 6 variables (incluyendo los histogramas en la diagonal principal)

library("MVA")
library("HSAUR2")
#data("USairpollution", package = "HSAUR2")

USairpollution[,-1]
cor(USairpollution[,-1])
#usair_pca <- princomp(covmat = cor(USairpollution[,-1]))
usair_pca <- princomp(USairpollution[,-1], cor = TRUE)
summary(usair_pca, loadings = TRUE)

#recordar la alta correlación entre manu y popul
# Los primeros 3 componentes tienen varianzas (valores propios) mayores que 1
# y juntos representan casi el 85% de la varianza de las variables originales
## ** los puntajes de estos 3 componentes pueden usarse para graficar los datos con poca pérdida de info.

### Nota: Podemos vernos tentados a buscar un interpretación de los componentes que les permita ser 
### "etiquetados" en algún sentido

## Para hacer esto, es necesario examinar los coeficientes que definen cada componente
#*** Recordar que los coeficientes al cuadrado suman 1
# Primer componente: podría considerarse como: un índice de "calidad de vida",
# con valores altos que indican un entorno relativamente pobre

## Segundo componente: se relaciona en gran medida con la lluvia de una ciudad que tiene altos 
## coeficientes de precip y predays y podría etiquetarse como el componente de "clima humedo"

## Tercer componente: es esencialmente un contraste entre precip y temp y 
## separará las ciudades que tienen altas temperaturas y mucha lluvia de aquellas
## que son más frías pero más secas. Una etiqueta adecuada podría ser "tipo de clima"

## intentar etiquetar componentes de esta manera es común, pero tal vez debería ser un poco menos común,
## ****  Advertencia sobre los peligros de la sobreinterpretación:
##        No existe ningún método matemático diseñado para dar resultados físicamente significativos
##        Si una expresión tiene significado físico obvio, debe atribuirse a un cambio afortunado,o 
##        al hecho de que los datos tienen una estructura fuertemente marcada 
##        La rectificación requiere una habilidad y expericia considerables si se trata de 
##        dar una imagen real del significado físico de los datos


## incluso si no nos importa etiquetar los tres componentes, aún pueden usarse como base 
## de varias presentaciones gráficas de las ciudades 
## éste es el aspecto más útil del análisis de componentes principales 
## como un medio para proporcionar una visión informativa de los datos multivariados 
## lo cual hace que sea menos urgente o tentador tratar de interpretar y etiquetar los componentes

## comenzamos observando la matriz del diagrama de dispersión y en cada panel mostraremos el diagrama 
## de caja bivariado, los puntos están etiquetados con los nombres de las ciudades abreviados

pairs(usair_pca$scores[,1:3], ylim = c(-6, 4), xlim = c(-6,4), panel = function(x,y, ...){
  text(x, y, abbreviate(row.names(USairpollution)), cex = 0.6)
  bvbox(cbind(x,y), add = TRUE)
  })

## observamos que Chicago es un caso atípico y sugiere que Phoenix y Filadelfia también pueden ser sospechosos a este respecto
## Phoenix parece ofrecer la mejor calidad de vida (sobre la variable limitada de las seis variables registradas)
## y Buffalo es una ciudad que debe evitar si prefiere un ambiente más seco