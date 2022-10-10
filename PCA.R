library("MVA")
demo("Ch-PCA")
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

