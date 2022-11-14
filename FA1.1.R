library("MVA")
demo("Ch-EFA")


### Los datos muestran la esperanza de vida en años por
### país, edad y sexo. Los datos provienen de Keyfitz y Flieger (1971) y
### se relacionan con las expectativas de vida en la década de 1960.

#sapply(1:3, function(f) {factanal(life, factors = f , method = "mle") $ PVAL})
factanal(life, factors = 3, method ="mle")

## observando las cargas factoriales estimadas
## vemos que el primer factor esta dominado por la esperanza de vida 
## al nacer tanto para hombres como para mujeres
## f1: "fuerza de vida al nacer"
## El segundo factor refleja la esperanza de vida en edades más avanzadas,
## f2: "fuerza de vida de las personas mayores"
## El tercer factor (de la rotación varimax) tiene sus cargas mas altas para
## las expectativas de vida de los hombres de 50 y 75 años, 
## f3: "fuerza de vida para hombres mayores"


## Las puntuaciones factoriales estimadas se encuentran de la siguiente manera

scores <- factanal(life, factors = 3, method = "mle", scores = "regression")$scores
scores2<-as.data.frame(scores)
plot(Factor2 ~ Factor1, data = scores2, xlab = "Factor1", ylab = "Factor2")
plot(Factor2 ~ Factor1, data = scores2, cex.lab = 0.9, xlab = "Factor1", ylab = "Factor2", type="n")
##text(scores2$Factor2, scores2$Factor1, cex = 0.6, labels = abbreviate(row.names(scores2)))
with(scores2, text(Factor1, Factor2, cex = 0.6, labels = abbreviate(row.names(scores))))

plot(Factor3 ~ Factor1, data = scores2, xlab = "Factor1", ylab = "Factor2")
plot(Factor3 ~ Factor1, data = scores2, cex.lab = 0.9, xlab = "Factor1", ylab = "Factor2", type="n")
##text(scores2$Factor2, scores2$Factor1, cex = 0.6, labels = abbreviate(row.names(scores2)))
with(scores2, text(Factor1, Factor3, cex = 0.6, labels = abbreviate(row.names(scores))))

plot(Factor3 ~ Factor2, data = scores2, xlab = "Factor1", ylab = "Factor2")
plot(Factor3 ~ Factor2, data = scores2, cex.lab = 0.9, xlab = "Factor1", ylab = "Factor2", type="n")
##text(scores2$Factor2, scores2$Factor1, cex = 0.6, labels = abbreviate(row.names(scores2)))
with(scores2, text(Factor2, Factor3, cex = 0.6, labels = abbreviate(row.names(scores))))


## El orden a lo largo del primer factor refleja la fuerza vital al nacer, desde
## Camerún y Madagascar hasta países como los EE. UU.
## Y en el tercer factor destaca Argelia porque tiene una alta esperanza de vida entre los
## hombres a edades más altas
## con Camerún en el extremo inferior de la escala con una baja esperanza de vida
## para los hombres mayores de 50 años.


### Uso de drogas por estudiantes universitarios estadounidenses

## La mayoría de los estadounidenses adultos y adolescentes consumen regularmente 
## sustancias psicoactivas durante una proporción cada vez mayor de sus vidas.
## Prevalecen varias formas de consumo de sustancias psicoactivas lícitas e ilícitas
## lo que sugiere que los patrones de consumo de sustancias psicoactivas
## son una parte importante del repertorio conductual del individuo y
## tienen implicaciones generalizadas para el desempeño de otros comportamientos.
## En una investigación de estos fenómenos, Huba, Wingard y Bentler (1981)
## recopilaron datos sobre las tasas de consumo de drogas de
## 1634 estudiantes de séptimo a noveno grado en 11 escuelas del área metropolitana de Los Ángeles.
## Cada participante llenó un cuestionario sobre 
## el número de veces que alguna vez había usado una sustancia en particular
## Las sustancias sobre las que se preguntó fueron las siguientes:

## cigarrillos;
## cerveza;
## vino;
## liquor = bebida alcohólica fuerte;
## cocaína;
## tranquilizantes;
## medicamentos de farmacia que se usan para drogarse;
## heroína y otros opiáceos;
## marijuana;
## hachís = producto de la marihuana;
## inhalantes (pegamento, gasolina, etc.);
## alucinógenos (LSD, mescalina, etc.);
## estimulantes anfetamínicos.

## Las respuestas se registraron en una escala de cinco puntos: 
## nunca lo intenté, solo una vez, algunas veces, muchas veces y regularmente.

## Primero determinamos el numero de factores usando la prueba de maxima verosimilitud
sapply(1:7, function(nf) factanal(covmat = druguse, factors = nf, method = "mle", n.obs = 1634)$PVAL)
factanal(covmat = druguse, factors = 6, method = "mle", n.obs = 1634)

## f1 --- Las sustancias que cargan mucho en el primer factor son 
## los cigarrillos, la cerveza, el vino, el licor y la marihuana,
## f1: “uso de drogas blandas/sociales”
## f2 --- La cocaína, los tranquilizantes y la heroína cargan mucho en el segundo factor
## f2 : "uso de drogas duras"
## f3 : "uso de anfetaminas"
## f4: "uso de hashish"

## No intentaremos interpretar los ultimos dos factores, aunque la prueba nos indico
## que eran necesarios 6 factores
## Podemos tomar al numero indicado de factores por la prueba
## como un limite superior en el numero de factores con importancia practica
## ademas tenemos 13 variables manifiestas vs 6 factores, con esto 
## tendriamos problemas para interpretar los factores

## Con el gran tamaño de la muestra en este ejemplo, incluso las pequeñas
## discrepancias entre la matriz de correlación predicha por un modelo propuesto
## y la matriz de correlación observada pueden conducir al rechazo del modelo.
## Una forma de investigar esta posibilidad es simplemente observar las diferencias
## entre las correlaciones observadas y predichas

pfun <- function(nf) {
  fa <- factanal(covmat = druguse, factors = nf, method = "mle", n.obs = 1634)
  est <- tcrossprod(fa$loadings) + diag(fa$uniquenesses)
  ret <- round(druguse - est, 3)
  colnames(ret) <- rownames(ret) <- abbreviate(rownames(ret),3)
  ret
} 
pfun(6)

## Las diferencias son todas muy pequeñas, 
## lo que subraya que el modelo de seis factores describe muy bien los datos.
## tomemos 3 y 4 factores 
## en ambos casos los residuos son todos relativamente pequeños, lo que sugiere 
## quizás que el uso de la prueba formal para el número de factores conduce, 
## en este caso, a un sobreajuste
## El modelo de tres factores parece proporcionar un ajuste perfectamente adecuado para estos datos.