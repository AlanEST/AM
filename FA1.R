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