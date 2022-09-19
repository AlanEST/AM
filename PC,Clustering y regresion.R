setwd("/Users/alaneduardosuareztorres/Desktop")
baseCA<-read.csv("cajasdeahorro.csv")
baseCA.sc <- scale(baseCA)

##Componentes Principales
baseCA.svd <- svd(baseCA.sc)
baseCA.scores <- baseCA.svd$u %*% diag(baseCA.svd$d)
baseCA.loadings <- baseCA.svd$v

#varianzas
baseCA.vars <- baseCA.svd$d^2 / (nrow(baseCA) - 1)
baseCA.totalvar <- sum(baseCA.vars)
baseCA.relvars <- baseCA.vars / baseCA.totalvar
variances <- 100 * round(baseCA.relvars, digits = 3)
variances
length(variances)
###la varianza explicada por cada uno de los 37 CP es muy baja, en total los primeros dos explican 11.9+9.4=21.3
###de la varianza total
#graficas scores y loadings
x11()
plot(baseCA.scores[,1:2], type = "n",xlab = paste("PC 1 (", variances[1], "%)", sep = ""),ylab = paste("PC 2 (", variances[2], "%)", sep = ""))
abline(h = 0, v = 0, col = "gray")
points(baseCA.scores[,1:2])
###aqui se muestra la dispersion de los datos en los primeros 2 CP
plot(baseCA.loadings[,1] * 1.2, baseCA.loadings[,2], type = "n",xlab = paste("PC 1 (", variances[1], "%)", sep = ""),ylab = paste("PC 2 (", variances[2], "%)", sep = ""))
arrows(0, 0, baseCA.loadings[,1], baseCA.loadings[,2],col = "darkgray", length = .15, angle = 20)
text(baseCA.loadings[,1:2], labels = colnames(baseCA))
#graficas de varianzas
x11()
par(mfrow = c(2,2))
barplot(baseCA.vars[1:10], main = "Variances",names.arg = paste("PC", 1:10))
barplot(log(baseCA.vars[1:10]), main = "log(Variances)",names.arg = paste("PC", 1:10))
barplot(baseCA.relvars[1:10], main = "Relative variances",names.arg = paste("PC", 1:10))
barplot(cumsum(100 * baseCA.relvars[1:10]),main = "Cumulative variances (%)",names.arg = paste("PC", 1:10), ylim = c(0, 100))
x11()
baseCA.som <- som(baseCA.sc, somgrid(5, 4, "hexagonal"))
plot(baseCA.som, type = "codes")

####Clustering
#liga simple
subset <- sample(nrow(baseCA), 80)
baseCA.dist <- dist(baseCA.sc[subset,])
baseCA.hcsingle <-  hclust(baseCA.dist, method = "single")
#plot(baseCA.hcsingle)
#liga completa
baseCA.hccomplete <- hclust(baseCA.dist, method = "complete")
#plot(baseCA.hccomplete)
x11()
par(mfrow = c(2,2))
plot(baseCA.hcsingle)
plot(baseCA.hccomplete)
table(cutree(baseCA.hcsingle, k = 8))
table(cutree(baseCA.hccomplete, k =8))


##regresion
regres<-read.csv("regresion.csv",header=T)
ytr<-log(regres[,2])
xtr<-regres[,1]
t(solve(crossprod(ytr), t(ytr)) %*% xtr)

Blm <- lm(ytr ~ xtr)
summary(Blm)
