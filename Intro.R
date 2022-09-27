install.packages("MVA")
library("MVA")
demo("Ch-MVA")
#measure
#USairpollution
x <- measure[,c("chest", "waist", "hips")]
cov(x)
xfemale <- subset(measure, gender == "female")[,c("chest", "waist", "hips")]
cov(xfemale)
xmale <- subset(measure, gender == "male")[,c("chest", "waist", "hips")]
cov(xmale)
cor(x)
##distancias
dist(scale(x, center = FALSE))
### densidad normal multivariada
x <- measure[,c("chest", "waist", "hips")]
cm <- colMeans(x)
S <- cov(x)
d <- apply(x, MARGIN = 1, function(x)
						{t(x - cm) %*% solve(S) %*% (x - cm)})
##gráfica qq
qqnorm(x[,"chest"], main = "chest"); qqline(x[,"chest"])
qqnorm(x[,"waist"], main = "waist"); qqline(x[,"waist"])
qqnorm(x[,"hips"], main = "hips"); qqline(x[,"hips"])
##multivariada
plot(qchisq((1:nrow(x) - 1/2) / nrow(x), df = 3), sort(d), xlab =expression(paste(chi[3]^2, "Cuantil")), ylab = "distancias ordenadas")
abline(a = 0, b = 1)