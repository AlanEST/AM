library("MVA")
## Diagrama de dispersión
mlab <- "Empresas manufactureras con 20 o más trabajadores"
plab <- "Tamaño de la población (censo de 1970) en miles"
plot(popul ~ manu, data = USairpollution, xlab = mlab, ylab = plab)

## Diagrama de caja bivariado
plot(popul ~ manu, data = USairpollution, xlab = mlab, ylab = plab)
rug(USairpollution$manu, side = 1)
rug(USairpollution$popul, side = 2)

# dispersión y densidades marginales
layout(matrix(c(2, 0 , 1 , 3), nrow = 2, byrow = TRUE), widths = c(2, 1), heights = c(1, 2), respect = TRUE)
#xlim <- with(USairpollution, range(manu)) * 1.1
xlim <- range(USairpollution$manu) * 1.1
plot(popul ~ manu, data = USairpollution, cex.lab = 0.9, xlab = mlab, ylab = plab, type="n", xlim=xlim)
text(USairpollution$manu, USairpollution$popul, cex = 0.6, labels = abbreviate(row.names(USairpollution)))
#with(USairpollution, text(manu, popul, cex = 0.6, labels = abbreviate(row.names(USairpollution))))
hist(USairpollution$manu, main = "", xlim = xlim)
#with(USairpollution, hist(manu, main = "", xlim = xlim))
boxplot(USairpollution$popul)
#with(USairpollution, boxplot(popul))

