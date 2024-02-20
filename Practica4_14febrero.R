#Ejercicio	1.  Importamos la tabla 
install.packages("readxl")
library(readxl)
##La ruta no muy larga
spear <- read_excel("C:/practica14feb/spearheads.xlsx")
View(spear)##Muestra una tabla en el editor de registros, no en la consola. 
str(spear)##Tipo de datos (campos)
class(spear)##Tipo de estructura de datos del objeto. 

#Convertimos a data frame
spear <- as.data.frame(spear)
class(spear)

#Ejercicio	2.  renombramos las variables 
names(spear)[names(spear) == "Mat"] <- "Materiales"
names(spear)[names(spear) == "Con"] <- "Contexto"
names(spear)[names(spear) == "Cond"] <- "Conservacion"
names(spear)[names(spear) == "Peg"] <- "Remache"
names(spear)[names(spear) == "Maxle"] <- "Longitud_max"
spear
View(spear)


#Ejercicio	3. Asigna		etiquetas 
spear$Contexto=factor(spear$Contexto,levels=c('1','2','3'),labels=c("S/C",	"Habitacional",	"Funerario"))
spear$Conservacion=factor(spear$Conservacion, levels=c(1,2,3,4), labels=c("Excelente",	 "Bueno",	 "Regular",'Malo'))
spear$Remache=factor(spear$Remache, levels=c(1,2), labels=c('Si','No'))
spear$Materiales=factor(spear$Materiales,levels=c(1,2), labels=c('Bronce','Hierro'))

##Visualizamos los cambios en el df spear
View(spear)

#Ejercicio	4. Genera	tablas	de	frecuencia	de	las	variables	Materiales,	Contextos	y	Conservacion
freq.mat=table(spear$Materiales)
View(freq.mat)
freq.con=table(spear$Contexto)
View(freq.con)
freq.cond=table(spear$Conservacion)
View(freq.cond)

#Ejercicio	5. Genera	tablas	cruzadas	de	Materiales	sumadas	sobre	Contexto	y	de	Materiales	sumadas	sobre	Conservacion 

cross.condcon=table(spear$Conservacion,spear$Contexto)
cross.condcon#Visualizar tabla de frecuecia en la consola. 
cross.condmat=table(spear$Conservacion,spear$Materiales)
cross.condmat#Visualizar tabla de frecuecia en la consola.

#Ejercicio	6. Genera	tablas	de	porcentaje	de	las	variables	Materiales,	Contextos	y	Conservaci n
#The prop.table() function in R can be used to calculate the value of each cell in a table as a proportion of all values.
prop.mat=prop.table(freq.mat)
View(prop.mat)
prop.mat <- as.data.frame(prop.mat)
#Los datos no aparecen en porcentaje. Multiplicamos los valores de las columnas x 100
#Creamos una nueva variable en el objeto 'prop.mat' 
prop.mat$Porcentaje <- prop.mat$Freq * 100
prop.mat
--------------------------
prop.con=prop.table(freq.con)
View(prop.con)
prop.con <- as.data.frame(prop.con)
#Los datos no aparecen en porcentaje. Multiplicamos los valores de las columnas x 100
#Creamos una nueva variable en el objeto 'prop.mat' 
prop.con$Porcentaje <- prop.con$Freq * 100
prop.con
--------------------------
prop.cond=prop.table(freq.cond)
View(prop.cond)
prop.cond <- as.data.frame(prop.cond)
#Los datos no aparecen en porcentaje. Multiplicamos los valores de las columnas x 100
#Creamos una nueva variable en el objeto 'prop.mat' 
prop.cond$Porcentaje <- prop.cond$Freq * 100
prop.cond

#Ejercicio	7. Genera	 tablas	 cruzadas	 de	 porcentaje	 de	 Materiales	 sumadas	 sobre	 Contexto	 
# y	 de	Materiales	sumadas	sobre	Conservacion
prop.cross.condcon=round(prop.table(cross.condcon)*100,0)
View(prop.cross.condcon)
prop.cross.condmat=round(prop.table(cross.condmat)*100,0)
View(prop.cross.condmat)
#El codigo hace lo siguiente
#round(x,0) redondea el resultado a el numero de decimales detras de la ,
#*100 convierte la escala a 100 en vez de 1
#prop.table(cross.condmat) genera la tabla de porcentages a partir de la de frecuencia

# Ejercicio	8. Genera	graficos	de	barras	verticales	para	la	variable	Conservacion	y	Contexto.	
# Indica	la	frecuencia	de	cada	factor

#Para generar el barplot usar la tabla de frecuencias
bar.cond=barplot(table(spear$Conservacion))
bar.con=barplot(table(spear$Contexto))


# Ejercicio	9. Genera	graficos	de	barras	horizontales	para	las	variables	Materiales	y	Remache 

barh.mater=barplot(table(spear$Materiales), horiz=TRUE)

xlim <- c(0, 1.2*max(table(spear$Materiales)))
yy <- barplot(table(spear$Materiales), yaxt = 'n', ylab = '', width = 0.85, xlim = xlim,
              main = "Materiales", 
              xlab = "Frecuencia",
              horiz=TRUE)
text(y = yy, x = table(spear$Materiales)+.4, label = table(spear$Materiales), pos = 3, cex = 0.8, col = 1)
axis(2, at=yy, labels=c("Hierro", "Bronce"), tick=FALSE, las=2, line=-0.5)

-----------------------------------------------
  
  barh.remac=barplot(table(spear$Remache), horiz=TRUE)

xlim <- c(0, 1.2*max(table(spear$Remache)))
yy <- barplot(table(spear$Remache), yaxt = 'n', ylab = '', width = 0.85, xlim = xlim,
              main = "Remache", 
              xlab = "Frecuencia",
              horiz=TRUE)
text(y = yy, x = table(spear$Remache)+.4, label = table(spear$Remache), pos = 3, cex = 0.8, col = 1)
axis(2, at=yy, labels=c("Si", "No"), tick=FALSE, las=2, line=-0.5)


# Ejercicio	10. Genera	graficos	de	barra	agrupados	por	Material	para	la	variable	Conservacion

bar.cond= barplot(cross.condmat, width = 0.85, ylim=c(0,sum(cross.condmat[,1])*1.1), #ylim max he sumado la columna con las puntuaciones de Bronce para calcular el total, que es la altura de la barra y le he dado margen extra para que la leyenda no solape
                  main = "Estado de conservacion vs. Materiales", 
                  ylab = "Frecuencia",
                  legend=T)

# Ejercicio	11. Genera	un	grafico	de	sectores	para	la	variable	Conservacion.	Indica	el	porcentaje	de	cada	factor	y	la	frecuencia

labs<- paste("(",freq.cond,")\n", names(freq.cond), sep="")
pie(freq.cond, labels = labs, main="Conservacion recuento", col=gray.colors(length(levels(factor(names(freq.cond)))), start = 0.3, end = 0.9))

# col=gray.colors(length(levels(factor(names(freq.cond)))), start = 0.3, end = 0.9) genera un vector con colores en escala de grises para las categorias de freq.cond, para ello
#necesitamos calcular la longitud de los niveles de freq.cond que nos indica el numero de colores con lo que generamos un vector factorial
#de los nombres de la tabla ferq.cond, le decimos que nos diga que niveles tienes y que nos injdique cuantos hay.
#el primer color es 30% de negro y el ultimo 90%

# Ejercicio	12. Genera	un	histograma	de	probabilidad	de	las	variables	continuas	en	el	data	frame	spear 
spear.num=Filter(is.double, spear) # generamos un subconjunto con la variables numericas continuas
hist(spear.num$Longitud_max, probability=T,xlab='Longitud maxima', ylab='Probabilidad') #generamos el histograma
m=mean(spear.num$Longitud_max, na.rm=T) #para calcular la normal hay que calcular la media y la desviacion estándar
std=sqrt(var(spear.num$Longitud_max, na.rm=T))
curve(dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")#A adimos una curva normal para la media y la desviacion estandar calculada de nuestra distribucion

#repetir para cada variable numerica

#opcion 2

library(psych)
multi.hist(spear.num, density=T)
