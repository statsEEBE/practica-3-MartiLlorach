rm(list=ls())

x = c(0,1,2,3,4,5)
f = c(1,2,2,3,4,3)/15

#Representa gráficamente la función de probabilidad del número de botellas vendidas por día
plot(x, f, type="h", lwd=3, col="red")
points(x, f, col="red", lwd=5)

#Representa gráficamente la función de distribución del número de botellas vendidas por día
dist_f = cumsum(f)
plot(x, dist_f, type="s", lwd=3, col="red")
points(x, dist_f, col="red", lwd=5)

f_2 = f[3]
F_2 = dist_f[3]
f_3.5 = 0
F_3.5 = dist_f[4] 
f_6 = 0
F_6 = dist_f[6]

#¿Cuál es el valor esperado del número de botellas vendidas por día?
esperanza = sum(x*f)
esperanza

#¿Cuál es la mediana del número de botellas vendidas por día?
mediana = median(x)
mediana

#¿Cuál es la varianza del número de botellas vendidas por día?
varianza = sum( (x-esperanza)^2 * f )
varianza

#Realiza 30 simulaciones del número de botellas de leche vendidas por día. (Utiliza como semilla el número 12)
set.seed(12)
simu = sample(x,30,replace=TRUE, prob=f)
simu

#Representa gráficamente la frecuencia de los resultados obtenidos y compáralo con el diagrama de la función de probabilidad.
fi = table(simu)/length(simu)
xb = barplot(fi)
lines(xb, f, type="h", lwd=3, col="red")
points(xb, f, lwd=5, col="red")

#¿Cuál es el valor de la media de los resultados de las simulaciones?
media = mean(simu) #sum(simu)/length(simu)
media

#¿Cuál es la varianza de los resultados de las simulaciones?
f_simu = as.vector(fi)
esperanza_simu = sum(x*f_simu)
varianza_simu = sum( (x-esperanza_simu)^2 * f_simu )
varianza_simu
var(simu)


