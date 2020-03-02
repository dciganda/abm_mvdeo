###########################################
# Curso Intro. Simulacion MBA Mvdeo       #
# Daniel Ciganda - Marzo 2020             # 
###########################################

####### Simulando Valores de la Distribucion Gamma ###############
# cargar datos del "International Value Survey"
list.files()
ivs <- read.table(list.files()[1])

# visualizar datos
head(ivs)
table(ivs)
table(ivs$ideal_nr, useNA = "always")
class(ivs)
class(ivs$ideal_nr)

# convertir el tabulado en data frame
ivs <- as.data.frame(table(ivs$ideal_nr), stringsAsFactors = F)
names(ivs) <- c("nr","freq")

# graficar frequencias
plot(x = ivs$nr, y = ivs$freq, type = "p")

# graficar probs.
plot(ivs$nr, ivs$freq/sum(ivs$freq), type = "p")

# parametros de la distribucion Gamma
alpha <- 5.9 # shape
beta <- 2.3 # rate

# dgamma genera la distribucion de probabilidad de gamma 
gamma_d <- dgamma(as.numeric(ivs$nr), shape = alpha, rate = beta)

# ver ajuste de esta parametrizacion de Gamma a los datos
points(ivs$nr, gamma_d, col = "red", lwd = 2)

# rgamma genera valores aleatorios 
sim_vals <- rgamma(n = 100, shape = alpha, rate = beta)
sim_vals <- round(sim_vals,0)
table(sim_vals)
table(sim_vals)/sum(table(sim_vals))

# frequencias
plot(table(sim_vals), type = "p")

# probabilidades
plot(table(sim_vals)/sum(table(sim_vals)), type = "p", ylim = c(0, 0.5), xlim = c(0,10))
points(ivs$nr, gamma_d, col = "red", lwd = 2)

sim_vals <- rgamma(10000, shape = alpha, rate = beta)
sim_vals <- round(sim_vals, 0)
# frequencias
plot(table(sim_vals), type = "p")

# probabilidades
plot(table(sim_vals)/sum(table(sim_vals)), type = "p", ylim = c(0, 0.5), xlim = c(0,10))
points(ivs$nr, gamma_d, col = "red", lwd = 2)

############# Seed ###############
# La semilla (seed) permite reproducir los resultados de una simulacion,
# asegurando que la sequencia de numeros aleatorios es siempre la misma.

# obtenemos 5 valores aleatorias de Gamma
rgamma(5, alpha, beta)

# obtenemos 5 nuevos valores, que seran (con gran probabilidad) distintos
rgamma(5, alpha, beta)

# si fijamos la semilla, los numeros que nos da el generador de numeros aleatorios son los mismos
set.seed(1)
rgamma(5, alpha, beta)

# otra vez, ahora deberian ser iguales
set.seed(1)
rgamma(5, alpha, beta)

############# Distribución exponencial ###################
# Se utiliza para modelar una gran cantidad de procesos, 
# entre ellos las duraciones cuando el riesgo de un evento es constante. 
# en R se pueden simular realizaciones de la distribucion exponencial con rexp(n, lambda) 
# lambda es la frecuencia a la que se observa un evento, por unidad de tiempo.
# El tiempo promedio entre eventos es 1/lambda. 

# Si el tiempo medio de vida de un telefono celular es 3 anos, 
# podemos simular los tiempos de vida de un conjunto de n telefonos
dur <- rexp(2000, 1/3)

# la duracion media tiene que ser de 3 anos
mean(dur)

# plot
r_dur <- round(dur, 0)
plot(table(r_dur), type = "p")

# Extendemos la vida de los aparatos a 5 anos
dur <- rexp(2000, 1/5)
mean(dur)
# plot
r_dur <- round(dur, 0)
points(table(r_dur), col = "red")

##############
# Ejercicios #
##############

# 1) Imaginemos que estamos trabajando en un modelo de competencia electoral
#    para el que tenemos que simular un electorado de 10000 personas con opiniones 
#    politicas en el espectro izquierda-derecha (escala de 0-extrema izq. a 10-extrema derecha). 
#    Asumiendo que estas opiniones vienen de una distribucion normal con media 5 y desviacion 
#    estandar 1:
#    Simular los valores
#    Graficar el resultado con hist(). 
#    Graficar la probabilidad de cada valor.
#    Superponer la distribucion de probabilidad teorica correspondiente.

# asumiendo una distribucion normal, centrada en 5.
op <- rnorm(10000, mean = 5)
hist(round(op, 0))

op_d <- as.data.frame(table(round(op, 0)))

plot(as.numeric(op_d$Var1), op_d$Freq/sum(op_d$Freq), type = "p", ylim = c(0,0.4))
points(dnorm(as.numeric(op_d$Var1), mean = 5), col = "red")

# 2) Otra funcion clave en la simulacion es sample(), utilizando esta funcion:
#    Obtener una muestra aleatoria de 5 numeros entre el 1 y el 100 
#    Obtener una muestra aleatoria de 150 numeros entre el 1 y el 100
#    Obtener una muestra aleatoria de 10 letras del alfabeto
#    Obtener una combinacion aleatoria de los numeros del 1 al 10

sample(1:100, 5)
sample(1:100, 150, replace = T)
sample(letters, 10)
sample(1:10)

# 3) Volver a simular los valores en el ej. 1, esta vez asumiendo una desvio estandard = 3.
#    Observar el resultado: cual es el problema?
#    Obtener una solucion. Ayuda: Existen paquetes en R que nos permiten tomar muestras de 
#    distribuciones truncadas.

op <- rnorm(10000, mean = 5, sd = 3)
hist(round(op, 0))

library(truncdist)

op <- rtrunc(10000, spec = "norm", a = 0, b = 10, mean = 5, sd = 3)
table(round(op, 0))

op_d <- as.data.frame(table(round(op, 0)))

plot(as.numeric(op_d$Var1), op_d$Freq/sum(op_d$Freq), type = "p")


