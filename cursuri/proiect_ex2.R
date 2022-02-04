# a

# stim ca functia f este proportonala cu h(asadar, f=c*h)
# definim functia h
h <- function(x) {
  return(exp(-(x - 2)^2 / 8) * ((sin(2 * x))^2 - 2 * (cos(x))^2 * (sin(3 * x))^2 + 5))
}
par(mfrow = c(1, 2))
v <- seq(-15, 15, 0.01) # discretizam intervalul -15,15 pentru a plota
plot(v, h(v), type = "l", col = "magenta") # trasam graficul functiei h

# ne uitam la densitatea normalei de medie 2 si dispersie 4 notata cu g
# g=(exp(-(x-2)^2/8))/(2*sqrt(2*pi))

# definesc raportul h/g(i.e functia f)
f <- function(x) {
  return (2 * sqrt(2 * pi) * ((sin(2 * x))^2 - 2 * (cos(x))^2 * (sin(3 * x))^2 + 5))
}

# aflu valoarea maxima a acestui raport
M <- optimise(f, c(0, 2), maximum = TRUE)


v <- seq(-15, 15, 0.01) # discretizam intervalul -15,15 pentru a plota
plot(v, h(v), type = "l", col = "magenta") # trasam graficul functiei h
lines(v, dnorm(v, 2, 2) * M[[2]], col = "black")


# b

# generam 100000 observatii cu metoda respingerii
n <- 100000 # numar observatii
val_obtinuta <- c() # definim un vector gol in care stocam valorile gasite
k <- 0 # numar observatii valide
for (i in 1:n) {
  u <- runif(1) # generam o observatie din uniforma
  x <- rnorm(1, 2, 2) # generam o observatie din normala de medie 2 si dispersie 4
  if (u <= h(x) / (M[[2]] * dnorm(x, 2, 2))) # conditia din algoritmul metodei respingerii
  {
    k <- k + 1 # contorizam numarul observatiilor valide pentru a calcula constanta c
    val_obtinuta[k] <- x # punem in vector valoarea x
  }
}

# numarul de iteratii necesar pentru a obtine o valoare din X prin metoda respingerii
# este o v.a repartizata geometric cu media c

p <- k / n # calculam probabilitatea de succes(rata de acceptare)

media <- 1 / (M[[2]] * p) # valoarea constantei calculata ca media unei v.a distribuita geometric

# trasam histograma valorilor generate cu metoda respingerii
hist(val_obtinuta,
  breaks = 50, freq = FALSE, col = "cyan", ylim = c(0, 0.25), xlab = "Valori valide(acceptate)", ylab = "Densitate",
  main = "Histograma valorilor generate in urma metodei respingerii", cex.main = 0.7
)

# peste histograma punem reprezentarea grafica a functiei normalizata
lines(v, media * h(v), col = "black", type = "l")