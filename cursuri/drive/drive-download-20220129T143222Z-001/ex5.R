# Exercitiul 5
# a)
# Cazul discret:

medie_discret <- function(X, p) {
  return(sum(X * p))
}
# Luam exemplul:
X <- 1:5
p <- c(2 / 10, 3 / 10, 3 / 10, 1 / 10, 1 / 10)
E <- medie_discret(X, p)

# Cazul continuu:

# Consideram variabila aleatoare continua X~Exp(lambda) care are densitatea
#             f(x)= lambda*exp(-lambda*x), x>=0
#                   0, in rest
lambda <- 5
f <- function(x) {
  return(x * lambda * exp(-lambda * x))
}
# f este functia din interiorul integralei pentru medie

medie_continuu <- function(f) {
  E <- integrate(f, lower = 0, upper = Inf)
  return(E$value)
}
medie2 <- medie_continuu(f) # e bine, ne da la fel si pe foaie


# b)
dispersie_discret <- function() {
  var <- medie_discret(X^2, p) - medie_discret(X, p)^2
  return(sqrt(var))
}
dispersie1 <- dispersie_discret()


f2 <- function(x) {
  return(x^2 * lambda * exp(-lambda * x))
}
# f2 este functia din interiorul integralei pt E[X^2]

dispersie_continuu <- function() {
  var2 <- medie_continuu(f2) - medie_continuu(f)^2
  return(sqrt(var2))
}
dispersie2 <- dispersie_continuu()


# c)
t <- seq(from = 0, to = 2, length.out = 10)

FGM_discret <- function(t) {
  phi <- c()
  for (i in 1:length(t))
  {
    phi[i] <- sum(p * exp(t[i] * X))
  }
  return(phi)
}
phi1 <- FGM_discret(t)


# FGM_continuu <- function(t)
# { phi2 <- c()
#   for(i in 1:length(t))
#   { f3 <- function(x) {return(exp(t[i]*x)*lambda*exp(-lambda*x))} #functia din interiorul integralei pt FGM
#     phi2[i] <- integrate(f3,lower=0,upper=Inf)}
#    return(phi2)
# }
# phi2=FGM_continuu(t)
FGM_continuu <- function(t) {
  return(lambda / (lambda - t))
}

phi2 <- FGM_continuu(t)



# d)
#  Aici era foarte simplu de calculat daca reuseam sa calculez functiile generatoare
# de momente ca niste expresii simbolice pentru ca imediat le derivam si apoi le
# evaluam in 0. Dar nu am reusit sa fac asta in R...daca lucram in Matlab sigur
# reuseam. Asa ca o sa scriu direct derivatele de ordin 1 si 2 ale FGM.

# Pt cazul discret in exemplul nostru:
der1 <- function(t) {
  return(2 / 10 * exp(t) + 3 / 5 * exp(2 * t) + 9 / 10 * exp(3 * t) + 2 / 5 * exp(4 * t) + 1 / 2 * exp(5 * t))
}
a <- der1(0) # a este E[X] si ne-a dat la fel ca la a)
der2 <- function(t) {
  return(2 / 10 * exp(t) + 6 / 5 * exp(2 * t) + 27 / 10 * exp(3 * t) + 8 / 5 * exp(4 * t) + 5 / 2 * exp(5 * t))
}
b <- der2(0)
disp1 <- sqrt(b - a^2)
# Observam ca disp1 ne-a dat la fel ca dispersia calculata in functia de la b).

# Pt cazul continuu in exemplul nostru:
der11 <- function(t) {
  return(lambda / ((lambda - t)^2))
}
c <- der11(0) # c este E[X] si ne-a dat la fel ca la a)
der22 <- function(t) {
  return(2 * lambda * (lambda - t) / ((lambda - t)^4))
}
d <- der22(0)
disp2 <- sqrt(d - c^2)
# Observam ca disp2 ne-a dat la fel ca dispersia calculata in functia de la b).