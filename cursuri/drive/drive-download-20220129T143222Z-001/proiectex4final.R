# Proiect
# Exercitiul 4
# Marginea inferioara


# Rao-Cramer pentru repartitia Poisson
n <- 100
pmf <- expression(log(lambda^x * exp(-lambda) / factorial(x))) # logaritmam functia de masa a rep. Poisson
d1 <- D(pmf, "lambda") # prima derivata a functiei de mai sus
d2 <- D(d1, "lambda") # derivata de ord 2 a functiei de mai sus

frcpois <- function(n, lambda, x) {
  der2 <- function(lambda, x) {
    (lambda^((x - 1) - 1) * (x - 1) * x * exp(-lambda) - lambda^(x - 1) * x * exp(-lambda) - (lambda^(x - 1) * x * exp(-lambda) - lambda^x * exp(-lambda))) / factorial(x) / (lambda^x * exp(-lambda) / factorial(x)) - (lambda^(x - 1) * x * exp(-lambda) - lambda^x * exp(-lambda)) / factorial(x) * ((lambda^(x - 1) * x * exp(-lambda) - lambda^x * exp(-lambda)) / factorial(x)) / (lambda^x * exp(-lambda) / factorial(x))^2
  }
  # derivata de ordin doi introdusa manual
  MIRC <- 1 / (-n * mean(eval(der2(lambda, x))))
  return(MIRC)
}

p <- rpois(n, 5)
MIRC <- frcpois(n, 5, p)
# Observatie: MIRC-ul obtinut in R e aproximativ egal cu cel pe care l-am obtinut pe hartie
#                  (in exemplul nostru concret, 5/100, i.e. lambda/n)


# Marginea inferioara Rao-Cramer pentru repartitia Exponentiala
n <- 100

pdfexp <- expression(log(lambda * exp(-lambda * x))) # logaritmam functia de dens. a rep. Exponentiala
dexp1 <- D(pdfexp, "lambda") # prima derivata a functiei de mai sus
dexp2 <- D(dexp1, "lambda") # derivata de ord 2 a functiei de mai sus

frcexp <- function(n, X, lambda) {
  deriv2 <- function(lambda, X) {
    -((exp(-lambda * X) * X + ((exp(-lambda * X) * X) - lambda * (exp(-lambda * X) * X^2))) /
      (lambda * exp(-lambda * X)) + (exp(-lambda * X) - lambda * (exp(-lambda * X) * X)) * (exp(-lambda * X) -
        lambda * (exp(-lambda * X) * X)) / (lambda * exp(-lambda * X))^2)
  }
  # derivata de ordin 2 introdusa manual, din nou
  MIRC <- 1 / (-n * mean(eval(deriv2(lambda, X))))
  return(MIRC)
}

e <- rexp(n)
MIRC <- frcexp(n, e, 3)
# Aceeasi observatie: MIRC-ul obtinut in R e aproximativ egal cu cel pe care l-am obtinut pe hartie
#                  (in exemplul nostru concret, 9/100, i.e. lambda^2/n)