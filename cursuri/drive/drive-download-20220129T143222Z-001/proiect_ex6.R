
#generam valori din repartitia logistica cu functia predefinita rlogis

n <- 200
x <-rlogis(n,0,1) #x este un vector de 100 de elemente 
                  #ce va contine valori din repartitia logistica

L <- 1; #initializam produsul cu 1
beta <- 1; #alegem o valoare potrivita pentru beta fixat


#mai jos vom calcula functia de verosimilitate
#miu este parametrul nostru(theta din formulele din curs)

verosim_logi <- function(miu)
{
  for(i in 1:n)
  {
    L <- L*(1/beta)*exp(-(x[i]-miu)/beta)/(1+exp(-(x[i]-miu)/beta))^2;
  }
  return(L)
}


#trasam graficul functiei de verosimilitate pentru repartitia Logistica
plot(verosim_logi,xlab='miu',ylab='L(miu)', col="purple")
title("Graficul functiei de verosimilitate pentru repartitia Logistica")

#cautam valoarea optima pentru miu
miu_logis <- optimise(verosim_logi,c(0,1),maximum=TRUE)
miu_optim <- miu_logis[[1]]




##################################################################

#procedam exact ca mai sus de data aceasta pentru repartitia Cauchy


y<- rcauchy(n,0,1)
sigma <- 1;
L <- 1
verosim_cauchy <- function(miu){
  for(i in 1:n){
    L <- L*(1/(pi*sigma))*1/(1+((y[i]-miu)/sigma)^2)
  }
  return(L)
}

plot(verosim_cauchy,xlab='miu',ylab='L(miu)', col="red")
title("Graficul functiei de verosimilitate pentru repartitia Cauchy")

miu_cauchy <- optimise(verosim_cauchy,c(0,1),maximum=TRUE)
miu_optim2 <- miu_cauchy[[1]]
