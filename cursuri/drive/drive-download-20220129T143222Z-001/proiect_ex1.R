#a

png(file = "plot.png")


n <- 100000

valori_repLogistica <- function(n, miu, beta) 
{
  U <- runif(n)  #generam 100000 de observatii dintr-o uniforma                  
  return (miu-1/beta*log(1-U)+1/beta*log(U))  #returnam inversa functiei de repartitie
}

var_1L<-valori_repLogistica(n, 0, 1) #generarea cu ajutorul functiei create de noi
var_2L<-rlogis(n)                    #generarea cu ajutorul functiei prestabilite rlogis

par(mfrow=c(1,2)) #impartim spatiul de plotare in 2

hist(var_1L,main="Observatii generate cu metoda transformarii inverse",xlab="Valoarea obtinuta",
     ylab="Frecventa",xlim=c(-9, 9),col="magenta",cex.main=0.7)

hist(var_2L,main="Observatii generate cu rlogis",xlab="Valoarea obtinuta",
     ylab="Frecventa",xlim=c(-9, 9),col="yellow",cex.main=0.9)



#b

valori_repCauchy <- function(n, miu, sigma){
  
  U <- runif(n) #generam 100000 de observatii dintr-o uniforma 
  return (miu+sigma*tan(pi*(U-1/2))) #returnam inversa functiei de repartitie
}

var_1C <- valori_repCauchy (n,0,1) #observatii generate cu metoda transformarii inverse
var_2C <- rcauchy(n,0,1) #observatii generate cu ajutorul functiei prestabilite rcauchy


par(mfrow=c(1,2)) #impartim spatiul de plotare in 2



hist(var_1C,main="Observatii generate cu metoda transformarii inverse", xlab="Valorea obtinuta",
     ylab="Frecventa", xlim=c(-50000,50000) ,col='magenta',cex.main=0.7)

hist(var_2C,main="Observatii generate cu rcauchy", xlab="Valorea obtinuta",
     ylab="Frecventa", xlim=c(-50000,50000),col='yellow',cex.main=0.9)


dev.off()
