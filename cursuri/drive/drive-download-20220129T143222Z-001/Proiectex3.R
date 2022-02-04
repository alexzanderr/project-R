#Proiect
#exercitiul 3
n = 1000
#functia h, a carei integrala vrem sa o aproximam
h = function(x) {
  return ( (1-x^2)^(3/2) )
}
s = 0    #observatiile
int = c() 
int[0] = 0
for( i in 1:n) {
  x = runif(1,0,1)            #generam cate o observatie
  s = s + h(x) 
  int[i] = s/i                #aici calculam integrala la fiecare iteratie(pentru fiecare observatie)
}
MonteCarlo = s/n              #aproximarea finala a integralei
plot(int, type = "l",         #se vede mai frumos cu linii decat cu puncte :) 
     col = "magenta",
     xlab = " Pasul i ",
     ylab = "Aproximarea integralei")
grid( nx = NULL,
      col = "lightgray",
      equilogs = TRUE)
abline(h = 0.589, lty = "dotted")      #valoarea analitica

