tempo = c(3.4, 2.5, 4.8, 2.9, 3.6, 2.8, 3.3, 5.6, 3.7, 2.8, 4.4, 4.0, 5.2, 3.0, 4.8)
####Questão 2
media = mean(tempo)
mediana = median(tempo)
desvio = sd(tempo)
variancia = desvio*desvio

cv = desvio/media*100

mode = function(x){
  modal <- unique(x)
  modal[which.max(tabulate(match(x, modal)))]
}

moda = mode(tempo)

####Questão 3
radiacao = c(6.3, 6.4, 7.7, 8.4, 8.5, 8.8, 8.9, 9.0, 9.1, 10.0, 10.1,
             10.2, 10.6, 10.6, 10.7, 10.7, 10.8, 10.9, 11.1, 11.2, 11.2,
             11.4, 11.9, 11.9, 12.2, 13.1)

media_radiacao = mean(radiacao)
mediana_rad = median(radiacao) 
summary(radiacao)
desvio_rad = sd(radiacao)
var_rad = desvio_rad**2


#Questão 4
funcionarios = c(41, 35, 27, 55, 18, 27, 39, 21, 24)
media_func = mean(funcionarios)
mediana_func = median(funcionarios)  
desvio_func = sd(funcionarios)
var_func = desvio_func**2
summary(funcionarios)
coefv = 100*desvio_func/media_func

casas_por_quarteirao = c(2, 2, 3, 10, 13, 14, 15, 15, 16, 16, 18, 18, 20, 21, 22, 59, 61, 26, 
                         27, 29, 30, 32, 36, 42, 44, 45, 45, 46, 48, 52, 58, 61, 61, 6, 66, 68,
                         75, 78, 80, 89, 90, 92, 97, 22, 23, 24, 25, 25, 65)

mean(casas_por_quarteirao)
median(casas_por_quarteirao)


sd(casas_por_quarteirao)**2

100*sd(casas_por_quarteirao)/mean(casas_por_quarteirao)


ar = c(9.3, 10.7, 8.5, 9.6, 12.2, 15.6, 9.2, 10.5, 9.0, 13.2, 11.0, 8.8, 13.7, 12.1, 9.8)
mean(ar)
median(ar)
sd(ar)
sd(ar)**2
summary(ar)
min(ar)
max(ar)
quantile(ar, probs = 0.70)
100*sd(ar)/mean(ar)


m = matrix(1:9, nrow=3, ncol=3)

m[3,] = 111

aux = 911:959
m17 = matrix(1:80, ncol=10)


set.seed(7)
df2 <- data.frame(var1 = rnorm(10),
                  var2 = rnorm(10),
                  var3 = rnorm(10))

set.seed(9)
df2 <- data.frame(var1 = rnorm(10),
                  var2 = rnorm(10),
                  var3 = rnorm(10))


v = letters[sample(1:26,size=10)]

paste0(letters[1:7], 1:7)
