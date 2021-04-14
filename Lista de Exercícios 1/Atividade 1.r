##### Lista de Exercícios 1 #####

### Tratamento dos Dados
library("xlsx")

# Essa base de dados apresenta a renda média por hora trabalhada (a preços de 2008) para homens (1) e mulheres (2) com diploma 
# universitário (College Graduates) de 1992 a 2008 nos Estados Unidos.
cps <- read.xlsx("D:/Dropbox/UFMG/4º Sem/Métodos/R/cps_ch3.xlsx", sheetName = "Data", as.data.frame = TRUE)
str(cps)

# QUESTÃO 1. Seja Z∼ N (0,1), isto é, seja Z uma variável aleatória que segue uma Distribuição Normal de
# média 0 e variância 1. Calcule o valor da função densidade de probabilidade no ponto z = 3.
# Dica: pesquise sobre o comando “dnorm”

z <- 3

FDC_z <- dnorm(z)
print(FDC_z)

# QUESTÃO 2. Seja Z∼ N (0,1). Calcule P(|Z|≤ 1.64), isto é, a probabilidade de que o valor absoluto de Z
# seja menor ou igual a 1.64. Dica: pesquise sobre o comando “pnorm”

z1 <- 1.64
z2 <- -1.64
FDA_z1 <- pnorm(z1)
FDA_z2 <- pnorm(z2)
print(FDA_z1-FDA_z2)

# ? QUESTÃO 3. Seja Y ∼ N(5,25). Calcule o quantil 99% dessa distribuição. Isto é, encontre o valor de y tal
# que os valores acima de y apresentam uma probabilidade de apenas 1% de serem
# observados. Dica: pesquise sobre o comando “qnorm”.

y <- 0.99
quant_y <- qnorm(y, mean = 5, sd = 5)
print(quant_y)

# QUESTÃO 4. O que acontece se o output (resultado) da função “qnorm” servir de input (entrada) para a
# função “pnorm” e vice versa? O que isso demonstra sobre a relação entre as funções? 

# R.: Isso demonstra que as funções operam de modo inverso. Enquanto a pnorm define a probabilidade acumulada até um ponto,
# a qnorm define qual ponto possui FDA em determinado valor percentual (quantil). Fazendo a operação indicada, 
# chegamos ao mesmo valor inicial.

y <- 0.99
quant_y <- qnorm(y, mean = 5, sd = 5)
print(quant_y)

FDA_y <- pnorm(quant_y, mean = 5, sd = 5)
print(FDA_y)

print(FDA_y == y)

# QUESTÃO 5. Seja X ∼ t(10000), isto é, seja X uma variável aleatória que segue uma Distribuição t de Student
#com 10000 graus de liberdade. E seja Z∼ N (0,1). Calcule o quantil 95% de ambas as
# distribuições. Dica: pesquise sobre o comando “qt”.

quantil <- 0.95

quant_t <- qt(quantil, df = 10000)
print(quant_t)

quant_x <- qnorm(quantil)
print(quant_x)

# QUESTÃO 6. Repita o exercício anterior, mas ao invés de 10000 graus de liberdade, utilize apenas 10. O
# que mudou? E o que isso significa? 

# R.: Isso significa que quanto maior a quantidade de graus de liberdade da t de Student, mais esta se 
# aproxima de uma distribuição normal, aparentemente.  
    
quantil <- 0.95

quant_t <- qt(quantil, df = 10)
print(quant_t)

quant_t <- qt(quantil, df = 100)
print(quant_t)

quant_t <- qt(quantil, df = 1000)
print(quant_t)

quant_t <- qt(quantil, df = 10000)
print(quant_t)


quant_x <- qnorm(quantil)
print(quant_x)

# QUESTÃO 7. Seja X ∼ t(1). Gere 1000 números aleatórios dessa distribuição e atribua esses 1000 números à
# variável “x”. Calcule a média amostral dos valores atribuídos a “x”. Repita o procedimento
# algumas vezes. Considerando que a Distribuição t de Student é simétrica em torno do zero, o
# que explica que as médias amostrais de “x” são tão diferentes de zero? Dica nº 1: pesquise
# sobre o comando “rt”. Dica nº 2: faça esse mesmo teste usando outros valores de graus de liberdade.

n = 1000

x <- rt(n, df = 1)
media_x <- mean(x)

y <- rt(n, df = 1)
media_y <- mean(y)

z <- rt(n, df = 1)
media_z <- mean(z)

print(media_x)
print(media_y)
print(media_z)