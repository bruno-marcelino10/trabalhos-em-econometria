# RESOLUÇÃO LISTA 3 MÉTODOS 08/02

library("lmtest")
library("AER")
library("strucchange")
library("readr")

df <- read_csv("D:/Dropbox/UFMG/Programação/R/Métodos/Lista 3/Base de Dados Parque dos Anjos.csv")

# Q1

mod_1 <- lm(Visitantes ~ PIB, data = df)
summary(mod_1)

# Q2

plot(residuals(mod_1)) # Há heteroedasticidade nos termos de erro do modelo

# Q3 

bptest(mod_1) # O modelo é heterocedastico

# Q4

coeftest(mod_1, vcov. = vcovHC)

# Q5
mod_2 <- lm(log(Visitantes) ~ log(PIB), data = df)
summary(mod_2)

# Q6 

coeftest(mod_2, vcov. = vcovHC)

# Q7

sctest(log(Visitantes) ~ log(PIB), data = df, type = "Chow", point = 15) #teste de quebra estrutural no ponto 2005

# Q8

