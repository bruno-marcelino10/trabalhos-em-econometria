# ---- Lista de Exercícios 2 - Bruno Marcelino ----

# Importando dados e carregando bibliotecas

library("MASS")
library("AER")
library("lmtest")

data("Boston")

library(readr)
despesas <- read_csv("D:/Dropbox/UFMG/4º Sem/Métodos/Módulo 2/despesas.csv")
bankwages <- read_delim("D:/Dropbox/UFMG/4º Sem/Métodos/Módulo 2/bankwages.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE)

#### Respostas das questões da lista 

# ------ Q1 ------

?Boston

# ------ Q2 ------

summary(Boston)

# ------ Q3 ------

bh_mod <- lm(medv ~ lstat, data = Boston)
summary(bh_mod)

# ------ Q4 ------

mod <- lm(medv ~ age + crim + lstat, data = Boston)
summary(mod)

# ------ Q5 ------

summary(mod)$r.squared > summary(bh_mod)$r.squared

# ------ Q6 ------

summary(mod)$adj.r.squared > summary(bh_mod)$adj.r.squared

# ------ Q7 ------

full_mod <- lm(medv ~ .,  data = Boston) 

summary(full_mod)

# ------ Q8 ------

summary(full_mod)$coefficients

# lstat apresentou maior significância, pois possui menor p-valor

# ------ Q9 ------

summary(full_mod)$adj.r.squared > summary(mod)$adj.r.squared

# ------ Q10 ------

coefs <- summary(mod)$coefficients

# ------ Q11 ------

p_valor1 <- as.numeric(vector())
p_valor2 <- as.numeric(vector())
r_quadrado <- as.numeric(vector())
g <- as.numeric(vector())

for (i in colnames(despesas)[-1]){

    modelo <- lm(log(despesas[[i]]) ~ n, data = despesas)
    p_valor1[i] <- summary(modelo)$coefficients[7]
    p_valor2[i] <- summary(modelo)$coefficients[8]
    r_quadrado[i] <- summary(modelo)$r.squared
    g[i] <- exp(summary(modelo)$coefficients[2])-1
    matriz <- cbind(p_valor1, p_valor2, r_quadrado, g)
    } 



