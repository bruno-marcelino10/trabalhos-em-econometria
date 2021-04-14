###### ------ Prova 2 - Econometria: Bruno Marcelino ------

### Importando Bibliotecas
library("readxl")
library("tidyverse")
library("tidyquant")
library("timetk")
library("devtools")
library("AER")
library("fBasics")

# Importando Dados

f1 <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/prova 3/f1.xlsx")
f2 <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/prova 3/f2.xlsx")
f3 <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/prova 3/f3.xlsx")

# ----- QUESTÃO 1 -----

### Criação dos modelos

# Modelo restrito: CAPM

mod_rest <- lm(fundo ~ mercado, data = f2)
summary(mod_rest)

# Modelo irrestrito: CAPM com dummy que analisa a influência ou não do mês de Janeiro
mod_irr <- lm(fundo ~ mercado + dum_janeiro, data = f2)
summary(mod_irr)

# Teste F
anova(mod_rest,mod_irr)

# ----- QUESTÃO 2 -----

# Modelo restrito: CAPM
mod_rest <- lm(fundo ~ mercado, data = f3)
summary(mod_rest)

# Modelo irrestrito: CAPM com dummy que analisa a influência ou não do mês de Janeiro
mod_irr <- lm(fundo ~ mercado + dum_janeiro, data = f3)
summary(mod_irr)

# Teste RESET

resettest(mod_rest, power = c(2,3,4))
resettest(mod_irr, power = c(2,3,4))

# Medidas de Influência

outliers_mod_rest <- influence.measures(mod_rest)
outliers_mod_irr <- influence.measures(mod_irr)

dfbetaPlots(mod_rest)
dfbetaPlots(mod_irr)

# Teste de Jarque-Bera

jarqueberaTest(residuals(mod_rest))
jarqueberaTest(residuals(mod_irr))

# ----- QUESTÃO 3 -----

### Criação dos modelos

# Modelo restrito: CAPM

mod_rest <- lm(fundo ~ mercado, data = f1)
summary(mod_rest)

# Modelo irrestrito: CAPM com dummy que analisa a influência ou não do mês de Janeiro
mod_irr <- lm(fundo ~ mercado + dum_janeiro, data = f1)
summary(mod_irr)

### Testes de heterocedasticidade de Breusch-Pagan

bptest(mod_rest)
bptest(mod_irr)

### Testes de Correlação Serial

# Teste de Breusch-Godfrey

bgtest(mod_rest)
bgtest(mod_irr)

# Teste de Durbin-Watson

dwtest(mod_rest)
dwtest(mod_irr)



