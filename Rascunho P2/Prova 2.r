# ---- Prova 2 - Métodos Econométricos ----

# Importando dados
library(readxl)
automoveis <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/prova2/automoveis.xlsx")
demanda <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/prova2/demanda.xlsx")
vendas <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/prova2/vendas.xlsx")

# ------ Q4 ------

### Variáveis 
# packs <- Demanda anual média de cigarros per capita
# price <- preço médio de cigarros 

# elasticidade <- beta (log-log)

modelo1 <- lm(log(packs) ~ log(price), data = demanda)
summary(modelo1)
elast <- summary(modelo1)$coefficients[2]
print(elast)

# ------ Q2 ------

### Variáveis 
# MARCA <- número de carros vendidos no período
# OBS <- tempo 
# ln(y) = ln(yo) + t*(ln(1+g))
# y = yo(1+g)^t

n <- seq(1, length(automoveis$OBS), 1)
automoveis$n <- n

# Teste
modelo2 <- lm(log(SUZUKI) ~ n, data = automoveis)
summary(modelo2)

g <- as.numeric(vector()) # vetor nulo que irá receber os valores de g

for (i in colnames(automoveis)[-c(1,7)]){
    
    modelo <- lm(log(automoveis[[i]]) ~ n, data = automoveis)
    g[i] <- exp(summary(modelo)$coefficients[2])-1
    
} 

# Testando de outra forma
new_data <- data.frame(n = c(1, 14))
y_chapeu <- predict(modelo2, newdata = new_data)
delta_y <- diff(y_chapeu)

#A) Menor CAGR mensal 
CAGR_max_mensal <- g[which.max(g)]

#B) Menor CAGR anual
CAGR__max_anual <- (1+CAGR_max)^12-1
round(CAGR_anual, 4)

#C) Menor CAGR mensal 
CAGR_min_mensal <- g[which.min(g)]

#D) Menor CAGR anual
CAGR_min_anual <- (1+CAGR_min_mensal)^12-1
round(CAGR_min_anual, 4)

# ------ Q3 ------

### Variáveis

# wage <- salário médio 
# education <- anos de estudo
# experience <- tempo no mercado de trabalho
# ethnicity <- dummy (1 = afrodescendente, 0 = caso contrário)
# modelo <- lm(wage ~ education + experience + I(experience^2) + ethnicity)

#A)
round(54.26492, 2)

#B) experiencia que situa no ponto max de y = x do vértice = -b/2a

round(-34.30875/(-2*0.53957), 2)

# ------ Q1 ------

### Variáveis

# VENDAS <- unidades vendidas (milhares)
# D2 <- dummy (1 para 2 trimestre)
# D3 <- dummy (1 para 3 trimestre)
# D4 <- dummy (1 para 4 trimestre)

#A) 
modelo_4 <- lm(VENDAS ~ D2 + D3 + D4, data = vendas)

#B) 
round(73.183 + 57.115)

#C) 
var <- summary(modelo_4)$coefficients
round(73.18343 + 27.96471, 2)

#D) 
res <- residuals(modelo_4)
plot(modelo_4)

round(54.26492, 3)

