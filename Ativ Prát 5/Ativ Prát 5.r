##### ------ Atividade Prática 5 - Econometria (RASCUNHO) ------#####

##### ----- Importando Pacotes ------ #####

library("tidyverse")
library("tidyquant")
library("Quandl")
library("timetk")
library("devtools")
library("readxl")
library("AER")
library("fBasics")
library("readr")

dados <- read_csv("D:/Dropbox/UFMG/Programação/R/Métodos/Ativ Prát 5/MarketingProduto.csv")

dados <- dados %>% na.omit()

#### QUESTÃO 1 ####

# RESPONSE = clicou ou não no anúncio
# GENDER = sexo do usuário
# ACTIVITY = interações passadas com a página
# AGE = idade do usuário
# AGE2 = idade do usuário ao quadrado

logit <- glm(RESPONSE ~ GENDER + ACTIVITY + AGE + AGE2,
             data = dados,
             family = binomial(link = "logit"))

summary(logit)

#### QUESTÃO 2 ####

multiplicador_1 <- exp(summary(logit)$coefficients[2])-1

# A mudança no gênero de uma pessoa multiplica a razão de chances por aproximadamente 160% tendo em vista
# a natureza da função logística (razão de chances = exp(beta * x))

#### QUESTÃO 3 ####

multiplicador_2 <- exp(summary(logit)$coefficients[3])-1

# O fato de a pessoa ter tido uma interação passada com a comunidade multiplica a razão de chances por aproximadamente 149% 
# tendo em vista a natureza da função logística (razão de chances = exp(beta * x))

#### QUESTÃO 4 ####

# A partir da função de distribuição logística, temos que P(Y=1) = exp(alfa + beta1 * x1...). Logo:

prob1 <- exp(summary(logit)$coefficients[1] + 20*summary(logit)$coefficients[4] + 1*summary(logit)$coefficients[3])
prob1 <- prob1/(1+prob1)

#### QUESTÃO 5 ####

prob2 <- exp(summary(logit)$coefficients[1] + (40*summary(logit)$coefficients[4]) + (0*summary(logit)$coefficients[3]))
prob2 <- prob2/(1+prob2)

#### QUESTÃO 6 ####

# Para maximizar a probabilidade dada a idade da pessoa, utilizaremos uma regressão logística que utiliza uma nova forma funcional
# mantendo tudo constante, analisaremos AGE e AGE^2, que representaria uma função de segundo grau (ax^2 + bx + c). Para maximizá-la, basta
# encontrar o x do seu vértice, dado por -b/2a. Maximizando a função, temos que ao isolar a probabilidade, maximizaremos o termo exponencial
# e assim teremos o valor que apresenta a maior probabilidade. 

# Função: y = AGE^2 + AGE + alfa

vertice <- -summary(logit)$coefficients[4]/(2*summary(logit)$coefficients[5])

# logo, a idade de 51 anos maximiza a prob de o indivíduo responder ao anúncio



'''
prob <- vector("numeric", length = length(dados$OBS))

for (i in seq_along(prob)){
probabilidade <- exp(summary(logit)$coefficients[1] + (dados$AGE*summary(logit)$coefficients[4]))
prob[i] <- probabilidade/(1+probabilidade)
} 

which.max(prob)
''' 



