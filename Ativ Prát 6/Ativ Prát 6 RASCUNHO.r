##### ------ Atividade Prática 6 - Econometria (RASCUNHO) ------#####

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
library("plm")

producao <- read_csv("D:/Dropbox/UFMG/Programação/R/Métodos/Ativ Prát 6/Producao.csv")

### Variáveis

# Ano = ano da observação
# Empresa = número de identificação da empresa
# Capital = valor total dos ativos imobilizados da empresa
# Trabalho = valor gasto com mão de obra pela empresa
# Producao = valor agregado da produção da empresa em determinado ano

#### QUESTÃO 1 ####

producao <- producao %>% select(Empresa, Ano, Capital, Trabalho, Producao)

producao_fe <- plm(log(Producao) ~ log(Capital) + log(Trabalho), data = producao, model = "within")
summary(producao_fe)

#### QUESTÃO 2 ####

round(summary(producao_fe)$coefficients[1] * 2.5, 2)

# A interpretação é a mesma para modelos log-log. Logo, uma variação de 2,5% no valor investido em máquinas e equipamentos (Capital) equivale a uma variação de 
# 0,44% na produção total da empresa. 

#### QUESTÃO 3 ####

round(summary(producao_fe)$coefficients[2] * 10, 2)

# Similarmente ao obtido no raciocínio anterior, obtemos que uma variação de 10% no valor investido em máquinas e equipamentos (Capital) equivale a uma variação 
# de 8,39% na produção total da empresa.

#### QUESTÃO 4 ####

# Basta comparar os alfas obtidos para cada empresa, dado que estamos tratando de modelos em painel de efeitos fixos para cada uma. Tendo em vista que os coeficientes
# obtidos são os mesmos para todas as empresas, a que obteria o melhor valor agregado bruto será a que apresenta o maior valor para seu dado intercepto. 

summary(fixef(producao_fe))

# Visualmente, obtemos que o maior dentre os alfas significativos é o da empresa 3332. 

#### QUESTÃO 5 ####

producao_re <- plm(log(Producao) ~ log(Capital) + log(Trabalho), data = producao, model = "random", random.method = "walhus")
summary(producao_re)

#### QUESTÃO 6 ####

phtest(producao_re, producao_fe)

# Dado que o p-valor do teste está acima do nível de significância adotado, podemos aceitar a hipótese nula de que ambos os modelos são consistentes. Dessa maneira, 
# Podemos afirmar que o modelo de efeitos fixos é eficiente e portanto melhor do que o modelo de efeitos aleatórios. 

