##### ------ Atividade Prática 4 - Econometria (RASCUNHO) ------#####

##### ----- Importando Pacotes ------ #####

library("tidyverse")
library("tidyquant")
library("Quandl")
library("timetk")
library("devtools")
library("readxl")
library("AER")
library("fBasics")

##### ------ Importando Dados ------ #####

# Cotações
ativos <- c("VALE3.SA", "^BVSP") # Localizar e Substituir o nome do ativo por outros
inicio <- as.Date("2010-01-04")
fim <- as.Date("2020-12-30")

cotacao <- ativos %>% 
    tq_get(get = "stock.prices", from = inicio, to = fim) %>% 
    mutate(adjusted = na.locf(adjusted))

# Retornos
df_retornos <- cotacao %>% 
    group_by(symbol) %>% 
    select(date, adjusted) %>% 
    tq_transmute(select = adjusted,
                 mutate_fun = periodReturn,
                 period = "daily",
                 type = "arithmetic",
                 col_rename = "retorno") %>% 
    spread(key = symbol, value = retorno)

##### ------ Importando a taxa SELIC da API do BACEN (de 2010 até hoje) ------ #####

Quandl.api_key('TC1ow5j6G7s4SFHTzgDz') # Acrescentando a chave da API - Comando necessário pra acessar o Quandl
selic_diaria <-  as.data.frame(Quandl("BCB/11", type = "xts")/100) # Importando a serie do selic do Bacen
df_selic <- selic_diaria %>% 
    rownames_to_column() %>% 
    tibble() %>% 
    rename("date" = rowname, "selic" = V1) %>% 
    mutate(selic = na.locf(selic))

df_selic$date <- as.Date(df_selic$date)

# acréscimo na base de dados
df_retornos <- df_retornos %>% 
    left_join(df_selic, by = "date")

##### ------ Montando regressão do CAPM ------ #####

df_capm <- df_retornos %>% 
    transmute("date" = date,
              "Rp-Rf" = VALE3.SA - selic,
              "Rm-Rf" = `^BVSP` - selic)

capm1 <- lm(df_capm$`Rp-Rf` ~ df_capm$`Rm-Rf`)
summary(capm1)

##### ------ Montando regressão de Fama-French ------ #####

MKT_Factor <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/Ativ Prát 3/Market_Factor.xls")
HML_Factor <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/Ativ Prát 3/HML_Factor.xls")
SMB_Factor <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/Ativ Prát 3/SMB_Factor.xls")

MKT <- MKT_Factor %>% select(date, Rm_minus_Rf) %>% rename("MKT" = Rm_minus_Rf)
MKT$date <- as.Date(MKT$date)

HML <- HML_Factor %>% select(date, HML)
HML$date <- as.Date(HML$date)

SMB <- SMB_Factor %>% select(date, SMB)
SMB$date <- as.Date(SMB$date)

fatores <- df_capm %>% 
    left_join(HML, by = "date") %>% 
    left_join(SMB, by = "date") %>% 
    left_join(MKT, by = "date") %>% 
    select(-`Rm-Rf`)

ffrench1 <- lm(`Rp-Rf` ~ MKT + HML + SMB, data = fatores)
summary(ffrench1)

##### ------ QUESTÃO 1 ------ #####

# A) Teste de normalidade dos resíduos de Jarque-Bera

jarqueberaTest(residuals(capm1))
jarqueberaTest(residuals(ffrench1))

# B) Medidas de Influência das observações

outliers_capm <- influence.measures(capm1)
outliers_ffrench <- influence.measures(ffrench1)

# Gráficos de identificação dos outliers

par(mfrow=c(2,3))
plot(capm1, which=1:6)

par(mfrow=c(2,3))
plot(ffrench1, which=1:6)

# C) Testes de heterocedasticidade de Breusch-Pagan

bptest(capm1)
bptest(ffrench1)

# D) Testes de Correlação Serial

# Teste de Durbin-Watson

dwtest(capm1)
dwtest(ffrench1)

# Teste de Breusch-Godfrey

bgtest(capm1)
bgtest(ffrench1)

###### ------ QUESTÃO 2 ------ #####

# CAPM e Fama-French sem outliers

outliers_capm <- which(apply(influence.measures(capm1)$is.inf, 1, any))
outliers_ffrench <- which(apply(influence.measures(ffrench1)$is.inf, 1, any))

capm2 <- lm(`Rp-Rf` ~ `Rm-Rf`, data = df_capm[-outliers_capm, ])
summary(capm2)

ffrench2 <- lm(`Rp-Rf` ~ MKT + HML + SMB, data = fatores[-outliers_ffrench, ])
summary(ffrench2)

# A) Teste de normalidade dos resíduos de Jarque-Bera

jarqueberaTest(residuals(capm2))
jarqueberaTest(residuals(ffrench2))

# B) Medidas de Influência das observações

# Gráficos de identificação dos outliers

par(mfrow=c(2,3))
plot(capm2, which=1:6)

par(mfrow=c(2,3))
plot(ffrench2, which=1:6)

# C) Testes de heterocedasticidade de Breusch-Pagan

bptest(capm2)
bptest(ffrench2)

# D) Testes de Correlação Serial

# Teste de Durbin-Watson

dwtest(capm2)
dwtest(ffrench2)

# Teste de Breusch-Godfrey

bgtest(capm2)
bgtest(ffrench2)

###### ------ QUESTÃO 3 ------ #####

### Correção da heterocedasticidade -> erros-padrão de White ### 
 
# sem correção
summary(capm2)
summary(ffrench2)

# com correção (atribui menores pesos a informações discrepantes)
coeftest(capm2, vcov. = vcovHAC)
coeftest(ffrench2, vcov. = vcovHAC)

### Correção da normalidade dos resíduos -> dummy que equivale a 1 se a observação for identificada como um outlier ###

df_capm <- df_capm %>% 
    mutate("position" = seq(1:length(date))) %>% 
    mutate("D1" = ifelse(position %in% outliers_capm, 1, 0))

fatores <- fatores %>% 
    mutate("position" = seq(1:length(date))) %>% 
    mutate("D2" = ifelse(position %in% outliers_capm, 1, 0))

# Análise da regressão com dummy que só vale para os outliers

capm3 <- lm(`Rp-Rf` ~ `Rm-Rf` + D1, data = df_capm)
summary(capm3) 

ffrench3 <- lm(`Rp-Rf` ~ MKT + HML + SMB + D2, data = fatores)
summary(ffrench3)

# Concluimos que os outliers não estão contidos nos padrões gerais da amostra, pois a variável dummy não é significativa. 
# Isso significa que sua presença não contribui para as especificações do modelo.

### Correção da correlação serial



# Seria inviável tentar corrigir a correlação serial em modelos que envolvem séries temporais, como os estipulados pelo grupo. 
# O mercado de ações envolve a influência dos retornos históricos no retorno atual das ações. 

###### ------ QUESTÃO 4 ------ #####




