##### ------ Trabalho Final - Econometria (RASCUNHO) ------ #####

##### ----- Importando Pacotes ------ #####

library("tidyverse")
library("tidyquant")
library("Quandl")
library("timetk")
library("devtools")
library("readxl")
library("AER")
library("fBasics")

##### ------ QUESTÃO 1 ------ #####

##### ------ Importando Dados ------ #####

# Cotações
ativos <- c("VALE3.SA", "PETR4.SA") # Localizar e Substituir o nome do ativo por outros
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
                 type = "log",
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

##### ------ Importando fatores de mercado do NEFIN-USP ------ #####

MKT_Factor <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/Trabalho Final/Market_Factor.xls")
HML_Factor <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/Trabalho Final/HML_Factor.xls")
SMB_Factor <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/Trabalho Final/SMB_Factor.xls")

MKT <- MKT_Factor %>% select(date, Rm_minus_Rf) %>% rename("MKT" = Rm_minus_Rf)
MKT$date <- as.Date(MKT$date)

HML <- HML_Factor %>% select(date, HML)
HML$date <- as.Date(HML$date)

SMB <- SMB_Factor %>% select(date, SMB)
SMB$date <- as.Date(SMB$date)

##### ------ Unindo todos os dados obtidos ------ #####

df <- df_retornos %>% 
    transmute("date" = date,
              "VALE3-Rf" = VALE3.SA - selic,
              "PETR4-Rf" = PETR4.SA - selic) %>% 
    left_join(HML, by = "date") %>% 
    left_join(SMB, by = "date") %>% 
    left_join(MKT, by = "date") 

##### ------ CAPM ------ #####

capm_a <- lm(`VALE3-Rf` ~ MKT, data = df)
summary(capm_a)

capm_b <- lm(`PETR4-Rf` ~ MKT, data = df)
summary(capm_b)

##### ------ Fama-French ------ #####

ffrench_a <- lm(`VALE3-Rf` ~ MKT + HML + SMB, data = df)
summary(ffrench_a)

ffrench_b <- lm(`PETR4-Rf` ~ MKT + HML + SMB, data = df)
summary(ffrench_b)

##### ------ QUESTÃO 2 ------ #####

##### ------ Teste de Heterocedasticidade ------ #####

# Teste de Breusch-Pagan

bptest(capm_b) # Heterocedastico

bptest(ffrench_b) # Heterocedastico

##### ------ Testes de Correlação Serial ------ #####

# Teste de Breusch-Godfrey

bgtest(capm_b) # Tem correl serial

bgtest(ffrench_b) # Não tem correl serial

# Outliers 

outliers_capm_b <- which(apply(influence.measures(capm_b)$is.inf, 1, any))
outliers_ffrench_b <- which(apply(influence.measures(ffrench_b)$is.inf, 1, any))

capm_b_corrigido <- lm(`PETR4-Rf` ~ MKT, data = df[-outliers_capm_b, ])
summary(capm_b_corrigido)

ffrench_b_corrigido <- lm(`PETR4-Rf` ~ MKT + HML + SMB, data = df[-outliers_ffrench_b, ])
summary(ffrench_b_corrigido)

##### ------ Teste de Heterocedasticidade ------ #####

# Teste de Breusch-Pagan

bptest(capm_b_corrigido) # Heterocedastico

bptest(ffrench_b_corrigido) # Heterocedastico

##### ------ Testes de Correlação Serial ------ #####

# Teste de Breusch-Godfrey

bgtest(capm_b_corrigido) # Sem correl serial

bgtest(ffrench_b_corrigido) # Sem correl serial

##### ------ QUESTÃO 3 ------ #####

##### ------ Testes de Normalidade dos Resíduos ------ #####

# Teste de Jarque-Bera 

jarqueberaTest(residuals(capm_b_corrigido)) # Residuos não normalizados
jarqueberaTest(residuals(ffrench_b_corrigido)) # Residuos não normalizados

# Corrigir Heterocedasticidade

# Natureza das variáveis
# Outliers
# Formas funcionais 
# Transformação logarítmica

resettest(capm_b_corrigido, power = c(2,3,4,5)) # Erro na forma funcional

capm_b_corrigido <- lm(`PETR4-Rf` ~ MKT + I(MKT^4) + I(MKT^5), data = df[-outliers_capm_b, ])
summary(capm_b_corrigido)

resettest(capm_b_corrigido, power = c(2,3,4,5))


# depois
jarqueberaTest(residuals(capm_b_corrigido))
jarqueberaTest(residuals(ffrench_b_corrigido))

# Concluímos que não foi possível corrigir a normalidade da série de resíduos obtida pelas nossas regressões com o ativo analisado. Isso ocorre principalmente
# pelo fato de que estamos trabalhando com séries temporais de retornos, o que significa que o retorno no período n geralmente carrega informações sobre qual 
# será o retorno no próximo período. 

##### ------ QUESTÃO 4 ------ #####

linearHypothesis(capm_a, "MKT=1")

linearHypothesis(capm_b_corrigido, c("MKT=1", "I(MKT^4)=1"))

linearHypothesis(ffrench_a, "MKT=1")

linearHypothesis(ffrench_b_corrigido, "MKT=1")

# O fato de o beta da ação VALE3 ser muito próximo de um (aprox. 0.98) fez com que aceitássemos a hipótese nula de que seu beta seria estatisticamente igual
# a 1, no modelo de Fama-French. Entretanto, o modelo CAPM rejeita essa hipótese de maneira muito significante. 

# No caso da PETR4, ambos os testes de hipóteses realizados rejeitaram severamente a hipótese de que o vetor de betas é estatisticamente igual a 1. 

summary(capm_a) # Beta = 1.17
summary(capm_b_corrigido) # Beta = 1.53
summary(ffrench_a) # Beta = 0.98
summary(ffrench_b_corrigido) # Beta = 1.44

# Analisando os betas do fator de mercado para ambas as ações, tanto o modelo de Fama-French quanto o modelo CAPM propuseram que, de maneira significante, a 
# ação da PETR4 está mais exposta ao risco sistêmico, por apresentar maior beta (que representa a maior influência do fator mercado no retorno da ação). 

##### ------ QUESTÃO 5 ------ #####

# Não é possível realizar o teste F entre dois modelos que possuem quantidades de observações diferentes, por isso não utilizamos os modelos corrigidos 
# que retiraram ou outliers para fazer a análise.

anova(capm_b, ffrench_b)

# O teste F para os dois modelos rejeita a hipótese nula de que o modelo restrito é válido, indicando que o modelo de Fama-French é preferível ao CAPM.

##### ------ QUESTÃO 6 ------ #####

linearHypothesis(ffrench_b_corrigido, "(Intercept) = 0")

# O teste de hipótese corrobora com o p-valor do teste-t fornecido pelo sumário do modelo, que diz que o valor obtido para o intercepto era não era estatisticamente
# significante. O p-valor do teste aceita o modelo restrito de que o intercepto é igual a zero.

##### ------ QUESTÃO 7 ------ #####

residuos <- residuals(ffrench_b_corrigido)
n <- seq(from = 1, to = length(residuos), by = 1)

df_residuos <- tibble(tempo = n, residuos = residuos) %>% 
    mutate("residuos_2" = residuos^2)

graf_1 <- df_residuos %>% 
    ggplot(aes(x = tempo, y = residuos)) +
    geom_line() +
    labs(x = "Amostras",
         y = "Resíduos",
         title = "Resíduos",
         subtitle = "Empresa: Petrobrás.SA",
         caption = "Dados coletados do Yahoo Finance") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw()

graf_2 <- df_residuos %>% 
    ggplot(aes(x = tempo, y = residuos_2)) +
    geom_line() +
    labs(x = "Amostras",
         y = "Resíduos^2",
         title = "Resíduos ao Quadrado",
         subtitle = "Empresa: Petrobrás.SA",
         caption = "Dados coletados do Yahoo Finance") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw()

# library("gridExtra")

grid.arrange(graf_1, graf_2)






