##### Atividade Prática 3 - Econometria (RASCUNHO) #####

# ----- Importando Pacotes ------

library("tidyverse")
library("tidyquant")
library("Quandl")
library("timetk")
library("devtools")
library("quantmod")
library("readxl")
library("GetQuandlData")
library("AER")

# ------ Importando Dados ------

# Cotações
ativos <- c("VALE3.SA", "^BVSP")
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

# ---- Importando a taxa SELIC da API do BACEN (de 2010 até hoje) ----

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

# ---- Montando regressão do CAPM ---- 
df_capm <- df_retornos %>% 
    transmute("date" = date, "Rp-Rf" = VALE3.SA - selic, "Rm-Rf" = `^BVSP` - selic)

capm1 <- lm(df_capm$`Rp-Rf` ~ df_capm$`Rm-Rf`)
summary(capm1)

# Gráfico CAPM

graf_1 <- df_capm %>% 
    ggplot(aes(x = `Rm-Rf`, y = `Rp-Rf`)) +
    geom_point() +
    labs(x = "Prêmio de Risco do Mercado",
         y = "Prêmio de Risco do Ativo",
         title = "CAPM",
         subtitle = "Empresa: Vale S.A",
         caption = "Dados coletados do Yahoo Finance e da API do Quandl") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_smooth(method = "lm", color = "red") + 
    theme_bw()

graf_1

# ---- Montando regressão de Fama-French ----

# (adaptar os arquivos diretamente no Excel, manipulando a data para o formato YYYY-MM-DD)
# Posso usar a função as.Date(paste(year, month, day, sep = "-")) também
MKT_Factor <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/Ativ Prát 3/Market_Factor.xls")
HML_Factor <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/Ativ Prát 3/HML_Factor.xls")
SMB_Factor <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/Ativ Prát 3/SMB_Factor.xls")

# Gráfico MKT
MKT <- MKT_Factor %>% select(date, Rm_minus_Rf) %>% rename("MKT" = Rm_minus_Rf)
MKT$date <- as.Date(MKT$date)

graf_2 <- MKT %>% 
    ggplot(aes(x = date, y = MKT)) +
    geom_point() +
    labs(x = "",
         y = "MKT",
         title = "Fator MKT",
         caption = "Dados coletados do site NEFIN-USP") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw()

graf_2

# Gráfico HML
HML <- HML_Factor %>% select(date, HML)
HML$date <- as.Date(HML$date)

graf_3 <- HML %>% 
    ggplot(aes(x = date, y = HML)) +
    geom_point() +
    labs(x = "",
         y = "HML",
         title = "Fator HML",
         caption = "Dados coletados do site NEFIN-USP") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw()

graf_3

# Gráfico SMB
SMB <- SMB_Factor %>% select(date, SMB)
SMB$date <- as.Date(SMB$date)

graf_4<- SMB %>% 
    ggplot(aes(x = date, y = SMB)) +
    geom_point() +
    labs(x = "",
         y = "SMB",
         title = "Fator SMB",
         caption = "Dados coletados do site NEFIN-USP") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw()

graf_4

SMB <- SMB_Factor %>% select(date, SMB)
SMB$date <- as.Date(SMB$date)


# Gráfico Retornos 

graf_5 <- df_retornos %>% 
    ggplot(aes(x = date, y = VALE3.SA)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Retornos Diários",
         subtitle = "Empresa: Vale S.A",
         y = "Retornos Mensais",
         x = "") + 
    theme_tq() + 
    scale_fill_tq()

graf_5

# ---- Montando regressão de três fatores de Fama-French ----
fatores <- df_capm %>% 
    left_join(HML, by = "date") %>% 
    left_join(SMB, by = "date") %>% 
    left_join(MKT, by = "date") %>% 
    select(-`Rm-Rf`)
    
ffrench1 <- lm(`Rp-Rf` ~ MKT + HML + SMB, data = fatores)
summary(ffrench1)

### Q2

# Teste F entre os dois modelos
anova(capm1,ffrench1)

### Q3

# VIF do modelo Fama-French
vif(ffrench1)

### Q4

# Teste RESET em cada modelo 

resettest(capm1, power = c(2,3,4))
resettest(ffrench1, power = c(2,3,4))

# Avalia o teste como potências dos regressores, e não potências de todas as variáveis em conjunto

resettest(ffrench1, power = c(2,3,4), type = "regressor") 

### Q5 

# Teste de Chow (quebra estrutural)

sctest(df_capm$`Rp-Rf` ~ df_capm$`Rm-Rf`, data = df_capm, type = "Chow")

sctest(`Rp-Rf` ~ MKT + HML + SMB, data = fatores, type = "Chow")

# Gráfico do teste F para subgrupos da amostra
capm_f <- Fstats(df_capm$`Rp-Rf` ~ df_capm$`Rm-Rf`, data = df_capm, from = 0.1, to = 0.9)
plot(capm_f, main = "CAPM Estatísticas F") # Gráfico do F
plot(capm_f, pval = T, main = "CAPM p-valores") # Gráfico do p-valor

ffrench1_f <- Fstats(`Rp-Rf` ~ MKT + HML + SMB, data = fatores, from = 0.1, to = 0.9)
plot(ffrench1_f, main = "Fama-French Estatísticas F") # Gráfico do F
plot(ffrench1_f, pval = T, main = "Fama-French p-valores") # Gráfico do p-valor

