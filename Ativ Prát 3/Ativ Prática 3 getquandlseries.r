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
ativos <- c("PETR4.SA", "^BVSP")
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

# SELIC

# set symbol and dates
my_api_key <- 'zm_uxwyTE_FH3EuzCbkn'
my_symbol <- "BCB / 11"
first_date <- as.Date('2010-01-04')
last_date <- as.Date('2020-12-30')

# get data
df_selic <- get_Quandl_series(id_in = my_symbol,
                              api_key = my_api_key, 
                              first_date = first_date,
                              last_date = last_date)

# conversão p/ mensal
df_selic <- df_selic %>% 
    rename("date" = ref_date, "selic" = value) %>% 
    select(date, selic) %>% 
    mutate(selic = selic/100)

# ---- Montando regressão do CAPM ---- 
df_capm <- df_retornos %>% 
    transmute("date" = date, "Rp-Rf" = PETR4.SA - selic, "Rm-Rf" = `^BVSP` - selic)

capm1 <- lm(df_capm$`Rp-Rf` ~ df_capm$`Rm-Rf`)
summary(capm1)

# Gráfico CAPM

graf_1 <- df_capm %>% 
    ggplot(aes(x = `Rm-Rf`, y = `Rp-Rf`)) +
    geom_point() +
    labs(x = "Prêmio de Risco do Mercado",
         y = "Prêmio de Risco do Ativo",
         title = "CAPM",
         subtitle = "Empresa: Petrobrás S.A",
         caption = "Dados coletados do Yahoo Finance e da API do Quandl") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_smooth(method = "lm", color = "red") + 
    theme_bw()


# ---- Montando regressão de Fama-French ----

# (adaptar os arquivos diretamente no Excel, manipulando a data para o formato YYYY-MM-DD)
# Posso usar a função as.Date(paste(year, month, day, sep = "-")) também
HML_Factor <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/Ativ Prát 3/HML_Factor.xls")
SMB_Factor <- read_excel("D:/Dropbox/UFMG/Programação/R/Métodos/Ativ Prát 3/SMB_Factor.xls")

# Gráfico HML
HML <- HML_Factor %>% select(date, HML)
HML$date <- as.Date(HML$date)

graf_2 <- HML %>% 
    ggplot(aes(x = date, y = HML)) +
    geom_point() +
    labs(x = "",
         y = "HML",
         title = "Fator HML",
         caption = "Dados coletados do site NEFIN-USP") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw()

# Gráfico SMB
SMB <- SMB_Factor %>% select(date, SMB)
SMB$date <- as.Date(SMB$date)

graf_3 <- SMB %>% 
    ggplot(aes(x = date, y = SMB)) +
    geom_point() +
    labs(x = "",
         y = "SMB",
         title = "Fator SMB",
         caption = "Dados coletados do site NEFIN-USP") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_bw()

SMB <- SMB_Factor %>% select(date, SMB)
SMB$date <- as.Date(SMB$date)

fatores <- df_capm %>% 
    left_join(HML, by = "date") %>% 
    left_join(SMB, by = "date")

# Gráfico Retornos 

graf_4 <- df_retornos %>% 
    ggplot(aes(x = date, y = PETR4.SA)) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Retornos Mensais",
         subtitle = "Empresa: Petrobrás S.A",
         y = "Retornos Mensais",
         x = "") + 
    theme_tq() + 
    scale_fill_tq()

# ---- Montando regressão de três fatores de Fama-French ----
ffrench1 <- lm(`Rp-Rf` ~ `Rm-Rf` + HML + SMB, data = fatores)
summary(ffrench1)

# Teste F entre os dois modelos
anova(capm1,ffrench1)

# VIF do modelo Fama-French
vif(ffrench1)
