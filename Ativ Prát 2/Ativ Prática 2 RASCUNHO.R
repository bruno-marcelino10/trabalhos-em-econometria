##### Atividade Prática 2 - Econometria (RASCUNHO) #####

library("MASS")
library("AER")

df <- tibble(Boston)

#### SIGNIFICADOS DAS VARIÁVEIS: -----------------------

# medv = valor medio das casas em uma vizinhanças
# lstat = percentual de moradores de baixo status economico
# chas = variável dummy (1 = perto do rio, 0 = caso contrário)
# old = variável dummy (1 = age>=95, 0 = caso contrário)
# indus = proporção de acres de negócios não varejistas por cidade.

#### Respostas

# ---- Q1 --------------------------

mod <- lm(df$medv ~ df$lstat)
summary(mod)

# ---- Q2 --------------------------

grafico_1 <- ggplot(df, aes(x = lstat , y = medv)) +
    geom_point() + 
    ggtitle("Gráfico 1") +
    xlab("% Moradores de Baixa Renda") +
    ylab("Valor Médio das Casas") +
    geom_smooth(method = "lm", color = "red")

print(grafico_1)

# ---- Q3 --------------------------

log_mod <- lm(df$medv ~ I(log(df$lstat, base = exp(1))))
summary(log_mod)

# ---- Q4 --------------------------

grafico_2 <- ggplot(df, aes(x = lstat , y = medv)) +
  geom_point() + 
  ggtitle("Gráfico 2") +
  xlab("Log Neperiano da % de Moradores de Baixa Renda") +
  ylab("Valor Médio das Casas") +
  geom_smooth(method = lm, formula = y ~ log(x, base = exp(1)), color = "red")

print(grafico_2)

# ---- Q5 --------------------------

log2_mod <- lm(df$medv ~ log(df$lstat) + I(log(df$lstat)^2))
summary(log2_mod)

grafico_3 <- ggplot(df, aes(x = lstat , y = medv)) +
  geom_point() + 
  ggtitle("Gráfico 3") +
  geom_smooth(method = lm, formula = y ~ x + I(log(x, base = exp(1))^2), color = "red")

print(grafico_3)

# ---- Q6 --------------------------

## Argumentos

x1 = 10
x2 = 11

# Função que retorna a variação de y em um intervalo de x, que se situa entre por x1 e x2 
delta_y <- function(x1, x2){
  c = summary(log2_mod)$coefficients[1]
  b = summary(log2_mod)$coefficients[2]
  a = summary(log2_mod)$coefficients[3]
  
  y1 = b*log(x1, base = exp(1)) + a*log(x1, base = exp(1))^2 + c
  y2 = b*log(x2, base = exp(1)) + a*log(x2, base = exp(1))^2 + c
  return(y2 - y1)
  }
  
delta_y(x1, x2)

# ---- Q7 --------------------------

df <- df %>% 
  mutate(old = ifelse(age >= 95, 1, 0))

mod_co <- lm(df$medv ~ df$chas + df$old + I(df$chas*df$old))
summary(mod_co)

# ---- Q8 --------------------------



# ---- Q9 --------------------------

mod_io <- lm(df$medv ~ df$indus + df$old + I(df$indus*df$old))
summary(mod_io)

# ---- Q10 -------------------------

