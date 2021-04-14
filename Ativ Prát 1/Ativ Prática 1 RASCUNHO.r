##### Atividade Prática 1 - Econometria (RASCUNHO) #####

library("AER")

#Tratamento dos Dados

n_alunos <- c(23,19,30,22,23,29,35,36,33,25)

pont_med <- c(430,430,333,410,390,377,325,310,328,375)

library("tidyverse")
df <- tibble("Num_de_Alunos" = c(23,19,30,22,23,29,35,36,33,25),
                "Pont_Media" = c(430,430,333,410,390,377,325,310,328,375),
                )

#Q1:

grafico_1 <- ggplot(df, aes(x = Num_de_Alunos , y = Pont_Media)) +
 geom_point() + 
 ggtitle("Gráfico 1") +
 geom_line() +
 xlab("Número de Alunos") +
 ylab("Pontuação Média") +
 xlim(10, 40) +
 ylim(200, 450)   

#Q2: 

media <- df$Pont_Media %>% mean 
 
variancia <- df$Pont_Media %>% var 

desvio_p <- df$Pont_Media %>% sd

#Q3:

cova <- cov(df$Num_de_Alunos, df$Pont_Media)

corr <- cor(df$Num_de_Alunos, df$Pont_Media)

#Q4: 

reg <- lm(df$Pont_Media ~ df$Num_de_Alunos)

#Q5: 

grafico_2 <- ggplot(df, aes(x = Num_de_Alunos , y = Pont_Media)) +
        geom_point() + 
        ggtitle("Gráfico 2") +
        geom_line() +
        xlab("Número de Alunos") +
        ylab("Pontuação Média") +
        xlim(10, 40) +
        ylim(200, 450) +
        geom_smooth(method = "lm")

#Q6: 

r_quadrado <- round(summary(reg)$r.squared, 2)

#Q7: 

est_da_reg <- summary(reg)
coef_estimados <- tibble("Coeficientes" = c("Intercepto", "Inclinação"),
                         "Valor dos Coef" = est_da_reg$coefficients[c(1:2)],
                         "R-quadrado" = est_da_reg$r.squared,
                         "Erro-padrão" = est_da_reg$coefficients[c(3:4)],
                         "T-Valor" = est_da_reg$coefficients[c(5:6)],
                         "P_Valor" = est_da_reg$coefficients[c(7:8)],
                         )

#Q8:

matriz_cov <- est_da_reg$cov.unscaled

#Q9: 

coef_estimados[1,]

#Q10 


#Q11: 

reg_sem_intercepto <- lm(df$Pont_Media ~ df$Num_de_Alunos - 1)
plot(reg_sem_intercepto)

grafico_2 <- ggplot(df, aes(x = Num_de_Alunos , y = Pont_Media)) +
        geom_point() + 
        ggtitle("Gráfico 2") +
        geom_line() +
        xlab("Número de Alunos") +
        ylab("Pontuação Média") +
        xlim(10, 40) +
        ylim(200, 450) +
        geom_smooth(method = "lm") +
        geom_abline(slope = est_coef_estimados2$coefficients[1] , intercept = 0, color = "red")

print(grafico_2)
coef_estimados2[c(7,8)]

reg_sem_intercepto <- lm(df$Pont_Media ~ df$Num_de_Alunos - 1)

print(summary(reg_sem_intercepto))

#Q12: 
        

est_coef_estimados2 <- summary(reg_sem_intercepto)

coef_estimados2 <- tibble("Coeficientes" = c("Intercepto", "Inclinação"),
                          "Valor dos Coeficientes" = est_coef_estimados2$coefficients[c(1:2)],
                          "R-quadrado" = est_coef_estimados2$r.squared,
                          "Erro-padrão" = est_coef_estimados2$coefficients[c(3:4)],
                          "T-Valor" = est_coef_estimados2$coefficients[c(5:6)],
                          "P-Valor" = est_coef_estimados2$coefficients[c(7:8)]
)

coef_estimados2[2]

grafico_2 <- ggplot(df, aes(x = Num_de_Alunos , y = Pont_Media)) +
        geom_point() + 
        ggtitle("Gráfico 2") +
        geom_line() +
        xlab("Número de Alunos") +
        ylab("Pontuação Média") +
        xlim(0, 40) +
        ylim(0, 450) +
        geom_smooth(method = "lm") +
        geom_abline(slope = est_coef_estimados2$coefficients[1] , intercept = 0, color = "red")

print(grafico_2)


