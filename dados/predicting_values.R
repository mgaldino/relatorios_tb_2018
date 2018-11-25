
library(rstanarm)
options(mc.cores = parallel::detectCores())
setwd("C:/Users/mczfe/Transparencia Brasil/Projetos/Ta de Pe/Report campanha emails/relatorios_tb_2018/dados")
load("convenio_mais_de_uma_obra.Rdata")
load("convenio_uma_obra.Rdata")

library(dplyr)
glimpse(convenio_uma_obra)

convenio_uma_obra1 <- convenio_uma_obra %>%
  filter(ano_convenio != "-") %>%
  filter(tipo_do_projeto != "Escola Infantil - Tipo B (Projeto Novo)") %>%
  filter(tipo_do_projeto != "Escola 06 Salas com Quadra - Projeto FNDE") %>%
  ungroup() %>%
  mutate(percentual_de_execucao = as.numeric(percentual_de_execucao),
         pagto_total_cte_nov2018 = as.numeric(pagto_total_cte_nov2018)) %>%
  select(percentual_de_execucao, ano_convenio, situacao_segundo_tbrasil, tipo_do_projeto,
         status_segundo_simec, pagto_total_cte_nov2018) %>%
  mutate_if(sapply(., is.character), as.factor)

convenio_mais_de_uma_obra1 <- convenio_mais_de_uma_obra %>%
  ungroup() %>%
  filter(ano_convenio != "-") %>%
  filter(ano_convenio != "") %>%  
  mutate(percentual_de_execucao = as.numeric(percentual_de_execucao),
         pagto_total_cte_nov2018 = as.numeric(pagto_total_cte_nov2018)) %>%
  select(percentual_de_execucao, ano_convenio, situacao_segundo_tbrasil, tipo_do_projeto,
         status_segundo_simec, pagto_total_cte_nov2018) %>%
  mutate_if(sapply(., is.character), as.factor)
  
table(convenio_mais_de_uma_obra1$status_segundo_simec)
table(convenio_uma_obra1$status_segundo_simec)

table(convenio_mais_de_uma_obra1$ano_convenio)
table(convenio_uma_obra1$ano_convenio)

summary(convenio_mais_de_uma_obra1$tipo_do_projeto)
summary(convenio_uma_obra1$tipo_do_projeto)

reg <- lm(pagto_total_cte_nov2018 ~ ano_convenio + status_segundo_simec +
            percentual_de_execucao + tipo_do_projeto, data=convenio_uma_obra1)

summary(reg)
round(coef(reg), 3)

#3 bayesiano
post <- stan_lm(pagto_total_cte_nov2018 ~ ano_convenio + status_segundo_simec +
                  percentual_de_execucao + tipo_do_projeto, data=convenio_uma_obra1,
                prior = R2(location = 0.4), 
                chains = 2, cores = 4, seed = 2)
post
convenio_mais_de_uma_obra2 <- convenio_mais_de_uma_obra1 %>%
  select(-pagto_total_cte_nov2018)

y1_predict <- posterior_predict(post, newdata = convenio_mais_de_uma_obra1)
