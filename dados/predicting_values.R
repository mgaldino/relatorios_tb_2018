
library(rstanarm)
library(bayesplot)
options(mc.cores = parallel::detectCores())
setwd("C:/Users/mczfe/Transparencia Brasil/Projetos/Ta de Pe/Report campanha emails/relatorios_tb_2018/dados")
load("convenio_mais_de_uma_obra.Rdata")
load("convenio_uma_obra.Rdata")

library(dplyr)
glimpse(convenio_uma_obra)
convenio_uma_obra <- convenio_uma_obra %>%
  mutate(pagto_total_cte_nov2018 = as.numeric(pagto_total_cte_nov2018))

convenio_uma_obra1 <- convenio_uma_obra %>%
  filter(pagto_total_cte_nov2018 < 12000000) %>%
  filter(!is.na(pagto_total_cte_nov2018)) %>%
  filter(ano_convenio != "-") %>%
  filter(tipo_do_projeto != "Escola Infantil - Tipo B (Projeto Novo)") %>%
  filter(tipo_do_projeto != "Escola 06 Salas com Quadra - Projeto FNDE") %>%
  ungroup() %>%
  mutate(percentual_de_execucao = as.numeric(percentual_de_execucao),
         pagto_total_cte_nov2018 = as.numeric(pagto_total_cte_nov2018)) %>%
  select(id, percentual_de_execucao, ano_convenio, situacao_segundo_tbrasil, tipo_do_projeto,
         status_segundo_simec, pagto_total_cte_nov2018) %>%
  mutate_if(sapply(., is.character), as.factor) %>%
  mutate(id = as.character(id))

convenio_mais_de_uma_obra1 <- convenio_mais_de_uma_obra %>%
  ungroup() %>%
  filter(ano_convenio != "-") %>%
  filter(ano_convenio != "") %>%  
  filter(tipo_do_projeto != "Construção") %>%
  mutate(percentual_de_execucao = as.numeric(percentual_de_execucao),
         pagto_total_cte_nov2018 = as.numeric(pagto_total_cte_nov2018)) %>%
  select(id, percentual_de_execucao, ano_convenio, situacao_segundo_tbrasil, tipo_do_projeto,
         status_segundo_simec, pagto_total_cte_nov2018) %>%
  mutate_if(sapply(., is.character), as.factor)  %>%
  mutate(id = as.character(id))
  
reg <- lm(pagto_total_cte_nov2018 ~ ano_convenio + status_segundo_simec +
            percentual_de_execucao + tipo_do_projeto, data=convenio_uma_obra1)


#3 bayesiano
# post <- stan_lm(pagto_total_cte_nov2018 ~ ano_convenio + status_segundo_simec +
#                   percentual_de_execucao + tipo_do_projeto, data=convenio_uma_obra1,
#                 prior = R2(location = 0.6), 
#                 chains = 2, cores = 4, seed = 2)
# post

post_log <- stan_lm(log(pagto_total_cte_nov2018) ~ ano_convenio + status_segundo_simec +
                  percentual_de_execucao + tipo_do_projeto, data=convenio_uma_obra1,
                prior = R2(location = 0.6), 
                chains = 2, cores = 4, seed = 2)
post_log

convenio_mais_de_uma_obra2 <- convenio_mais_de_uma_obra1 %>%
  select(-pagto_total_cte_nov2018)

## posterior predictive checks
color_scheme_set("brightblue")

# vriando vetor com vd
y <- convenio_uma_obra1$pagto_total_cte_nov2018
y_log <- log(y)
yrep_log <- posterior_predict(post_log, draws = 500)

ppc_dens_overlay(y_log, yrep_log[1:50, ])

y1_predict <- posterior_predict(post_log, newdata = convenio_mais_de_uma_obra2)
y1_predict <- exp(y1_predict)


prev_media <- colMeans(y1_predict)
n <- ncol(y1_predict)
sd_prev <- numeric()
for ( i in 1:n) {
  sd_prev[i] <- sd(y1_predict[, i])
}

result <- data.frame(prev_media = prev_media,
                     prev_sd = sd_prev,
                     id = convenio_mais_de_uma_obra2$id)

repasse_total <- rowSums(y1_predict)

hist(repasse_total/n)
mean(repasse_total/n)
sd(repasse_total/n)

sum(convenio_uma_obra1$pagto_total_cte_nov2018)/nrow(convenio_uma_obra1)

convenio_mais_de_uma_obra <- convenio_mais_de_uma_obra %>%
  left_join(result, by="id") %>%
  rename(estimativa_repasse = prev_media,
         margem_erro_estimativa = prev_sd) %>%
  mutate(margem_erro_estimativa = 2*margem_erro_estimativa)

summary(convenio_mais_de_uma_obra$estimativa_repasse)
save(convenio_mais_de_uma_obra, file="convenio_mais_de_uma_obra_com_estimativa.Rdata")
