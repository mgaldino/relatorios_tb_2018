## Relatório Controle - Lei LINDB

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(janitor)

## importando dados de improbidade administrativa
# Fonte
# http://www.cnj.jus.br/improbidade_adm/relatorioQuantitativoCondenacoes.php

setwd("C:\\Users\\mgaldino\\Downloads")
ajuste_inicio_ano <- as.numeric(Sys.Date() - as.Date("2018-01-01"))

improb <- read_xlsx("dados_improb_v2_2.xlsx") %>%
  mutate(ano = as.Date(as.character(ano), '%Y') - ajuste_inicio_ano) #3 115 dias se passaram no ano até hoje (26/04)

## Figura 1 - condenações por improbidade administrativa
p_cond1 <- improb %>%
  slice(-1) %>%
  mutate(condenacoes = federal + estadual) %>%
  ggplot(aes(ano, condenacoes)) + geom_line() + 
  scale_y_continuous(limits= c(0, 2500)) +
  scale_x_date() + theme_bw() + xlab("") + ylab("Condenações")

# salvando figura 1
setwd("C:\\Users\\mgaldino\\2018\\Geral TB\\PLs")
ggsave(plot=p_cond1, filename="condenacoes_totais.png", 
       width = 20, height = 10, scale=.2)

# Figura 2 - contas irregulares

# importando dados dos tribunais de contas
# fonte: https://contas.tcu.gov.br/cadiconWeb/index.html

setwd("C:\\Users\\mgaldino\\2018\\Geral TB\\PLs\\tribunal-contas-uniao-cadicon")
tcs <- fread("tribunal-contas-uniao-cadicon_v2.csv") %>%
  mutate(DATA_TRANSITO_JULGADO = as.Date(DATA_TRANSITO_JULGADO, "%d/%m/%Y"))


p_contas <- tcs %>%
  mutate(ano = format(DATA_TRANSITO_JULGADO, "%Y")) %>%
  group_by(ano, NOME_TRIBUNAL) %>%
  summarise(data = min(DATA_TRANSITO_JULGADO),
            num_processos = n()) %>%
  filter(data > "1992-01-01" & data < "2018-01-01") %>%
  ggplot(aes(data, num_processos)) + geom_line() + 
  scale_y_continuous() + facet_wrap( ~ NOME_TRIBUNAL, scales = "free") +
  scale_x_date() + theme_bw() + xlab("") + ylab("Condenações")

# salvando figura 2  
setwd("C:\\Users\\mgaldino\\2018\\Geral TB\\PLs")
ggsave(plot=p_contas, filename="condenacoes_tcs.png", 
       width = 20, height = 10, scale=.5)

# figura 3
# Dados do Fiscobras
setwd("C:\\Users\\mgaldino\\2018\\Geral TB\\PLs")

fiscobras2 <- fread("fiscobras2.csv")

p_fiscobras <- fiscobras2 %>%
  clean_names() %>%
  mutate(data = as.Date(as.character(ano), '%Y') - 116,
         perc = irregularidades_graves/auditorias) %>%
  ggplot(aes(data, perc)) + geom_line() + geom_point() +
  scale_y_continuous(limits=c(0,.5), labels = scales::percent) +
  scale_x_date() + theme_bw() + xlab("") + ylab("Irregularidades graves")

# salvando
ggsave(plot=p_fiscobras, filename="fiscobras.png", 
       width = 20, height = 10, scale=.3)
