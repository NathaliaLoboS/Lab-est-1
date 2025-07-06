
require(readxl)
library(openxlsx)
library(tidyverse)
library(plyr)

banco_temas <- read_excel("taxas_temas_municipio_dash.xlsx")
banco_atividades <- read_excel("taxas_atividades_municipio_dash.xlsx")
banco_tudo <- read_excel("Dashboard_tudo.xlsx")

pop_cadastrada_2023 <- read_excel("~/Unb/LabEst/Lab-est-1/DadosAtividadesePop/grupos 1 e 2 População Cadastrada/pop_cadastrada_2023.xls", 
                                       sheet = "BRASIL")
Bfa_porMunicipio <- read_excel("~/Unb/LabEst/Lab-est-1/DadosAtividadesePop/Bolsa Família/Bfa_Consolidado-geral-22023-porMunicipio.xlsx", 
                               sheet = "Bfa_Consolidado-geral-22023-por", 
                               skip = 7)



Bfa_porMunicipio <- Bfa_porMunicipio[c(2,3,4,5,6,7)]

Bfa_pop <- merge(pop_cadastrada_2023, Bfa_porMunicipio, by = "IBGE")
Bfa_pop <- Bfa_pop[c(3,5,9,11,12,13)]


Bfa_reg <- Bfa_pop %>%
  group_by(Região) %>%
  summarize(tot_acomp = sum(`Qtd. beneficiários a serem acompanhados`),
            acomp = sum(`Qtd. beneficiários acompanhados`),
            perc_acomp = acomp/tot_acomp)

Bfa_reg <- Bfa_reg %>%
  mutate(so_regiao = `Região`)

Bfa_Br <- Bfa_pop %>%
  summarize(tot_acomp = sum(`Qtd. beneficiários a serem acompanhados`),
            acomp = sum(`Qtd. beneficiários acompanhados`),
            perc_acomp = acomp/tot_acomp) %>%
  cbind(Pais = "Brasil")


Bfa_est <- Bfa_pop %>%
  group_by(Estado) %>%
  summarize(tot_acomp = sum(`Qtd. beneficiários a serem acompanhados`),
            acomp = sum(`Qtd. beneficiários acompanhados`),
            perc_acomp = acomp/tot_acomp)

Bfa_est <- Bfa_est %>%
  mutate(so_estado = Estado)

Bfa_pop <- Bfa_pop %>%
  rename('tot_acomp' = 'Qtd. beneficiários a serem acompanhados',
         'acomp' = 'Qtd. beneficiários acompanhados',
         "perc_acomp" = 'Perc. cobertura de beneficiários acompanhados (%)')


Bfa_todos <-  rbind.fill(Bfa_pop,Bfa_est,Bfa_reg,Bfa_Br)



