library(openxlsx)
library(xlsx)
library(tidyverse)
library(ggplot2)
library(sf)

pop_cad_2023 <- read.xlsx('C:/Users/helio/OneDrive/Documents/UNB/Laboratório de Estatística/DadosAtividadesePop/grupos 1 e 2 População Cadastrada/pop_cadastrada_2023.xlsx', sheetName = 'BRASIL')

prom_saude_2023 <- read.xlsx('C:/Users/helio/OneDrive/Documents/UNB/Laboratório de Estatística/DadosAtividadesePop/grupo 2 Práticas para a Saúde/praticas2023.xlsx', sheetName = 'Dados', startRow = 12)

prom_saude_2023 <- prom_saude_2023 %>%
  rename('IBGE' = 'Ibge',
        'Praticas_atividades_corporais' = 'Práticas.corporais...atividade')

'glimpse(prom_saude_2023)'

pop_prom_saude_2023 <- merge(pop_cad_2023, prom_saude_2023, by = "IBGE")

pop_prom_saude_2023 <- pop_prom_saude_2023 %>%
  mutate(across(
    c(
      Antropometria,
      Aplicação.tópica.de.flúor,
      Desenvolvimento.da.linguagem,
      Escovação.dental.supervisionad,
      Outro.procedimento.coletivo,
      Programa.nacional.de.controle.,
      Saúde.auditiva,
      Saúde.ocular,
      Verificação.da.situação.vacina,
      População
    ),
    as.numeric
  ))


#pop_prom_saude_2023$Verificação.da.situação.vacina
  
ind_prom_saude <- pop_prom_saude_2023 %>%
  mutate(ind_Antropometria = Antropometria / População,
            ind_APlic_top_fluor = Aplicação.tópica.de.flúor / População,
            ind_desenv_ling = Desenvolvimento.da.linguagem / População,
            ind_escov_dent_superv = Escovação.dental.supervisionad / População,
            ind_Outr_proced_col = Outro.procedimento.coletivo / População,
            ind_prog_contr_nac = Programa.nacional.de.controle. / População,
            ind_saude_aud = Saúde.auditiva / População,
            ind_saude_ocular = Saúde.ocular / População,
            ind_verif_sit_vacina = Verificação.da.situação.vacina / População) %>%
  select(Uf, IBGE, Municipio, ind_Antropometria, ind_APlic_top_fluor,ind_desenv_ling,
         ind_escov_dent_superv, ind_Outr_proced_col, ind_prog_contr_nac, ind_saude_aud,
         ind_saude_ocular, ind_verif_sit_vacina)

ind_prom_saude_mapa <- mapa_municipios %>%
  left_join(ind_prom_saude, by = "code_muni")

ind_prom_saude <- ind_prom_saude %>%
  rename(code_muni = IBGE)

ggplot(ind_prom_saude_mapa) +
  geom_sf(aes(fill = Uf), color = NA) +
  scale_fill_manual(values = c("#CC0033", "#FF9966", "#FFFF00", "#33CC33", "#99CCFF", "#3300CC")) +
  labs(
    title = "Indicador", 
    caption = "IPEADATA"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )