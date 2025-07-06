pacman::p_load(readxl, tidyverse, stringr, stringi, dplyr, writexl)

# carregando dados
municipal_socioeco <- read_excel("tabela_municipal_consolidada.csv")

estadual_socioeco <- read_excel("tabela_estadual_consolidada.csv")

estadual_socioeco <- dplyr::rename(estadual_socioeco, 
                                   PIB_per_capita = Renda_Per_capita_mensal)

# dados_PIB_EST_PC <- read_excel("dados_PIB_EST_PC.xlsx")
# 
# dados_PIB_EST_PC <- dados_PIB_EST_PC %>%
#   select(UF_SIGLA, PIB_EST_PC)
# 
# estadual_socioeco <- estadual_socioeco %>%
#   left_join(dados_PIB_EST_PC, by = "UF_SIGLA")

# padronizar municipios
padronizar_mun <- function(nome) {
  nome %>%
    str_to_lower() %>%                             # tudo minúsculo
    str_remove("\\s*\\([^\\)]+\\)") %>%            # remove (UF) se houver
    str_squish() %>%                               # remove espaços extras
    str_replace_all("[\u00A0\t]", " ") %>%         # remove espaços invisíveis
    stri_trans_general("Latin-ASCII")              # remove acentos
}

municipal_socioeco <- municipal_socioeco %>%
  mutate(municipio_pad = padronizar_mun(Município))


# temas ----
temas_final <- temas_final %>%
  mutate(municipio_pad = padronizar_mun(municipio))

temas_final <- temas_final %>%
  left_join(
    municipal_socioeco %>%
      select(municipio_pad, IDHM, PIB_per_capita),
    by = "municipio_pad"
  )


temas_final <- temas_final %>%
  left_join(
    estadual_socioeco %>%
      select(UF_SIGLA, IDHM, PIB_per_capita, GINI, IVS),
    by = c("uf" = "UF_SIGLA")
  ) %>%
  mutate(
    IDHM = coalesce(IDHM.y, IDHM.x),
    PIB_per_capita = coalesce(PIB_per_capita.y, PIB_per_capita.x)
  ) %>%
  select(
    -IDHM.x, -IDHM.y,
    -PIB_per_capita.x, -PIB_per_capita.y
  )


temas_final <- temas_final %>%
  left_join(ind_socio_reg, by = c("nivel", "regiao")) %>% 
  mutate(
    IDHM = coalesce(IDHM.y, IDHM.x),
    IVS = coalesce(IVS.y, IVS.x),
    GINI = coalesce(GINI.y,GINI.x),
    PIB_per_capita = coalesce(PIB_per_capita.y, PIB_per_capita.x)
  ) %>%
  select(
    -IDHM.x, -IDHM.y,
    -IVS.y, -IVS.x,
    -GINI.y, -GINI.x,
    -PIB_per_capita.y, -PIB_per_capita.x
  )

write_xlsx(temas_final, "temas_final.xlsx")













# praticas ----
praticas_final <- praticas_final %>%
  mutate(municipio_pad = padronizar_mun(municipio))

praticas_final <- praticas_final %>%
  mutate(UF_SIGLA = case_when(
    UF == "ACRE" ~ "AC",
    UF == "ALAGOAS" ~ "AL",
    UF == "AMAPA" ~ "AP",
    UF == "AMAZONAS" ~ "AM",
    UF == "BAHIA" ~ "BA",
    UF == "CEARA" ~ "CE",
    UF == "DISTRITO FEDERAL" ~ "DF",
    UF == "ESPIRITO SANTO" ~ "ES",
    UF == "GOIAS" ~ "GO",
    UF == "MARANHAO" ~ "MA",
    UF == "MATO GROSSO" ~ "MT",
    UF == "MATO GROSSO DO SUL" ~ "MS",
    UF == "MINAS GERAIS" ~ "MG",
    UF == "PARA" ~ "PA",
    UF == "PARAIBA" ~ "PB",
    UF == "PARANA" ~ "PR",
    UF == "PERNAMBUCO" ~ "PE",
    UF == "PIAUI" ~ "PI",
    UF == "RIO DE JANEIRO" ~ "RJ",
    UF == "RIO GRANDE DO NORTE" ~ "RN",
    UF == "RIO GRANDE DO SUL" ~ "RS",
    UF == "RONDONIA" ~ "RO",
    UF == "RORAIMA" ~ "RR",
    UF == "SANTA CATARINA" ~ "SC",
    UF == "SAO PAULO" ~ "SP",
    UF == "SERGIPE" ~ "SE",
    UF == "TOCANTINS" ~ "TO",
    TRUE ~ NA_character_
  ))

praticas_final <- praticas_final %>%
  left_join(
    municipal_socioeco %>%
      select(municipio_pad, IDHM, PIB_per_capita),
    by = "municipio_pad"
  )


praticas_final <- praticas_final %>%
  left_join(
    estadual_socioeco %>%
      select(UF_SIGLA, IDHM, PIB_per_capita, GINI, IVS),
    by = "UF_SIGLA"
  ) %>%
  mutate(
    IDHM = coalesce(IDHM.y, IDHM.x),
    PIB_per_capita = coalesce(PIB_per_capita.y, PIB_per_capita.x)
  ) %>%
  select(
    -IDHM.x, -IDHM.y,
    -PIB_per_capita.x, -PIB_per_capita.y
  )


praticas_final <- praticas_final %>%
  left_join(ind_socio_reg, by = c("nivel", c("Região"="regiao"))) %>% 
  mutate(
    IDHM = coalesce(IDHM.y, IDHM.x),
    IVS = coalesce(IVS.y, IVS.x),
    GINI = coalesce(GINI.y,GINI.x),
    PIB_per_capita = coalesce(PIB_per_capita.y, PIB_per_capita.x)
  ) %>%
  select(
    -IDHM.x, -IDHM.y,
    -IVS.y, -IVS.x,
    -GINI.y, -GINI.x,
    -PIB_per_capita.y, -PIB_per_capita.x
  )

write_xlsx(praticas_final, "praticas_final.xlsx")

























