# carregando pacotes
pacman::p_load(tidyverse, ipeadatar, readr)


# Indicadores socioeconomicos e desigualdade social ----

## Indice de vulnerabilidade social - IVS ----
search_series("Vulnerabilidade Social") # series relacionadas

IVS <- ipeadata("AVS_IVS", language = "br") %>% 
  filter(date == max(date))
write_csv(IVS, "DadosAtividadesePop/Socioeconomico/IVS.csv")
# ultimo ano foi 2022
# Brasil, regiao e estado

IVS <- ipeadata("AVS_IVS", language = "br") %>%
  filter(date == max(date)) %>%
  filter(uname %in% c("Brasil", "Regiões")) %>%
  mutate(
    regiao = case_when(
      tcode == 1 ~ "NORTE",
      tcode == 2 ~ "NORDESTE",
      tcode == 3 ~ "SUDESTE",
      tcode == 4 ~ "SUL",
      tcode == 5 ~ "CENTRO-OESTE"
    )
  ) %>% 
  select(nivel = uname, IVS = value, regiao)


## Indice de desenvolvimento municipal - IDHM ----
search_series("IDHM")  # series relacionadas
print(search_series("IDHM"), n=38)

IDHM <- ipeadata("IDHM", language = "br") %>% 
  filter(year(date) == 2021)
write_csv(IDHM, "DadosAtividadesePop/Socioeconomico/IDHM.csv")
# ultimo ano foi 2021
# Brasil e estados

IDHM <- ipeadata("IDHM", language = "br") %>% 
  filter(year(date) == 2021) %>%
  filter(uname %in% c("Brasil")) %>% 
  select(nivel = uname, IDHM = value)

## Produto interno bruto - PIB per capita ----
search_series("PIB") # series relacionadas
teste <- print(search_series("PIB"), n=170)

# brasil - 2024
PIB_B <- ipeadata("GAC_PIBCAPN", language = 'br') %>% 
  filter(date == max(date))
PIB_B$uname <- "Brasil"
PIB_B$tcode <- 0
# estadual - 2021
PIB_est <- ipeadata("PIBPCE", language = 'br') %>% 
  filter(date == max(date))

PIB <- rbind(PIB_B, PIB_est)
write_csv(PIB, "DadosAtividadesePop/Socioeconomico/PIB.csv")


## Indice de GINI ----
search_series("Gini") # series relacionadas

# brasil, regiao e municipio - 2023
GINI <- ipeadata("PNADCA_GINIUF", language = 'br') %>% 
  filter(date == max(date))
write_csv(GINI, "DadosAtividadesePop/Socioeconomico/GINI.csv")


GINI <- ipeadata("PNADCA_GINIUF", language = 'br') %>% 
  filter(date == max(date)) %>%
  filter(uname %in% c("Brasil", "Regiões")) %>%
  mutate(
    regiao = case_when(
      tcode == 1 ~ "NORTE",
      tcode == 2 ~ "NORDESTE",
      tcode == 3 ~ "SUDESTE",
      tcode == 4 ~ "SUL",
      tcode == 5 ~ "CENTRO-OESTE"
    )
  ) %>% 
  select(nivel = uname, GINI = value, regiao)

############################
PIB_per_capita <- data.frame(
  nivel = "Brasil",
  PIB_per_capita = 47802.02,
  stringsAsFactors = FALSE
)


ind_socio_reg <- IVS %>%
  mutate(nivel = as.character(nivel)) %>%
  left_join(GINI %>% mutate(nivel = as.character(nivel)), by = c("nivel", "regiao")) %>%
  left_join(IDHM %>% mutate(nivel = as.character(nivel)), by = "nivel") %>%
  left_join(PIB_per_capita %>% mutate(nivel = as.character(nivel)), by = "nivel")

ind_socio_reg <- ind_socio_reg %>%
  mutate(nivel = case_when(
    nivel == "Regiões" ~ "regiao",
    nivel == "Brasil" ~ "Brasil"
  ))




# Teste
# Pacotes
pacman::p_load(tidyverse, ipeadatar, geobr, sf, ggplot2, viridis)

# TEMA
tema_padrao <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.caption = element_text(hjust = 0.5, size = 9),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
}

# MAPA
mapa_padrao <- function(data, titulo, legenda, paleta, limites = NULL, fonte = "Fonte: IPEA") {
  ggplot(data) +
    geom_sf(aes(fill = value), color = "white", size = 0.2) +
    geom_text(aes(x = coord_x, y = coord_y, 
                  label = format(round(value, 3), nsmall = 3)),
              color = "white", size = 2.5, fontface = "bold") +
    scale_fill_viridis_c(name = legenda, option = paleta, direction = -1, limits = limites) +
    labs(title = titulo, caption = fonte) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

# DADOS DO IPEA
IVS <- ipeadata("AVS_IVS", language = "br") %>% filter(date == max(date))
GINI <- ipeadata("PNADCA_GINIUF", language = "br") %>% filter(date == max(date))
IDHM <- ipeadata("IDHM", language = "br") %>% filter(date == max(date))
PIB <- ipeadata("PIBPMCE", language = "br") %>% filter(date == max(date))


# Criar tabela de estados
ufs <- data.frame(
  tcode = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27,
            28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53),
  estado = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
             "MA", "PI", "CE", "RN", "PB", "PE", "AL",
             "SE", "BA", "MG", "ES", "RJ", "SP",
             "PR", "SC", "RS", "MS", "MT", "GO", "DF"))

# Criar tabela de regiões
regioes <- data.frame(
  tcode = c(1, 2, 3, 4, 5),
  regiao = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
)

# Criar tabela unificada de nomes
lookup_nomes <- bind_rows(
  data.frame(tcode = 0, nome = "Brasil"),          # Código do Brasil
  regioes %>% rename(nome = regiao),               # Regiões
  ufs %>% rename(nome = estado)                    # Estados
)

# Tratamento dos dados
tratamento <- function(df) {
  df %>% 
    left_join(lookup_nomes, by = "tcode") %>% 
    select(tcode, nome, value) %>% 
    filter(tcode > 5)
}

dados_IVS  <- tratamento(IVS)
dados_GINI <- tratamento(GINI)
dados_IDHM <- tratamento(IDHM)
dados_PIB  <- tratamento(PIB)

# Mapa de calor
heatmap_padrao <- function(dados, titulo, legenda) {
  ggplot(dados, aes(x = reorder(nome, -value), y = legenda, fill = value)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(aes(label = format(round(value, 3), nsmall = 3)), color = "white", size = 3) +
    scale_fill_viridis_c(option = "magma", direction = -1, name = legenda) +
    labs(title = titulo, x = "", y = "") +
    tema_padrao()
}

heatmap_padrao(dados_IVS, "Índice de Vulnerabilidade Social por Estado - 2022", "IVS")
heatmap_padrao(dados_GINI, "Índice de Gini por Estado - 2023", "GINI")
heatmap_padrao(dados_IDHM, "IDHM por Estado - 2021", "IDHM")

# Mapas e shapes
shape_estados <- read_state(code_state = "all", year = 2020)

prep_mapa <- function(dados) {
  shape_estados %>%
    left_join(dados, by = c("abbrev_state" = "nome")) %>%
    mutate(
      centroid = sf::st_centroid(geom),
      coord_x = sf::st_coordinates(centroid)[, 1],
      coord_y = sf::st_coordinates(centroid)[, 2]
    )
}

# Mapas
# IVS
estados_sf_IVS <- prep_mapa(dados_IVS)
mapa_padrao(estados_sf_IVS, "IVS por Estado - 2022", "IVS", "magma")

# GINI
estados_sf_GINI <- prep_mapa(dados_GINI)
mapa_padrao(estados_sf_GINI, "Índice de Gini por Estado - 2023", "GINI", "viridis", limites = c(0.4, 0.56))

# IDHM
estados_sf_IDHM <- prep_mapa(dados_IDHM)
mapa_padrao(estados_sf_IDHM, "IDHM por Estado - 2021", "IDHM", "viridis", limites = c(0.65, 0.85))

# PIB
mapa_padrao_pib <- function(data, titulo, legenda, paleta, limites = NULL, fonte = "Fonte: IPEA") {
  ggplot(data) +
    geom_sf(aes(fill = value), color = "white", size = 0.2) +
    geom_text(aes(x = coord_x, y = coord_y, 
                  label = format(round(value / 1e9, 1), big.mark = ".", decimal.mark = ",")
    ),
    color = "black", size = 2.8, fontface = "bold") +
    scale_fill_viridis_c(name = legenda, option = paleta, direction = -1,
                         limits = limites,
                         labels = scales::number_format(scale = 1e-9, suffix = " B", big.mark = ".", decimal.mark = ",")) +
    labs(title = titulo, caption = fonte) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

# Aplicar aos dados do PIB
estados_sf_PIB <- prep_mapa(dados_PIB)

# Plotar mapa padronizado do PIB
mapa_padrao_pib(
  estados_sf_PIB,
  titulo = "PIB por Estado - 2021",
  legenda = "PIB (R$ bilhões)",
  paleta = "plasma"
)
















# Carregar pacotes necessários
pacman::p_load(tidyverse, ipeadatar, ggcorrplot)


# Baixar e tratar os indicadores


# IVS
IVS <- ipeadata("AVS_IVS", language = "br") %>% filter(date == max(date)) %>%
  select(tcode, value_IVS = value)

# IDHM
IDHM <- ipeadata("IDHM", language = "br") %>% filter(lubridate::year(date) == 2021) %>%
  select(tcode, value_IDHM = value)

# GINI
GINI <- ipeadata("PNADCA_GINIUF", language = "br") %>% filter(date == max(date)) %>%
  select(tcode, value_GINI = value)

# PIB per capita estadual
PIB <- ipeadata("PIBPCE", language = 'br') %>% filter(date == max(date)) %>%
  select(tcode, value_PIB = value)


# Adicionar nomes de estados e regiões


# Estados e códigos
ufs <- tibble(
  tcode = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27,
            28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53),
  estado = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
             "MA", "PI", "CE", "RN", "PB", "PE", "AL",
             "SE", "BA", "MG", "ES", "RJ", "SP",
             "PR", "SC", "RS", "MS", "MT", "GO", "DF"),
  regiao = c(rep("Norte",7), rep("Nordeste",9), rep("Sudeste",4), 
             rep("Sul",3), rep("Centro-Oeste",4))
)


# Juntar os indicadores em uma base unificada


dados <- ufs %>%
  left_join(IVS,  by = "tcode") %>%
  left_join(IDHM, by = "tcode") %>%
  left_join(GINI, by = "tcode") %>%
  left_join(PIB,  by = "tcode")


#  Correlograma dos indicadores por região


# Para cada região, fazer o correlograma entre IVS, IDHM, GINI e PIB
for (reg in unique(dados$regiao)) {
  df_reg <- dados %>%
    filter(regiao == reg) %>%
    select(value_IVS, value_IDHM, value_GINI, value_PIB) %>%
    na.omit()
  
  if (nrow(df_reg) >= 3) {
    mat_cor <- cor(df_reg, use = "pairwise.complete.obs")
    p <- ggcorrplot(mat_cor, lab = TRUE, type = "lower", title = paste("Correlograma -", reg))
    print(p)  # Mostra no Plots
    
    # Salva o arquivo PNG
    ggsave(filename = paste0("correlograma_", reg, ".png"), plot = p, width = 6, height = 5)
  }
}










# Junta todos os bancos do IPEA
pacman::p_load(tidyverse, ipeadatar)
dados_ivs <- ipeadata("AVS_IVS", language = "br") %>%
  filter(date == max(date)) %>%
  select(tcode, IVS = value)
dados_idhm <- ipeadata("IDHM", language = "br") %>%
  filter(year(date) == 2021) %>% # Usando o ano de 2021 para garantir consistência
  select(tcode, IDHM = value)
dados_gini <- ipeadata("PNADCA_GINIUF", language = "br") %>%
  filter(date == max(date)) %>%
  select(tcode, GINI = value)
dados_pib <- ipeadata("PIBPCE", language = 'br') %>%
  filter(date == max(date)) %>%
  select(tcode, PIB_per_capita = value)
base_geografica <- tibble(
  tcode = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27,
            28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53),
  estado = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
             "MA", "PI", "CE", "RN", "PB", "PE", "AL",
             "SE", "BA", "MG", "ES", "RJ", "SP",
             "PR", "SC", "RS", "MS", "MT", "GO", "DF"),
  regiao = c(rep("Norte", 7), rep("Nordeste", 9), rep("Sudeste", 4),
             rep("Sul", 3), rep("Centro-Oeste", 4))
)
dados_consolidados <- base_geografica %>%
  left_join(dados_ivs, by = "tcode") %>%
  left_join(dados_idhm, by = "tcode") %>%
  left_join(dados_gini, by = "tcode") %>%
  left_join(dados_pib, by = "tcode")
print(dados_consolidados)
# write_csv(dados_consolidados, "indicadores_sociodemograficos_consolidados_uf.csv")









# Junta todos o bancos

# Por UF
library(dplyr)
library(purrr)
library(stringr)
lista_dados_estaduais <- list(
  dados_gini = dados_GINI,
  dados_idhm_uf = dados_IDHM,
  dados_ivs = dados_IVS,
  dados_pib_uf = dados_PIB,
  pib_per_capita_uf = PIB_PER_CAPITA,
  renda_per_capita_uf = UF_Renda_Per_Capita
)
padronizar_chave_uf <- function(dataframe, nome_original) {
  nomes_possiveis <- c("tcode", "Cod UF", "UF", "Estado")
  nome_chave <- intersect(nomes_possiveis, names(dataframe))
  
  if (length(nome_chave) > 0) {
    nome_chave <- nome_chave[1] # Usa a primeira correspondência
    cat("Ok: No dataframe '", nome_original, "', a chave '", nome_chave, "' foi encontrada.\n", sep = "")
    
    # Renomeia para 'codigo_uf' e converte para texto
    dataframe %>%
      rename(codigo_uf = all_of(nome_chave)) %>%
      mutate(codigo_uf = as.character(codigo_uf))
    
  } else {
    cat("Aviso: Nenhuma chave estadual encontrada para '", nome_original, "'. O dataframe não será modificado.\n", sep = "")
    dataframe 
  }
}
cat("\n--- Padronizando as chaves de junção ---\n")
lista_estaduais_padronizada <- imap(lista_dados_estaduais, padronizar_chave_uf)
lista_pronta_para_juncao <- keep(lista_estaduais_padronizada, ~ "codigo_uf" %in% names(.))
cat("\n--- Unificando", length(lista_pronta_para_juncao), "tabelas estaduais... ---\n")
tabela_estadual_consolidada <- reduce(
  lista_pronta_para_juncao,
  full_join,
  by = "codigo_uf"
)
cat("\n\n--- TABELA ESTADUAL CONSOLIDADA COM SUCESSO! ---\n")
cat("O resultado foi salvo no objeto 'tabela_estadual_consolidada'.\n\n")
glimpse(tabela_estadual_consolidada)

tabela_estadual_consolidada <- tabela_estadual_consolidada %>%
  select(-UF_SIGLA.y,-UF_SIGLA.x.x,-UF_SIGLA.y.y,-estado,Renda_Per_capita_mensal)

tabela_estadual_consolidada <- tabela_estadual_consolidada %>%
  select(Codigo_uf = codigo_uf,UF_SIGLA = UF_SIGLA.x, Região, UF,
         ,GINI,IDHM,IVS,PIB,Renda_Per_capita_mensal = value
  )

library(writexl)
write_xlsx(tabela_estadual_consolidada, "tabela_estadual_consolidada.xlsx")


# Por município
library(dplyr)
library(purrr)
library(stringr)
lista_dados_municipais <- list(
  idhm_municipal = IDHM,
  pib_per_capita_municipal = PIB_PER_CAPITA_Municipal
)
padronizar_chave_municipal <- function(dataframe, nome_original) {
  nomes_possiveis <- c("Cod. Município", "IBGE", "cod_mun", "Cod.IBGE")
  nome_chave <- intersect(nomes_possiveis, names(dataframe))
  if (length(nome_chave) > 0) {
    nome_chave <- nome_chave[1]
    cat("Ok: No dataframe '", nome_original, "', a chave '", nome_chave, "' foi encontrada.\n", sep = "")
    dataframe %>%
      rename(codigo_municipio_ibge = all_of(nome_chave)) %>%
      mutate(codigo_municipio_ibge = as.character(codigo_municipio_ibge))
  } else {
    cat("Aviso: Nenhuma chave municipal encontrada para '", nome_original, "'. O dataframe não será modificado.\n", sep = "")
    dataframe
  }
}

cat("\n--- Padronizando as chaves de junção ---\n")
lista_municipais_padronizada <- imap(lista_dados_municipais, padronizar_chave_municipal)

lista_pronta_para_juncao_mun <- keep(lista_municipais_padronizada, ~ "codigo_municipio_ibge" %in% names(.))

cat("\n--- Unificando", length(lista_pronta_para_juncao_mun), "tabelas municipais... ---\n")

tabela_municipal_consolidada <- reduce(
  lista_pronta_para_juncao_mun,
  full_join,
  by = "codigo_municipio_ibge"
)

cat("\n\n--- TABELA MUNICIPAL CONSOLIDADA COM SUCESSO! ---\n")
cat("O resultado foi salvo no objeto 'tabela_municipal_consolidada'.\n\n")

glimpse(tabela_municipal_consolidada)


tabela_municipal_consolidada  <- tabela_municipal_consolidada  %>%
  select(-Codigo_Ajustado,-codigo_municipio_ibge,-Município.y,-IBGE_Completo)

tabela_municipal_consolidada  <- tabela_municipal_consolidada  %>%
  select(UF_SIGLA = UF_SIGLA.x, Codigo_Original, Município = Município.x,IDHM = IDHM_2010, PIB_per_capita
  )

library(writexl)
write_xlsx(tabela_municipal_consolidada, "tabela_municipal_consolidada.xlsx")
