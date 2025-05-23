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

## Indice de desenvolvimento municipal - IDHM ----
search_series("IDHM")  # series relacionadas
print(search_series("IDHM"), n=38)

IDHM <- ipeadata("IDHM", language = "br") %>% 
  filter(year(date) == 2021)
write_csv(IDHM, "DadosAtividadesePop/Socioeconomico/IDHM.csv")
# ultimo ano foi 2021
# Brasil e estados


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
















