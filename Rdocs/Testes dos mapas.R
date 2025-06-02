# Carregando pacotes
pacman::p_load(tidyverse, ipeadatar, ggplot2, viridis, geobr, sf, corrplot, heatmaply)

# Indicadores socioeconômicos e desigualdade social - IVS ----

IVS <- ipeadata("AVS_IVS", language = "br") %>% 
  filter(date == max(date))

IDHM <- ipeadata("IDHM", language = "br") %>% 
  filter(date == max(date))

GINI <- ipeadata("PNADCA_GINIUF", language = 'br') %>% 
  filter(date == max(date))


# brasil - 2024
PIB_B <- ipeadata("SCN10_DIPIBG10", language = 'br') %>% 
  filter(date == max(date))
# municipio - teste 
PIB_mun <- ipeadata("PIB", language = 'br') %>% 
  filter(date == max(date))
# estadual - 2021
PIB_mun <- ipeadata("PIBPMCE", language = 'br') %>% 
  filter(date == max(date))


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

# Realizar a junção
df_IVS <- IVS %>%
  left_join(lookup_nomes, by = "tcode") %>% 
  select(-uname) %>%
  relocate(nome, .after = tcode)

# Realizar a junção
df_GINI <- GINI %>%
  left_join(lookup_nomes, by = "tcode") %>% 
  select(-uname) %>%
  relocate(nome, .after = tcode)

# Realizar a junção
df_IDHM <- IDHM %>%
  left_join(lookup_nomes, by = "tcode") %>% 
  select(-uname) %>%
  relocate(nome, .after = tcode)


# Filtrar apenas estados (excluir Brasil e Regiões)
dados_estados_IVS <- df_IVS %>% 
  filter(tcode > 5) # Valores acima de 5 são estados

dados_estados_GINI <- df_GINI %>% 
  filter(tcode > 5) # Valores acima de 5 são estados

dados_estados_IDHM <- df_IDHM %>% 
  filter(tcode > 5) # Valores acima de 5 são estados

# Região
dados_regiao_IVS <- df_IVS %>% 
  filter(tcode %in% 1:5)  # Seleciona apenas as regiões (códigos 1 a 5)

dados_regiao_GINI <- df_GINI %>% 
  filter(tcode %in% 1:5)  # Seleciona apenas as regiões (códigos 1 a 5)

dados_regiao_IDHM <- df_IDHM %>% 
  filter(tcode %in% 1:5)  # Seleciona apenas as regiões (códigos 1 a 5)

# Criar mapa de calor
ggplot(dados_estados_IVS, aes(x = reorder(nome, -value), y = "IVS", fill = value)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = round(value, 3)), color = "white", size = 3) +
  scale_fill_viridis(name = "Valor IVS", option = "magma", direction = -1) +
  labs(title = "Índice de Vulnerabilidade Social (IVS) por Estado - 2022",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")

ggplot(dados_estados_GINI, aes(x = reorder(nome, -value), y = "IVS", fill = value)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = round(value, 3)), color = "white", size = 3) +
  scale_fill_viridis(name = "Valor IVS", option = "magma", direction = -1) +
  labs(title = "Índice de Vulnerabilidade Social (IVS) por Estado - 2022",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")

ggplot(dados_estados_IDHM, aes(x = reorder(nome, -value), y = "IVS", fill = value)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = round(value, 3)), color = "white", size = 3) +
  scale_fill_viridis(name = "Valor IVS", option = "magma", direction = -1) +
  labs(title = "Índice de Vulnerabilidade Social (IVS) por Estado - 2022",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")

# Mapas

# IVS
# Baixar shapefile do Brasil
estados_sf <- read_state(code_state = "all", year = 2020)

# Juntar com dados
estados_sf <- estados_sf %>% 
  left_join(dados_estados_IVS, by = c("abbrev_state" = "nome"))

# Mapa coroplético
ggplot(estados_sf) +
  geom_sf(aes(fill = value), color = "white") +
  scale_fill_viridis(name = "IVS", option = "magma") +
  labs(title = "Distribuição Espacial do IVS por estado") +
  theme_void()

# GINI
# Baixar shapefile do Brasil
estados_sf <- read_state(code_state = "all", year = 2020)

# Juntar com dados
estados_sf <- estados_sf %>% 
  left_join(dados_estados_GINI, by = c("abbrev_state" = "nome"))

# Mapa coroplético
ggplot(estados_sf) +
  geom_sf(aes(fill = value), color = "white") +
  scale_fill_viridis(name = "IVS", option = "magma") +
  labs(title = "Distribuição Espacial do GINI por estado") +
  theme_void()

# IDHM
# Baixar shapefile do Brasil
estados_sf <- read_state(code_state = "all", year = 2020)

# Juntar com dados
estados_sf <- estados_sf %>% 
  left_join(dados_estados_GINI, by = c("abbrev_state" = "nome"))

# Mapa coroplético
ggplot(estados_sf) +
  geom_sf(aes(fill = value), color = "white") +
  scale_fill_viridis(name = "IVS", option = "magma") +
  labs(title = "Distribuição Espacial do IDHM por estado") +
  theme_void()


# IVS
# Carregar pacotes
pacman::p_load(geobr, sf, ggplot2, viridis, dplyr)

# 1. Baixar shapefile do Brasil
estados_sf <- geobr::read_state(code_state = "all", year = 2020)

# 2. Carregar e preparar dados (substitua pelo seu dataframe real)
dados_estados <- data.frame(
  nome = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE",
           "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP",
           "PR", "SC", "RS", "MS", "MT", "GO", "DF"),
  value = c(0.167, 0.332, 0.288, 0.225, 0.269, 0.204, 0.201, 0.289, 0.263,
            0.238, 0.252, 0.296, 0.300, 0.283, 0.280, 0.258, 0.173, 0.189,
            0.262, 0.209, 0.164, 0.113, 0.191, 0.166, 0.197, 0.217, 0.236)
)

# 3. Juntar dados ao shapefile
estados_sf <- estados_sf %>% 
  left_join(dados_estados, by = c("abbrev_state" = "nome")) %>% 
  mutate(
    centroid = sf::st_centroid(geom),
    coord_x = sf::st_coordinates(centroid)[,1],
    coord_y = sf::st_coordinates(centroid)[,2]
  )

# 4. Criar mapa
ggplot(estados_sf) +
  geom_sf(aes(fill = value), color = "white", size = 0.2) +
  geom_text(aes(x = coord_x, y = coord_y, 
                label = format(round(value, 3), nsmall = 3)),
            color = "white", 
            size = 1.6,
            fontface = "bold") +
  scale_fill_viridis(
    name = "IVS", 
    option = "viridis", 
    direction = -1,
    begin = 0.1,
    end = 0.9
  ) +
  labs(
    title = "Índice de Vulnerabilidade Social (IVS) por Estado - 2022",
    caption = "Fonte: IPEA"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5)
  )


# GINI

# Carregar pacotes
pacman::p_load(geobr, sf, ggplot2, viridis, dplyr)

# 1. Baixar shapefile do Brasil
estados_sf <- geobr::read_state(code_state = "all", year = 2020)

# 2. Preparar dados do Gini (substitua com seu dataframe real)
dados_estados <- data.frame(
  nome = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE",
           "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP",
           "PR", "SC", "RS", "MS", "MT", "GO", "DF"),
  value = c(0.455, 0.511, 0.512, 0.520, 0.501, 0.491, 0.477, 0.492, 0.552,
            0.513, 0.535, 0.559, 0.496, 0.486, 0.507, 0.490, 0.476, 0.486,
            0.540, 0.504, 0.463, 0.418, 0.466, 0.477, 0.452, 0.473, 0.543)
)

# 3. Juntar dados ao shapefile
estados_sf <- estados_sf %>% 
  left_join(dados_estados, by = c("abbrev_state" = "nome")) %>% 
  mutate(
    centroid = sf::st_centroid(geom),
    coord_x = sf::st_coordinates(centroid)[,1],
    coord_y = sf::st_coordinates(centroid)[,2]
  )

# 4. Criar mapa do Gini
ggplot(estados_sf) +
  geom_sf(aes(fill = value), color = "white", size = 0.2) +
  geom_text(aes(x = coord_x, y = coord_y, 
                label = format(round(value, 3), nsmall = 3)),
            color = "yellow", 
            size = 1.6,
            fontface = "bold") +
  scale_fill_viridis(
    name = "Índice de Gini",
    option = "viridis",
    direction = -1,
    begin = 0.1,
    end = 0.9,
    limits = c(0.4, 0.56)  # Ajuste conforme sua escala
  ) +
  labs(
    title = "Índice de Gini por Estado - 2023",
    caption = "Fonte: IPEA/PNAD Contínua"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.caption = element_text(hjust = 0.5, margin = margin(t = 10)),
    legend.key.width = unit(2, "cm")
  )


# IDHM

# Carregar pacotes
pacman::p_load(geobr, sf, ggplot2, viridis, dplyr)

# 1. Baixar shapefile do Brasil
estados_sf <- geobr::read_state(code_state = "all", year = 2020)

# 2. Preparar dados do IDHM (substitua com seu dataframe real)
dados_estados <- data.frame(
  nome = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE",
           "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP",
           "PR", "SC", "RS", "MS", "MT", "GO", "DF"),
  value = c(0.700, 0.710, 0.700, 0.699, 0.690, 0.688, 0.731, 0.676, 0.690,
            0.734, 0.728, 0.698, 0.719, 0.684, 0.702, 0.691, 0.774, 0.771,
            0.762, 0.806, 0.769, 0.792, 0.771, 0.742, 0.736, 0.737, 0.814)
)

# 3. Juntar dados ao shapefile
estados_sf <- estados_sf %>% 
  left_join(dados_estados, by = c("abbrev_state" = "nome")) %>% 
  mutate(
    centroid = sf::st_centroid(geom),
    coord_x = sf::st_coordinates(centroid)[,1],
    coord_y = sf::st_coordinates(centroid)[,2]
  )

# 4. Criar mapa do IDHM
ggplot(estados_sf) +
  geom_sf(aes(fill = value), color = "white", size = 0.2) +
  geom_text(aes(x = coord_x, y = coord_y, 
                label = format(round(value, 3), nsmall = 3)),
            color = "white",  # Cor alterada para melhor contraste
            size = 3.2,
            fontface = "bold") +
  scale_fill_viridis(
    name = "IDHM",
    option = "viridis",
    direction = -1,  # Inverte a escala para valores maiores = cores mais escuras
    begin = 0.1,
    end = 0.9,
    limits = c(0.65, 0.85)  # Faixa típica do IDHM brasileiro
  ) +
  labs(
    title = "Índice de Desenvolvimento Humano Municipal (IDHM) por Estado - 2021",
    caption = "Fonte: PNUD/IPEA"
  ) +
  theme_void() +
  theme(
    legend.position = c(0.2, 0.15),  # Posição personalizada da legenda
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.caption = element_text(hjust = 0.95, size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold")
  )




# ============== ADIÇÃO: PIB MUNICIPAL ============== #

# Carregar dados do PIB Municipal (ajustar código conforme necessário)
PIB_mun <- ipeadata("PIBPMCE", language = 'br') %>% 
  filter(date == max(date))  # Selecionar ano mais recente

# Juntar com a tabela de lookup de nomes
df_PIB <- PIB_mun %>%
  left_join(lookup_nomes, by = "tcode") %>% 
  select(-uname) %>%
  relocate(nome, .after = tcode)

# Filtrar dados estaduais
dados_estados_PIB <- df_PIB %>% 
  filter(tcode > 5)  # Valores acima de 5 são estados

# Filtrar dados regionais
dados_regiao_PIB <- df_PIB %>% 
  filter(tcode %in% 1:5)  # Seleciona apenas as regiões

# ============== VISUALIZAÇÕES PARA PIB ============== #

# 1. Heatmap para PIB
ggplot(dados_estados_PIB, aes(x = reorder(nome, -value), y = "PIB", fill = value)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = format(round(value/1e9, 1), big.mark = ".", decimal.mark = ",")), 
            color = "white", size = 3) +
  scale_fill_viridis(name = "PIB (bilhões)", option = "plasma", direction = -1,
                     labels = scales::number_format(scale = 1e-9, suffix = "B")) +
  labs(title = "Produto Interno Bruto por Estado - 2021",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")

# 2. Mapa Coroplético
# Ler os dados geográficos dos estados e juntar com o PIB
estados_sf_pib <- read_state(code_state = "all", year = 2020) %>%
  left_join(dados_estados_PIB, by = c("abbrev_state" = "nome")) %>%
  mutate(
    centroid = sf::st_centroid(geom),
    coord_x = sf::st_coordinates(centroid)[,1],
    coord_y = sf::st_coordinates(centroid)[,2]
  )

# Criar o mapa coroplético com rótulo
ggplot(estados_sf_pib) +
  geom_sf(aes(fill = value), color = "white") +
  geom_text(
    aes(x = coord_x, y = coord_y, label = round(value, 0)),
    color = "black",
    fontface = "bold",
    size = 3
  ) +
  scale_fill_viridis_c(option = "plasma", name = "PIB (bi R$)") +
  labs(
    title = "PIB por Estado - Brasil",
    caption = "Fonte: IPEA"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )


# Carregar pacotes
pacman::p_load(tidyverse, corrplot, ggcorrplot)

# Exemplo de dados combinados (substitua com seus dados reais)
dados_completos <- tibble(
  nome = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE",
           "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP",
           "PR", "SC", "RS", "MS", "MT", "GO", "DF"),  # Nomes dos estados
  IDHM = c(0.700, 0.710, 0.700, 0.699, 0.690, 0.688, 0.731, 0.676, 0.690,
           0.734, 0.728, 0.698, 0.719, 0.684, 0.702, 0.691, 0.774, 0.771,
           0.762, 0.806, 0.769, 0.792, 0.771, 0.742, 0.736, 0.737, 0.814), # Valores do IDHM
  IVS = c(0.167, 0.332, 0.288, 0.225, 0.269, 0.204, 0.201, 0.289, 0.263,
          0.238, 0.252, 0.296, 0.300, 0.283, 0.280, 0.258, 0.173, 0.189,
          0.262, 0.209, 0.164, 0.113, 0.191, 0.166, 0.197, 0.217, 0.236),   # Valores do IVS
  GINI = c(0.455, 0.511, 0.512, 0.520, 0.501, 0.491, 0.477, 0.492, 0.552,
           0.513, 0.535, 0.559, 0.496, 0.486, 0.507, 0.490, 0.476, 0.486,
           0.540, 0.504, 0.463, 0.418, 0.466, 0.477, 0.452, 0.473, 0.543)   # Valores do GINI
)

# Matriz de correlação entre as variáveis
cor_matrix <- dados_completos %>% 
  select(IDHM, IVS, GINI) %>%  # Selecionar apenas as variáveis numéricas
  cor(method = "pearson")       # Calcular correlações

# Versão 1 - com ggcorrplot
ggcorrplot(
  cor_matrix,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE,
  colors = c("#E46726", "white", "#6D9EC1"),
  title = "Correlação entre IDHM, IVS e GINI"
)

# Versão 2 - com corrplot
corrplot(
  cor_matrix,
  method = "number",
  type = "upper",
  title = "Correlação entre Indicadores",
  mar = c(0, 0, 2, 0)
)








# Carregar pacotes
pacman::p_load(tidyverse, corrplot, viridis)

# Dados de exemplo
dados_completos <- data.frame(
  nome = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE",
           "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP",
           "PR", "SC", "RS", "MS", "MT", "GO", "DF"),
  GINI = c(0.455, 0.511, 0.512, 0.520, 0.501, 0.491, 0.477, 0.492, 0.552,
           0.513, 0.535, 0.559, 0.496, 0.486, 0.507, 0.490, 0.476, 0.486,
           0.540, 0.504, 0.463, 0.418, 0.466, 0.477, 0.452, 0.473, 0.543)
)

# Criar matriz de valores do GINI
matriz_gini <- matrix(dados_completos$GINI, ncol = 1)
rownames(matriz_gini) <- dados_completos$nome
colnames(matriz_gini) <- "GINI"

# Visualizar como mapa de calor
dados_completos %>%
  mutate(nome = fct_reorder(nome, GINI)) %>%
  ggplot(aes(x = 1, y = nome, fill = GINI)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Valores do Índice de Gini por Estado",
       x = NULL, y = "Estados") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())





# Matriz com estados como linhas e indicadores como colunas
matriz_estados <- dados_completos %>%
  column_to_rownames("nome") %>%
  as.matrix()

# Correlação entre os estados (transpõe para calcular correlação entre linhas)
matriz_corr_estados <- cor(t(matriz_estados))

ggcorrplot(matriz_corr_estados,
           lab = FALSE,           # use TRUE se quiser rótulos numéricos
           type = "lower",
           title = "Correlograma entre Estados com base nos Indicadores")
