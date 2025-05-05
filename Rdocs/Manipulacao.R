# carregando pacotes
pacman::p_load(tidyverse, ipeadatar)


# Indicadores socioeconomicos e desigualdade social ----

## Indice de vulnerabilidade social - IVS ----
search_series("Vulnerabilidade Social") # series relacionadas

IVS <- ipeadata("AVS_IVS", language = "br") %>% 
  filter(date == max(date))
# ultimo ano foi 2022
# Brasil, regiao e estado

## Indice de desenvolvimento municipal - IDHM ----
search_series("IDHM")  # series relacionadas
print(search_series("IDHM"), n=38)

IDHM <- ipeadata("IDHM", language = "br") %>% 
  filter(date == max(date))
# ultimo ano foi 2021
# Brasil e estados


## Produto interno bruto - PIB ----
search_series("PIB") # series relacionadas
teste <- print(search_series("PIB"), n=170)

# brasil - 2024
PIB_B <- ipeadata("SCN10_DIPIBG10", language = 'br') %>% 
  filter(date == max(date))
# municipio - teste 
PIB_mun <- ipeadata("PIB", language = 'br') %>% 
  filter(date == max(date))
# estadual - 2021
PIB_mun <- ipeadata("PIBPMCE", language = 'br') %>% 
  filter(date == max(date))

## Indice de GINI ----
search_series("Gini") # series relacionadas


# brasil, regiao e municipio - 2023
GINI <- ipeadata("PNADCA_GINIUF", language = 'br') %>% 
  filter(date == max(date))









# Teste
# UFs
ufs <- data.frame(
  tcode = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27,
            28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53),
  estado = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
             "MA", "PI", "CE", "RN", "PB", "PE", "AL",
             "SE", "BA", "MG", "ES", "RJ", "SP",
             "PR", "SC", "RS", "MS", "MT", "GO", "DF"))

# Regiões
regioes <- data.frame(
  tcode = c(1, 2, 3, 4, 5),
  regiao = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
)

# Juntar as tabelas de nome de estado e região ao dataframe original
df_dados_completo <- IVS %>%
  left_join(ufs, by = "tcode") %>%
  left_join(regioes, by = "tcode")

# Visualizar resultado
head(df_dados_completo)


# Teste
# Carregar pacotes
pacman::p_load(tidyverse, ipeadatar)

# Coeficiente de GINI por UF (2023)
gini_uf <- ipeadata("PNADCA_GINIUF", language = "br") %>%
  filter(date == as.Date("2023-01-01")) %>%
  select(uf = uname, gini = value)

# 2. Ínidice de vunerabilidade social (IVS) por UF (2022)
ivs_uf <- ipeadata("AVS_IVS", language = "br") %>%
  filter(date == as.Date("2022-01-01")) %>%
  select(uf = uname, ivs = value)

# 3. IDHM por UF (2021)
idhm_uf <- ipeadata("IDHM", language = "br") %>%
  filter(date == as.Date("2021-01-01")) %>%
  select(uf = uname, idhm = value)

# 4. PIB PER CAPITA por UF (2021)
# Código da série por estado é: SCN10_PIBPCUF
pibpc_uf <- ipeadata("SCN10_PIBPCUF", language = "br") %>%
  filter(date == as.Date("2021-01-01")) %>%
  select(uf = uname, pib_per_capita = value)

# 5. Juntando os  indicadores
indicadores_uf <- gini_uf %>%
  left_join(ivs_uf, by = "uf") %>%
  left_join(idhm_uf, by = "uf") %>%
  left_join(pibpc_uf, by = "uf")

# 6. Resultados
print(indicadores_uf)
