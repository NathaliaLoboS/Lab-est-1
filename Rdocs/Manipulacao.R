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


