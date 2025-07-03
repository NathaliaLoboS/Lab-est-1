(SUM(total_temas) / SUM(pop_total)) * 100000


pacman::p_load(readxl, tidyverse)


# temas ----
temas <- read_excel("Dash/taxas_temas_municipio_dash.xlsx")

temas_mun <- temas %>%
  group_by(municipio) %>%
  summarise(
    total_temas = sum(total_temas, na.rm = TRUE),
    pop_total = sum(pop_total, na.rm = TRUE)
  ) %>%
  mutate(Ind_temas = (total_temas / pop_total) * 100000)

temas_uf <- temas %>%
  group_by(uf) %>%
  summarise(
    total_temas = sum(total_temas, na.rm = TRUE),
    pop_total = sum(pop_total, na.rm = TRUE)
  ) %>%
  mutate(Ind_temas = (total_temas / pop_total) * 100000)

temas_reg <- temas %>%
  group_by(regiao) %>%
  summarise(
    total_temas = sum(total_temas, na.rm = TRUE),
    pop_total = sum(pop_total, na.rm = TRUE)
  ) %>%
  mutate(Ind_temas = (total_temas / pop_total) * 100000)

temas_br <- temas %>%
  summarise(
    total_temas = sum(total_temas, na.rm = TRUE),
    pop_total = sum(pop_total, na.rm = TRUE)
  ) %>%
  mutate(Ind_temas = (total_temas / pop_total) * 100000)

temas_mun <- temas_mun %>% mutate(nivel = "municipio")
temas_uf <- temas_uf %>% mutate(nivel = "uf")
temas_reg <- temas_reg %>% mutate(nivel = "regiao")
temas_br <- temas_br %>% mutate(nivel = "Brasil")

temas_br <- temas_br %>% mutate(municipio = NA, uf = NA, regiao = NA) 

temas_final <- bind_rows(temas_mun, temas_uf, temas_reg, temas_br)

# praticas ----
praticas <- read_excel("Dash/Dashboard_tudo.xlsx")

praticas <- praticas %>% 
  mutate(
    municipio = str_remove(Município_UF, "\\s*\\([^\\)]+\\)")
  )

praticas_mun <- praticas %>%
  group_by(municipio) %>%
  summarise(
    total_prat = sum(Praticas, na.rm = TRUE),
    pop_total = sum(População, na.rm = TRUE)
  ) %>%
  mutate(Ind_praticas = (total_prat / pop_total) * 100000)

praticas_uf <- praticas %>%
  group_by(UF) %>%
  summarise(
    total_prat = sum(Praticas, na.rm = TRUE),
    pop_total = sum(População, na.rm = TRUE)
  ) %>%
  mutate(Ind_praticas = (total_prat / pop_total) * 100000)

praticas_reg <- praticas %>%
  group_by(Região) %>%
  summarise(
    total_prat = sum(Praticas, na.rm = TRUE),
    pop_total = sum(População, na.rm = TRUE)
  ) %>%
  mutate(Ind_praticas = (total_prat / pop_total) * 100000)

praticas_br <- praticas %>%
  summarise(
    total_prat = sum(Praticas, na.rm = TRUE),
    pop_total = sum(População, na.rm = TRUE)
  ) %>%
  mutate(Ind_praticas = (total_prat / pop_total) * 100000)

praticas_mun <- praticas_mun %>% mutate(nivel = "municipio")
praticas_uf <- praticas_uf %>% mutate(nivel = "UF")
praticas_reg <- praticas_reg %>% mutate(nivel = "Região")
praticas_br <- praticas_br %>% mutate(nivel = "Brasil")

praticas_br <- praticas_br %>% mutate(municipio = NA, UF = NA, Região = NA) 

praticas_final <- bind_rows(praticas_mun, praticas_uf, 
                            praticas_reg, praticas_br)

# atividade ----
atividade <- read_excel("Dash/taxas_atividades_municipio_dash.xlsx")

atividade_mun <- atividade %>%
  group_by(municipio) %>%
  summarise(
    total_ativ = sum(total_atividade, na.rm = TRUE),
    pop_total = sum(escolas, na.rm = TRUE)
  ) %>%
  mutate(Ind_ativ = (total_ativ / pop_total) * 100000)

atividade_uf <- atividade %>%
  group_by(uf) %>%
  summarise(
    total_ativ = sum(total_atividade, na.rm = TRUE),
    pop_total = sum(escolas, na.rm = TRUE)
  ) %>%
  mutate(Ind_ativ = (total_ativ / pop_total) * 100000)

atividade_reg <- atividade %>%
  group_by(regiao) %>%
  summarise(
    total_ativ = sum(total_atividade, na.rm = TRUE),
    pop_total = sum(escolas, na.rm = TRUE)
  ) %>%
  mutate(Ind_ativ = (total_ativ / pop_total) * 100000)

atividade_br <- atividade %>%
  summarise(
    total_ativ = sum(total_atividade, na.rm = TRUE),
    pop_total = sum(escolas, na.rm = TRUE)
  ) %>%
  mutate(Ind_ativ = (total_ativ / pop_total) * 100000)

atividade_mun <- atividade_mun %>% mutate(nivel = "municipio")
atividade_uf <- atividade_uf %>% mutate(nivel = "uf")
atividade_reg <- atividade_reg %>% mutate(nivel = "regiao")
atividade_br <- atividade_br %>% mutate(nivel = "Brasil")

atividade_br <- atividade_br %>% mutate(municipio = NA, uf = NA, regiao = NA) 

atividade_final <- bind_rows(atividade_mun, atividade_uf, 
                             atividade_reg, atividade_br)

atividade_final <- atividade_final %>%
  mutate(Ind_ativ = ifelse(is.infinite(Ind_ativ), NA, Ind_ativ))







