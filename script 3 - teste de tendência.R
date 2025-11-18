library(tidyverse)
library(Kendall)

source("/home/mcure/TCC Amanda/script 1.R")

teste <- dados_limpos$por_tempo %>%
  # Transformar para formato longo
  pivot_longer(
    cols = c(Humana, Informacao),
    names_to = "Tipo",
    values_to = "Quantidade"
  ) %>%
  # Traduzir os nomes para português
  mutate(Tipo = case_when(
    Tipo == "Humana" ~ "Acidentes com humanos",
    Tipo == "Informacao" ~ "Registros sem acidentes",
    TRUE ~ Tipo
  )) %>%
  # Agrupar por Ano e Tipo para calcular média e desvio padrão
  group_by(Ano, Tipo) %>%
  summarise(
    Media_Casos = sum(Quantidade, na.rm = TRUE),
    DP_Casos = sd(Quantidade, na.rm = TRUE),
    .groups = 'drop'
  )

teste

# Testes de Mann-Kendall:
MannKendall(teste %>% filter(Tipo == "Acidentes com humanos") %>% pull(Media_Casos))
MannKendall(teste %>% filter(Tipo == "Registros sem acidentes") %>% pull(Media_Casos))
