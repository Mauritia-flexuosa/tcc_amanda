library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(zoo)

df_bruto <- read.xlsx("/home/mcure/TCC Amanda/tabela Tityus serrulatus e Tityus spp.xlsx")

dados_brutos <- structure(list(Contagem.de.Número.de.fichas = c("Rótulos de Linha", 
                                                                "CACHOEIRA DO BOM JESUS", "CANASVIEIRAS", "CÓRREGO GRANDE", 
                                                                "INGLESES", "INGLESES DO RIO VERMELHO", "Não preenchido", "Não se aplica", 
                                                                "PONTA DAS CANAS", "PRAIA BRAVA", "TAPERA", "UPA NORTE", "VARGEM DO BOM JESUS", 
                                                                "VARGEM GRANDE", "Total Geral", "Contagem de Número de fichas", 
                                                                "Rótulos de Linha", "2019", "Janeiro", "Fevereiro", "Maio", 
                                                                "Dezembro", "2020", "Janeiro", "Agosto", "Setembro", "Outubro", 
                                                                "Dezembro", "2021", "Fevereiro", "Março", "Maio", "Junho", "Setembro", 
                                                                "Novembro", "Dezembro", "2022", "Maio", "Junho", "Agosto", "Outubro", 
                                                                "Novembro", "Dezembro", "2023", "Janeiro", "Maio", "Setembro", 
                                                                "Dezembro", "2024", "Março", "Agosto", "Setembro", "Outubro", 
                                                                "Novembro", "Dezembro", "Total Geral"), Rótulos.de.Coluna = c("Humana", 
                                                                                                                              "9", "7", NA, "3", "1", "3", NA, "4", "2", "1", "3", "1", "3", 
                                                                                                                              "37", "Rótulos de Coluna", "Humana", "5", "2", "1", "1", "1", 
                                                                                                                              "6", NA, "1", "2", "2", "1", "7", NA, "1", "1", "1", "2", "1", 
                                                                                                                              "1", "7", "1", NA, "1", "2", "1", "2", "4", "1", "1", "1", "1", 
                                                                                                                              "8", "1", "3", NA, "2", "1", "1", "37"), X3 = c("Informacão", 
                                                                                                                                                                              "1", NA, "1", NA, NA, NA, "4", NA, "1", NA, NA, NA, NA, "7", 
                                                                                                                                                                              NA, "Informacão", NA, NA, NA, NA, NA, "1", "1", NA, NA, NA, 
                                                                                                                                                                              NA, "2", "1", NA, "1", NA, NA, NA, NA, "1", NA, "1", NA, NA, 
                                                                                                                                                                              NA, NA, NA, NA, NA, NA, NA, "3", NA, NA, "1", NA, "2", NA, "7"
                                                                                                                              ), X4 = c("Total Geral", "10", "7", "1", "3", "1", "3", "4", 
                                                                                                                                        "4", "3", "1", "3", "1", "3", "44", NA, "Total Geral", "5", "2", 
                                                                                                                                        "1", "1", "1", "7", "1", "1", "2", "2", "1", "9", "1", "1", "2", 
                                                                                                                                        "1", "2", "1", "1", "8", "1", "1", "1", "2", "1", "2", "4", "1", 
                                                                                                                                        "1", "1", "1", "11", "1", "3", "1", "2", "3", "1", "44"), X5 = c("Bairro", 
                                                                                                                                                                                                         "CACHOEIRA DO BOM JESUS", "CANASVIEIRAS", "CÓRREGO GRANDE", 
                                                                                                                                                                                                         "INGLESES", "INGLESES DO RIO VERMELHO", "Não preenchido", "PONTA DAS CANAS", 
                                                                                                                                                                                                         "PRAIA BRAVA", "TAPERA", "UPA NORTE", "VARGEM DO BOM JESUS", 
                                                                                                                                                                                                         "VARGEM GRANDE", "Total Geral", NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                         NA, NA), X6 = c("Humanas", "9", "7", NA, "3", "1", "3", "4", 
                                                                                                                                                                                                                         "2", "1", "3", "1", "3", "37", NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                         NA, NA), X7 = c("Informação", "1", NA, "1", NA, NA, "4", NA, 
                                                                                                                                                                                                                                         "1", NA, NA, NA, NA, "7", NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                         NA), X8 = c("Total", "10", "7", "1", "3", "1", "7", "4", "3", 
                                                                                                                                                                                                                                                     "1", "3", "1", "3", "44", NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                                                                                                                                                     NA)), row.names = c(NA, 56L), class = "data.frame")
# Função para limpar e estruturar os dados
limpar_dados_escorpiao <- function(dados_brutos) {
  
  # 1. Identificar onde começam os dados temporais (a partir de "2019")
  linha_inicio_tempo <- which(dados_brutos$Contagem.de.Número.de.fichas == "2019")[1]
  
  # 2. Extrair dados por bairro 
  dados_bairro <- dados_brutos[2:14, ] %>%  # Linhas 2 a 14 contêm dados por bairro
    select(
      Bairro = Contagem.de.Número.de.fichas,
      Humana = Rótulos.de.Coluna,
      Informacao = X3,
      Total = X4
    ) %>%
    filter(!is.na(Bairro), Bairro != "Total Geral") %>%
    mutate(across(c(Humana, Informacao, Total), as.numeric))
  
  # 3. Extrair dados temporais
  dados_tempo <- dados_brutos[linha_inicio_tempo:(nrow(dados_brutos)-1), ] %>%
    select(
      Periodo = Contagem.de.Número.de.fichas,
      Humana = Rótulos.de.Coluna,
      Informacao = X3,
      Total = X4
    ) %>%
    filter(!is.na(Periodo), Periodo != "Total Geral")
  
  # 4. Identificar anos e meses
  dados_tempo_limpo <- dados_tempo %>%
    mutate(
      Ano = ifelse(grepl("^[0-9]{4}$", Periodo), Periodo, NA_character_),
      Ano = zoo::na.locf(Ano),
      Mes = ifelse(!grepl("^[0-9]{4}$", Periodo), Periodo, NA_character_)
    ) %>%
    filter(!is.na(Mes)) %>%
    mutate(
      across(c(Humana, Informacao, Total), as.numeric),
      Ano = as.numeric(Ano)
    ) %>%
    select(Ano, Mes, Humana, Informacao, Total)
    # 5. Ordenar meses
  ordem_meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                   "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
  dados_tempo_limpo <- dados_tempo_limpo %>%
    mutate(
      Mes = factor(Mes, levels = ordem_meses)
    ) %>%
    arrange(Ano, Mes)
  # 6. Retornar lista
  return(list(
    por_bairro = dados_bairro,
    por_tempo = dados_tempo_limpo
  ))
}

# Aplicar a função
dados_limpos <- limpar_dados_escorpiao(dados_brutos)

dados_limpos$por_tempo <- dados_limpos$por_tempo %>%
  replace(is.na(.), 0)
dados_limpos$por_bairro <- dados_limpos$por_bairro %>%
  replace(is.na(.), 0)
