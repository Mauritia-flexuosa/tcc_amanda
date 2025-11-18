library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(ggthemes)
library(patchwork) 

theme_set(theme_minimal(base_size = 12) +
            theme(plot.title = element_text(face = "bold", size = 14),
                  legend.position = "bottom"))

# 1. GRÁFICO DE SÉRIE TEMPORAL MENSAL
p1 <- dados_limpos$por_tempo %>%
  mutate(Mes_Numero = case_when(
    Mes == "Janeiro" ~ 1,
    Mes == "Fevereiro" ~ 2,
    Mes == "Março" ~ 3,
    Mes == "Abril" ~ 4,
    Mes == "Maio" ~ 5,
    Mes == "Junho" ~ 6,
    Mes == "Julho" ~ 7,
    Mes == "Agosto" ~ 8,
    Mes == "Setembro" ~ 9,
    Mes == "Outubro" ~ 10,
    Mes == "Novembro" ~ 11,
    Mes == "Dezembro" ~ 12,
    TRUE ~ NA_real_
  ),
  Data = as.Date(paste(Ano, Mes_Numero, "01", sep = "-"))) %>%
  ggplot(aes(x = Data, y = Total)) +
  geom_line(color = "#E74C3C", linewidth = 1, alpha = 0.8) +
  geom_point(color = "#C0392B", size = 2) +
  geom_smooth(method = "loess", se = TRUE, color = "#2980B9", alpha = 0.3) +
  labs(title = "EVOLUÇÃO TEMPORAL DE CASOS DE ESCORPIÃO",
       subtitle = "Casos mensais de Tityus serrulatus (2019-2024)",
       x = "Data", y = "Total de Casos") +
  geom_smooth(method = "lm", formula = y ~ x, 
              se = TRUE, color = "#E74C3C", linetype = "dashed") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
  scale_y_continuous(breaks = pretty_breaks()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1<- dados_limpos$por_tempo %>%
  mutate(Mes_Numero = case_when(
    Mes == "Janeiro" ~ 1,
    Mes == "Fevereiro" ~ 2,
    Mes == "Março" ~ 3,
    Mes == "Abril" ~ 4,
    Mes == "Maio" ~ 5,
    Mes == "Junho" ~ 6,
    Mes == "Julho" ~ 7,
    Mes == "Agosto" ~ 8,
    Mes == "Setembro" ~ 9,
    Mes == "Outubro" ~ 10,
    Mes == "Novembro" ~ 11,
    Mes == "Dezembro" ~ 12,
    TRUE ~ NA_real_
  ),
  Data = as.Date(paste(Ano, Mes_Numero, "01", sep = "-"))) %>%
  pivot_longer(
    cols = c(Humana, Informacao),
    names_to = "Tipo_Caso",
    values_to = "Quantidade"
  ) %>%
  mutate(Tipo_Caso = case_when(
    Tipo_Caso == "Humana" ~ "Acidentes com humanos",
    Tipo_Caso == "Informacao" ~ "Registros sem acidentes",
    TRUE ~ Tipo_Caso
  )) %>%
  ggplot(aes(x = Data, y = Quantidade)) +
  geom_line(color = "blue", linewidth = 1, alpha = 0.8) +
  geom_point(color = "blue", size = 2) +
#  geom_smooth(method = "loess", se = TRUE, color = "#2980B9", alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, 
              se = FALSE, color = "red", linetype = "dashed") +
  facet_wrap(~ Tipo_Caso, ncol = 1, scales = "free_y") +
  labs(
       x = "Data", y = "Quantidade de Registros") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y") +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(fill = "#2C3E50"),
        strip.text = element_text(color = "white", face = "bold"))

data <- dados_limpos$por_tempo %>%
  mutate(Mes_Numero = case_when(
    Mes == "Janeiro" ~ 1,
    Mes == "Fevereiro" ~ 2,
    Mes == "Março" ~ 3,
    Mes == "Abril" ~ 4,
    Mes == "Maio" ~ 5,
    Mes == "Junho" ~ 6,
    Mes == "Julho" ~ 7,
    Mes == "Agosto" ~ 8,
    Mes == "Setembro" ~ 9,
    Mes == "Outubro" ~ 10,
    Mes == "Novembro" ~ 11,
    Mes == "Dezembro" ~ 12,
    TRUE ~ NA_real_
  ),
  Data = as.Date(paste(Ano, Mes_Numero, "01", sep = "-")))

# 2. GRÁFICO DE BARRAS POR ANO
p2 <- dados_limpos$por_tempo %>%
  group_by(Ano) %>%
  summarise(Total_Anual = sum(Total, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(Ano), y = Total_Anual, fill = factor(Ano))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  geom_text(aes(label = Total_Anual), vjust = -0.5, size = 4, fontface = "bold") +
  labs(title = "b",
       x = "Ano", y = "Total de Casos") +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 13)

# 3. GRÁFICO DE DISTRIBUIÇÃO POR MÊS (HEATMAP)
p3 <- dados_limpos$por_tempo %>%
  mutate(Mes_Numero = case_when(
    Mes == "Janeiro" ~ 1,
    Mes == "Fevereiro" ~ 2,
    Mes == "Março" ~ 3,
    Mes == "Abril" ~ 4,
    Mes == "Maio" ~ 5,
    Mes == "Junho" ~ 6,
    Mes == "Julho" ~ 7,
    Mes == "Agosto" ~ 8,
    Mes == "Setembro" ~ 9,
    Mes == "Outubro" ~ 10,
    Mes == "Novembro" ~ 11,
    Mes == "Dezembro" ~ 12,
    TRUE ~ NA_real_
  ),
  Data = as.Date(paste(Ano, Mes_Numero, "01", sep = "-"))) %>% 
  group_by(Ano, Mes_Numero) %>%
  summarise(Total = sum(Total), .groups = 'drop') %>%
  mutate(Mes = factor(Mes_Numero, levels = month.name)) %>%
  ggplot(aes(x = factor(Ano), y = factor(Mes_Numero), fill = Total)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = ifelse(Total > 0, Total, "")), 
            color = "white", fontface = "bold", size = 3.5) +
  scale_fill_gradient(low = "#D6EAF8", high = "#1B4F72", 
                      name = "Nº de Casos") +
  labs(title = "DISTRIBUIÇÃO DE CASOS POR MÊS E ANO",
       subtitle = "Heatmap temporal",
       x = "Ano", y = "Mês") +
  theme(panel.grid = element_blank())

# 4. GRÁFICO DE COMPOSIÇÃO POR TIPO DE INFORMAÇÃO
p4 <- dados_limpos$por_tempo %>%
  group_by(Ano) %>%
  summarise(Humana = sum(Humana, na.rm = TRUE),
            Informacao = sum(Informacao, na.rm = TRUE)) %>%
  pivot_longer(cols = c(Humana, Informacao), 
               names_to = "Tipo", values_to = "Casos") %>%
  mutate(Tipo = factor(Tipo, levels = c("Humana", "Informacao"),
                       labels = c("Acidentes com humanos", "Registro sem acidentes"))) %>%
  ggplot(aes(x = factor(Ano), y = round(Casos), fill = Tipo)) +
  geom_col(position = "stack", alpha = 0.9) +
  geom_text(aes(label = Casos, group = Tipo), 
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold") +
  scale_fill_manual(values = c("#27AE60", "#E67E22")) +
  ylim(c(0,13))+
  labs(x = "Ano", y = "Total de Casos", fill = "Legenda:") +
  theme(legend.position = "bottom")

# 5. GRÁFICO DE BARRAS POR BAIRRO
p5 <- dados_limpos$por_bairro %>%
  arrange(desc(Total)) %>%
  mutate(Bairro = factor(Bairro, levels = rev(Bairro))) %>%
  pivot_longer(cols = c(Humana, Informacao), 
               names_to = "Tipo", values_to = "Casos") %>%
  mutate(Tipo = factor(Tipo, levels = c("Humana", "Informacao"),
                       labels = c("Acidentes com humanos", "Registro sem acidentes"))) %>%
  
  # MUDANÇA PRINCIPAL AQUI:
  ggplot(aes(x = Casos, y = Bairro, fill = Tipo)) +  # Troquei Total por Casos e add fill = Tipo
  geom_col(position = "dodge", alpha = 0.8) +        # position = "dodge" para barras lado a lado
  geom_text(aes(label = Casos), 
            position = position_dodge(width = 0.9),
            hjust = -0.2, size = 3.5, fontface = "bold") +
  labs(x = "Número de Casos", y = NULL, fill = "Tipo de Caso") +
  scale_fill_manual(values = c("purple", "orange")) +
  theme(panel.grid.major.y = element_blank())

# 6. GRÁFICO DE SAZONALIDADE (média por mês)
p6 <- dados_limpos$por_tempo %>%
  mutate(Mes_Numero = case_when(
    Mes == "Janeiro" ~ 1,
    Mes == "Fevereiro" ~ 2,
    Mes == "Março" ~ 3,
    Mes == "Abril" ~ 4,
    Mes == "Maio" ~ 5,
    Mes == "Junho" ~ 6,
    Mes == "Julho" ~ 7,
    Mes == "Agosto" ~ 8,
    Mes == "Setembro" ~ 9,
    Mes == "Outubro" ~ 10,
    Mes == "Novembro" ~ 11,
    Mes == "Dezembro" ~ 12,
    TRUE ~ NA_real_
  ),
  Data = as.Date(paste(Ano, Mes_Numero, "01", sep = "-"))) %>%
  group_by(Mes) %>%
  summarise(Media_Casos = mean(Total, na.rm = TRUE),
            DP_Casos = sd(Total, na.rm = TRUE)) %>%
  ggplot(aes(x = Mes, y = Media_Casos, group = 1)) +
  geom_line(color = "#3498DB", linewidth = 1) +
  geom_point(color = "#2980B9", size = 3) +
  geom_ribbon(aes(ymin = Media_Casos - DP_Casos, 
                  ymax = Media_Casos + DP_Casos), 
              alpha = 0.2, fill = "#3498DB") +
  facet_wrap(~Tipo)+
  labs(title = "SAZONALIDADE - MÉDIA DE CASOS POR MÊS",
       subtitle = "Linha: média | Área: desvio padrão",
       x = "Mês", y = "Média de Casos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 7. GRÁFICO DE PIZZA - DISTRIBUIÇÃO POR TIPO
p7 <- dados_limpos$por_tempo %>%
  summarise(Humana = sum(Humana, na.rm = TRUE),
            Informacao = sum(Informacao, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Tipo", values_to = "Total") %>%
  mutate(Percentual = Total/sum(Total) * 100,
         Tipo = factor(Tipo, 
                       levels = c("Humana", "Informacao"),
                       labels = c("Acidentes", "Sem acidentes"))) %>%
  ggplot(aes(x = "", y = Total, fill = Tipo)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Percentual, 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("#27AE60", "#E67E22")) +
  labs(title = "Capturas com e sem acidentes",
       fill = "Fonte:") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))


# ====================
  

library(plotly)

# Converter o gráfico de série temporal para interativo
p1_interativo <- ggplotly(p1) %>%
  layout(hoverlabel = list(bgcolor = "white"),
         title = list(text = "EVOLUÇÃO TEMPORAL DE CASOS DE ESCORPIÃO<br><sub>Casos mensais de Tityus serrulatus (2019-2024)</sub>"))



dados_limpos$por_tempo %>%
  mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
  pivot_longer(cols = c(Humana, Informacao), 
               names_to = "Tipo", values_to = "Quantidade") %>%
  mutate(
    Tipo = factor(Tipo, levels = c("Humana", "Informacao"),
                  labels = c("Acidentes com humanos", "Captura sem acidentes")),
    Mes_Numero = case_when(
      Mes == "Janeiro" ~ 1,
      Mes == "Fevereiro" ~ 2,
      Mes == "Março" ~ 3,
      Mes == "Abril" ~ 4,
      Mes == "Maio" ~ 5,
      Mes == "Junho" ~ 6,
      Mes == "Julho" ~ 7,
      Mes == "Agosto" ~ 8,
      Mes == "Setembro" ~ 9,
      Mes == "Outubro" ~ 10,
      Mes == "Novembro" ~ 11,
      Mes == "Dezembro" ~ 12,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(Mes_Numero = replace_na(Mes_Numero, 0)) %>%
  group_by(Mes, Tipo) %>%
  summarise(
    Media_Casos = mean(Quantidade, na.rm = TRUE),
    DP_Casos = sd(Quantidade, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = Mes, y = Media_Casos, group = 1)) +
  geom_line(color = "#3498DB", linewidth = 1) +
  geom_point(color = "#2980B9", size = 3) +
  geom_ribbon(aes(ymin = Media_Casos - DP_Casos, 
                  ymax = Media_Casos + DP_Casos), 
              alpha = 0.2, fill = "#3498DB") +
  facet_wrap(~Tipo)+
#  geom_smooth(method = "lm", formula = y ~ x, 
#              se = TRUE, color = "#E74C3C", linetype = "dashed") +
  labs(title = "SAZONALIDADE - MÉDIA DE CASOS POR MÊS",
       subtitle = "Linha: média | Área: desvio padrão",
       x = "Mês", y = "Média de Casos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


p8 <- dados_limpos$por_tempo %>%
  mutate(Mes_Numero = case_when(
    Mes == "Janeiro" ~ 1,
    Mes == "Fevereiro" ~ 2,
    Mes == "Março" ~ 3,
    Mes == "Abril" ~ 4,
    Mes == "Maio" ~ 5,
    Mes == "Junho" ~ 6,
    Mes == "Julho" ~ 7,
    Mes == "Agosto" ~ 8,
    Mes == "Setembro" ~ 9,
    Mes == "Outubro" ~ 10,
    Mes == "Novembro" ~ 11,
    Mes == "Dezembro" ~ 12,
    TRUE ~ NA_real_
  ),
  Data = as.Date(paste(Ano, Mes_Numero, "01", sep = "-"))) %>%
  pivot_longer(
    cols = c(Humana, Informacao),
    names_to = "Tipo",
    values_to = "Quantidade"
  ) %>%
  mutate(Tipo = case_when(
    Tipo == "Humana" ~ "Acidentes com humanos",
    Tipo == "Informacao" ~ "Registros sem acidentes",
    TRUE ~ Tipo
  )) %>%
  group_by(Mes_Numero, Mes, Tipo) %>%
  summarise(
    Media_Casos = mean(Quantidade, na.rm = TRUE),
    DP_Casos = sd(Quantidade, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Mes = factor(Mes, levels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                                      "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"))) %>%
  ggplot(aes(x = Mes_Numero, y = Media_Casos, group = 1)) +
  geom_line(color = "#3498DB", linewidth = 1) +
  geom_point(color = "#2980B9", size = 3) +
#  geom_ribbon(aes(ymin = Media_Casos - DP_Casos, 
#                  ymax = Media_Casos + DP_Casos), 
#              alpha = 0.2, fill = "#3498DB") +
#  geom_smooth(method = "lm", formula = y ~ x,
#              se = FALSE,
#              color = "#E74C3C", linetype = "dashed",
#              aes(group = Tipo)) +
  facet_wrap(~ Tipo) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
               "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  ) +
  labs(x = "Mês", 
       y = "Média de Casos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        strip.background = element_rect(fill = "#2C3E50"),
        strip.text = element_text(color = "white", face = "bold"))

p9 <- dados_limpos$por_tempo %>%
  pivot_longer(
    cols = c(Humana, Informacao),
    names_to = "Tipo",
    values_to = "Quantidade"
  ) %>%
  mutate(Tipo = case_when(
    Tipo == "Humana" ~ "Acidentes com humanos",
    Tipo == "Informacao" ~ "Registros sem acidentes",
    TRUE ~ Tipo
  )) %>%
  group_by(Ano, Tipo) %>%
  summarise(
    Media_Casos = sum(Quantidade, na.rm = TRUE),
    DP_Casos = sd(Quantidade, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = Ano, y = Media_Casos, group = 1)) +
  geom_line(color = "#3498DB", linewidth = 1) +
  geom_point(color = "#2980B9", size = 3) +
#  geom_ribbon(aes(ymin = Media_Casos - DP_Casos, 
#                  ymax = Media_Casos + DP_Casos), 
#              alpha = 0.2, fill = "#3498DB") +
  geom_smooth(method = "lm", formula = y ~ x, 
              se = FALSE, color = "#E74C3C", linetype = "dashed",
              aes(group = Tipo)) +
  facet_wrap(~ Tipo) +
  labs(x = "Ano", 
       y = "Média de Casos") +
  scale_x_continuous(breaks = seq(min(dados_limpos$por_tempo$Ano, na.rm = TRUE), 
                                  max(dados_limpos$por_tempo$Ano, na.rm = TRUE), by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        strip.background = element_rect(fill = "#2C3E50"),
        strip.text = element_text(color = "white", face = "bold"))

