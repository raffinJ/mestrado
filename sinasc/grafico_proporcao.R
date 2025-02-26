library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyverse)

options(digits = 10, 
        OutDec = ",",
        useFancyQuotes = FALSE,
        decimal.mark = ",",
        big.mark = ".",
        show.signif.stars = FALSE,
        useDingbats = FALSE)


 



# Gráfico de barras - VIA DE PARTO _____________________________________________

tab_parto_ano <- tab_freq_grupo(dados_sinasc, PARTO, ANO)
tab_parto_ano$PARTO[is.na(tab_parto_ano$PARTO)] <- "Sem informações"
tab_parto_ano <- tab_parto_ano %>%
  mutate(PARTO = factor(PARTO, levels = c("Cesáreo", "Vaginal", "Sem informações")))

plt <- ggplot(tab_parto_ano, aes(x = ANO, y = porc, fill = PARTO)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", porc)), position = position_stack(vjust = 0.5), size = 3) + # Adiciona os rótulos
  labs(x = "Ano", y = "Proporção de nascidos vivos entre as vias de nascimento (%)", fill = "Via de nascimento") +
  scale_fill_manual(values = c("Vaginal" = "lightsteelblue1", "Cesáreo" = "darkseagreen2", "Sem informações" = "lightsalmon1")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "hotpink") +
  theme_minimal()

# Saving the plot
ggsave("via_parto_ano.jpg", plot = plt, width = 9, height = 6, dpi = 800)



# Gráfico de barras - CONSULTAS PRÉ-NATAL_______________________________________

tab_consultas_ano <- tab_freq_grupo(dados_sinasc, CONSULTAS, ANO)
tab_consultas_ano$CONSULTAS[is.na(tab_consultas_ano$CONSULTAS)] <- "Sem informações"
tab_consultas_ano <- tab_consultas_ano %>%
  mutate(CONSULTAS = factor(CONSULTAS, levels = c("7 ou mais vezes", "4 a 6 vezes",
                                                  "1 a 3 vezes", "Nenhuma", "Sem informações")))

plt <- ggplot(tab_consultas_ano, aes(x = ANO, y = porc, fill = CONSULTAS)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = subset(tab_consultas_ano, CONSULTAS != "Sem informações"),
            aes(label = sprintf("%.1f%%", porc)),
            position = position_stack(vjust = 1), size = 3) +
  labs(x = "Ano", y = "Proporção de nascidos vivos pelo número de consultas pré-natais realizadss (%)",
       fill = "Número de consultas pré-natal") +
  scale_fill_manual(values = c("7 ou mais vezes" = "mistyrose2", 
                               "4 a 6 vezes" = "darkseagreen2",
                               "1 a 3 vezes" = "lightsteelblue1", 
                               "Nenhuma" = "khaki1", 
                               "Sem informações" = "lightsalmon1")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_minimal()


# Saving the plot
ggsave("consultas_ano.jpg", plot = plt, width = 9, height = 6, dpi = 800)


# Gráfico de barras - IDADE GESTACIONAL ________________________________________

# Reordenar os níveis da variável CLASS_IG na ordem desejada
tab_class_ig_ano <- tab_freq_grupo(dados_sinasc, CLASS_IG, ANO)
tab_class_ig_ano$CLASS_IG <- factor(tab_class_ig_ano$CLASS_IG, 
                                    levels = c("Termo", "Prematuro moderado a tardio", 
                                               "Pós-termo", "Muito prematuro", "Prematuro extremo"))

# Gráfico de barras
plt <- ggplot(tab_class_ig_ano, aes(x = ANO, y = porc, fill = CLASS_IG)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = subset(tab_class_ig_ano, CLASS_IG != "Prematuro extremo" & CLASS_IG != "Muito prematuro"),
            aes(label = sprintf("%.1f%%", porc)),
            position = position_stack(vjust = 1), size = 3) +
  labs(x = "Ano", y = "Proporção de nascidos vivos por classificação de idade gestacional do nascimento (%)",
       fill = "Classificação de idade 
gestacional do nascimento") +
  scale_fill_manual(values = c("Muito prematuro" = "lightsalmon1",
                               "Prematuro extremo" = "lightsteelblue1", 
                               "Prematuro moderado a tardio" = 'darkseagreen2',
                               "Pós-termo" = 'khaki1', 
                               "Termo" = "mistyrose2")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_minimal()


# Saving the plot
ggsave("ig_ano.jpg", plot = plt, width = 9, height = 6, dpi = 800)



# Gráfico de barras - IDADE MATERNA ____________________________________________
tab_idade_class_ano <- tab_freq_grupo(dados_sinasc, CLASS_IDADEMAE, ANO)
tab_idade_class_ano$CLASS_IDADEMAE[is.na(tab_idade_class_ano$CLASS_IDADEMAE)] <- "Sem informações"
tab_idade_class_ano$CLASS_IDADEMAE <- factor(tab_idade_class_ano$CLASS_IDADEMAE, 
                                    levels = c("< 20 anos",
                                               "20-29 anos",
                                               "30-35 anos", 
                                               "> 35 anos",
                                               "Sem informações"))

# Gráfico de barras
plt <- ggplot(tab_idade_class_ano, aes(x = ANO, y = porc, fill = CLASS_IDADEMAE)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = subset(tab_idade_class_ano, CLASS_IDADEMAE != "Sem informações"),
            aes(label = sprintf("%.1f%%", porc)),
            position = position_stack(vjust = 1), size = 3) +
  labs(x = "Ano", y = "Proporção de nascidos vivos por classificação de idade materna (%)",
       fill = "Faixa etária materna") +
  scale_fill_manual(values = c("20-29 anos" = "mistyrose2",
                               "30-35 anos" = 'darkseagreen2', 
                               "< 20 anos" = 'khaki1',
                               "> 35 anos" = "lightsteelblue1",
                               "Sem informações" = "lightsalmon1")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_minimal()



# Saving the plot
ggsave("idade_ano.jpg", plot = plt, width = 9, height = 6, dpi = 800)



# Gráfico de barras - PESO AO NASCER ____________________________________________

tab_peso_class_ano <- tab_freq_grupo(dados_sinasc, CLASS_PESO, ANO)
tab_peso_class_ano$CLASS_PESO <- factor(tab_peso_class_ano$CLASS_PESO, 
                                             levels = c("Macrossomia",
                                                        "Peso adequado",
                                                        "Baixo peso ao nascer",
                                                        "Muito baixo peso ao nascer",
                                                        "Extremo baixo peso ao nascer"))

# Gráfico de barras
plt <- ggplot(tab_peso_class_ano, aes(x = ANO, y = porc, fill = CLASS_PESO)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = subset(tab_peso_class_ano, CLASS_PESO != "Extremo baixo peso ao nascer" & CLASS_PESO != "Muito baixo peso ao nascer"),
            aes(label = sprintf("%.1f%%", porc)),
            position = position_stack(vjust = 1), size = 3) +
  labs(x = "Ano", y = "Proporção de nascidos vivos pela classificação de peso ao nascer (%)",
       fill = "Classificação de
peso ao nascer") +
  scale_fill_manual(values = c("Peso adequado" = "mistyrose2",
                               "Baixo peso ao nascer" = 'darkseagreen2', 
                               "Macrossomia" = 'khaki1',
                               "Muito baixo peso ao nascer" = "lightsteelblue1",
                               "Extremo baixo peso ao nascer" = "lightsalmon1")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_minimal()




# Saving the plot
ggsave("peso_ano.jpg", plot = plt, width = 9, height = 6, dpi = 800)



