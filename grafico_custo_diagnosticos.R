library(dplyr)
library(tidyr)
library(grid)
library(tidyverse)
library(ggrepel)
library(ggplot2)
library(gridGraphics)
library(ggthemes)
library(Polychrome)



options(digits = 10, 
        OutDec = ",",
        useFancyQuotes = FALSE,
        decimal.mark = ",",
        big.mark = ".",
        show.signif.stars = FALSE,
        useDingbats = FALSE)

aih_cid <- tab_freq(dados_sih_neo, DIAG_PRINC) # Responde: "Qual é o n (frequência) e a porcentagem (%) de registros por diagnóstico CID-10?"
  aih_cid <- aih_cid[aih_cid$DIAG_PRINC %in% top20_cid, ]

tot_cid <- descriptive(dados_sih_neo, DIAG_PRINC, val_tot_corrigido)
  tot_cid <- tot_cid[tot_cid$DIAG_PRINC %in% top20_cid, ]

  
aih_cid <- merge(tot_cid, aih_cid, by = "DIAG_PRINC", all = TRUE)
aih_cid <- aih_cid[, -9]
aih_cid <- aih_cid[, -c(6:7)]
aih_cid <- aih_cid[, -c(2:4)]

aih_cid$soma <- (aih_cid$soma / 1000000)


kelly <- kelly.colors(20)
kelly[1] <- "#e25822"  # Choosing a darker color for Kelly's index 1 (previously white)


# Criando o vetor de códigos únicos
unique_codes <- unique(aih_cid$DIAG_PRINC)

# Aplicar a paleta modificada
color_mapping <- setNames(kelly[seq_along(unique_codes)], unique_codes)


#Plotting the Figure 1 from the article
plt <- ggplot(aih_cid, aes(x = soma, y = n.y, color = DIAG_PRINC, label = DIAG_PRINC)) +
  geom_point(size = 2) +
  scale_color_manual(values = color_mapping, guide = "none") + 
  labs(x = "Custo das internações hospitalares neonatais em milhões de Reais (R$)", y = "Número de registros") +
  theme_minimal() +
  
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ".", decimal.mark = ","), trans = 'log2') +
  
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ".", decimal.mark = ","), trans = 'log2', breaks = seq(14000, 605000, length.out = 5)) +
  
  theme(
    axis.title.x = element_text(size = 8, margin = margin(t = 20)),  
    axis.title.y = element_text(size = 8, margin = margin(r = 20)),
    axis.text.x = element_text(size = 7),  
    axis.text.y = element_text(size = 7),
    legend.position = "none"  
  ) +
  geom_text_repel(
    aes(label = DIAG_PRINC),
    size = 3,
    box.padding = 0.3,
    color = 'gray0',
    max.overlaps = 50
  )


# Visualizing the plot
print(plt)

# Saving the plot
ggsave("cid_custo.jpg", plot = plt, width = 9, height = 6, dpi = 800)
