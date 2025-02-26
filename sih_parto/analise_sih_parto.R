library(tidyverse)
library(read.dbc)
library(readxl)
library(flextable)
library(officer)


# Funções para calcular a média, a mediana, o desvio padrão, o mínimo e o máximo, a soma..:


## Função da análise descritiva de uma variável agrupada por outra
descriptive <- function(dados, grupo, vd) {
  dados %>%
    group_by({{grupo}}) %>%
    summarise(
      n = n(),
      media = mean({{vd}}, na.rm = TRUE),
      desvio_padrao = sd({{vd}}, na.rm = TRUE),
      soma = sum({{vd}}, na.rm = TRUE),
      IC_inf = media - qt(0.975, df = n - 1) * (desvio_padrao / sqrt(n)),
      IC_sup = media + qt(0.975, df = n - 1) * (desvio_padrao / sqrt(n))
    )
}


## Função de análise descritiva de uma variável agrupada por outra - somente para os registros admitidos na UTI
descriptive_icu <- function(dados, filtro, grupo, vd) {
  dados %>%
    filter({{filtro}} == "Sim") %>%
    group_by({{grupo}}) %>%
    summarise(
      n = n(),
      media = mean({{vd}}, na.rm = TRUE),
      desvio_padrao = sd({{vd}}, na.rm = TRUE),
      soma = sum({{vd}}, na.rm = TRUE),
      IC_inf = media - qt(0.975, df = n - 1) * (desvio_padrao / sqrt(n)),
      IC_sup = media + qt(0.975, df = n - 1) * (desvio_padrao / sqrt(n))
    )
}


## Função para análise descritiva de apenas uma variável
descriptive_uniq <- function(dados, vd) {
  n <- nrow(dados %>% filter(!is.na({{vd}})))
  media <- mean(dados[[deparse(substitute(vd))]], na.rm = TRUE)
  desvio_padrao <- sd(dados[[deparse(substitute(vd))]], na.rm = TRUE)
  
  dados %>%
    summarise(
      media = mean({{vd}}, na.rm = TRUE),
      desvio_padrao = sd({{vd}}, na.rm = TRUE),
      soma = sum({{vd}}, na.rm = TRUE),
      IC_inf = media - qt(0.975, df = n - 1) * (desvio_padrao / sqrt(n)),
      IC_sup = media + qt(0.975, df = n - 1) * (desvio_padrao / sqrt(n))
    )
}



## Função para análise descritiva de apenas uma variável - somente para os registros admitidos na UTI
descriptive_uniq_icu <- function(dados, vd) {
  dados_filtrados <- dados %>%
    filter(USO_UTI == "Sim")
  
  n <- nrow(dados_filtrados)  # Tamanho da amostra
  mean_vd <- mean(dados_filtrados[[deparse(substitute(vd))]], na.rm = TRUE)
  sd_vd <- sd(dados_filtrados[[deparse(substitute(vd))]], na.rm = TRUE)
  
  error_margin <- qt(0.975, df = n - 1) * (sd_vd / sqrt(n))  # Margem de erro
  
  dados_filtrados %>%
    summarise(
      media = mean({{vd}}, na.rm = TRUE),
      desvio_padrao = sd({{vd}}, na.rm = TRUE),
      soma = sum({{vd}}, na.rm = TRUE),
      IC_inf = mean_vd - error_margin,
      IC_sup = mean_vd + error_margin
    )
}




# Analisando os dados:

    #Frequência de AIHs e % 
aih_ano <- tab_freq(dados_sih, ANO_CMPT) # Responde: "Quantas AIHs de parto foram registradas por ano e quanto cada ano representa do todo?"
aih_reg <- tab_freq(dados_sih, Regiao)
aih_uf <- tab_freq(dados_sih, REG_ESTAB)
aih_uti <- tab_freq(dados_sih, USO_UTI)
aih_morte <- tab_freq(dados_sih, MORTE)
aih_procedimento <- tab_freq(dados_sih, DS_REGRA)
aih_viaparto <- tab_freq(dados_sih, VIA_PARTO)
aih_cid <- tab_freq(dados_sih, DIAG_PRINC)
aih_risco <- tab_freq(dados_sih, GESTRISCO)
aih_idade <- tab_freq_grupo(dados_sih, IDADE, COD_IDADE)


    # Dados de média, mediana... dos custos, agrupadas por outra variável
val_tot_ano <- descriptive(dados_sih, ANO_CMPT, val_tot_corrigido)
val_uti_ano <- descriptive_icu(dados_sih, USO_UTI, ANO_CMPT, val_uti_corrigido)

val_tot_ano <- descriptive(dados_sih, ANO_CMPT, val_tot_int)
val_uti_ano <- descriptive_icu(dados_sih, USO_UTI, ANO_CMPT, val_uti_int)

val_tot_reg <- descriptive(dados_sih, Regiao, val_tot_corrigido)
val_uti_reg <- descriptive_icu(dados_sih, USO_UTI, Regiao, val_uti_corrigido)

val_tot_reg <- descriptive(dados_sih, Regiao, val_tot_int)
val_uti_reg <- descriptive_icu(dados_sih, USO_UTI, Regiao, val_uti_int)

val_tot_uf <- descriptive(dados_sih, REG_ESTAB, val_tot_corrigido) # Responde: "Qual a média, mediana[...] dos custos total da aih para cada UF?"
val_uti_uf <- descriptive_icu(dados_sih, USO_UTI, REG_ESTAB, val_uti_corrigido)

val_tot_uf <- descriptive(dados_sih, REG_ESTAB, val_tot_int) 
val_uti_uf <- descriptive_icu(dados_sih, USO_UTI, REG_ESTAB, val_uti_int)

val_tot_via <- descriptive(dados_sih, VIA_PARTO, val_tot_corrigido)
val_uti_via <- descriptive_icu(dados_sih, USO_UTI, VIA_PARTO, val_uti_corrigido) # Responde: "Qual a média, mediana[...] dos custos de UTI para cada CID/diagnóstico?"

val_tot_via <- descriptive(dados_sih, VIA_PARTO, val_tot_int)
val_uti_via <- descriptive_icu(dados_sih, USO_UTI, VIA_PARTO, val_uti_int)


  #Idade das parturientes
idades <- dados_sih %>%
  summarise(media = mean(IDADE),
          desvio_padrao = sd(IDADE),
          mediana = median(IDADE),
          q1 = quantile(IDADE, p = 0.25),
          q3 = quantile(IDADE, p = 0.75),
          min = min(IDADE),
          max = max(IDADE))


    #Frequência de uma variável agrupada por outra
obito_ano <- tab_freq_certo(dados_sih, ANO_CMPT, MORTE) # Responde: "Qual o numero e % de óbitos por ano?"
obito_reg <- tab_freq_certo(dados_sih, Regiao, MORTE)
obito_uf <- tab_freq_certo(dados_sih, REG_ESTAB, MORTE)
procedimento_ano <- tab_freq_certo(dados_sih, ANO_CMPT, VIA_PARTO)
via_reg <- tab_freq_certo(dados_sih, Regiao, VIA_PARTO)
via_uf <- tab_freq_certo(dados_sih, REG_ESTAB, VIA_PARTO)



  ## Frequência e n agrupado por 2 variáveis
uti_via <- tab_freq_grupo(dados_sih, VIA_PARTO, USO_UTI) # Responde: "quantas AIHs utilizaram UTI ou não para cada via de parto, e quantos % isso representa?"
uti_ano <- tab_freq_grupo(dados_sih, USO_UTI, ANO_CMPT) #por ano
uti_uf <- dados_sih %>%
  group_by(REG_ESTAB, USO_UTI) %>%
  count() #por UF
uti_reg <- tab_freq_grupo(dados_sih, USO_UTI, Regiao) #por ano





#_______________________________________________________________________________


# Permanência hospitalar/diárias:___________________________________________

#Permanência hospitalar (engloba tudo = geral + UTI)

permanencia <- descriptive_uniq(dados_sih, DIAS_PERM)

#por ano
diarias_anos <- descriptive(dados_sih, ANO_CMPT, DIAS_PERM)

#por uso de UTI (sim ou não)
diarias_uti <- descriptive(dados_sih, USO_UTI, DIAS_PERM)

#por diagnóstico
diarias_cid <- diarias_anos <- descriptive(dados_sih, DIAG_PRINC, DIAS_PERM)

#por óbito
dias_obito <- descriptive(dados_sih, MORTE, DIAS_PERM)

#por via de parto
diarias_via <- descriptive(dados_sih, VIA_PARTO, DIAS_PERM)


#Diárias de UTI (somente UTI)

dias_uti <- descriptive_uniq_uti(dados_sih, UTI_MES_TO)

#por ano
diarias_anos <- descriptive_icu(dados_sih, USO_UTI, ANO_CMPT, UTI_MES_TO)

#por óbito
dias_obito <- descriptive_icu(dados_sih, USO_UTI, MORTE, UTI_MES_TO)

#por via de parto
diarias_via <- descriptive_icu(dados_sih, USO_UTI, VIA_PARTO, UTI_MES_TO)


#_______________________________________________________________________________


# Diagnósticos principais

aih_cid <- tab_freq(dados_sih, DIAG_PRINC) # n
tot_cid <- descriptive(dados_sih, DIAG_PRINC, val_tot_corrigido) # custo total por diagnóstico
uti_cid <- descriptive_icu(dados_sih, USO_UTI, DIAG_PRINC, val_uti_corrigido) # custo com UTI por diagnóstico


# Dados de média, DP, IC95%... do custo total e de UTI para todos os registros
tot_aih <- descriptive_uniq(dados_sih, val_tot_corrigido)
uti_aih <- descriptive_uniq_icu(dados_sih, val_uti_corrigido) # Considerando apenas quem usou UTI



#_______________________________________________________________________________
# Custo a cada 1.000 nascidos vivos agrupado por UF

custo_ufs <- dados_sih %>%
  group_by(REG_ESTAB) %>% 
  summarise(n = n(),
            custo_tot = sum(val_tot_corrigido),
            custo_uti = sum(val_uti_corrigido)) 


# Número de nascidos vivos no sistema público por região
live_births_uf <- c(
  "Acre"	= 146975,
  "Alagoas"	= 153619,
  "Amapá"	= 147998,
  "Amazonas"	= 745667,
  "Bahia" =	1344625,
  "Ceará"	= 693362,
  "Distrito Federal"	= 492952,
  "Espírito Santo"	= 186076,
  "Goiás"	= 374267,
  "Maranhão"	= 1045885,
  "Mato Grosso"	= 180372,
  "Mato Grosso do Sul" = 157487,
  "Minas Gerais"	= 606794,
  "Paraná"	= 471817,
  "Paraíba"	= 463686,
  "Pará"	= 845747,
  "Pernambuco" =	949272,
  "Piauí"	= 457141,
  "Rio Grande do Norte"	= 334271,
  "Rio Grande do Sul"	= 182678,
  "Rio de Janeiro"	= 1400188,
  "Rondônia" =	225006,
  "Roraima"	= 124613,
  "Santa Catarina"	= 322311,
  "Sergipe"	= 108609,
  "São Paulo" =	2457430,
  "Tocantins" =	184655)

# Adicionando a coluna com o custo por 1000 nascidos vivos
custo_ufs <- custo_ufs %>%
  mutate(n_mil = 1000 * (n / live_births_uf[REG_ESTAB]),
         tot_mil = 1000 * (custo_tot / live_births_uf[REG_ESTAB]),
         uti_mil = 1000 * (custo_uti / live_births_uf[REG_ESTAB]))

# Formatando as colunas numéricas sem casas decimais
custo_ufs <- custo_ufs %>%
  mutate(
    n_mil = format(round(n_mil), big.mark = ".", decimal.mark = ",", scientific = FALSE),
    tot_mil = format(round(tot_mil), big.mark = ".", decimal.mark = ",", scientific = FALSE),
    uti_mil = format(round(uti_mil), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  )






# Apagar os objetos criados com exceção do df base ('dados_sih')


# Obtém uma lista de todos os dataframes disponíveis
lista_dataframes <- ls()

# Remove todos os dataframes, exceto 'dados_sih'
for (df in lista_dataframes) {
  if (df != 'dados_sih') {
    rm(list = df)
  }
}
