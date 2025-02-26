library(tidyverse)


# Analisando os dados:__________________________________________________________



  ##Distribuição média, mediana...______________________________________________

info_val_tot <- descriptive_uniq(dados_sih_neo, val_tot_corrigido) # Responde: "Qual é a média, mediana, max, mix... do valor TOTAL considerando TODAS as AIHs?"
info_val_uti <- descriptive_uniq_icu(dados_sih_neo, val_uti_corrigido) # Responde: "Qual é a média, mediana, max, mix... do valor de UTI considerando apenas as AIHs que usaram UTI?"

info_val_tot <- descriptive_uniq(dados_sih_neo, val_tot_int)
info_val_uti <- descriptive_uniq_icu(dados_sih_neo, val_uti_int)


      #Distribuição média, mediana... agrupadas
  # Por ano
tot_ano <- descriptive(dados_sih_neo, ANO_CMPT, val_tot_corrigido) # Responde: "Qual é a média, mediana, max, mix... do valor do custo total por ano?"
tot_ano <- descriptive(dados_sih_neo, ANO_CMPT, val_tot_int)
uti_ano <- descriptive_icu(dados_sih_neo, USO_UTI, ANO_CMPT, val_uti_corrigido) # Responde: "Qual é a média, mediana, max, mix... do valor de UTI considerando apenas as AIHs que usaram UTI?"
uti_ano <- descriptive_icu(dados_sih_neo, USO_UTI, ANO_CMPT, val_uti_int)

  # Por região
tot_reg <- descriptive(dados_sih_neo, Regiao, val_tot_corrigido) # Responde: "Qual é a média, mediana, max, mix... do valor do custo total por região?"
uti_reg <- descriptive_icu(dados_sih_neo, USO_UTI, Regiao, val_uti_corrigido)

tot_reg <- descriptive(dados_sih_neo, Regiao, val_tot_int)
uti_reg <- descriptive_icu(dados_sih_neo, USO_UTI, Regiao, val_uti_int)

  # Por Unidade Federativa
tot_uf <- descriptive(dados_sih_neo, REG_ESTAB, val_tot_corrigido) # Responde: "Qual é a média, mediana, max, mix... do valor do custo total por UF?"
uti_uf <- descriptive_icu(dados_sih_neo, USO_UTI, REG_ESTAB, val_uti_corrigido)

  # Por quem usou UTI ou não
tot_uti <- descriptive(dados_sih_neo, USO_UTI, VAL_TOT)

  # Por diagnóstico
tot_cid <- descriptive(dados_sih_neo, DIAG_PRINC, val_tot_corrigido)
tot_cid_int <- descriptive(dados_sih_neo, DIAG_PRINC, val_tot_int)
uti_cid <- descriptive_icu(dados_sih_neo, USO_UTI, DIAG_PRINC, val_uti_corrigido)
uti_cid_int <- descriptive_icu(dados_sih_neo, USO_UTI, DIAG_PRINC, val_uti_int)

# Criando uma variável que diz quais os CIDs que devo manter na tabela quando eu chamá-lo
# Estes CIDs são os que apresentam maior custo total
top20_cid <- c('P073', 'P220', 'P229', 'P228', 'P072', 'P285', 'A419', 'P071', 'P599', 'P221', 'P050',	'P210',	'P219',	'P289',	'P369',	'P200',	'P070',	'P399',	'P051',	'A499')


    tot_cid <- tot_cid[tot_cid$DIAG_PRINC %in% top20_cid, ] # Mantendo somente os top 10 CIDs que escolhi na variável
    uti_cid <- uti_cid[uti_cid$DIAG_PRINC %in% top20_cid, ]
    tot_cid_int <- tot_cid_int[tot_cid_int$DIAG_PRINC %in% top20_cid, ] 
    uti_cid_int <- uti_cid_int[uti_cid_int$DIAG_PRINC %in% top20_cid, ]

    tot_cid <- tot_cid[, -c(4:8)]
    tot_cid_int <- tot_cid_int[, -c(4:8)]
    uti_cid <- uti_cid[, -c(4:8)]
    uti_cid_int <- uti_cid_int[, -c(4:8)]
    
costs <- tot_cid %>%
      left_join(tot_cid_int, by = "DIAG_PRINC") %>%
      left_join(uti_cid, by = "DIAG_PRINC") %>%
      left_join(uti_cid_int, by = "DIAG_PRINC")




    ##Frequência de AIHs e %____________________________________________________

aih_ano <- tab_freq(dados_sih_neo, ANO_CMPT) # Responde: "Qual é o n (frequência) e a porcentagem (%) de registros por ano?"
aih_reg <- tab_freq(dados_sih_neo, Regiao) # Responde: "Qual é o n (frequência) e a porcentagem (%) de registros por região geográfica?"
aih_uf <- tab_freq(dados_sih_neo, REG_ESTAB) # Responde: "Qual é o n (frequência) e a porcentagem (%) de registros por unidade federativa do Brasil?"
aih_uti <- tab_freq(dados_sih_neo, USO_UTI) # Responde: "Qual é o n (frequência) e a porcentagem (%) de registros por quem usou ou não UTI?"
aih_obito <- tab_freq(dados_sih_neo, MORTE) # Responde: "Qual é o n (frequência) e a porcentagem (%) de registros por quem veio a óbito ou não?"
aih_procedimento <- tab_freq(dados_sih_neo, DS_REGRA) # Responde: "Qual é o n (frequência) e a porcentagem (%) de registros por procedimento realizado?"
aih_cid <- tab_freq(dados_sih_neo, DIAG_PRINC) # Responde: "Qual é o n (frequência) e a porcentagem (%) de registros por diagnóstico CID-10?"
    aih_cid <- aih_cid[aih_cid$DIAG_PRINC %in% top20_cid, ]


    #Frequência de AIHs e % agrupados 

procedimento_ano <-tab_freq_group(dados_sih_neo, ANO_CMPT, DS_REGRA)

cid_ano <- tab_freq_group(dados_sih_neo, DIAG_PRINC, ANO_CMPT)
    cid_ano <- cid_ano[cid_ano$DIAG_PRINC %in% top20_cid, ]
    cid_ano <- cid_ano[-4]
    cid_ano <- cid_ano %>% pivot_wider(names_from = ANO_CMPT, values_from = n) #PIVOTEANDO

aih_reg_uti <- tab_freq_variavel(dados_sih_neo, Regiao, USO_UTI) # Responde "Qual o n e % de registros que usaram UTI por região?"

cid_reg <- tab_freq_variavel(dados_sih_neo, Regiao, DIAG_PRINC)
      cid_reg <- cid_reg[cid_reg$DIAG_PRINC %in% top20_cid, ]

aih_cid <- tab_freq_variavel(dados_sih_neo, ANO_CMPT, DIAG_PRINC) # Responde: "Qual é o n (frequência) e a porcentagem (%) de registros por diagnóstico CID-10 EM CADA ANO?"
      aih_cid <- aih_cid[aih_cid$DIAG_PRINC %in% top20_cid, ] # Mantendo apenas os 10 CIDs identificados (anteriormente) como os mais custosos

      
      
#_______________________________________________________________________________
      
      # Custos__________________________________________________________________
      

#Custo por CID agrupado por outra variável:

    # Agrupado por ano

tot_cid_ano <- dados_sih_neo %>%
  group_by(DIAG_PRINC, ANO_CMPT) %>% 
  summarise(somaTOT = sum(val_tot_corrigido)) # Responde: "Qual foi a média, DP e soma total de todos os custos totais de AIHs para cada CID em cada ano?"

uti_cid_ano <- dados_sih_neo %>%
  group_by(DIAG_PRINC, ANO_CMPT) %>% 
  summarise(somaUTI = sum(val_uti_corrigido))

  tot_cid_ano <- tot_cid_ano[tot_cid_ano$DIAG_PRINC %in% top20_cid, ] # Mantendo somente os top 10 CIDs mais custosos
  tot_cid_ano <- tot_cid_ano %>% pivot_wider(names_from = ANO_CMPT, values_from = somaTOT) #PIVOTEANDO
  uti_cid_ano <- uti_cid_ano[uti_cid_ano$DIAG_PRINC %in% top20_cid, ]
  uti_cid_ano <- uti_cid_ano %>% pivot_wider(names_from = ANO_CMPT, values_from = somaUTI) #PIVOTEANDO
  
  
  
# Custo de cada CID a cada 1.000 nascidos vivos agrupado por região
  
custo_cid_reg <- dados_sih_neo %>%
    group_by(DIAG_PRINC, Regiao) %>% 
    summarise(custo = sum(val_tot_corrigido)) 
  
  custo_cid_reg <- custo_cid_reg[custo_cid_reg$DIAG_PRINC %in% top20_cid, ] # Mantendo somente os top 10 CIDs que escolhi na variável
  
  
# Número de nascidos vivos no sistema público por região
  live_births <- c(
    "Centro-Oeste" = 1164323,
    "Nordeste" = 5244313,
    "Norte" = 2284373,
    "Sudeste" = 4341985,
    "Sul" = 910109)
  
# Adicionando a coluna com o custo por 1000 nascidos vivos
  custo_cid_reg <- custo_cid_reg %>%
    mutate(val_per_live_births = 1000 * (custo / live_births[Regiao]))
  
custo_cid_reg <- custo_cid_reg[, -3]
  
custo_cid_reg <- custo_cid_reg %>% pivot_wider(names_from = Regiao, values_from = val_per_live_births) #PIVOTEANDO



# Custo a cada 1.000 nascidos vivos agrupado por UF

custo_ufs <- dados_sih_neo %>%
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



#_______________________________________________________________________________


# Custo a cada 1.000 nascidos vivos agrupado por UF e CID-10

custos_ufs <- dados_sih_neo %>%
  group_by(REG_ESTAB, DIAG_PRINC) %>% 
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
custos_ufs <- custos_ufs %>%
  mutate(n_mil = 1000 * (n / live_births_uf[REG_ESTAB]),
         tot_mil = 1000 * (custo_tot / live_births_uf[REG_ESTAB]),
         uti_mil = 1000 * (custo_uti / live_births_uf[REG_ESTAB]))

# Formatando as colunas numéricas sem casas decimais
custos_ufs <- custos_ufs %>%
  mutate(
    n_mil = format(round(n_mil), big.mark = ".", decimal.mark = ",", scientific = FALSE),
    tot_mil = format(round(tot_mil), big.mark = ".", decimal.mark = ",", scientific = FALSE),
    uti_mil = format(round(uti_mil), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  )






  
#_______________________________________________________________________________
  
  
    # Uso UTI:__________________________________________________________________

uso_uti <- tab_freq(dados_sih_neo, USO_UTI)
  
# Qual condição de saíde tem mais registros com uso de UTI?
uso_uti_cid <- tab_freq_group(dados_sih_neo, USO_UTI, DIAG_PRINC)
  uso_uti_cid <- uso_uti_cid[uso_uti_cid$DIAG_PRINC %in% top10_cid, ] # Mantendo somente os top 10 CIDs mais custosos

uso_uti_cid <- dados_sih_neo %>%
  filter(USO_UTI == "Sim") %>%
  group_by(DIAG_PRINC) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(prop = round(prop.table(n) * 100, 3))
  
#Uso de UTI por ano
uti_ano <- tab_freq_variavel(dados_sih_neo, ANO_CMPT, USO_UTI)
  

  
#_______________________________________________________________________________


      # Óbitos:_________________________________________________________________



# Quantidade e proporção de óbitos para cada CID por ano
obito_cid_ano <- tab_freq_group2(dados_sih_neo, MORTE, ANO_CMPT, DIAG_PRINC) 



# Quantidade e proporção de óbitos para cada região/UF/ano/DIAGNÓSTICO
obito_reg <- tab_freq_group(dados_sih_neo, MORTE, Regiao)
obito_uf <- tab_freq_group(dados_sih_neo, MORTE, REG_ESTAB)
obito_ano <- tab_freq_group(dados_sih_neo, MORTE, ANO_CMPT)
obito_cid <- tab_freq_group(dados_sih_neo, MORTE, DIAG_PRINC)
    obito_cid <- obito_cid[obito_cid$DIAG_PRINC %in% top20_cid, ]



#_______________________________________________________________________________


    # Permanência hospitalar/diárias:___________________________________________



#Quantidade de diárias  (geral)

diarias <- descriptive_uniq(dados_sih_neo, DIAS_PERM)


  #por ano
diarias_anos <- descriptive(dados_sih_neo, ANO_CMPT, DIAS_PERM)

 
  #por diagnóstico
diarias_cid <- diarias_anos <- descriptive(dados_sih_neo, DIAG_PRINC, DIAS_PERM)



#Diárias UTI (somente UTI)

diarias_uti <- descriptive_uniq_icu(dados_sih_neo, UTI_MES_TO)


#_______________________________________________________________________________




# Apagar os objetos criados com exceção do df base ('dados_sih_neo')


# Obtém uma lista de todos os dataframes disponíveis
lista_dataframes <- ls()

# Remove todos os dataframes, exceto 'dados_sih_neo'
for (df in lista_dataframes) {
  if (df != 'dados_sih_neo') {
    rm(list = df)
  }
}

