#Instalar o pacote microdatasus para download e leitura dos arquivos###
devtools::install_github("rfsaldanha/microdatasus")
install.packages("tidyverse")
install.packages("read.dbc")

#Pacotes para instalar
install.packages("readxl")
install.packages("foreign")
install.packages("dplyr")
install.packages("psych")

#Liberar pacotes
library(tidyverse)
library(microdatasus)
library(read.dbc)
library(readxl)
library(foreign)
library(dplyr)
library(psych)

glimpse(dados_sih)


# Iniciando com os ajustes do df com a inclusão da tabela sigtap de procedimentos
dados_sih$PROC_REA <- as.factor(dados_sih$PROC_REA) #transforma em actor os códigos de procedimento para conseguir realizar o match
TB_SIGTAP <- read.dbc("TB_SIGTAP.dbc") #baixa a tabela sigtap
dados_sih <- dados_sih %>%
  left_join(TB_SIGTAP, c("PROC_REA"="CHAVE")) #adiciona ao df primário uma nova coluna "DS_REGRA" com os nomes dos procedimentos 

# Ajustando coluna sobre gestação de risco (sim ou não)
dados_sih$GESTRISCO = ifelse(dados_sih$GESTRISCO %in% c("1"), c("Sim"), "Não")

# Adicionando coluna para saber quem fez uso de UTI ou não
dados_sih$USO_UTI = ifelse(dados_sih$MARCA_UTI %in% c("00"), c("Não"), "Sim")

# Adicionando coluna para resumir o tipo de parto
dados_sih$VIA_PARTO = ifelse(dados_sih$DS_REGRA %in% c("PARTO CESARIANO", "PARTO CESARIANO C/ LAQUEADURA TUBARIA", "PARTO CESARIANO EM GESTACAO DE ALTO RISCO"),c("Parto cesariano"),
                        ifelse(dados_sih$DS_REGRA %in% c("PARTO NORMAL", "PARTO NORMAL EM CENTRO DE PARTO NORMAL (CPN)", "PARTO NORMAL EM GESTACAO DE ALTO RISCO"),c("Parto vaginal"), "Ignorado"))

# Modificando a coluna COD_IDADE que define a idade em dias, meses e anos
dados_sih <- dados_sih %>%
  mutate(COD_IDADE = case_when(
    COD_IDADE == "2" ~ "dias",
    COD_IDADE == "3" ~ "meses",
    COD_IDADE == "4" ~ "anos",
    TRUE ~ "NA"
  ))



# Modificando a coluna MORTE de numero para classificação
dados_sih$MORTE=ifelse(dados_sih$MORTE %in% c("1"), c("Óbito"), "Não")

#Criar a variavel para os estados e regioes do local de internação
dados_sih <- dados_sih %>%
  mutate(REG_ESTAB = ifelse(substr(MUNIC_MOV, 1, 2) == "11", "Rondônia",
                            ifelse(substr(MUNIC_MOV, 1, 2) == "12", "Acre", 
                                   ifelse(substr(MUNIC_MOV, 1, 2) == "13", "Amazonas",
                                          ifelse(substr(MUNIC_MOV, 1, 2) == "14", "Roraima",
                                                 ifelse(substr(MUNIC_MOV, 1, 2) == "15", "Pará",
                                                        ifelse(substr(MUNIC_MOV, 1, 2) == "16", "Amapá",
                                                               ifelse(substr(MUNIC_MOV, 1, 2) == "17", "Tocantins",
                                                                      ifelse(substr(MUNIC_MOV, 1, 2) == "21", "Maranhão",
                                                                             ifelse(substr(MUNIC_MOV, 1, 2) == "22", "Piauí",
                                                                                    ifelse(substr(MUNIC_MOV, 1, 2) == "23", "Ceará",
                                                                                           ifelse(substr(MUNIC_MOV, 1, 2) == "24", "Rio Grande do Norte",
                                                                                                  ifelse(substr(MUNIC_MOV, 1, 2) == "25", "Paraíba",
                                                                                                         ifelse(substr(MUNIC_MOV, 1, 2) == "27", "Alagoas",
                                                                                                                ifelse(substr(MUNIC_MOV, 1, 2) == "28", "Sergipe",
                                                                                                                       ifelse(substr(MUNIC_MOV, 1, 2) == "29", "Bahia",
                                                                                                                              ifelse(substr(MUNIC_MOV, 1, 2) == "31", "Minas Gerais",
                                                                                                                                     ifelse(substr(MUNIC_MOV, 1, 2) == "32", "Espírito Santo",
                                                                                                                                            ifelse(substr(MUNIC_MOV, 1, 2) == "33", "Rio de Janeiro",
                                                                                                                                                   ifelse(substr(MUNIC_MOV, 1, 2) == "35", "São Paulo",
                                                                                                                                                          ifelse(substr(MUNIC_MOV, 1, 2) == "41", "Paraná",
                                                                                                                                                                 ifelse(substr(MUNIC_MOV, 1, 2) == "42", "Santa Catarina",
                                                                                                                                                                        ifelse(substr(MUNIC_MOV, 1, 2) == "43", "Rio Grande do Sul",
                                                                                                                                                                               ifelse(substr(MUNIC_MOV, 1, 2) == "50", "Mato Grosso do Sul",
                                                                                                                                                                                      ifelse(substr(MUNIC_MOV, 1, 2) == "51", "Mato Grosso",
                                                                                                                                                                                             ifelse(substr(MUNIC_MOV, 1, 2) == "52", "Goiás",
                                                                                                                                                                                                    ifelse(substr(MUNIC_MOV, 1, 2) == "53", "Distrito Federal",
                                                                                                                                                                                                           ifelse(substr(MUNIC_MOV, 1, 2) == "00", "Ignorado/Exterior", "Pernambuco"))))))))))))))))))))))))))))

dados_sih$Regiao=ifelse(dados_sih$REG_ESTAB %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul"),c("Sul"),
                           ifelse(dados_sih$REG_ESTAB %in% c("São Paulo", "Rio de Janeiro", "Espírito Santo", "Minas Gerais"),c("Sudeste"),
                                  ifelse(dados_sih$REG_ESTAB %in% c("Mato Grosso", "Mato Grosso do Sul", "Goiás", "Distrito Federal"),c("Centro-Oeste"),
                                         ifelse(dados_sih$REG_ESTAB %in% c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins"),c("Norte"), "Nordeste"))))



##Mudando tipo de variavel
dados_sih$ANO_CMPT <- as.factor(dados_sih$ANO_CMPT)
dados_sih$VAL_SH <- as.numeric(dados_sih$VAL_SH)
dados_sih$VAL_SP <- as.numeric(dados_sih$VAL_SP)
dados_sih$VAL_TOT <- as.numeric(dados_sih$VAL_TOT)
dados_sih$VAL_UTI <- as.numeric(dados_sih$VAL_UTI)
dados_sih$US_TOT <- as.numeric(dados_sih$US_TOT)
dados_sih$QT_DIARIAS <- as.numeric(dados_sih$QT_DIARIAS)
dados_sih$DIAS_PERM <- as.numeric(dados_sih$DIAS_PERM)
dados_sih$UTI_MES_TO <- as.numeric(dados_sih$UTI_MES_TO)
dados_sih$IDADE <- as.numeric(dados_sih$IDADE)


#Ajustando os custos pela inflação (IPCA - https://www3.bcb.gov.br/CALCIDADAO/publico/corrigirPorIndice.do?method=corrigirPorIndice)

# Fatores de correção por ano para 2023
fatores_correcao <- c("2011" = 1.9999,
                      "2012" = 1.8950,
                      "2013" = 1.7916,
                      "2014" = 1.6814,
                      "2015" = 1.5219,
                      "2016" = 1.4225,
                      "2017" = 1.3837,
                      "2018" = 1.3299,
                      "2019" = 1.2878,
                      "2020" = 1.2345,
                      "2021" = 1.1148,
                      "2022" = 1.0527,
                      "2023" = 1.0000)

# Adicionando as colunas corrigidas no dataframe
dados_sih <- dados_sih %>%
  mutate(
    val_tot_corrigido = VAL_TOT * fatores_correcao[as.character(ANO_CMPT)],
    val_uti_corrigido = VAL_UTI * fatores_correcao[as.character(ANO_CMPT)],
    val_sh_corrigido = VAL_SH * fatores_correcao[as.character(ANO_CMPT)],
    val_sp_corrigido = VAL_SP * fatores_correcao[as.character(ANO_CMPT)]
  )


#Converting to Int$ using PPP of 2.44

dados_sih <- dados_sih %>%
  mutate(
    val_tot_int = val_tot_corrigido / 2.44,
    val_uti_int = val_uti_corrigido / 2.44,
    val_sh_int = val_sh_corrigido / 2.44,
    val_sp_int = val_sp_corrigido / 2.44
  )


write.csv2(dados_sih, "dados_sih.csv", row.names = F)


