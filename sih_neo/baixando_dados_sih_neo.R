# Installing and loading packages

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(read.dbc, remotes, tidyverse)

remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)


# Baixando os df de parto dos arquivos previamente baixados pelo site do datasus
  # e filtrados pela idade no TabWin (0 - 27 dias de vida)
sih11a15 <- read_csv("sih_neonatal_11a15.csv")
sih16a22 <- read_csv("sih_neonatal_16a22.csv")



## Selecionar as variáveis de interesse, de acordo com o nome delas no banco
sih11a15 <- sih11a15 |> select(c("N_AIH", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "QT_DIARIAS", "DIAS_PERM", "UTI_MES_TO", "UTI_INT_TO", "NASC", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "MORTE"))
sih16a22 <- sih16a22 |> select(c("N_AIH", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "QT_DIARIAS", "DIAS_PERM", "UTI_MES_TO", "UTI_INT_TO", "NASC", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "MORTE"))



# Juntando tudo e processando a base
dados_sih_neo <- bind_rows(sih11a15, sih16a22)
dados_sih_neo <- microdatasus::process_sinasc(dados_sih_neo)


# Deletando os df isolados
rm(sih11a15, sih16a22)


## Salvar o banco reduzido (com apenas as variáveis de interesse selecionadas):
write.csv2(dados_sih_neo, "Banco_SIH_neo.csv", row.names = F)

