# Instalação e carregamento dos pacotes

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(read.dbc, remotes, tidyverse)

remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)


# Baixando os df de parto dos arquivos previamente baixados pelo site do datasus
  # e filtrados pelo TabWin considerando os códigos de procedimento de parto

sih2011 <- read_csv("2011_parto.csv")
sih2012 <- read_csv("2012_parto.csv")
sih2013 <- read_csv("2013_parto.csv")
sih2014 <- read_csv("2014_parto.csv")
sih2015 <- read_csv("2015_parto.csv")
sih2016 <- read_csv("2016_parto.csv")
sih2017 <- read_csv("2017_parto.csv")
sih2018 <- read_csv("2018_parto.csv")
sih2019 <- read_csv("2019_parto.csv")
sih2020 <- read_csv("2020_parto.csv")
sih2021 <- read_csv("2021_parto.csv")
sih2022 <- read_csv("2022_parto.csv")


## Selecionar as variáveis de interesse, de acordo com o nome delas no banco
sih2011 <- sih2011 |> select(c("N_AIH", "GESTRISCO", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "DIAS_PERM","UTI_MES_TO", "QT_DIARIAS", "MORTE"))
sih2012 <- sih2012 |> select(c("N_AIH", "GESTRISCO", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "DIAS_PERM","UTI_MES_TO", "QT_DIARIAS", "MORTE"))
sih2013 <- sih2013 |> select(c("N_AIH", "GESTRISCO", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "DIAS_PERM","UTI_MES_TO", "QT_DIARIAS", "MORTE"))
sih2014 <- sih2014 |> select(c("N_AIH", "GESTRISCO", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "DIAS_PERM","UTI_MES_TO", "QT_DIARIAS", "MORTE"))
sih2015 <- sih2015 |> select(c("N_AIH", "GESTRISCO", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "DIAS_PERM","UTI_MES_TO", "QT_DIARIAS", "MORTE"))
sih2016 <- sih2016 |> select(c("N_AIH", "GESTRISCO", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "DIAS_PERM","UTI_MES_TO", "QT_DIARIAS", "MORTE"))
sih2017 <- sih2017 |> select(c("N_AIH", "GESTRISCO", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "DIAS_PERM","UTI_MES_TO", "QT_DIARIAS", "MORTE"))
sih2018 <- sih2018 |> select(c("N_AIH", "GESTRISCO", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "DIAS_PERM","UTI_MES_TO", "QT_DIARIAS", "MORTE"))
sih2019 <- sih2019 |> select(c("N_AIH", "GESTRISCO", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "DIAS_PERM","UTI_MES_TO", "QT_DIARIAS", "MORTE"))
sih2020 <- sih2020 |> select(c("N_AIH", "GESTRISCO", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "DIAS_PERM","UTI_MES_TO", "QT_DIARIAS", "MORTE"))
sih2021 <- sih2021 |> select(c("N_AIH", "GESTRISCO", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "DIAS_PERM","UTI_MES_TO", "QT_DIARIAS", "MORTE"))
sih2022 <- sih2022 |> select(c("N_AIH", "GESTRISCO", "UF_ZI", "ANO_CMPT", "DT_INTER", "DT_SAIDA", "IDENT", "MARCA_UTI", "PROC_REA", "VAL_SH", "VAL_SP", "VAL_TOT", "VAL_UTI", "US_TOT", "DIAG_PRINC", "DIAG_SECUN", "MUNIC_MOV", "IDADE", "COD_IDADE", "DIAS_PERM","UTI_MES_TO", "QT_DIARIAS", "MORTE"))


# Juntando tudo
dados_sih <- bind_rows(sih2011, sih2012, sih2013, sih2014, sih2015, sih2016, sih2017, sih2018, sih2019, sih2020, sih2021, sih2022)

# Deletando os df isolados
rm(sih2011, sih2012, sih2013, sih2014, sih2015, sih2016, sih2017, sih2018, sih2019, sih2020, sih2021, sih2022)


## Salvar o banco reduzido (com apenas as variáveis de interesse selecionadas):
write.csv2(dados_sih, "Banco_SIH_parto.csv", row.names = F)

