# Instalação e carregamento dos pacotes

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(read.dbc, remotes, tidyverse)

remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)



# Baixando e processando a base SINASC 2011 A 2013

dados_1 <- read_csv("DADOS_SINASC_1.csv")
dados_2 <- read_csv("DADOS_SINASC_2.csv")
dados_3 <- read_csv("DADOS_SINASC_3.csv")
dados_4 <- read_csv("DADOS_SINASC_4.csv")



## Selecionar as variáveis de interesse, de acordo com o nome delas no banco e aplicando a função de processamento do pacote microdatasus:

dados_1 <- dados_1 |> select(c("CONSULTAS", "PESO", "CODMUNNASC", "IDADEMAE", "GESTACAO", "PARTO", "DTNASC", "CODESTAB", "SEMAGESTAC", "CONSPRENAT", "ESTCIVMAE", "ESCMAE", "SEXO", "RACACOR"))
dados_1 <- microdatasus::process_sinasc(dados_1)

dados_2 <- dados_2 |> select(c("CONSULTAS", "PESO", "CODMUNNASC", "IDADEMAE", "GESTACAO", "PARTO", "DTNASC", "CODESTAB", "SEMAGESTAC", "CONSPRENAT", "ESTCIVMAE", "ESCMAE", "SEXO", "RACACOR"))
dados_2 <- microdatasus::process_sinasc(dados_2)

dados_3 <- dados_3 |> select(c("CONSULTAS", "PESO", "CODMUNNASC", "IDADEMAE", "GESTACAO", "PARTO", "DTNASC", "CODESTAB", "SEMAGESTAC", "CONSPRENAT", "ESTCIVMAE", "ESCMAE", "SEXO", "RACACOR"))
dados_3 <- microdatasus::process_sinasc(dados_3)

dados_4 <- dados_4 |> select(c("CONSULTAS", "PESO", "CODMUNNASC", "IDADEMAE", "GESTACAO", "PARTO", "DTNASC", "CODESTAB", "SEMAGESTAC", "CONSPRENAT", "ESTCIVMAE", "ESCMAE", "SEXO", "RACACOR"))
dados_4 <- microdatasus::process_sinasc(dados_4)




#Unindo as bases
dados_sinasc <- bind_rows(dados_1, dados_2, dados_3, dados_4)


rm(dados_1, dados_2, dados_3, dados_4)
