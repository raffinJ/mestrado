# Instalação e carregamento dos pacotes

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(read.dbc, remotes, tidyverse, vroom, readxl, stringi)


# remotes::install_github("rfsaldanha/microdatasus")
library(microdatasus)


### Antes de gerar as análises, precisamos padronizar/organizar os dados de 
### acordo com nossos interesses...

#Mudando tipo de variável
dados_sinasc$PESO <- as.numeric(dados_sinasc$PESO)
dados_sinasc$IDADEMAE <- as.numeric(dados_sinasc$IDADEMAE)
dados_sinasc$SEMAGESTAC <- as.numeric(dados_sinasc$SEMAGESTAC)
dados_sinasc$CONSPRENAT <- as.numeric(dados_sinasc$CONSPRENAT)



# Excluindo dados com "possíveis erros de preenchimento"
#Excluindo idades gestacionais abaixo de 23 semanas
dados_sinasc <- dados_sinasc %>%
  filter(SEMAGESTAC >= 23) %>%
  filter(PESO >= 400 & PESO <= 6000)



### Criar variável de esfera do estabelecimento (público ou privado)
esfera_estab <- read_xlsx("tb_cnes.xlsx")
colnames(esfera_estab) <- c("CODESTAB", "Privado", "Privado", "Público")
esfera_estab$Público <- ifelse(esfera_estab$Público %in% c("Sim"), c("Público"), "Privado")
esfera_estab <- esfera_estab[, -2]
esfera_estab <- esfera_estab[, -2]
colnames(esfera_estab) <- c("CODESTAB", "Esfera")


dados_sinasc <- dados_sinasc %>%
  left_join(esfera_estab, by = "CODESTAB")


#_______________________________________________________________________________


# Excluindo os registros do setor privado e mantendo somente os de esfera pública 

dados_sinasc <- dados_sinasc %>%
  filter(Esfera != 'Privado')

#_______________________________________________________________________________


#Identificando quantos registros que apresentam dados ausentes "NA"

linhas_com_NA <- sum(is.na(dados_sinasc$CONSPRENAT)) #1.384.380
linhas_com_NA <- sum(is.na(dados_sinasc$CONSULTAS)) #98.061
linhas_com_NA <- sum(is.na(dados_sinasc$PESO)) #0
linhas_com_NA <- sum(is.na(dados_sinasc$IDADEMAE)) #57
linhas_com_NA <- sum(is.na(dados_sinasc$GESTACAO)) #0
linhas_com_NA <- sum(is.na(dados_sinasc$PARTO)) #12.250
linhas_com_NA <- sum(is.na(dados_sinasc$SEMAGESTAC)) #0



# Ajustando células com dados inválidos
dados_sinasc$CONSPRENAT[dados_sinasc$CONSPRENAT == 99] <- NA #dos 1.384.380 NA, agora se tem 1.465.008



##### Adicionando as colunas de localização (município e UF) por meio do microdatasus > tabMun
cod_mun <- microdatasus::tabMun |> select(1, 4, 5) #Cria um df cod_mun com o código do município, nome do mun e UF correspondente
colnames(cod_mun) <- c("CODMUNNASC", "NOME", "UF") 
cod_mun$NOME <- stringi::stri_unescape_unicode(cod_mun$NOME)
cod_mun$UF <- stringi::stri_unescape_unicode(cod_mun$UF)
glimpse(cod_mun)
dados_sinasc$CODMUNNASC <- as.integer(dados_sinasc$CODMUNNASC)

dados_sinasc <- dplyr::left_join(dados_sinasc, cod_mun)

dados_sinasc <- dados_sinasc |> select(-CODMUNNASC)

rm(cod_mun)





#### CRIANDO A VARIÁVEL "ANO" A PARTIR DA DATA DE NASCIMENTO

dados_sinasc$ANO <- lubridate::year(dados_sinasc$DTNASC)

dados_sinasc$ANO <- factor(dados_sinasc$ANO)



#_______________________________________________________________________________


#### Criar a variável de REGIAO
dados_sinasc$REGIAO=ifelse(dados_sinasc$UF %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul"),c("Sul"),
                           ifelse(dados_sinasc$UF %in% c("São Paulo", "Rio de Janeiro", "Espírito Santo", "Minas Gerais"),c("Sudeste"),
                                  ifelse(dados_sinasc$UF %in% c("Mato Grosso", "Mato Grosso do Sul", "Goiás", "Distrito Federal"),c("Centro-Oeste"),
                                         ifelse(dados_sinasc$UF %in% c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins"),c("Norte"), "Nordeste"))))


#_______________________________________________________________________________



#### E agora a variável de classificação de IDADE GESTACIONAL de acordo com a OMS
dados_sinasc$CLASS_IG=ifelse(dados_sinasc$GESTACAO %in% c("22 a 27 semanas"),c("Prematuro extremo"),
                             ifelse(dados_sinasc$GESTACAO %in% c("28 a 31 semanas"),c("Muito prematuro"),
                                    ifelse(dados_sinasc$GESTACAO %in% c("32 a 36 semanas"),c("Prematuro moderado a tardio"),
                                           ifelse(dados_sinasc$GESTACAO %in% c("37 a 41 semanas"),c("Termo"), 
                                                  ifelse(dados_sinasc$GESTACAO %in% c("42 semanas ou mais"), c("Pós-termo"), "Ignorado")))))




#_______________________________________________________________________________

#### TRANSFORMANDO AS VARIÁVEIS "PESO" E "IDADEMAE" EM CATEGÓRICAS/CLASSIFICAÇÕES

#PESO  
# Utilizando como referência a classificação de peso ao nascer da OMS
dados_sinasc <- dados_sinasc |> 
  mutate(CLASS_PESO = case_when(
    PESO >= 2500 & PESO <= 3999 ~ "Peso adequado",
    PESO >= 1500 & PESO < 2500 ~ "Baixo peso ao nascer",
    PESO > 1000 & PESO < 1500 ~ "Muito baixo peso ao nascer",
    PESO <= 1000 ~ "Extremo baixo peso ao nascer",
    PESO >= 4000 ~ "Macrossomia",
    TRUE ~ "Ignorado"
  ))

#IDADE DA MÃE 
dados_sinasc <- dados_sinasc |> 
  mutate(CLASS_IDADEMAE = case_when(
    IDADEMAE < 20 ~ "< 20 anos",
    IDADEMAE >= 20 & IDADEMAE <= 29 ~ "20-29 anos",
    IDADEMAE >= 30 & IDADEMAE <= 35 ~ "30-35 anos",
    IDADEMAE > 35 ~ "> 35 anos",
  ))




#_______________________________________________________________________________




# Criando uma coluna de data com apenas o mês e o dia do nascimento

dados_sinasc$DTNASC <- as.Date(dados_sinasc$DTNASC, format = "%Y-%m-%d")

dados_sinasc$mes_ano <- format(dados_sinasc$DTNASC, "%Y-%m")

dados_sinasc$mes_ano <- as.factor(dados_sinasc$mes_ano)
