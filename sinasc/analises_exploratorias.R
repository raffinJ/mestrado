# Instalação e carregamento dos pacotes

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(read.dbc, remotes, tidyverse, vroom, readxl, stringi)




glimpse(dados_sinasc)


#Nascidos vivos
nv_reg <- tab_freq(dados_sinasc, REGIAO) #Nascidos vivos por região
nv_uf <- tab_freq(dados_sinasc, UF) #Nascidos vivos por UF
nv_ano <- tab_freq(dados_sinasc, ANO) #Nascidos vivos por ano


############ INICIANDO AS ANÁLISES ##########


# Dados descritivos de variáveis numéricas

tab_peso <- tab_numerica(dados_sinasc, PESO) # peso do neonato
tab_idade_mae <- tab_numerica(dados_sinasc, IDADEMAE) #idade materna
tab_consultas <- tab_numerica(dados_sinasc, CONSPRENAT) # consultas pré-natal
tab_ig <- tab_numerica(dados_sinasc, SEMAGESTAC) # Semanas de idade gestacional ao nascimento


#_______________________________________________________________________________

# Dados descritivos de variáveis categóricas 

tab_consultas <- tab_freq(dados_sinasc, CONSULTAS) # classificação por n de consultas pré-natal
tab_gestacao <- tab_freq(dados_sinasc, GESTACAO) # classificação por n semanas de idade gestacional ao nascimento
tab_class_ig <- tab_freq(dados_sinasc, CLASS_IG) # classificação por n semanas de idade gestacional ao nascimento (OMS)
tab_parto <- tab_freq(dados_sinasc, PARTO) # via de nascimento
tab_peso_class <- tab_freq (dados_sinasc, CLASS_PESO) # classificação por peso ao nascimento
tab_idade_class <- tab_freq (dados_sinasc, CLASS_IDADEMAE) # classificação por faixa etária materna
tab_genero <- tab_freq (dados_sinasc, SEXO) # gênero do neonato
tab_racacor <- tab_freq (dados_sinasc, RACACOR) # raça/cor da mãe



# CRIANDO TABELAS COM DADOS DE FREQUENCIA PARA CADA VARIAVEL AGRUPADAS POR ALGUM FATOR: UF, REGIÃO, ANO...

        ###variáveis numéricas

#peso ao nascer
tab_peso_uf <- tab_numerica_grupo(dados_sinasc, PESO, UF)
tab_peso_reg <- tab_numerica_grupo(dados_sinasc, PESO, REGIAO)
tab_peso_ano <- tab_numerica_grupo(dados_sinasc, PESO, ANO)

#idade materna
tab_idade_mae_uf <- tab_numerica_grupo(dados_sinasc, IDADEMAE, UF)
tab_idade_mae_reg <- tab_numerica_grupo(dados_sinasc, PESO, REGIAO)
tab_idade_mae_ano <- tab_numerica_grupo(dados_sinasc, PESO, ANO)

#consultas
tab_consultas_uf <- tab_numerica_grupo(dados_sinasc, CONSPRENAT, UF)
tab_consultas_reg <- tab_numerica_grupo(dados_sinasc, CONSPRENAT, REGIAO)
tab_consultas_ano <- tab_numerica_grupo(dados_sinasc, CONSPRENAT, ANO)

#idade gestacional
tab_ig_uf <- tab_numerica_grupo(dados_sinasc, SEMAGESTAC, UF)
tab_ig_reg <- tab_numerica_grupo(dados_sinasc, SEMAGESTAC, REGIAO)
tab_ig_ano <- tab_numerica_grupo(dados_sinasc, SEMAGESTAC, ANO)


#_______________________________________________________________________________


#CATEGÓRICAS POR UF
tab_consultas_uf <- tab_freq_grupo(dados_sinasc, CONSULTAS, UF)
tab_gestacao_uf <- tab_freq_grupo(dados_sinasc, GESTACAO, UF)
tab_class_ig_uf <- tab_freq_grupo(dados_sinasc, CLASS_IG, UF)
tab_parto_uf <- tab_freq_grupo(dados_sinasc, PARTO, UF)
tab_peso_class_uf <- tab_freq_grupo(dados_sinasc, CLASS_PESO, UF)
tab_idade_class_uf <- tab_freq_grupo(dados_sinasc, CLASS_IDADEMAE, UF)

#CATEGÓRICAS POR REGIAO
tab_consultas_reg <- tab_freq_grupo(dados_sinasc, CONSULTAS, REGIAO)
tab_gestacao_reg <- tab_freq_grupo(dados_sinasc, GESTACAO, REGIAO)
tab_class_ig_reg <- tab_freq_grupo(dados_sinasc, CLASS_IG, REGIAO)
tab_parto_reg <- tab_freq_grupo(dados_sinasc, PARTO, REGIAO)
tab_peso_class_reg <- tab_freq_grupo(dados_sinasc, CLASS_PESO, REGIAO)
tab_idade_class_reg <- tab_freq_grupo(dados_sinasc, CLASS_IDADEMAE, REGIAO)

#CATEGÓRICAS POR ANO
tab_consultas_ano <- tab_freq_grupo(dados_sinasc, CONSULTAS, ANO)
tab_gestacao_ano <- tab_freq_grupo(dados_sinasc, GESTACAO, ANO)
tab_class_ig_ano <- tab_freq_grupo(dados_sinasc, CLASS_IG, ANO)
tab_parto_ano <- tab_freq_grupo(dados_sinasc, PARTO, ANO)
tab_peso_class_ano <- tab_freq_grupo(dados_sinasc, CLASS_PESO, ANO)
tab_idade_class_ano <- tab_freq_grupo(dados_sinasc, CLASS_IDADEMAE, ANO)


#_______________________________________________________________________________


# Apagar os objetos criados com exceção do df base ('dados_sinasc')


# Obtém uma lista de todos os dataframes disponíveis
lista_dataframes <- ls()

# Remove todos os dataframes, exceto 'dados_sinasc'
for (df in lista_dataframes) {
  if (df != 'dados_sinasc') {
    rm(list = df)
  }
}
