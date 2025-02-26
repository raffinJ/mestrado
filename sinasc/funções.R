########### CRIANDO FUNÇÕES PARA FACILITAR AS ANÁLISES ############


### Variáveis CATEGÓRICAS


## Função é para criar uma tabela com o n total de cada variável e as 
# proporções dentre as classificações da variável (somente variável categóricas)

tab_freq <- function(dados, variavel){
  
  tab <- dados |> 
    group_by({{variavel}}) |> 
    count() |> 
    ungroup() |> 
    mutate(porc = 100*n/sum(n))
  
  return(tab)
  
}


tab_freq_grupo <- function(dados, variavel, grupo){
  
  tab <- dados |>
    group_by({{grupo}}, {{variavel}}) |> 
    count() |> 
    ungroup() |> 
    group_by({{grupo}}) |> 
    mutate(porc = 100*n/sum(n))
  
  return(tab)
  
}


tab_numerica <- function(dados, variavel){
  dados %>%
    summarise(n = n(),
              media = mean({{variavel}}, na.rm = T),
              DP = sd({{variavel}}, na.rm = T),
              Mediana = median({{variavel}}, na.rm = T),
              Q1 = quantile({{variavel}}, na.rm = T, probs = 0.25),
              Q3 = quantile({{variavel}}, na.rm = T, probs = 0.75),
              Mínimo = min({{variavel}}, na.rm = T),
              Máximo = max({{variavel}}, na.rm = T),
              IC_inf = media - qt(0.975, df = n - 1) * (DP / sqrt(n)),
              IC_sup = media + qt(0.975, df = n - 1) * (DP / sqrt(n)))
}

tab_numerica_grupo <- function(dados, variavel, grupo){
  dados %>%
    group_by({{grupo}}) %>%
    summarise(n = n(),
              media = mean({{variavel}}, na.rm = T),
              DP = sd({{variavel}}, na.rm = T),
              Mediana = median({{variavel}}, na.rm = T),
              Q1 = quantile({{variavel}}, na.rm = T, probs = 0.25),
              Q3 = quantile({{variavel}}, na.rm = T, probs = 0.75),
              Mínimo = min({{variavel}}, na.rm = T),
              Máximo = max({{variavel}}, na.rm = T),
              IC_inf = media - qt(0.975, df = n - 1) * (DP / sqrt(n)),
              IC_sup = media + qt(0.975, df = n - 1) * (DP / sqrt(n)))
}
