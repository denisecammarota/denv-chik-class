library(tidyverse)
library(dplyr)
library(read.dbc)
library(ggplot2)
library(geobr)
library(lubridate)


#setwd('C:/Users/denis/Documents/denv-chik-symptoms-new')
#setwd('D:/bases_novas')


# Definindo funcao extra de decodificacao (do pysus) #########
decodifica_idade_SINAN <- function(idade, unidade = "Y") {
  
  fator <- c("Y" = 1.0, "M" = 12.0, "D" = 365.0, "H" = 365 * 24.0)
  
  idade <- as.numeric(idade)
  
  idade_anos <- case_when(
    idade >= 4000 ~ idade - 4000,
    idade >= 3000 & idade < 4000 ~ (idade - 3000) / 12.0,
    idade >= 2000 & idade < 3000 ~ (idade - 2000) / 365.0,
    idade >= 1000 & idade < 2000 ~ (idade - 1000) / (365 * 24.0),
    TRUE ~ NA_real_
  )
  
  idade_dec <- idade_anos * fator[unidade]
  return(idade_dec)
}


# Carregando as bases de dados ##########################

df <- read.dbc('data/DENGBR24.dbc')
df_chik <- read.dbc('data/CHIKBR24.dbc')

# Filtrando os dados ########################################

## Filtrando apenas os confirmados ######################

### Dengue 
df <- df %>% filter(CLASSI_FIN %in% c(10, 11, 12))

### Chikungunya 
df_chik <- df_chik %>% filter(CLASSI_FIN == 13)


## Filtrando os que tem pelo menos um sintoma ################

df_chik <- df_chik %>% filter(!(
  is.na(FEBRE) & is.na(MIALGIA) & is.na(CEFALEIA) & is.na(EXANTEMA) & is.na(VOMITO) &
    is.na(NAUSEA) & is.na(DOR_COSTAS) & is.na(CONJUNTVIT) & is.na(ARTRITE) & is.na(ARTRALGIA) &
    is.na(PETEQUIA_N) & is.na(LEUCOPENIA) & is.na(LACO) & is.na(DOR_RETRO)
))

df <- df %>% filter(!(
  is.na(FEBRE) & is.na(MIALGIA) & is.na(CEFALEIA) & is.na(EXANTEMA) & is.na(VOMITO) &
    is.na(NAUSEA) & is.na(DOR_COSTAS) & is.na(CONJUNTVIT) & is.na(ARTRITE) & is.na(ARTRALGIA) &
    is.na(PETEQUIA_N) & is.na(LEUCOPENIA) & is.na(LACO) & is.na(DOR_RETRO)
))


## Corrigir os sintomas #############################################

sintomas <- c('FEBRE', 'MIALGIA', 'CEFALEIA', 'EXANTEMA', 'VOMITO',
              'NAUSEA', 'DOR_COSTAS', 'CONJUNTVIT', 'ARTRITE', 'ARTRALGIA',
              'PETEQUIA_N', 'LEUCOPENIA', 'LACO', 'DOR_RETRO')
df <- df %>%
  mutate(across(all_of(sintomas), ~ ifelse(is.na(.) | . == 2, 0, .)))
df_chik <- df_chik %>%
  mutate(across(all_of(sintomas), ~ ifelse(is.na(.) | . == 2, 0, .)))


# Seleccionar colunas a utilizar #############################
df <- df %>% select('FEBRE', 'MIALGIA', 'CEFALEIA', 'EXANTEMA', 'VOMITO',
                    'NAUSEA', 'DOR_COSTAS', 'CONJUNTVIT', 'ARTRITE', 'ARTRALGIA',
                    'PETEQUIA_N', 'LEUCOPENIA', 'LACO', 'DOR_RETRO', 'SEM_PRI', 'SG_UF', 'CRITERIO', 'NU_IDADE_N', 'HOSPITALIZ', 'ID_MN_RESI', 'DT_SIN_PRI', 'DT_NOTIFIC', 'CS_SEXO')
df_chik <- df_chik %>% select('FEBRE', 'MIALGIA', 'CEFALEIA', 'EXANTEMA', 'VOMITO',
                              'NAUSEA', 'DOR_COSTAS', 'CONJUNTVIT', 'ARTRITE', 'ARTRALGIA',
                              'PETEQUIA_N', 'LEUCOPENIA', 'LACO', 'DOR_RETRO', 'SEM_PRI', 'SG_UF', 'CRITERIO', 'NU_IDADE_N', 'HOSPITALIZ', 'ID_MN_RESI', 'DT_SIN_PRI', 'DT_NOTIFIC', 'CS_SEXO')

# Indicar classificacao para cada dataframe e unir ##############
df['CHIK'] <- 0
df_chik['CHIK'] <- 1
df_total <- rbind(df, df_chik)

# Corrigir os ID_MN_RESI para latlong #############################
load('data/latlong.RData')
municipalities <- municipalities %>% mutate(ID_MN_RESI = as.numeric(ID_MN_RESI))
df_total <- df_total %>% mutate(ID_MN_RESI = as.numeric(as.character(ID_MN_RESI)))
df_total <- df_total %>% left_join(municipalities, by = join_by(ID_MN_RESI))
df_total <- df_total %>% filter(!is.na(ID_MN_RESI))
df_total <- df_total %>% filter(!is.na(lat))
df_total <- df_total %>% filter(!is.na(long))

# Corrigir idade ###################################################
df_total <- df_total %>% mutate(NU_IDADE_N = decodifica_idade_SINAN(NU_IDADE_N))
df_total <- df_total %>% filter(!is.na(NU_IDADE_N))

# Corrigir hospitalizacao #########################################
df_total <- df_total %>% mutate(HOSPITALIZ = ifelse(HOSPITALIZ == 1, 1, 0))
df_total <- df_total %>% mutate(HOSPITALIZ = ifelse(is.na(HOSPITALIZ),0,HOSPITALIZ))

# Buscar mes de diagnostico #######################################
df_total <- df_total %>% mutate(mes = month(DT_SIN_PRI))

# Calcular tempo de sintomas #######################################
df_total <- df_total %>% mutate(dias = difftime(DT_NOTIFIC, DT_SIN_PRI, units = "days"))

# Corrigindo sexo
set.seed(123)
prob_fem <- 3294853/(3294853 + 2715173) 
df_total <- df_total %>% mutate(CS_SEXO = case_when(
  CS_SEXO == 'M' ~ 0,
  CS_SEXO == 'F' ~ 1,
  T ~ 2
))
df_total <- df_total %>% mutate(CS_SEXO = ifelse(is.na(CS_SEXO), 2, CS_SEXO))
df_total <- df_total %>% mutate(CS_SEXO = ifelse(CS_SEXO == 2,
  ifelse(runif(n()) >= prob_fem, 1, 0),  
  CS_SEXO
))
table(df_total$CS_SEXO, useNA = 'always')

# Guardando os resultados - o df final ##################################
df_total <- df_total %>% select('FEBRE', 'MIALGIA', 'CEFALEIA', 'EXANTEMA', 'VOMITO',
                                'NAUSEA', 'DOR_COSTAS', 'CONJUNTVIT', 'ARTRITE', 'ARTRALGIA',
                                'PETEQUIA_N', 'LEUCOPENIA', 'LACO', 'DOR_RETRO', 
                                'SG_UF', 
                                'CRITERIO', 'NU_IDADE_N', 'HOSPITALIZ', 
                                'lat', 'long', 'mes', 'dias', 'CS_SEXO' ,'CHIK')
save(df_total, file = 'dados_ML.RData')
write.csv(df_total, "dados_ML.csv")



