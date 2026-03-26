library(tidyverse)
library(dplyr)
library(read.dbc)
library(ggplot2)
source('theme_Publication.R')
library(patchwork)
library(gridExtra)

# Carregando as bases de dados ##########################

df <- read.dbc('data/DENGBR24.dbc')
df_chik <- read.dbc('data/CHIKBR24.dbc')

# Filtrando apenas os confirmados ######################

## Dengue 
df <- df %>% filter(CLASSI_FIN %in% c(10, 11, 12))

## Chikungunya 
df_chik <- df_chik %>% filter(CLASSI_FIN == 13)

# Filtrando os que tem pelo menos um sintoma ################

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


# Eliminar as colunas que estao em aberto ##########################
df <- df %>% filter(CRITERIO != 3)
df_chik <- df_chik %>% filter(CRITERIO != 3)

# Looping and plotting all states ########################################
states <- unique(df$SG_UF)
plots <- list()
i = 1


for(state in states){
  print(state)
  df_lab <- df %>% filter(SG_UF == state) %>% filter(CRITERIO == 1) %>% 
    select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
           "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
           "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO") 
  
  df_epi <- df %>% filter(SG_UF == state) %>% filter(CRITERIO == 2)  %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                                                                                "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                                                                                "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")
  
  
  df_chik_lab <- df_chik %>% filter(CRITERIO == 1)  %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                                                               "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                                                               "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")
  
  df_chik_epi <- df_chik %>% filter(CRITERIO == 2)  %>% select("FEBRE","MIALGIA","CEFALEIA","EXANTEMA","VOMITO",
                                                               "NAUSEA","DOR_COSTAS", "CONJUNTVIT","ARTRITE","ARTRALGIA",
                                                               "PETEQUIA_N","LEUCOPENIA","LACO","DOR_RETRO")
  
  
  # Laboratorial total
  df_lab <- rbind(df_lab, df_chik_lab)
  df_lab <- df_lab %>% rename(Fever = FEBRE,
                              Myalgia = MIALGIA,
                              Headache = CEFALEIA,
                              Rash = EXANTEMA,
                              Vomiting = VOMITO,
                              Nausea = NAUSEA,
                              `Back pain` = DOR_COSTAS,
                              Conjunctivitis = CONJUNTVIT,
                              Arthritis = ARTRITE,
                              Arthralgia = ARTRALGIA,
                              Petechiae = PETEQUIA_N,
                              Leukopenia = LEUCOPENIA,
                              `Tourniquet test` = LACO,
                              `Retro orbital pain` = DOR_RETRO)
  # Epidemiologico total
  df_epi <- rbind(df_epi, df_chik_epi)
  df_epi <- df_epi %>% rename(Fever = FEBRE,
                              Myalgia = MIALGIA,
                              Headache = CEFALEIA,
                              Rash = EXANTEMA,
                              Vomiting = VOMITO,
                              Nausea = NAUSEA,
                              `Back pain` = DOR_COSTAS,
                              Conjunctivitis = CONJUNTVIT,
                              Arthritis = ARTRITE,
                              Arthralgia = ARTRALGIA,
                              Petechiae = PETEQUIA_N,
                              Leukopenia = LEUCOPENIA,
                              `Tourniquet test` = LACO,
                              `Retro orbital pain` = DOR_RETRO)
  
  # Totais
  total_lab <- dim(df_lab)[1]
  total_epi <- dim(df_epi)[1]
  
  df_lab <- df_lab %>% pivot_longer(cols = Fever:`Retro orbital pain`,
                                    names_to = 'sintoma', values_to = "Y") %>% mutate(Y = ifelse(Y == 1, 1, 0)) %>%
    filter(!is.na(Y)) %>% group_by(sintoma) %>% summarise(n = sum(Y, na.rm = TRUE))
  
  
  df_epi <- df_epi %>% pivot_longer(cols = Fever:`Retro orbital pain`,
                                    names_to = 'sintoma', values_to = "Y") %>% mutate(Y = ifelse(Y == 1, 1, 0)) %>%
    filter(!is.na(Y)) %>% group_by(sintoma) %>% summarise(n = sum(Y, na.rm = TRUE))
  
  
  df_lab <- df_lab %>% mutate(p = 100*(n/total_lab))
  df_epi <- df_epi %>% mutate(p = 100*(n/total_epi))
  
  df_lab['tipo'] <- 'Lab'
  df_epi['tipo'] <- 'Epi'
  
  df_lab <- df_lab %>% select(sintoma, p, tipo)
  df_epi <- df_epi %>% select(sintoma, p, tipo)
  
  df_total <- rbind(df_lab, df_epi)
  
  
  p <- ggplot(df_total, aes(tipo, sintoma, fill= p)) + 
    geom_tile() + scale_fill_distiller(palette = "RdPu") +
    geom_text(aes(label = paste0(round(p,2),'%')), color = "black", size = 4) +
    theme_bw() + labs(x = 'Criterion', y = 'Symptom', fill = 'Proportion') +
    ggtitle(paste0('State ',state))
  
  print(p)
  plots[[i]] <- p
  i = i + 1
  ggsave(paste0(state,'.jpg'), plot = p)
  
}










