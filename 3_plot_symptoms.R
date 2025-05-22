library(tidyverse)
library(dplyr)
library(read.dbc)
library(ggplot2)
source('theme_Publication.R')

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

# Seleccionar colunas de sintoma #############################
df <- df %>% select('FEBRE', 'MIALGIA', 'CEFALEIA', 'EXANTEMA', 'VOMITO',
                    'NAUSEA', 'DOR_COSTAS', 'CONJUNTVIT', 'ARTRITE', 'ARTRALGIA',
                    'PETEQUIA_N', 'LEUCOPENIA', 'LACO', 'DOR_RETRO', 'SEM_PRI', 'SG_UF', 'CRITERIO')
df_chik <- df_chik %>% select('FEBRE', 'MIALGIA', 'CEFALEIA', 'EXANTEMA', 'VOMITO',
                              'NAUSEA', 'DOR_COSTAS', 'CONJUNTVIT', 'ARTRITE', 'ARTRALGIA',
                              'PETEQUIA_N', 'LEUCOPENIA', 'LACO', 'DOR_RETRO', 'SEM_PRI', 'SG_UF', 'CRITERIO')

# Eliminar as colunas que estao em aberto ##########################
df <- df %>% filter(CRITERIO != 3)
df_chik <- df_chik %>% filter(CRITERIO != 3)

# Corrigir os sintomas #############################################

sintomas <- c('FEBRE', 'MIALGIA', 'CEFALEIA', 'EXANTEMA', 'VOMITO',
              'NAUSEA', 'DOR_COSTAS', 'CONJUNTVIT', 'ARTRITE', 'ARTRALGIA',
              'PETEQUIA_N', 'LEUCOPENIA', 'LACO', 'DOR_RETRO')
df <- df %>%
  mutate(across(all_of(sintomas), ~ ifelse(is.na(.) | . == 2, 0, .)))
df_chik <- df_chik %>%
  mutate(across(all_of(sintomas), ~ ifelse(is.na(.) | . == 2, 0, .)))

## Gráficos sintomas - dengue vs chik / lab vs epi ##################

# Gráficos sintomas - lab vs epi ###########################
df_lab_denv <- df %>% filter(CRITERIO == 1) %>% select(
  'FEBRE', 'MIALGIA', 'CEFALEIA', 'EXANTEMA', 'VOMITO',
  'NAUSEA', 'DOR_COSTAS', 'CONJUNTVIT', 'ARTRITE', 'ARTRALGIA',
  'PETEQUIA_N', 'LEUCOPENIA', 'LACO', 'DOR_RETRO'
)
df_lab_chik <- df_chik %>% filter(CRITERIO == 1) %>% select(
  'FEBRE', 'MIALGIA', 'CEFALEIA', 'EXANTEMA', 'VOMITO',
  'NAUSEA', 'DOR_COSTAS', 'CONJUNTVIT', 'ARTRITE', 'ARTRALGIA',
  'PETEQUIA_N', 'LEUCOPENIA', 'LACO', 'DOR_RETRO'
)
df_epi_denv <- df %>% filter(CRITERIO == 2) %>% select(
  'FEBRE', 'MIALGIA', 'CEFALEIA', 'EXANTEMA', 'VOMITO',
  'NAUSEA', 'DOR_COSTAS', 'CONJUNTVIT', 'ARTRITE', 'ARTRALGIA',
  'PETEQUIA_N', 'LEUCOPENIA', 'LACO', 'DOR_RETRO'
)
df_epi_chik <- df_chik %>% filter(CRITERIO == 2) %>% select(
  'FEBRE', 'MIALGIA', 'CEFALEIA', 'EXANTEMA', 'VOMITO',
  'NAUSEA', 'DOR_COSTAS', 'CONJUNTVIT', 'ARTRITE', 'ARTRALGIA',
  'PETEQUIA_N', 'LEUCOPENIA', 'LACO', 'DOR_RETRO'
)

## Dengue ####################################################
df_lab <- df_lab_denv
df_epi <- df_epi_denv
df_lab['Criterio'] <- 'Laboratorial'
df_epi['Criterio'] <- 'Clinical/Epidemiological'
df_res <- rbind(df_lab, df_epi)
df_res <- df_res %>% pivot_longer(cols = FEBRE:DOR_RETRO,
                                  names_to = 'sintoma', values_to = "Y") %>% mutate(Y = ifelse(Y == 1, 1, 0)) %>%
  filter(!is.na(Y)) %>% group_by(Criterio, sintoma) %>% summarise(n = sum(Y, na.rm = TRUE))

n_lab <- df_lab %>%  nrow()
n_epi <- df_epi %>% nrow()

df_aux <- data.frame(Criterio = c('Laboratorial', 'Clinical/Epidemiological'), ncasos = c(n_lab, n_epi))
df_res <- df_res %>% left_join(df_aux, by = join_by(Criterio))
df_res['p'] <- 100*df_res['n']/df_res['ncasos']

fig5 <- ggplot(df_res, aes(Criterio, sintoma, fill= p)) + 
  geom_tile() + scale_fill_distiller(palette = "RdPu") +
  geom_text(aes(label = paste0(round(p,2),'%')), color = "black", size = 6) +
  theme_Publication() + 
  labs(x = 'Criterion', y = 'Symptom', fill = 'Proportion') + 
  ggtitle('Dengue') +
  guides(fill = guide_colorbar(
    barwidth = 6,  # width of the bar
    barheight = 1 # height of the bar
  ))
fig5
ggsave("figures/figure5.png", width = 6, height = 6)

rm(df_res, df_aux)
rm(n_lab, n_epi)

## Chikungunya ################################################
df_lab <- df_lab_chik
df_epi <- df_epi_chik
df_lab['Criterio'] <- 'Laboratorial'
df_epi['Criterio'] <- 'Clinical/Epidemiological'
df_res <- rbind(df_lab, df_epi)
df_res <- df_res %>% pivot_longer(cols = FEBRE:DOR_RETRO,
                                  names_to = 'sintoma', values_to = "Y") %>% mutate(Y = ifelse(Y == 1, 1, 0)) %>%
  filter(!is.na(Y)) %>% group_by(Criterio, sintoma) %>% summarise(n = sum(Y, na.rm = TRUE))

n_lab <- df_lab %>%  nrow()
n_epi <- df_epi %>% nrow()

df_aux <- data.frame(Criterio = c('Laboratorial', 'Clinical/Epidemiological'), ncasos = c(n_lab, n_epi))
df_res <- df_res %>% left_join(df_aux, by = join_by(Criterio))
df_res['p'] <- 100*df_res['n']/df_res['ncasos']

fig6 <- ggplot(df_res, aes(Criterio, sintoma, fill= p)) + 
  geom_tile() + scale_fill_distiller(palette = "RdPu") +
  geom_text(aes(label = paste0(round(p,2),'%')), color = "black", size = 6) +
  theme_Publication() + 
  labs(x = 'Criterion', y = 'Symptom', fill = 'Proportion') + 
  ggtitle('Chikungunya') +
  guides(fill = guide_colorbar(
    barwidth = 6,  # width of the bar
    barheight = 1 # height of the bar
  ))
fig6
ggsave("figures/figure6.png", width = 6, height = 6)

rm(df_res, df_aux)
rm(n_lab, n_epi)


