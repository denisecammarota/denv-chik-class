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

# Fazendo séries temporais ##################################################################

## Por classi_fin #############################################################################
serie_denv <- df %>% group_by(SEM_PRI) %>% summarise(n = n())
serie_chik <- df_chik %>% group_by(SEM_PRI) %>% summarise(n = n())
serie_denv['Tipo'] <- 'Dengue'
serie_chik['Tipo'] <- 'Chikungunya'
serie_denv['Max'] <- max(serie_denv$n)
serie_chik['Max'] <- max(serie_chik$n)
serie <- rbind(serie_denv, serie_chik)
serie <- serie %>% mutate(SEM_PRI = as.numeric(substr(SEM_PRI, 5, 6)))
serie_res <- serie %>% group_by(SEM_PRI) %>% summarise(ntot = sum(n))
serie <- serie %>% left_join(serie_res, by = join_by(SEM_PRI))
serie['p'] <- serie['n']/serie['ntot']
serie['n_max'] <- serie['n']/serie['Max']

fig1 <- ggplot() + geom_line(data = serie, aes(x = SEM_PRI, y = n, color = Tipo), linewidth = 1) + 
  scale_colour_Publication()+ theme_Publication() +
  xlab('Week of symptom onset') + ylab('Cases') + labs(color = 'Classification')
fig1
ggsave("figures/figure1.png")

fig2 <- ggplot() + geom_line(data = serie, aes(x = SEM_PRI, y = n_max, color = Tipo), linewidth = 1) + 
  scale_colour_Publication()+ theme_Publication() +
  xlab('Week of symptom onset') + ylab('Cases (normalized)') + labs(color = 'Classification')
fig2
ggsave("figures/figure2.png")

rm(serie, serie_denv, serie_chik, serie_res)

## Estatisticas gerais ################################################
df_res <- df %>% select(CRITERIO)
df_chik_res <- df_chik %>% select(CRITERIO)
df_res['Tipo'] <- 'Dengue'
df_chik_res['Tipo'] <- 'Chikungunya'
df_tot <- rbind(df_chik_res, df_res)
rm(df_res, df_chik_res)
df_tot <- df_tot %>% group_by(CRITERIO, Tipo) %>% summarise(n = n())
df_tot <- df_tot %>% mutate(CRITERIO = case_when(
  CRITERIO == 1 ~ 'Laboratorial',
  CRITERIO == 2 ~ 'Clinical/Epidemiological',
  T ~ 'Open'
))
df_tot <- df_tot %>% pivot_wider(names_from = Tipo, values_from = n)
