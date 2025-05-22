library(tidyverse)
library(dplyr)
library(read.dbc)
library(ggplot2)
library(geobr)
library(viridis)
library(ggthemes)
source('theme_Publication.R')

# Carregando as bases de dados ##########################

df <- read.dbc('data/DENGBR24.dbc')
df_chik <- read.dbc('data/CHIKBR24.dbc')

# Filtrando apenas os confirmados ######################

## Dengue 
df <- df %>% filter(CLASSI_FIN %in% c(10, 11, 12))

## Chikungunya 
df_chik <- df_chik %>% filter(CLASSI_FIN == 13)

# Seleccionar colunas a usar #############################
df <- df %>% select('SEM_PRI', 'SG_UF', 'CRITERIO')
df_chik <- df_chik %>% select('SEM_PRI', 'SG_UF', 'CRITERIO')


# Eliminar as colunas que estao em aberto ##########################
df <- df %>% filter(CRITERIO != 3)
df_chik <- df_chik %>% filter(CRITERIO != 3)

# Mapa de incidencia de arboviroses ####################################
map_base <- read_state(year = 2020)
pop <- data.frame(
  SG_UF = c(11, 12, 13, 14, 15, 16, 17, 
            21, 22, 23, 24, 25, 26, 27, 28, 29,
            31, 32, 33, 35,
            41, 42, 43,
            50, 51, 52, 53),
  Population = c(1746227, 880631, 4281209, 716793, 8664306, 802837, 1577342,
                 7010960, 3375646, 9233656, 3446071, 4145040, 
                 9539029, 3220104, 2291077, 14850513,
                 21322691, 4102129, 17219679, 45973194,
                 11824665, 8058441, 11229915,
                 2901895, 3836399, 7350483, 2982818)
) # TABNET


df_res <- rbind(df, df_chik)
df_res <- df_res %>% group_by(SG_UF) %>% summarise(n = n())
df_res <- df_res %>% mutate(SG_UF = as.numeric(as.character(SG_UF)))
map_res <- map_base %>% left_join(df_res, by = join_by(code_state == SG_UF))
map_res <- map_res %>% left_join(pop, by = join_by(code_state == SG_UF))
map_res <- map_res %>% mutate(p = (10**5)*n/(Population))


# Mapa de incidencia de dengue #########################################
df_res <- df
df_res <- df_res %>% group_by(SG_UF) %>% summarise(n = n())
df_res <- df_res %>% mutate(SG_UF = as.numeric(as.character(SG_UF)))
map_res <- map_base %>% left_join(df_res, by = join_by(code_state == SG_UF))
map_res <- map_res %>% left_join(pop, by = join_by(code_state == SG_UF))
map_res <- map_res %>% mutate(p = (10**5)*n/(Population))

fig3 <- ggplot(data = map_res) +
  geom_sf(aes(fill = p)) +
  scale_fill_viridis_c() +
  theme_map() + 
  theme(title = element_text(size=16, face = 'bold'), 
        plot.title = element_text(hjust = 0.4),
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 10)) +
  labs(fill = '', title = 'Dengue') +
  guides(fill = guide_colorbar(
    barwidth = 1,  # width of the bar
    barheight = 6 # height of the bar
  ))
fig3
ggsave("figures/figure3.png")


# Mapa de incidencia de chikungunya ######################################
df_res <- df_chik
df_res <- df_res %>% group_by(SG_UF) %>% summarise(n = n())
df_res <- df_res %>% mutate(SG_UF = as.numeric(as.character(SG_UF)))
map_res <- map_base %>% left_join(df_res, by = join_by(code_state == SG_UF))
map_res <- map_res %>% left_join(pop, by = join_by(code_state == SG_UF))
map_res <- map_res %>% mutate(p = (10**5)*n/(Population))

fig4 <- ggplot(data = map_res) +
  geom_sf(aes(fill = p)) +
  scale_fill_viridis_c() +
  theme_map() + 
  theme(title = element_text(size=16, face = 'bold'), 
        plot.title = element_text(hjust = 0.4),
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 10)) +
  labs(fill = '', title = 'Chikungunya') +
  guides(fill = guide_colorbar(
    barwidth = 1,  # width of the bar
    barheight = 6 # height of the bar
  ))
fig4
ggsave("figures/figure4.png")


