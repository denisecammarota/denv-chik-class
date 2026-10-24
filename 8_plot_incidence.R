# Incidence maps: reported and estimated chikungunya #######

library(tidyverse)
library(dplyr)
library(read.dbc)
library(ggplot2)
library(geobr)
library(viridis)
library(ggthemes)
source('theme_Publication.R')

# Loading data ###########################################################
df_sp <- read.csv('state_35/full/35_total_state.csv')
df_mg <- read.csv('state_31/full/31_total_state.csv')
df_rj <- read.csv('state_33/full/33_total_state.csv')

# Plotting the incidence maps ############################################
map_base <- read_municipality(year=2020)
map_base <- map_base %>% filter(code_state %in% c(31, 33, 35))
map_base <- map_base %>% mutate(code_muni_6 = substr(code_muni, 1, 6))

pop <- read.csv2('data/pop_mun.csv')
pop <- pop %>% mutate(code_muni_6 = substr(Municipio, 1, 6))
pop <- pop %>% select(code_muni_6, Populacao_residente)

map_base <- map_base %>% left_join(pop, by = join_by(code_muni_6))

df_total <- rbind(df_sp, df_mg, df_rj)
rm(df_sp, df_mg, df_rj)

## Original incidence map ################################################
df_original <- df_total %>% filter(CHIK == 1)
df_original <- df_original %>% group_by(ID_MN_RESI) %>% summarise(casos_og = n())
df_original <- df_original %>% ungroup()
df_original <- df_original %>% mutate(ID_MN_RESI = as.character(ID_MN_RESI))

map_base <- map_base %>% left_join(df_original, by = join_by(code_muni_6 == ID_MN_RESI))
map_base <- map_base %>% mutate(inc_original = (10**5)*(casos_og/Populacao_residente))

fig1a <- ggplot(data = map_base) + ###
  geom_sf(aes(fill = inc_original)) +
  scale_fill_viridis_c(na.value = 'grey80') + ### zero reported cases in gray
  theme_map() +
  theme(title = element_text(size=16, face = 'bold'),
        plot.title = element_text(hjust = 0.4),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = 'right') +
  labs(fill = '', title = 'Chikungunya (Reported)') +
  guides(fill = guide_colorbar(
    barwidth = 1,  # width of the bar
    barheight = 6 # height of the bar
  ))
fig1a ###
ggsave("figures/figure_1a.jpg", plot = fig1a) ###

## Corrected incidence map ################################################
df_correct <- df_total %>% group_by(ID_MN_RESI) %>% summarise(casos_cr = sum(CHIK_CAL)) ### expected cases = sum of probabilities
df_correct <- df_correct %>% ungroup()
df_correct <- df_correct %>% mutate(ID_MN_RESI = as.character(ID_MN_RESI))

map_base <- map_base %>% left_join(df_correct, by = join_by(code_muni_6 == ID_MN_RESI))
map_base <- map_base %>% mutate(casos_cr = ifelse(casos_cr < 0.5, NA, casos_cr)) ### expected < 0.5 cases counted as zero (gray)
map_base <- map_base %>% mutate(inc_correct = (10**5)*(casos_cr/Populacao_residente))

fig1b <- ggplot(data = map_base) + ###
  geom_sf(aes(fill = inc_correct)) +
  scale_fill_viridis_c(na.value = 'grey80') + ### zero estimated cases in gray
  theme_map() +
  theme(title = element_text(size=16, face = 'bold'),
        plot.title = element_text(hjust = 0.4),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = 'right') +
  labs(fill = '', title = 'Chikungunya (Estimated)') +
  guides(fill = guide_colorbar(
    barwidth = 1,  # width of the bar
    barheight = 6 # height of the bar
  ))
fig1b ###
ggsave("figures/figure_1b.jpg", plot = fig1b) ###



