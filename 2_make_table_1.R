# Table 1: number (%) of missing values per feature, among CONFIRMED dengue and
# chikungunya notifications in each state (before any other filter)

library(tidyverse)
library(read.dbc)

estados <- c(MG = 31, RJ = 33, SP = 35)

sintomas <- c('FEBRE', 'MIALGIA', 'CEFALEIA', 'EXANTEMA', 'VOMITO',
              'NAUSEA', 'DOR_COSTAS', 'CONJUNTVIT', 'ARTRITE', 'ARTRALGIA',
              'PETEQUIA_N', 'LEUCOPENIA', 'DOR_RETRO', 'LACO')

colunas <- c(sintomas, 'SG_UF', 'CRITERIO', 'NU_IDADE_N', 'HOSPITALIZ',
             'ID_MN_RESI', 'DT_SIN_PRI', 'DT_NOTIFIC', 'CS_SEXO')

# same age decoding as the preprocessing (from pysus)
decodifica_idade_SINAN <- function(idade) {
  idade <- as.numeric(idade)
  case_when(
    idade >= 4000 ~ idade - 4000,
    idade >= 3000 & idade < 4000 ~ (idade - 3000) / 12.0,
    idade >= 2000 & idade < 3000 ~ (idade - 2000) / 365.0,
    idade >= 1000 & idade < 2000 ~ (idade - 1000) / (365 * 24.0),
    TRUE ~ NA_real_
  )
}

# Confirmed cases in each state ################################
df <- read.dbc('data/DENGBR24.dbc') %>% filter(CLASSI_FIN %in% c(10, 11, 12))
df_chik <- read.dbc('data/CHIKBR24.dbc') %>% filter(CLASSI_FIN == 13)

# everything as character so factors from the two files bind safely
raw <- bind_rows(
  df %>% select(all_of(colunas)) %>% mutate(across(everything(), as.character)),
  df_chik %>% select(all_of(colunas)) %>% mutate(across(everything(), as.character))
) %>%
  filter(SG_UF %in% as.character(estados)) %>%
  filter(CRITERIO %in% c('1', '2')) %>%
  mutate(estado = names(estados)[match(SG_UF, as.character(estados))])

# Municipality coordinates (residence), as in the preprocessing #
load('data/latlong.RData')
coords <- municipalities %>%
  mutate(ID_MN_RESI = as.numeric(as.character(ID_MN_RESI))) %>%
  distinct(ID_MN_RESI, .keep_all = TRUE) %>%
  select(ID_MN_RESI, lat, long)
raw <- raw %>%
  mutate(ID_MN_RESI = as.numeric(ID_MN_RESI)) %>%
  left_join(coords, by = 'ID_MN_RESI')

# TRUE = missing or recorded as unknown
miss <- raw %>% transmute(
  estado,
  across(all_of(sintomas), ~ is.na(.) | !(. %in% c('1', '2'))),
  NU_IDADE_N = is.na(decodifica_idade_SINAN(NU_IDADE_N)),
  CS_SEXO    = is.na(CS_SEXO) | !(CS_SEXO %in% c('M', 'F')),
  HOSPITALIZ = is.na(HOSPITALIZ) | !(HOSPITALIZ %in% c('1', '2')),
  dias       = is.na(DT_SIN_PRI) | is.na(DT_NOTIFIC),
  coord      = is.na(ID_MN_RESI) | is.na(lat) | is.na(long),  # no municipality or no coordinates
  mes        = is.na(DT_SIN_PRI)
)

features <- tribble(
  ~Feature,                     ~var,
  'Fever',                      'FEBRE',
  'Myalgia',                    'MIALGIA',
  'Headache',                   'CEFALEIA',
  'Rash (exanthema)',           'EXANTEMA',
  'Vomiting',                   'VOMITO',
  'Nausea',                     'NAUSEA',
  'Back pain',                  'DOR_COSTAS',
  'Conjunctivitis',             'CONJUNTVIT',
  'Arthritis',                  'ARTRITE',
  'Arthralgia',                 'ARTRALGIA',
  'Petechiae',                  'PETEQUIA_N',
  'Leukopenia',                 'LEUCOPENIA',
  'Retro-orbital pain',         'DOR_RETRO',
  'Tourniquet test (positive)', 'LACO',
  'Patient age',                'NU_IDADE_N',
  'Patient sex',                'CS_SEXO',
  'Hospitalization status',     'HOSPITALIZ',
  'Days since symptom onset',   'dias',
  'Latitude',                   'coord',
  'Longitude',                  'coord',
  'Month of symptom onset',     'mes'
)

contagem <- miss %>%
  pivot_longer(-estado, names_to = 'var', values_to = 'falta') %>%
  group_by(estado, var) %>%
  summarise(n = sum(falta), N = n(), .groups = 'drop') %>%
  mutate(celula = sprintf('%d (%.1f)', n, 100 * n / N))

tabela1 <- features %>%
  left_join(contagem %>% select(var, estado, celula) %>%
              pivot_wider(names_from = estado, values_from = celula),
            by = 'var') %>%
  select(Feature, MG, RJ, SP)

print(tabela1, n = Inf, width = Inf)
write.csv(tabela1, 'table1.csv', row.names = FALSE)

# denominators: confirmed notifications per state (for the table footnote)
print(contagem %>% distinct(estado, N))