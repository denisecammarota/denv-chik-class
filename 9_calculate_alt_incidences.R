library(tidyverse)

pop <- read.csv2('data/pop_mun.csv')
pop <- pop %>% mutate(sg_uf = substr(Municipio, 1, 2)) 
pop <- pop %>% group_by(sg_uf) %>% summarise(pop = sum(Populacao_residente))

pop_mg <- 21322691

inc_original <- (10**5)*(145114/pop_mg)
inc_alternative <- (10**5)*(207663/pop_mg)
inc_original
inc_alternative

pop_rj <- 17219679

inc_original <- (10**5)*(3791/pop_rj)
inc_alternative <- (10**5)*(11787/pop_rj)
inc_original
inc_alternative

pop_sp <- 45973194

inc_original <- (10**5)*(9262/pop_sp)
inc_alternative <- (10**5)*(19447/pop_sp)
inc_original
inc_alternative
