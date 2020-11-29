

library(readr)
library(tidyverse)
#' General information - age, total population, percentage in rural and urban areas
tabela200_general <- read_csv("data/sidra/tabela200_general.csv", na = c('-', '...', '..'))

tabela200_general %>%
  select(3,5,6,8) %>%
  setNames(c('age', 'area', 'microregion_code', 'value')) %>%
  mutate(age = str_replace(age, ' a ', '_')) %>%
  filter(str_detect(age, '_')) -> tabela200_general_2

tabela200_general_2 %>%
  filter(area == 'Total') %>%
  group_by(microregion_code) %>%
  summarise(total_pop = sum(value, na.rm = T)) -> total_population

tabela200_general_2 %>%
  filter(area == 'Total') %>%
  left_join(total_population) %>%
  mutate(value = value/total_pop) %>%
  mutate(age = str_replace(age, ' anos', '')) %>%
  pivot_wider(names_from = age,names_prefix = 'age_') %>%
  select(-area) -> tabela200_general_3

tabela200_general_3[is.na(tabela200_general_3)] <- 0

tabela200_general_3 %>%
  summary()


tabela200_general_2 %>%
  # filter(area == 'Total')# %>%
  group_by(microregion_code, area) %>%
  summarise(pop = sum(value, na.rm = T))%>%
  ungroup() %>%
  pivot_wider(names_from = area,names_prefix = 'area_', values_from = 'pop') %>%
  mutate(perc_rural = area_Rural/area_Total,
         perc_urban = area_Urbana/area_Total) %>%
  select(1,5,6) -> urban_rural

tabela200_general_3 %>%
  left_join(urban_rural) -> tabela200_general_4

tabela200_general_4 

tabela200_general_4 %>%
  summary()

#' Number of immigrants
tabela631_immigrants <- read_csv("data/sidra/tabela631_immigrants.csv", na = c('-', '...', '..'))


tabela631_immigrants2 <- tabela631_immigrants %>%
  select(1,3,7) %>%
  setNames(c('microregion_code', 'imm', 'value')) %>%
  filter(imm != 'Total') %>%
  select(1,3) %>%
  rename(no_immigrants = value) %>%
  left_join(total_population) %>%
  mutate(no_immigrants = no_immigrants/total_pop) %>%
  select(-total_pop)

tabela631_immigrants2[is.na(tabela631_immigrants2)] <- 0
  

tabela631_immigrants2

#' earnings


tabela3548_earnings <- read_csv("data/sidra/tabela3548_earnings2.csv", na = c('-', '...', '..'))

tabela3548_earnings %>%
  select(1,7,10) %>%
  setNames(c('microregion_code', 'income', 'value')) -> tabela3548_earnings2

# income mapping
tabela3548_earnings2 %>%
  mutate(income = str_replace(income, 'Mais de ', '')) %>%
  mutate(income = str_replace(income, ' salário mínimo', '')) %>%
  mutate(income = str_replace(income, ' salários mínimos', '')) %>%
  mutate(income = str_replace(income, 'Até 1/4 de', '0')) %>%
  separate(income, sep =' a ', into=c('income1', 'b')) %>%
  mutate(income1 = str_replace(income1, '1/4', '0.25')) %>%
  mutate(income1 = str_replace(income1, '1/2', '0.5')) %>%
  mutate(income1 = str_replace(income1, 'Sem rendimento', '0')) %>%
  select(-b) %>%
  filter(income1 != 'Total') %>%
  mutate(income1 = as.numeric(income1)) %>%
  rename(income = income1)-> tabela3548_earnings3_long

tabela3548_earnings3_long %>%
  left_join(total_population) %>%
  mutate(value = value/total_pop) %>%
  group_by(microregion_code, income) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  pivot_wider(names_from = 'income', names_prefix = 'inc', values_from = value) -> tabela3548_earnings3_wide


tabela3548_earnings3_wide
# inc15 - percentage of people earning more than 15 times the minimum wage


options(scipen =999)
a <-prcomp(tabela3548_earnings3_wide%>%select(-microregion_code))
summary(a)



tabela200_general_4 %>%
  left_join(tabela3548_earnings3_wide) %>%
  left_join(tabela631_immigrants2) -> spatial_all

spatial_all
#' Avaliable information:
#' - microregion code - to join with other datasets
#' - total_pop - population
#' - age_... - percentage of people in given age
#' - perc_rural, perc_urban - percentage of people living in urban and rural areas
#' - inc... - percentage of people having income at least ... times the minimum wage
#' - no_immigrants - percentage of immigrants

save(spatial_all, file = 'data/preprocessed/spatial_all.Rdata')



