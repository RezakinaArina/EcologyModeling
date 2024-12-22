#Резакина А.В.
#Задание 2
#постройте картосхему максимальных диаметров стволов деревьев родов Лиственница и Туя 
# install.packages("sf")
library(sf)
library(ggplot2)
library(readr)
library(dplyr)

#очистим полностью память 
rm(list=ls())

# Проверка рабочей директории
getwd()
# Рабочая директория
setwd ("C:/ModInf/Rezakina")

#считаем данные в переменные
greendb= read.csv("greendb.csv"); greendb
map=sf :: read_sf("moscow.geojson")

# график с заливкой
ggplot(map)+geom_sf(aes(fill=NAME))+theme(legend.position="none")

# Лиственница и Туя
spec=greendb$species_ru
spec
genus=stringr::str_split(spec, pattern=" ",simplify=T)[,1]
genus
data=greendb%>%mutate(Genus=genus)
data

max_d_t=data %>% group_by(adm_region,Genus)%>% 
  summarise(max_d=max(d_trunk_m), na.rm = T, .groups = "drop")%>% 
  filter(Genus %in% c("Лиственница","Туя"))
max_d_t

# install.packages("tidyr")
library(tidyr)
# Преобразуем данные в широкий формат
max_d_t= pivot_wider(max_d_t, names_from = Genus, values_from = max_d)

# Объединяем данные с картой
map = map %>% mutate(adm_region = NAME)
map = left_join(map, max_d_t, by = "adm_region")

# Построение картосхемы для Лиственницы
ggplot(map) +
  geom_sf(aes(fill = `Лиственница`)) + 
  theme() 

# Построение картосхемы для Туи
ggplot(map) +
  geom_sf(aes(fill = `Туя`)) + 
  theme()

