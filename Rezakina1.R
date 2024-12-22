#Резакина А.В. 
# выполнить дисперсионный анализ для доказательства утверждения, что для района Кунцево высота родов Лиственница и Туя значимо отличаются.

# задание 1
# очистим полностью память
rm(list=ls())

# Проверка рабочей директории
getwd()
# Рабочая директория
setwd ("C:/ModInf/Rezakina")

# Считаем данные в переменную adat и просмотрим их
greendb=read.csv("greendb.csv",sep=",",dec="."); greendb

#install.packages("dplyr")
library(dplyr)
#install.packages("readr")
library(readr)
#install.packages("stringr")
library(stringr)

# Но - высота обоих родов деревьев значимо не отличаются
# Н1 - высота обоих родов деревьев значимо отличаются
# выполнить дисперсионный анализ для доказательства утверждения, что для района Кунцево высота родов Лиственница и Туя значимо отличаются.
spec=greendb$species_ru
spec
#род
genus=stringr::str_split(spec, pattern=" ",simplify=T)[,1]
genus
data=greendb%>%mutate(Genus=genus)
data

data=data%>%filter(Genus%in% c("Лиственница","Туя")) %>%
  filter(adm_region=="район Кунцево")

greendb$Genus%>%unique()
greendb$adm_region%>%unique()

#Да, если отвергаем Но, то значимо отличаются
data.aov = aov(d_trunk_m ~ Genus, data=data)
summary(data.aov)
# для района Кунцево высота родов Лиственница и Туя значимо отличаются.

