library(tidyverse)
library(readr)
library(reshape2)

df <- read_csv("Elecciones_CDMX_Federales.csv")

Ganador_Part <- df %>% 
  select(Participacion, Porcentaje_Morena, Otros, Ganador) %>% 
  melt(id=c("Participacion", "Porcentaje_Morena", "Otros")) %>% 
  #rename("Morena"=Porcentaje_Morena) %>%
  #rename("PRI-PAN-PRD"=Otros)

Ganador_Part <- df %>%
  select(Participacion, Porcentaje_Morena, Otros, Ganador) %>%
  rename("MORENA"=Porcentaje_Morena) %>%
  rename("PRI-PAN-PRD"=Otros) %>% 
  melt(id=c("Participacion", 'Ganador')) %>%
  rename("Partido"=variable) %>%
  rename('Porcentaje_Votacion'=value) %>%
  drop_na() %>% 
  filter(Participacion <= 1)

####PLOT 1#####

gg1<-Ganador_Part %>% 
  ggplot(aes(x=Porcentaje_Votacion, y=Participacion, colour=Ganador))+
  geom_point()+
  theme_


gg1
