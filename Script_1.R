library(tidyverse)
library(readr)
library(reshape2)
library(gganimate)

df <- read_csv("Elecciones_CDMX_Federales.csv")

Ganador_Part <- df %>%
  select(Participacion, Porcentaje_Morena, Otros, Ganador) %>%
  rename("MORENA"=Porcentaje_Morena) %>%
  rename("PRI-PAN-PRD"=Otros) %>% 
  melt(id=c("Participacion", 'Ganador')) %>%
  rename("Partido"=variable) %>%
  rename('Porcentaje_Votacion'=value) %>%
  drop_na() %>% 
  filter(Participacion <= 1) %>% 
  mutate(observation = 1:n())

####PLOT 1#####
title='Participación Ciudadana vs Porcentaje de Votación por Partido en la CDMX'
subtitle='Elecciones Diputaciones Federales 2021'
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

l <- get_png("Original on Transparent.png")

gg1<-Ganador_Part %>% 
  ggplot(aes(x=Porcentaje_Votacion, y=Participacion, colour=Partido, stroke=0))+
  geom_point(alpha=0.4)+theme_classic()+
  scale_colour_manual(values=c('#9b1457','#16cdde'))+
  labs(title = title , subtitle= subtitle )+
  theme(plot.subtitle= element_text(size=12))+
  theme(legend.title = element_blank(),
        axis.text.y = element_text(face="bold"),
        axis.text.x = element_text(face="bold"),
        legend.position="bottom")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  xlab('Porcentaje de Votación')+
  ylab('Participación Ciudadana')+
  labs(caption="tmas.mx/otrosdatos")+
  theme(plot.caption= element_text(size=10, color='#2C3C5B', face="bold"))+
  theme(plot.caption= element_text(size=10, color='#2C3C5B', face="bold"))+
  theme(panel.grid.major.y = element_line(size = 1,linetype = "dotted"))+
  annotation_custom(l, xmin = -0.1 , xmax = 0,
                    ymin = -0.1, ymax = -0.2)+
  coord_cartesian(clip = "off")
  #transition_time(Porcentaje_Votacion)
  #shadow_mark(past=TRUE, size = 0.75)

gg1

animate(gg1, height=420,width=1000,fps=50,duration=20,end_pause = 120,res=100,rewind=F)
animate(gg1)

#Porcentaje en los ejes

