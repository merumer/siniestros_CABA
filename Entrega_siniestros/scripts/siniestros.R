#Voy a trabajar con la base de datos de siniestros de la ciudad de Buenos Aires


#Cargo librerías

library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)


#cargo el CSV. 


siniestros <- read.csv(file="entradas/Victimas_siniestros_2015-2018 (1).csv", stringsAsFactors = FALSE,
                       encoding = "UTF-8")

#Inspecciono los datos

head(siniestros)
sapply(siniestros, mode)

#modifico datos
siniestros$mes <- str_pad(siniestros$mes,width=2, side='left', pad='0') 


#Cantidad de siniestros por año y género 

siniestros_grafico1 <- siniestros %>% 
  filter(sexo!= ""  & tipo_colision1 !="" & periodo!="" & periodo!="2018" ) %>% 
  group_by(periodo, sexo) %>% 
count(periodo, sexo)
#se filtra el período 2018 porque está hasta el mes 5. 

ggplot(siniestros_grafico1) +
  geom_bar(aes(x = periodo, weight = n),fill="#CC6666")+
  labs(title = "Cantidad de siniestros por género",
       subtitle = "Ciudad Autónoma de Buenos Aires, 2015-2017",
       caption = "Fuente: portal de datos abiertos de la Ciudad - http://data.buenosaires.gob.ar",
       x = "Año",
       y = "Cantidad")+
  facet_wrap(facets ="sexo")+
  theme(  axis.text.x = element_text(size = 10, face = "italic"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black", size = 1))
#Se observa una mayor cantidad de siniestros generados por parte de hombres a lo largo de los años 2015-2017, visualizandose una leve tendencia a la baja

#Cantidad de siniestros en el tiempo


siniestros_grafico2 <- siniestros%>% 
  filter(sexo!= ""  &  periodo!="" & mes!="" ) %>% 
  group_by(periodo, mes, sexo) %>% 
  count() %>% 
  mutate(fecha_anio_mes = paste(periodo, mes, sep = "-")) %>% 
  ungroup() %>% 
  select(fecha_anio_mes, sexo, n) 



 siniestros_grafico2 %>% 
  ggplot(aes(x = fecha_anio_mes,
             y = n,
             color = sexo,
             linetype = sexo)) +
  geom_line(aes(group = sexo),
            size = 1.5) +
  scale_color_manual(values=c("#CC6666", "#9999CC"))+
  labs(title = "Cantidad de siniestros en el tiempo por sexo",
       subtitle = "Ciudad Autónoma de Buenos Aires, 2015-2018",
       x = "Fecha del siniestro",
       y = "Cantidad de casos") +
  theme  (axis.text.x = element_text(angle = 45,hjust = 1,  size = 10, face = "italic"),
                panel.grid = element_blank(),
                panel.background = element_rect(fill = "white", colour = "black", size = 1),
                legend.position = "bottom")

#Nuevamente se observa una mayor tendencia de hombres en la participación de siniestros. Además, se visualiza que en los meses de veranos hay una fuerte baja de siniestros, hecho que ocurre todos los años. 
 
#Cantidad de siniestros discriminando edad y lesión o homicidio. 
 
 
siniestros_grafico3<- siniestros %>% 
  filter(causa!= ""  & edad !="" & periodo!="" & mes!=""&   
           edad %in% c(0:110) ) %>% 
  mutate(fecha=mdy(fecha)) 

siniestros_grafico3%>% 
  ggplot(aes(x = fecha,
             y = edad, colour=causa))+
  geom_point(alpha = 1, 
             size = 1, 
           ) +
  geom_hline(yintercept = mean(siniestros_grafico3$edad, na.rm = T)) +
  scale_y_continuous(breaks = seq(0, 110, 10)) +
  scale_x_date(date_breaks = "month",
               date_labels = '%b %Y') +
  theme  (axis.text.x = element_text(angle = 45,hjust = 1,  size = 10, face = "italic"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black", size = 1),
          legend.position = "bottom")+
  labs(title = "Total casos por edad y fecha del siniestro",
       subtitle = "Ciudad Autónoma de Buenos Aires, 2015-2018",
       x = "Fecha del Siniestro",
       y = "Edad") +
  scale_color_manual(values=c("#CC6666", "#9999CC"))

#Se observa un promedio de edad de un poco menos de 40 años entre las víctimas. Se evidencian pocas victimas fatales, teniendo en consideración la cantidad de siniestros totales. 
 
#Intersección con más siniestros. 
siniestros_grafico4<- siniestros %>% 
  filter(direccion_normalizada!= ""  ) %>% 
  group_by(direccion_normalizada, tipo_colision1) %>% 
  count(direccion_normalizada,  tipo_colision1) %>% 
arrange(desc(n)) %>% 
head(n=20)

ggplot(siniestros_grafico4) +
  geom_bar(aes(x = reorder(direccion_normalizada,n), weight = n, fill=tipo_colision1))+
  labs(title = "Top 20: Calles con mayor cantidad de siniestros",
       subtitle = "Ciudad Autónoma de Buenos Aires, 2015-2018",
       caption = "Fuente: portal de datos abiertos de la Ciudad - http://data.buenosaires.gob.ar",
       x = "Calle o intersección",
       y = "Cantidad de siniestros", 
       fill="Tipo de colisión")+
  coord_flip()+
  scale_fill_brewer(palette = "Pastel1")+
  theme(  axis.text.x = element_text(size = 10, face = "italic"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black", size = 1))

#Como podríamos imaginar la calle con mayor cantida de siniestros es la Av. Gral Paz, seguida por otras avenidas de importancia de la ciudad. Las motos representan el mayor tipo de coalisión. Además hay una importante cantidad de siniestros con peatones en la intersección de Lima y Brasil, ubicación del CT. Constitución. 

#Intersección con más siniestros. (Calles)

siniestros_grafico5<- siniestros %>% 
  filter(direccion_normalizada!= "" , tipo_calle=="calle", tipo_colision1!= ""  ) %>% 
  group_by(direccion_normalizada, tipo_colision1) %>% 
  count(direccion_normalizada,  tipo_colision1) %>% 
  arrange(desc(n)) %>% 
  head(n=20)

ggplot(siniestros_grafico5) +
  geom_bar(aes(x = reorder(direccion_normalizada,n), weight = n, fill=tipo_colision1))+
  labs(title = "Top 20: Calles con mayor cantidad de siniestros",
       subtitle = "Ciudad Autónoma de Buenos Aires, 2015-2018",
       caption = "Fuente: portal de datos abiertos de la Ciudad - http://data.buenosaires.gob.ar",
       x = "Calle o intersección",
       y = "Cantidad de siniestros", 
       fill="Tipo de colisión")+
  coord_flip()+
  scale_fill_brewer(palette = "Pastel1")+
  theme(  axis.text.x = element_text(size = 10, face = "italic"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black", size = 1))

#Nuevamente los puntos cercanos al CT. Constitución demuestran ser un lugar de conflicto para los peatones. 


#Cantidad de siniestros por tipo de colisión y comunas

siniestros_grafico6<- siniestros %>% 
  filter(tipo_colision1!= "", comuna!= ""   ) %>% 
  count(tipo_colision1,comuna) %>% 
  transform(comuna=as.character(comuna)) %>% 
  arrange(desc(n)) %>% 
  head(n=150)

# Y ahora mostrar las cantidades mensuales como líneas 

ggplot(siniestros_grafico6) +
  geom_bar(aes(x = reorder(comuna,-n), weight =n, fill=tipo_colision1))+
  labs(title = "Cantidad de siniestros por comuna según tipo de colisión",
       subtitle = "Ciudad Autónoma de Buenos Aires, 2015-2018",
       caption = "Fuente: portal de datos abiertos de la Ciudad - http://data.buenosaires.gob.ar",
       x = "Comuna",
       y = "Cantidad de siniestros ", 
       fill="Tipo de colisión")+
  scale_fill_brewer(palette = "Set3")+

  theme(  axis.text.x = element_text(size = 10, face = "italic"),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", colour = "black", size = 1))

#Se evidencia que la Comuna 1 es la que tiene la mayor cantidad de siniestros, seguida por la 3. El mayor tipo de colisión es entre motos y vehículos, seguido de entre vehículos















