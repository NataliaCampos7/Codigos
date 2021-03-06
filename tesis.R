
library(tidyverse)

p_load(janitor, readxl, tidyverse)
library(ggplot2)
library(haven)
library(ineq)
library(readr)
library(readxl)
library(shiny)
library(tidyverse)


library(readr)


library(readxl)
library(scales)
Libro07 <- read_excel("E:/Tesis/Libro07.xlsx")
View(Libro07)

Libro07 %>%
  ggplot() +
  geom_col(aes(fct_reorder(`Obst�culos a la coordinaci�n`, Frecuencia), Frecuencia), fill = "seagreen") + 
           #stat="identity", position="dodge") +
  labs(title= "Obst�culos o desaf�os a la coordinaci�n m�s comunes en los SEA",
       subtitle= "Frecuencia de elecci�n",
       x = "Respuesta",
       y = "Frecuencia",
       caption = "Fuente: elaboraci�n propia con base en encuestas a miembros de CPC \ 
        G: No se ha consolidado una estrategia de coordinaci�n y colaboraci�n entre el Comit� Coordinador y el Comit� de Participaci�n Ciudadana \
        F: Existen intereses pol�ticos del Ejecutivo Estatal que interfieren en las acciones del Comit� Coordinador \
        E: Los miembros del Comit� Coordinador defienden los intereses espec�ficos de sus organizaciones y no han logrado converger en un inter�s com�n anticorrupci�n \
        D: No hay suficientes recursos econ�micos destinados al Sistema Estatal Anticorrupci�n \
        C: Los miembros del Comit� Coordinador no entienden la l�gica que sustenta la existencia del Sistema Estatal Anticorrupci�n 
        B: El Comit� de Participaci�n Ciudadana no ha conseguido ser un contrapeso ni un �rgano que ayude a fortalecer la coordinaci�n \
        A: Los miembros del Comit� de Participaci�n Ciudadana no entienden la l�gica que sustenta la existencia del Sistema Estatal Anticorrupci�n ") +
    coord_flip() +
  scale_x_discrete(labels = c("A","B","C","D","E","F","G","H","I")) +
  scale_y_continuous(breaks = c(seq(0,8,1)),
                     labels = c("0","1","2","3","4","5","6","7","8")) + 
  theme_light() + 
  theme(plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(color = "red"),
        axis.text.y = element_text(color = "blue"))
  
  Libro07 %>% 
  ggplot() +
  geom_bar(aes(`Obst�culos a la coordinaci�n`))

glimpse(Libro07)

library(readxl)
Libro08 <- read_excel("E:/Tesis/Libro07.xlsx", 
                      sheet = "camoio")



str(Libro08)

Libro08 <- Libro08 %>% 
  mutate(Escala = as.character(Escala))
glimpse(Libro08)


a <- factor(c("1","2","3","4","5","6","7","8","9","10"),
       order = T,
       level = c("1","2","3","4","5","6","7","8","9","10"))

Libro08 <- Libro08 %>% 
  mutate(Escala = as.numeric(Escala))

Libro08

ggplot(Libro08, aes(Escala, Frecuencia)) + 
  geom_point(size = 5, color = "deepskyblue") + 
    #binaxis='y', stackdir='center',
   #            stackratio=1.0, dotsize=3.0, fill="deepskyblue") +
labs(title= "Cambio en la forma en que se aborda el fen�meno de la corrupci�n a partir de la implementaci�n del SEA",
       subtitle= "Escala de 1(nivel m�nimo de cambio) a 10(nivel m�ximo de cambio)",
       x = "Nivel de cambio en la acci�n anticorrupci�n",
       y = "Frecuencia",
       caption = "Fuente: elaboraci�n propia con base en encuestas a miembros de CPC") +
  scale_x_continuous(
                   breaks = c(seq(1,10,1))) +
  scale_y_continuous(breaks = c(seq(0,9,1)),
                     labels = c("0","1","2","3","4","5","6","7","8","9")) + 
  theme_minimal() + 
  theme(plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(color = "blue"),
        axis.text.y = element_text(color = "red"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


library(readxl)
nivelcoord <- read_excel("E:/Tesis/Libro07.xlsx", 
                         sheet = "coord")

nivelcoord <- nivelcoord %>% 
  mutate(Escala = as.character(Escala))

nivelcoord <- nivelcoord %>% 
  mutate(Escala = as.numeric(Escala))


ggplot(nivelcoord, aes(Escala, Frecuencia)) + 
  geom_point(size = 5, color = "darkorange") +
  labs(title= "Cambio en el nivel de coordinaci�n percibido por los miembros de CPC",
       subtitle= "Escala de 1(nivel m�nimo de coordinaci�n) a 10(nivel m�ximo de coordinaci�n)",
       x = "Nivel de coordinaci�n",
       y = "Frecuencia",
       caption = "Fuente: elaboraci�n propia con base en encuestas a miembros de CPC")  + scale_x_continuous(breaks = c(seq(1,10,1))) +
  scale_y_continuous(breaks = c(seq(0,9,1)),
                     labels = c("0","1","2","3","4","5","6","7","8","9")) +
  theme_light() + 
  theme(plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(color = "blue"),
        axis.text.y = element_text(color = "red"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


 
library(readxl)
Libro007 <- read_excel("E:/Tesis/Libro007.xlsx")
Libro008 <- read_excel("E:/Tesis/Libro008.xlsx")


Libro007 <- Libro007 %>% mutate(porcentaje = Frecuencia*100/11, digits=2) 



ggplot(Libro007, aes(x=1, y=porcentaje, fill=Respuesta)) +
  geom_bar(stat="identity", color = "white") +
  geom_text(aes(label = paste0(round(porcentaje,1),"%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") + 
  theme_void() + 
  labs(title= "�Cree que el Sistema Nacional Anticorrupci�n 
es una estrategia efectiva para combatir la corrupci�n?",
       hjust= 0.5, caption = "Fuente: elaboraci�n propia con base en encuestas a miembros de CPC") + 
  scale_fill_manual(values = c("grey40", "salmon"))



Libro008 <- Libro008 %>% mutate (porcentaje= Frecuencia*100/11)
  
  ggplot(Libro008, aes(x=1, y=porcentaje, fill=Respuesta))+
    geom_bar(stat="identity", color="white") +
    geom_text(aes(label = paste0(round(porcentaje,1),"%")), 
              position = position_stack(vjust = 0.5)) +
    coord_polar(theta = "y") + 
    theme_void() +
    theme(legend.position = "right") +
    labs(title= "Es m�s probable que en 15 a�os el Sistema Nacional Anticorrupci�n...", hjust=0.5, 
caption = "Fuente: elaboraci�n propia con base en encuestas a miembros de CPC", hjust=0.5) + 
    scale_fill_manual(values = c("grey40", "salmon"), label = c("Desaparezca", "Se posicione como referente internacional\nen la lucha anticorrupci�n"))
