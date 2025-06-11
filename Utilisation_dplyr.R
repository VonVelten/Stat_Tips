# Utilisation de dplyr
rm(list=ls())
library(dplyr)
library(nycflights13)
data(flights)
data("airports")
data(airlines)


#Pour afficher des lignes 
slice(airports,24)
slice(airports,c(15,24,48))


#Selection de données
filter(flights,month%in%c(1,3))
filter(flights, dep_delay >= 10 & dep_delay <= 15)
filter(flights, dep_delay >= 10, dep_delay <= 15)
filter(flights, dep_delay >= 10, month==1)
