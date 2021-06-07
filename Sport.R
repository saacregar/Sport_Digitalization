##=========================================================================##
##                                                                         ##
##                DIGITALIZACIÓN DEL DEPORTE                               ##
##              Sabela Crecente y Albert Ortiz                             ##
##                                                                         ##
##=========================================================================##

# #### GESTIÓN DE PAQUETES ####

if(!require("tidyverse")) {
  install.packages("tidyverse")
  library("tidyverse")
}

if(!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}


# #### IMPORTACIÓN DE DATOS ####
setwd(getwd()) #CONFIRMAR Q ASI ESTA BIEN
#Datos extraídos de https://old.datahub.io/dataset/uk-premier-league-match-by-match-2011-2012
#exportados a csv desde Excel

df <- read.csv("Premier League 2011-12 Match by Match.csv", header=TRUE, sep=";", skip = 0, na.strings = "NA")

str(df)

df<-df %>% rename("Date"="ï..Date")

#Unir nombre y apellido de los jugadores
df$Player.Name<-paste(df$Player.Forename,df$Player.Surname)
df <- cbind(df, df$Player.Name)
Data<-write.table(df)
