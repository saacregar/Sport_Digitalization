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

if(!require("GGally")) {
  install.packages("GGally")
  library("GGally")
}

if(!require("ggthemes")) {
  install.packages("ggthemes")
  library("ggthemes")
}


# #### IMPORTACIÓN DE DATOS ####
setwd(getwd()) #CONFIRMAR Q ASI ESTA BIEN
#Datos extraídos de https://old.datahub.io/dataset/uk-premier-league-match-by-match-2011-2012
#exportados a csv desde Excel

df <- read.csv("Premier League 2011-12 Match by Match.csv", header=TRUE, sep=";", skip = 0, na.strings = "NA")

str(df)

df<-df %>% rename("Date"="ï..Date")
df$Date<-as.Date(df$Date,"%Y-%m-%d",optional=FALSE)

#Unir nombre y apellido de los jugadores
df$Player.Name<-paste(df$Player.Forename,df$Player.Surname)

#Añadir columna con tipo formación
df$Formation.Type <-(df$Team.Formation=="2")*442+(df$Team.Formation=="3")*41212+
  (df$Team.Formation=="4")*433+(df$Team.Formation=="5")*451+(df$Team.Formation=="6")*4411+
  (df$Team.Formation=="7")*4141+(df$Team.Formation=="8")*4231+(df$Team.Formation=="9")*4321+
  (df$Team.Formation=="10")*532+(df$Team.Formation=="11")*541+(df$Team.Formation=="12")*352+
  (df$Team.Formation=="13")*343+(df$Team.Formation=="14")*31312+(df$Team.Formation=="15")*4222+
  (df$Team.Formation=="16")*3511+(df$Team.Formation=="17")*3421+(df$Team.Formation=="18")*3412
    
#comprobamos que las columnas se han creado
colnames(df) 

# #### ANÁLISIS DESCRIPTIVO ####
#¿Cuántos jugadores estamos analizando?
str(as.factor(df$Player.ID))
#¿Cuántos equipos estamos analizando?
str(as.factor(df$Team))
#¿Cuántas formaciones hay?
str(as.factor(df$Team.Formation))


# #### VISUALIZACIÓN ####
#Goles por equipo en toda la temporada
#ggplot(df)+geom_col(aes(x=Team,y=Goals))+theme(axis.text.x=element_text(angle=90, hjust=1))
#INCORRECTO, lo haremos haciendo group_by(Team)

#Qué formaciones se usan más [NO POR PARTIDO]
ggplot(df)+geom_bar(aes(x=Team.Formation,fill=as.factor(Formation.Type)))+scale_fill_discrete(name = "Formation Type")

#partidos donde se usa Formación 442
df442<-df[(df$Formation.Type=="442"),]


# #### PARTIDOS ####
#### Goles ####
#Goals For (a favor)
dfTeam<-df%>%group_by(Team)%>%summarise(GoalFor=sum(Goals))
#Goals Against (en contra)
dfOpposition<-df%>%group_by(Opposition)%>%summarise(GoalAgainst=sum(Goals))

ggplot()+geom_col(data=dfTeam, aes(x=Team,y=GoalFor), fill="red")+
  geom_col(data=dfOpposition, aes(x=Opposition,y=GoalAgainst),fill="blue",alpha=0.5)+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(x="Team", y="Goals",title="Goals For and Against")

ggplot()+ geom_point(dfTeam,aes(x=Team,y=GoalFor,size=GoalFor),shape=21)

####

dfMatch<-df%>%group_by(Date,Team,Opposition)%>%summarise(Result=sum(Goals))



ggplot(dfMatch,aes(x=Team,y=Opposition))+geom_jitter(width=0.1,height=0.1,shape=21)+
  theme(axis.text.x=element_text(angle=90, hjust=1))

ggplot(dfMatch,aes(x=Team,y=Opposition))+geom_point(aes(size=Result),shape=21)+
  theme(axis.text.x=element_text(angle=90, hjust=1))

# #### ANÁLISIS MANCHESTER CITY, GANADOR DE LA LIGA ####
dfCity<-df[df$Team == "Manchester City",]

ggplot(dfCity)+geom_col(aes(x=Player.Name,y=Left.Foot.Goals), fill="red")+
  geom_col(aes(x=Player.Name,y=Left.Foot.Goals),fill="blue",position="dodge")+
  theme(axis.text.x=element_text(angle=90, hjust=1))



