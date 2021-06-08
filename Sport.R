##=========================================================================##
##             Práctica 2 - Business Intelligence                          ##
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

if(!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library("RColorBrewer")
}

# #### IMPORTACIÓN DE DATOS ####
getwd()
setwd(getwd())
#Datos extraídos de https://old.datahub.io/dataset/uk-premier-league-match-by-match-2011-2012
#exportados a csv desde Excel

df <- read.csv("Premier League 2011-12 Match by Match.csv", header=TRUE, sep=";", skip = 0, na.strings = "NA")

df %>% colnames() %>% length()
str(df)
#Comprobar que no haya valores ausentes
is.na(df)

df<-df %>% rename("Date"="ï..Date")
df$Date<-as.Date(df$Date,"%Y-%m-%d",optional=FALSE)

#Unir nombre y apellido de los jugadores
df$Player.Name<-paste(df$Player.Forename,df$Player.Surname)

#Añadir columna con tipo formación, según pdf con información
df$Formation.Type <-(df$Team.Formation=="2")*442+(df$Team.Formation=="3")*41212+
  (df$Team.Formation=="4")*433+(df$Team.Formation=="5")*451+(df$Team.Formation=="6")*4411+
  (df$Team.Formation=="7")*4141+(df$Team.Formation=="8")*4231+(df$Team.Formation=="9")*4321+
  (df$Team.Formation=="10")*532+(df$Team.Formation=="11")*541+(df$Team.Formation=="12")*352+
  (df$Team.Formation=="13")*343+(df$Team.Formation=="14")*31312+(df$Team.Formation=="15")*4222+
  (df$Team.Formation=="16")*3511+(df$Team.Formation=="17")*3421+(df$Team.Formation=="18")*3412
    
#comprobamos que ambas columnas se han creado
colnames(df) 

# #### ANÁLISIS DESCRIPTIVO ####
#¿Cuántos jugadores estamos analizando? -> 539
str(as.factor(df$Player.ID))
#¿Cuántos equipos estamos analizando? -> 20
str(as.factor(df$Team))
#¿Cuántos tipos de formaciones hay? -> 11
str(as.factor(df$Team.Formation))


# #### VISUALIZACIÓN ####
#Goles por equipo en toda la temporada
#ggplot(df)+geom_col(aes(x=Team,y=Goals))+theme(axis.text.x=element_text(angle=90, hjust=1))
#INCORRECTO, lo haremos haciendo group_by(Team)


# #### PARTIDOS ####
#### Goles ####
#Crear df de equipo de casa (Team), goles a favor
dfTeam<-df%>%group_by(Team)%>%summarise(TotalGoals=sum(Goals))
#Crear df de equipo visitante (Opposition), goles en contra
dfOpposition<-df%>%group_by(Opposition)%>%summarise(TotalGoals=sum(Goals))

# ggplot()+geom_col(data=dfTeam, mapping=aes(x=Team,y=GoalFor), fill="red")+
#   geom_col(data=dfOpposition, aes(x=Opposition,y=GoalAgainst),fill="blue",alpha=0.5)+
#   theme(axis.text.x=element_text(angle=90, hjust=1))+
#   labs(x="Team", y="Goals",title="Goals For and Against")

# ggplot()+geom_bar(data=dfOpposition, aes(x=Opposition,y=GoalAgainst),stat="identity",fill="blue")+
#   theme(axis.text.x=element_text(angle=90, hjust=1))+
#   labs(x="Team", y="Goals",title="Goals For and Against")

df2<-cbind(dfTeam,dfOpposition) #quizá eliminar, no la usamos, es mejor hacer rbind

# ggplot()+geom_bar(data=dfOpposition, aes(x=Opposition,y=GoalAgainst),stat="identity",fill="blue")+
#   geom_bar(data=dfTeam, aes(x=Team,y=GoalFor),stat="identity",fill="red")+
#   theme(axis.text.x=element_text(angle=90, hjust=1))+
#   labs(x="Team", y="Goals",title="Goals For and Against")

#Añadir columna que identifique si es equipo de casa o visitante
#CASA=1 -> Juega en casa; CASA=2-> es visitante
dfTeam$Casa<-rep(1,nrow(dfTeam))
dfOpposition$Casa<-rep(2,nrow(dfOpposition))

#renombrar columna Opposition para poder unir dfTeam y dfOpposition con rbind
dfOpposition<-dfOpposition %>% rename("Team"="Opposition")

df3<-rbind(dfTeam,dfOpposition)

#Goles a favor y en contra durante toda la liga, por equipos
Casa<-df3$Casa
ggplot(df3)+geom_bar(aes(x=Team,y=TotalGoals),stat="identity",position="stack",fill=Casa)+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(x="Team", y="Goals",title="Goals For and Against")+
  annotate(geom="text",x=16,y=120,vjust = .5, hjust = .5,label="Black: Goals for \n Red: Goals against", size=3)
####INTENTAR PONER FONDO A LA LEYENDA


####REVISAR WINNER####
TeamWinner<-(dfTeam$TotalGoals>dfOpposition$TotalGoals)*1
OppositionWinner<-(dfOpposition$TotalGoals>dfTeam$TotalGoals)*1

dfTeamWinner<-cbind(dfTeam,TeamWinner)
dfOppositionWinner<-cbind(dfOpposition,OppositionWinner)

dfTeamWinner<-dfTeamWinner %>% rename("Winner"="TeamWinner")
dfOppositionWinner<-dfOppositionWinner %>% rename("Winner"="OppositionWinner")

dfWinners<-rbind(dfTeamWinner,dfOppositionWinner)
# heatmap(dfWinners, Colv = NA, Rowv = NA, scale="none")

dfMatch<-df%>%group_by(Date,Team,Opposition)%>%summarise(Result=sum(Goals))
dfMatch$Result<-as.factor(Result)
ggplot(dfMatch,aes(Team,Opposition))+geom_tile(aes(fill=Result))+
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11, 'RdYlBu')), space='Lab')(100))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(x="Team, home", y="Opposition, visitor",title="Goals Scored in All Matches")
      # En este Heatmap se ve los goles que se marcaron en cada partido jugado entre dos equipos
      # No se sabe quién ganó, solo la gantidad de goles marcados en total por ambos equipos


#### intento INCORRECTO de correlograma para ver cuantos goles se hacen unos a otros
# ggplot(dfMatch,aes(x=Team,y=Opposition))+geom_jitter(width=0.1,height=0.1,shape=21)+
#   theme(axis.text.x=element_text(angle=90, hjust=1))
# ggplot(dfMatch,aes(x=Team,y=Opposition))+geom_point(aes(size=Result),shape=21)+
#   theme(axis.text.x=element_text(angle=90, hjust=1))


# #### ANÁLISIS MANCHESTER CITY, GANADOR DE LA LIGA ####
dfCity<-df[df$Team == "Manchester City",]

#Goles por partido (según fecha)
#########COMPROBAR RESULTADO, SI ES GOLES MARCADOS POR MANCHESTER O POR AMBOS!!!
dfCity2<-df[df$Team=="Manchester City",]%>%group_by(Date,Opposition)%>%summarise(TotalGoals=sum(Goals))
dfCity2$TeamNumber<-rep(1,nrow(dfTeam))
Opp<-as.factor(dfCity2$Opposition)
display.brewer.all()
ggplot(dfCity2)+ geom_point(mapping=aes(x=Date,y=TotalGoals,colour=Opp,size=3),stat="identity")+
  xlab("Date")+ ylab("Goals Scored")+
  ggtitle("Goals Socored By Manchester City")+theme_bw()

#HABRÍA QUE AÑADIR LEYENDA CON NUMEROS PARA CADA EUQIPO

#### Análisis Jugadores Manchester City ####
str(as.factor(dfCity$Player.Name))
  #Han jugado 24 jugadores para este equipo en toda la temporada

####REVISAR GRAFICOS, LEYENDA, ECT####
#Con qué pie chutan más goles (en toda la liga)
ggplot(dfCity)+geom_col(aes(x=Player.Name,y=Left.Foot.Goals), fill="red",width=0.5)+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  geom_col(aes(x=Player.Name,y=Right.Foot.Goals),fill="blue",width=0.5)+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#Tiempo por jugador (en toda la liga)
ggplot(dfCity)+geom_col(aes(x=Player.Name,y=Time.Played))+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(x="Player", y="Time Played (min)",title="Time played by player")

#####PARA USAR, Goles por Dia
# ggplot(data = afc, aes(x = as.numeric(Year), y = GF)) +
#   geom_line(size = 2.0, alpha = 0.7, color = "firebrick") +
#   geom_point(size = 1.0) +
#   xlab("Year") + 
#   ylab("Goals Scored") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.9)) +
#   scale_x_continuous(breaks = seq(1992,2015)) +
#   ggtitle("Arsenal Goals Scored per Season")


#### ANÁLISIS DE xxxxxx, FICHAJE MÁS CARO AL FINAL DE LA TEMPORADA? ####



#### ANÁLISIS FORMACIONES ####
#Qué formaciones se usan más [NO POR PARTIDO]
ggplot(df)+geom_bar(aes(x=Team.Formation,fill=as.factor(Formation.Type)))+scale_fill_discrete(name = "Formation Type")

#partidos donde se usa Formación 442
df442<-df[(df$Formation.Type=="442"),]

#### ANÁLISIS JUGADAS ####
#### Penaltis ####
dfPenaltisFailed<-df%>%group_by(Team)%>%summarise(numPenaltis=sum(Penalties.Not.Scored))
dfPenaltyGoals<-df%>%group_by(Team)%>%summarise(numPenaltisScored=sum(Penalty.Goals))

#Porcentaje penaltis marcados
sum(dfPenaltyGoals$numPenaltisScored)/sum(df$Penalties.Taken)
#Porcentaje penaltis fallados
sum(dfPenaltisFailed$numPenaltis)/sum(df$Penalties.Taken)
