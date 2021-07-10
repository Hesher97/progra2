library(tidyverse)
library(dplyr)
estaciones<-read.csv("https://raw.githubusercontent.com/Hesher97/progra2/master/raingaugeDataset.csv")
codigos<-read.csv("https://raw.githubusercontent.com/Hesher97/progra2/master/listRaingauge.csv")
codigo<-codigos[codigos$NOM_EST=="TABACONAS",]
codigo_n<-codigo[,"CODIGO"]
estacion<-estaciones[,codigo_n]
fecha<-estaciones[,"date"]
fecha2<-dmy(fecha)
df1<-data.frame(fecha2,estacion)
df1$dia<-day(df1$fecha2)
df1$mes<-month(df1$fecha2)
df1$año<-year(df1$fecha2)
missvaluedaily<-df1 %>% 
  group_by(fecha2=str_sub(fecha2,1,10)) %>% 
  mutate(missval=sum(is.na(estacion)))
ppm<-
  df1 %>% 
  group_by(fecha2=str_sub(fecha2,1,7)) %>% 
  mutate(
    missval=sum(is.na(estacion)) * 100 / n()
  ) %>%
  summarize(
    estacion=sum(estacion,na.rm=T),
    missval=unique(missval)
  ) %>% 
  mutate(
    estacion=ifelse(missval>=10,NA,estacion),
    fecha2 = as.Date(sprintf("%1$s-01",fecha2)),
    month=str_sub(fecha2,6,7)
  )
missvaluemonthly<-df1 %>% 
  group_by(fecha2=str_sub(fecha2,1,7)) %>% 
  mutate(missval=sum(is.na(estacion))) %>%
  summarize(
    estacion=sum(estacion,na.rm=T),
    missval=unique(missval)
  )

ggplot(ppm, aes(fecha2,estacion))+
  geom_line()
ggplot(ppm, aes(month,estacion))+
  geom_boxplot()+
  theme_bw()+
  scale_x_discrete(
    labels=month.abb
  )
climatologia<-
  ppm %>% 
  group_by(month) %>%
  summarize(
    estacion=mean(estacion,na.rm=T)
  )
