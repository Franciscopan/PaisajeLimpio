library(readxl)
Hoja_8_DatosInfraestrucTierrra <- read_excel("Andalucia/Hoja 8 DatosInfraestrucTierrra.xlsx")
InfraTAndalucia<-Hoja_8_DatosInfraestrucTierrra
rm(Hoja_8_DatosInfraestrucTierrra)
library("tidyverse")
library(modeest)
#Contenedores por puertos autonómicos y no autonómicos
ContPUertAND<-InfraTAndalucia%>%
  group_by(Competencia,Flujo)
(ContPuertAND1<-ContPUertAND%>%
    summarise(NumeroFlujos=n(), Total_cont=sum(Numero),Tam_medio=mean(Capacidad_litros),Tam_Max
              =max(Capacidad_litros),Tam_min=min(Capacidad_litros),Tam_Mediana=median(
                Capacidad_litros
              ),Tam_mas_frec=mfv(Capacidad_litros)[1]))
#todos
Cont<-InfraTAndalucia%>%
  group_by(Flujo,Capacidad_litros)%>%
  summarise(Ntotal=sum(Numero,na.rm = TRUE))
Cont
#Contenedores por puerto
ContP<-InfraTAndalucia%>%
  group_by(Competencia,Nombre_puerto,Capacidad_litros)%>%
  summarise(NtotalP=sum(Numero,na.rm = TRUE))
ContP
#contenedores por flujo y capacidad
ContPR<-InfraTAndalucia%>%
  group_by(Nombre_puerto,Flujo,Capacidad_litros)%>%
  summarise(NtotalP=sum(Numero,na.rm = TRUE))
ContPR
#Contenedores por totales
SumaContTamno<-Cont%>%
  group_by(Capacidad_litros)%>%
  summarise(Total=n(),Total1=sum(Ntotal))
SumaContTamno
#Flujos por puerto
ContPRF<-InfraTAndalucia%>%
  group_by( Competencia,Nombre_puerto,Flujo)%>%
  summarise(NtotalP=sum(Numero,na.rm = TRUE),n())
ContPRF
#Pasándolo a un EXCEL
library("r2excel")
str(ContPuertAND1)
wb<-createWorkbook(type = "xlsx")
sh1<-createSheet(wb,sheetName = "InfraFlujos")
xlsx.addHeader(wb,sh1,value = "FLUJOS DE RECOGIDA EN CONTENDORES Y CARACTERÍSTICAS DE LOS CONTENEDORES",
               color = "black")
xlsx.addLineBreak(sh1,3)
xlsx.addTable(wb,sh1,as.data.frame(ContPuertAND1),startCol = 3)
saveWorkbook(wb,"FlujosinfraestructuraAndalucia.xlsx")