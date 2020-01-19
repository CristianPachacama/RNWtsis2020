#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#------------------        global.R       ------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

pkgTest <- function(x){
  if (!require(x,character.only = TRUE)){
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Paquete no encontrado")
  }
}
#Descarga de Paquetes ================================
pkgTest("shinydashboard")
pkgTest("ggplot2")
pkgTest("dygraphs")
pkgTest("TSstudio")
pkgTest("leaflet")
pkgTest("htmltools")
pkgTest("rgdal")
pkgTest("readr")
pkgTest("DT")
pkgTest("dplyr")
pkgTest("reshape2")
pkgTest("lmtest")
pkgTest("TSdist")
pkgTest("xts")
pkgTest("stlplus")
pkgTest("TSA")
pkgTest("forecast")
pkgTest("smacof")
pkgTest("cluster")
pkgTest("ks")
pkgTest("fpca")
pkgTest("fdapace")

# Paquetes Necesarios ===============
library(shiny)
library(shinythemes)
library(shinydashboard)
#Graficos
library(ggplot2)
library(dygraphs)
library(TSstudio)
#Mapas
library(leaflet)
library(htmltools)
library(rgdal)
#Tablas
library(readr)
library(DT)
library(dplyr)
library(reshape2)
#Estadisticos
library(lmtest)
#Series de Tiempo
library(TSdist)
library(xts)
library(TSA)
library(forecast)
# STL - Loess
library(stlplus)
#MDS y Cluster
library(smacof)
library(cluster)
#ACP Funcional
library(ks)
library(fpca)
library(fdapace)

#>> Carga de Datos
load('Data/Actual/InterfazMes.RData')
load('Data/Actual/DataVazoes.RData')
load("Data/Actual/VazoesCode.RData")
clima_dat=clima_dat2
particion = 0.20 #Particion Entrenamiento
set.seed(2) 
# Matiz de Distancias 
source(file ="Code/SARIMAX/Extras/DistanciasAVazoes.R",local = TRUE) 
