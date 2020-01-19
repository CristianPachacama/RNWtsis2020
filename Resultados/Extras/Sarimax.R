library(readxl)
library(readr)
library(dplyr)
library(lmtest)
library(TSA)
getwd()
# Datos -----------------
load(file = "Data/Vazoe.RData")
# source("GraficosTablasFunc.R")
Clima1 = read_csv("Data/ClimaLimpio1.csv"); Clima1 = Clima1[,-1]
Clima2 = read_csv("Data/ClimaLimpio2.csv"); Clima2 = Clima2[,-1]
Clima3 = read_csv("Data/ClimaLimpio3.csv"); Clima3 = Clima3[,-1]
Clima4 = read_csv("Data/ClimaLimpio4.csv"); Clima4 = Clima4[,-1]

VazoeClima = read_excel("Data/MinVazoeClima_corregido.xlsx"); VazoeClima = VazoeClima[,-1]

# Medoides Clusters ------
vaz_clust = c("MACHADINHO (217)", "ERNESTINA (110)","MIRANDA (206)","ITAIPU ARTIFICIAL (66)")



#....................................
ModelosXCl =list()
# nclus = 1
#.......................................................
for(nclus in 4){
  # Datos Necesarios
  { 
    
    clima = switch(as.character(nclus),
                   '1' = Clima1,
                   '2' = Clima2,
                   '3' = Clima3,
                   '4' = Clima4
    )
    Vazoe = Vazoe2015 %>% filter(Fecha<=clima$Fecha[dim(clima)[1]])
    # nmb_vazoe = VazoeClima %>% filter(Cluster==nclus) %>% select(Vazoes)
    nmb_vazoe = vaz_clust[nclus]
    vazoe = Vazoe %>% select(nmb_vazoe)
    fecha0r = c(2000,1)
    
    #.........................
    VazTrain = vazoe
    nmb_clim = VazoeClima %>% filter(Vazoes == names(VazTrain)) %>% select(Clima)
    
    if(length(nmb_clim$Clima)>0){
      Xtrain = clima %>% select(starts_with(nmb_clim$Clima))
      if(dim(Xtrain)[2]==0){
        print(paste("+++++ No hay estacion de clima",nmb_clim$Clima,", se uso clima de medoide +++++"))
        nmb_clim = VazoeClima %>% filter(Vazoes == vaz_clust[nclus]) %>% select(Clima)
        Xtrain = clima %>% select(starts_with(nmb_clim$Clima))
      }
    }else{
      print("*** Error: No existe la variable climatica .....")
      break
    }
    XData = Xtrain ; XData$Fecha = clima$Fecha
    Xtrain = ts(Xtrain,start=fecha0r,frequency = 12)
    # Xblanc = Xtrain
    
    VazData = VazTrain ; VazData$Fecha = Vazoe$Fecha
    VazTrain = ts(VazTrain,start=fecha0r,frequency = 12)
    
    # Cambio Nombre Regresoras
    y=window(VazTrain,start=c(2001,1))
    XblancNmb = window(Xtrain,start=c(2001,1))
    colnames(XblancNmb) = c("Precipitacion","TemperaturaMax","TemperaturaMin","HumedadRelativa")
    
  }
  
  # Modelos SARIMAX  ===================================
  
  if(nclus==1){
    # Parametros del modelo ...............
    media = FALSE
    p=1;q=0
    P=2;Q=0
    d=0;D=1
    p_lags = rep(NA, p);q_lags = rep(NA, q)
    P_lags = rep(NA, P);Q_lags = rep(NA, Q)
    
    #.......................................
    # Ordenes Funciones de Respuesta al Impulso
    tar1 = 2 #Transf - AR
    tma1 = 0 #Transf - MA
    
    tar2 = 0 #Transf - AR
    tma2 = 0 #Transf - MA
    
    tar3 = 0 #Transf - AR
    tma3 = 0  #Transf - MA
    
    tar4 = 2 #Transf - AR
    tma4 = 0  #Transf - MA
    
    tar1_lags = rep(NA, tar1); tma1_lags = rep(NA, tma1+1);
    tar2_lags = rep(NA, tar2); tma2_lags = rep(NA, tma2+1);
    tar3_lags = rep(NA, tar3); tma3_lags = rep(NA, tma3+1);
    tar4_lags = rep(NA, tar4); tma4_lags = rep(NA, tma4+1);
    
    # tar1_lags[5] = 0
    # tar4_lags[2] = 0
    tma3_lags[1] = 0
    tma2_lags[1] = 0
    tar4_lags[1] = 0
    #........................................
    retardos = c(p_lags,q_lags,P_lags,Q_lags)
    if(d==0 & D==0 & media) retardos = c(retardos,NA)  # Si include.mean = TRUE  compilar
    retardos = c(retardos,
                 tar1_lags,tma1_lags,
                 tar2_lags,tma2_lags,
                 tar3_lags,tma3_lags,
                 tar4_lags,tma4_lags)
    
  }
  if(nclus==2){
    # Parametros del modelo ................
    media = FALSE
    p=1;q=0
    P=2;Q=0
    d=0;D=1
    p_lags = rep(NA, p);q_lags = rep(NA, q)
    P_lags = rep(NA, P);Q_lags = rep(NA, Q)
    
    #.......................................
    # Ordenes Funciones de Respuesta al Impulso
    tar1 = 0 #Transf - AR
    tma1 = 0 #Transf - MA
    
    tar2 = 0 #Transf - AR
    tma2 = 0 #Transf - MA
    
    tar3 = 0 #Transf - AR
    tma3 = 0  #Transf - MA
    
    tar4 = 3 #Transf - AR
    tma4 = 0  #Transf - MA
    
    tar1_lags = rep(NA, tar1); tma1_lags = rep(NA, tma1+1);
    tar2_lags = rep(NA, tar2); tma2_lags = rep(NA, tma2+1);
    tar3_lags = rep(NA, tar3); tma3_lags = rep(NA, tma3+1);
    tar4_lags = rep(NA, tar4); tma4_lags = rep(NA, tma4+1);
    
    tma1_lags[1] = 0
    tma2_lags[1] = 0
    tma3_lags[1] = 0
    tar4_lags[1] = 0
    #........................................
    retardos = c(p_lags,q_lags,P_lags,Q_lags)
    if(d==0 & D==0 & media) retardos = c(retardos,NA)  # Si include.mean = TRUE  compilar
    retardos = c(retardos,
                 tar1_lags,tma1_lags,
                 tar2_lags,tma2_lags,
                 tar3_lags,tma3_lags,
                 tar4_lags,tma4_lags)
    
  }
  if(nclus==3){
    # Parametros del modelo ................
    media = FALSE
    p=1;q=0
    P=1;Q=0
    d=0;D=1
    p_lags = rep(NA, p);q_lags = rep(NA, q)
    P_lags = rep(NA, P);Q_lags = rep(NA, Q)
    
    #.......................................
    # Ordenes Funciones de Respuesta al Impulso
    tar1 = 1 #Transf - AR
    tma1 = 0 #Transf - MA
    
    tar2 = 0 #Transf - AR
    tma2 = 0 #Transf - MA
    
    tar3 = 0 #Transf - AR
    tma3 = 0  #Transf - MA
    
    tar4 = 0 #Transf - AR
    tma4 = 0  #Transf - MA
    
    tar1_lags = rep(NA, tar1); tma1_lags = rep(NA, tma1+1);
    tar2_lags = rep(NA, tar2); tma2_lags = rep(NA, tma2+1);
    tar3_lags = rep(NA, tar3); tma3_lags = rep(NA, tma3+1);
    tar4_lags = rep(NA, tar4); tma4_lags = rep(NA, tma4+1);
    
    
    tma3_lags[1] = 0
    tma4_lags[1] = 0
    # tar4_lags[1] = 0
    #........................................
    retardos = c(p_lags,q_lags,P_lags,Q_lags)
    if(d==0 & D==0 & media) retardos = c(retardos,NA)  # Si include.mean = TRUE  compilar
    retardos = c(retardos,
                 tar1_lags,tma1_lags,
                 tar2_lags,tma2_lags,
                 tar3_lags,tma3_lags,
                 tar4_lags,tma4_lags)
    
  }
  if(nclus==4){
    # Parametros del modelo ................
    media = FALSE
    p=0;q=3
    P=1;Q=1
    d=0;D=1
    p_lags = rep(NA, p);q_lags = rep(NA, q)
    P_lags = rep(NA, P);Q_lags = rep(NA, Q)
    
    
    #.......................................
    # Ordenes Funciones de Respuesta al Impulso
    tar1 = 0 #Transf - AR
    tma1 = 0 #Transf - MA
    
    tar2 = 0 #Transf - AR
    tma2 = 0 #Transf - MA
    
    tar3 = 0 #Transf - AR
    tma3 = 0  #Transf - MA
    
    tar4 = 0 #Transf - AR
    tma4 = 0  #Transf - MA
    
    tar1_lags = rep(NA, tar1); tma1_lags = rep(NA, tma1+1);
    tar2_lags = rep(NA, tar2); tma2_lags = rep(NA, tma2+1);
    tar3_lags = rep(NA, tar3); tma3_lags = rep(NA, tma3+1);
    tar4_lags = rep(NA, tar4); tma4_lags = rep(NA, tma4+1);
    
    # tar1_lags[5] = 0
    # tar4_lags[2] = 0
    tma1_lags[1] = 0
    tma3_lags[1] = 0
    tma2_lags[1] = 0
    #........................................
    retardos = c(p_lags,q_lags,P_lags,Q_lags)
    if(d==0 & D==0 & media) retardos = c(retardos,NA)  # Si include.mean = TRUE  compilar
    retardos = c(retardos,
                 tar1_lags,tma1_lags,
                 tar2_lags,tma2_lags,
                 tar3_lags,tma3_lags,
                 tar4_lags,tma4_lags)
    
  }
  
  ModelosXCl[[nclus]] = arimax(y, order = c(p,d,q),include.mean = media,
                               seasonal = list(order = c(P,D,Q), 
                                               period = 12),
                               fixed = retardos,
                               # fixed = NULL,
                               xtransf = XblancNmb, transfer = list(c(tar1,tma1),
                                                                    c(tar2,tma2),
                                                                    c(tar3,tma3),
                                                                    c(tar4,tma4))
  )
  
  print(paste("Caudal:",names(vazoe),"***********"))
  print(paste("Clima:",nmb_clim$Clima))
  
  print(paste("!!!!!!!!!!!      Cluster: ",nclus,"     !!!!!!!!!!!"))
  print(coeftest(ModelosXCl[[nclus]]))
  tsdiag(ModelosXCl[[nclus]], gof.lag = 36)
  
}


save(ModelosXCl,file = "sarimax_representantes.RData")

# Tiempo: 550 segundos
