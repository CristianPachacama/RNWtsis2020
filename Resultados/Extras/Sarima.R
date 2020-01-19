library(readxl)
library(readr)
library(dplyr)
library(lmtest)

getwd()
VazACP1 = read_excel("Data/VazoeACP1.xlsx")
VazACP2 = read_excel("Data/VazoeACP2.xlsx") # Revisar APP!!! no esta filtrando por cluster
VazACP3 = read_excel("Data/VazoeACP3.xlsx")
VazACP4 = read_excel("Data/VazoeACP4.xlsx")

#....................................

ModelosCl =list()
#....................................
for(nclus in 1:4){
  
  vazoeP = switch(as.character(nclus),
                  '1' = VazACP1,
                  '2' = VazACP2,
                  '3' = VazACP3,
                  '4' = VazACP4
  )
  vazoeP$Fecha = as.Date(vazoeP$Fecha)
  y = ts(vazoeP$VazoePCA,start = c(2000,1),frequency = 12)
  # Modelos SARIMA  ===================================
  
  if(nclus==1){
    #Parametros del modelo
    p=4;q=0
    P=1;Q=0
    d=0;D=1
    p_lags = rep(NA, p);q_lags = rep(NA, q)
    P_lags = rep(NA, P);Q_lags = rep(NA, Q)
    
    retardos = c(p_lags,q_lags,P_lags,Q_lags)
    if(d==0 & D==0) retardos = c(retardos,NA)
  }
  if(nclus==2){
    
    p=4;q=3
    P=1;Q=0
    d=0;D=1
    p_lags = rep(NA, p);q_lags = rep(NA, q)
    P_lags = rep(NA, P);Q_lags = rep(NA, Q)
    
    retardos = c(p_lags,q_lags,P_lags,Q_lags)
    if(d==0 & D==0) retardos = c(retardos,NA)
    
  }
  if(nclus==3){
    
    p=2;q=1
    P=1;Q=0
    d=0;D=1
    p_lags = rep(NA, p);q_lags = rep(NA, q)
    P_lags = rep(NA, P);Q_lags = rep(NA, Q)
    
    retardos = c(p_lags,q_lags,P_lags,Q_lags)
    if(d==0 & D==0) retardos = c(retardos,NA)
    
  }
  if(nclus==4){
    
    p=3;q=2
    P=1;Q=0
    d=0;D=1
    p_lags = rep(NA, p);q_lags = rep(NA, q)
    P_lags = rep(NA, P);Q_lags = rep(NA, Q)
    
    retardos = c(p_lags,q_lags,P_lags,Q_lags)
    if(d==0 & D==0) retardos = c(retardos,NA)
    
  }
  
  ModelosCl[[nclus]] = arima(y,order = c(p,d,q),
                         seasonal = list(order = c(P,D,Q), period = 12),
                         fixed = retardos)
  
  print(paste("!!!!!!!!!!!      Cluster: ",nclus,"     !!!!!!!!!!!"))
  print(coeftest(ModelosCl[[nclus]]))
  tsdiag(ModelosCl[[nclus]], gof.lag = 36)
}

save(ModelosCl,file = "sarima_representantes.RData")



