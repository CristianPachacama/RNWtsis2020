#=====================    Estadisticos Validacion   =============================

aic = function(modelo){ AIC(modelo) }

# BIC
bic = function(modelo){
  if(is.null(modelo$nobs)){
    n=length(modelo$residuals)
  }else{
    n = modelo$nobs
  }
  bc=AIC(modelo,k = log(n))
  return(bc)
}
# AICc    
aicc = function(modelo){
  if(is.null(modelo$nobs)){
    n=length(modelo$residuals)
  }else{
    n = modelo$nobs
  }
  p = length(modelo$coef)
  aic0 = AIC(modelo) + 2*p*((p+1)/(n-p-1))
  return(aic0)
}
# HQC
hannanQ = function(modelo){
  if(is.null(modelo$nobs)){
    n=length(modelo$residuals)
  }else{
    n = modelo$nobs
  }
  AIC(modelo, k=2.1*log(log(n))  )
}




#================       Funcion de Graficas Series de Tiempo      ========================

ts_ggplot = function(serieDF , n_variable=3 , ylabel ="Caudal"){
  serNomb = names(serieDF)
  serNomb = paste0("`",serNomb,"`")
  ggplot(data = serieDF, 
         aes_string(x = "Fecha", 
                    y = serNomb[n_variable] )) +
    geom_line(size = 0.7) + 
    theme_minimal() +
    scale_x_date(                                        
      breaks = "6 months",
      date_labels = "%b %Y"
    )+
    labs(
      # title = "Caudal del clÃºster 1", 
      # subtitle = "Media Funcional",
      y = ylabel
    )+
    theme(
      axis.text.x=element_text(size = 6 , angle=90),
      axis.text.y=element_text(size = 6 )
    )
  
  # return(p1)
}






#============    Funcion de Graficas AutocorrelaciÃ³n y Parcial    =======================

acf_ggplot = function(serie){
  
  acf_vz = autoplot(acf(serie,lag.max = 36, plot = FALSE),
                    conf.int.fill = '#0000FF', 
                    conf.int.value = 0.8, 
                    conf.int.type = 'ma') + 
    theme_minimal() +
    labs(
      title = "Autocorrelación"#,
      # subtitle = "Media Funcional",
      # y = "Caudal"
    )
  
  
  pacf_vz = autoplot(pacf(serie,lag.max = 36, plot = FALSE),
                     conf.int.fill = '#0000FF', 
                     conf.int.value = 0.8, 
                     conf.int.type = 'ma') + 
    theme_minimal() +
    labs(
      title = "Autocorrelación Parcial"#,
      # subtitle = "Media Funcional",
      # y = "Caudal"
    )
  
  grid.arrange(
    grobs = list(acf_vz,pacf_vz),
    # widths = c(3, 2),
    layout_matrix = rbind(c(1), c(2))
  )
  
  
}






#============    Funcion de Graficas AutocorrelaciÃ³n y Parcial    =======================

ccfab_ggplot = function(serie_y,serie_x = NULL, modelo = NULL,txt_titulo=NULL){
  
  
  ## Correlacion cruzada  GGPLOT  ---
  if(is.null(serie_x)){
    
    if(!is.null(modelo)){
      modx = modelo
      alf = modx$residuals
      s_alf = sd(alf)
      # beta
      bet = forecast::forecast(object = serie_y ,model=modx)
      bet = bet$residuals
      s_bet = sd(bet)
    }else{
      print("Error: Debe ingresar la serie_x o modelo")
    }
    
    cc_ab = autoplot(ccf(as.numeric(alf), as.numeric(bet),
                         lag.max = 36, plot = FALSE),
                     conf.int.fill = '#0000FF', 
                     conf.int.value = 0.8, 
                     conf.int.type = 'ma') + 
      theme_minimal() +
      labs(
        title =TeX("Correlación Cruzada $\\alpha_t$ vs. $\\beta_t$") #,
        # subtitle = "Media Funcional",
        # y = "Caudal"
      )
  }else{
    
    if(is.null(txt_titulo)){
      titulo = "Correlación Cruzada"
    }else{
      titulo = paste("Correlación Cruzada",txt_titulo)
    }
    
    cc_ab = autoplot(ccf(as.numeric(serie_y), as.numeric(serie_x),
                         lag.max = 36, plot = FALSE),
                     conf.int.fill = '#0000FF', 
                     conf.int.value = 0.8, 
                     conf.int.type = 'ma') + 
      theme_minimal() +
      labs(
        title =TeX(titulo) #,
        # subtitle = "Media Funcional",
        # y = "Caudal"
      )
  }
  
  
  return(cc_ab)
  
}






#===================      Tabla de Modelos SARIMAX        ===============================

tab_model = function(modelo,caption_text=NULL,label_text = NULL,orientacion="vertical"){
  options(digits=4)
  t_model = coeftest(modelo)
  m = dim(t_model)[1]
  tabla = data_frame("Coef"=NA,"Estimate"=NA,"Std.Error"=NA,"z-value"=NA,"Pr(>|z|)"=NA)
  for(i in seq(m)){
    tabla[i,] = c(0,as.numeric(t_model[i,]))
  }
  tabla$Coef = rownames(t_model)
  Pval = tabla$`Pr(>|z|)`
  rangos = cut(Pval,breaks = c(0,0.001,0.01,0.05,0.1,1),
               labels = c("***","**","*","."," "))
  rangos[is.na(rangos)] = "   "
  tabla$Signif = rangos
  
  # Caption   --------
  if(is.null(caption_text)){
    caption_text0 = "Modelo"
  }else{
    caption_text0 = paste("Modelo",caption_text)
  }
  
  # Texto CAPTION  ------
  texto = NULL
  if(is.null(label_text)){
    texto = caption_text0 
  }else{
    texto = paste0("\\label{",label_text,"}",caption_text0)
  }
  
  #Estadisticos AIC BIC LOG -----
  nota = paste0("sigma^2 = ", round(modelo$sigma2,2),", ",
                "loglikelihood = ", round(modelo$loglik,2),", ",
                "AIC = ", round(aic(modelo),2),", ",
                "BIC =", round(bic(modelo),2),", ",
                "Hannan-Quinn =", round(hannanQ(modelo),2)
  )
  
  
  # Tabla final ------
  if(orientacion =="horizontal"){
    kable(tabla,format = "latex",
          escape = F,booktabs = T,
          # label = label_text,
          # caption = caption_text0) %>% 
          caption = texto) %>%
      footnote(general = nota,
               general_title = "Resumen:",
               title_format = c("italic"),
               # footnote_as_chunk = T,
               threeparttable = T
      ) %>%
      kable_styling(latex_options = c("striped")) %>%
      landscape()
  }else{
    kable(tabla,format = "latex",
          escape = F,booktabs = T,
          # label = label_text,
          # caption = caption_text0) %>% 
          caption = texto) %>%
      footnote(general = nota,
               general_title = "Resumen:",
               title_format = c("italic"),
               # footnote_as_chunk = T,
               threeparttable = T) %>%
      kable_styling(latex_options = c("striped"))
  }
  
}




#=======================    Tabla MODELOS CLUSTER     ===========================

ResumenModelos = function(Lista_Modelos){
  options(digits=4)
  t_model = coeftest(Lista_Modelos[[1]])
  TABLA = matrix(NA,nrow = length(Lista_Modelos),ncol = length(rownames(t_model))+ 5  )
  TABLA = data.frame(TABLA)
  names(TABLA) = c("Estación-Caudal",rownames(t_model),"sigma^2","Log-Verosim","AIC","BIC")
  nc = dim(TABLA)[2]
  
  # Info de cada modelo - Estacion
  for(md in seq(length(Lista_Modelos))){
    
    modelo = Lista_Modelos[[md]]
    t_model = coeftest(modelo)
    m = dim(t_model)[1]
    
    Pval = as.numeric(t_model[,4])
    rangos = cut(Pval,breaks = c(0,0.001,0.01,0.05,0.1,1),
                 labels = c("***","** ","*  ",".  ","   "))
    rangos[is.na(rangos)] = "   "
    coefMod = as.numeric(t_model[,1])
    coefMod = paste(round(coefMod,3),rangos)
    #Estadisticos AIC BIC LOG -----
    
    sigma2 = round(modelo$sigma2,2)
    loglike=  round(modelo$loglik,2)
    AICmd = round(aic(modelo),2)
    BICmd = round(bic(modelo),2)
    HannQ = round(hannanQ(modelo),2)
    
    # Completar Tabla Modelos -----
    TABLA[md,1] = names(Lista_Modelos)[md] 
    TABLA[md,(2:(1+m))] = coefMod
    TABLA[md,((2+m):nc)] = c(sigma2,loglike,
                             AICmd,BICmd)
   
    
  }
  
  return(TABLA)
  
}



kableResumen = function(TABLA,caption_text=NULL,label_text = NULL,orientacion="vertical"){
  # Caption   --------
  if(is.null(caption_text)){
    caption_text0 = "Modelo"
  }else{
    caption_text0 = paste("Modelo",caption_text)
  }
  
  # Texto CAPTION  ------
  texto = NULL
  if(is.null(label_text)){
    texto = caption_text0 
  }else{
    texto = paste0("\\label{",label_text,"}",caption_text0)
  }
  
  # Tabla final ------
  if(orientacion=="horizontal"){
    kable(TABLA,format = "latex",
          escape = F,booktabs = T,
          # label = label_text,
          # caption = caption_text0) %>%
          caption = texto) %>%
      kable_styling(latex_options = c("striped","scale_down")) %>% 
      landscape()
    
  }else{
    kable(TABLA,format = "latex",
          escape = F,booktabs = T,
          # label = label_text,
          # caption = caption_text0) %>%
          caption = texto) %>%
      kable_styling(latex_options = c("striped","scale_down"))
    
  }
   
  
}

