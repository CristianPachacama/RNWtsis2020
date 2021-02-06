# =================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     SERVER      !!!!!!!!!!!!!!!!!!!!!!!!
# =================================================================
function(input, output,session) {
  
  #Analisis Cluster  ------------------------------
  # source("Code/Clusters/MapaEstaciones.R",local = TRUE)
  source("Code/Clusters/Analisis.R",local = TRUE)
  source("Code/Clusters/Mapa.R",local = TRUE)
  source("Code/Clusters/Tabla.R",local = TRUE)
  source("Code/Clusters/Grafico.R",local = TRUE)
  #Link a panel Modelamiento
  observeEvent(input$pestania_acpf, {
    updateNavbarPage(session, "tesis", "ACP Funcional")
  })
  
  
  # Analisis Componentes Principales --------------
  observe({
    updateSelectInput(session,inputId = "n_clus_acpf2",
                      choices = 1:as.numeric(input$vaz_clus_k) )
  })
  source("Code/ACP Funcional/1_TablaVaz.R",local = TRUE)
  source("Code/ACP Funcional/1_GraficoVaz.R",local = TRUE)
  source("Code/ACP Funcional/2_ACP Funcional.R",local = TRUE)
  source("Code/ACP Funcional/2_ACPF Graficos.R",local = TRUE)
  observeEvent(input$pestania_cluster2, {
    updateNavbarPage(session, "tesis", "Clusters")
  })
  
  
  
  # Modelamiento SARIMAX  -------------------------
  observe({
    updateSelectInput(session,inputId = "n_clus_acpf3",
                      choices = 1:as.numeric(input$vaz_clus_k) )
  })
  source("Code/SARIMAX/0_Datos Clima.R",local = TRUE)
  source("Code/SARIMAX/1_Lista Vazoes.R",local = TRUE)
  source("Code/SARIMAX/1_Grafico Vazoe.R",local = TRUE)
  
  source("Code/ACP Funcional/Clima/ACP Clima.R",local = TRUE)
  source("Code/SARIMAX/2_Datos Reactivos.R",local = TRUE)
  source("Code/SARIMAX/2_Tabla Regresoras.R",local = TRUE)
  source("Code/SARIMAX/2_Grafico Regresoras.R",local = TRUE)
  
  source("Code/SARIMAX/Extras/TrainTest.R",local = TRUE)
  source("Code/SARIMAX/3_Modelo.R",local = TRUE)
  source("Code/SARIMAX/3_Resultados.R",local = TRUE)
  observeEvent(input$pestania_cluster3, {
    updateNavbarPage(session, "tesis", "Clusters")
  })
  #Subtitutlo Resultados
  output$estacion_modelada = renderText({
    paste("Modelamiento Estación:",input$nomb_est_vaz3)
  })
}
