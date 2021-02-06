# ================================================================
# !!!!!!!!!!!!!!!!!!!!!!    USER INTERFACE   !!!!!!!!!!!!!!!!!!!!!
# ================================================================

navbarPage(
  id = 'tesis' ,
  title = "Tesis",
  header = tags$h2(" - ", tags$head(
    tags$link(rel = 'shortcut icon',
              href = 'epn.ico',
              type = 'image/x-icon')
  )),
  position = "fixed-top",
  #theme=shinytheme('flatly'),#theme = 'estilo.css',
  footer = fluidRow(
    column(
      12,
      img(src = 'epn_logo.png', width = '30px', align = 'center'),
      tags$b('Proyecto: '),
      ' "Extreme low Levels of setreamflow in Hydropower Plants".' ,
      '-',
      tags$a('Departamento de Matemática - EPN (2018)', 
             href = 'http://www.epn.edu.ec'),
      tags$b('  ||  '),
      tags$b('Desarrollado por: '),
      tags$a('Cristian Pachacama', href =
               'http://www.linkedin.com/in/cristian-david-pachacama')
    )
  ),
  
  #INTRODUCCION E INFORMACION DEL PROYECTO ---------------------------
  tabPanel(
    'Introducción',
    icon = icon('home'),
    
    fluidRow(
      sidebarPanel(
        img(src = 'epn_logo2.png', width = '90%', align = 'center'),
        fluidRow(' '),
        hr(),
        fluidRow(
          column(3, tags$b('Proyecto Titulación:')),
          column(1),
          column(
            8,
            'Análisis Clúster para series de tiempo estacionales
            y modelización de caudales de ríos del Brasil.'
          )
          ),
        hr(),
        fluidRow(column(3, tags$b('Proyecto Semilla:')), column(1),
                 column(8, 'PIS-16-14')),
        hr(),
        fluidRow(
          column(3, tags$b('Linea de Investigación:')),
          column(1),
          column(8, 'Modelos Econométricos')
        ),
        hr(),
        fluidRow(column(3, tags$b('Departamento:')), column(1),
                 column(8, 'Matemática')),
        hr(),
        fluidRow(
          column(3, tags$b('Directora:')),
          column(1),
          column(8, 'PhD. Adriana Uquillas')
        ),
        hr(),
        fluidRow(column(3, tags$b('Autor:')), column(1),
                 column(8, 'Cristian Pachacama'))
        
        
        ),
      
      mainPanel(
        h3(
          'Análisis Clúster para series de tiempo estacionales
          y modelización de caudales de ríos del Brasil.'
        ),
        hr(),
        h4('Resume:'),
        fluidRow(' '),
        p(
          'This paper deals with the application of the
          Cluster Analysis for Time Series
          oriented to the modeling of flows of the main
          rivers of Brazil, which were measured
          in 150 stations distributed in them, this from
          climatic variables and the combination
          of techniques of modeling as Principal
          Functional Components Analysis (FPCA),
          SARIMAX and STL-Loess.'
        ),
        p(
          'Specifically what is done is to create a
          small number of clusters (from 2 to 4 clusters)
          from the 150 stations (where the flows were
          measured), where each group will
          contain stations in which their flows have
          a temporary behavior similar possible,
          then for each of these clusters, through the
          use of ACPF, we will find a single time
          series that summarizes the behavior of the
          flows of the cluster. Finally, the time series
          of each cluster is modeled from climatic
          variables, using them as explanatory
          variables in the SARIMAX modeling framework.'
        ),
        p(
          'We will show later the advantages and the
          efficiency of modeling a huge amount
          of time series with the use of these techniques,
          this because the model that explains
          each cluster can be extended (using the
          same delays and explanatory variables) to
          each of the time series that compose it.
          We perform comparative studies between
          an individual model (SARIMAX) for a specific
          flow and the model of the cluster
          to which it belongs, obtaining similar results
          in terms of predictability. Where an
          Average Quadratic Error (RMSE) of 0.3 % and
          an AIC of 652,21 was obtained for
          the individual model, while for the cluster
          model an RMSE of 0.4 % was obtained,
          and an AIC of 762,32'
        ),
        p(
          'Thus we show that we managed to move from
          the problem of modeling 150 time
          series, to modeling the time series of a few clusters.'
        ),
        br(),
        p(
          tags$b('Keywords:'),
          tags$i(
            "Time Series Cluster Analysis,
            STL-Loess decomposition, Functional
            Principal Component Analysis"
          )
          )
        
          )
      
        ),
    hr()
    
    
        ),
  
  # ANALISIS CLUSTER DE SERIES VAZOES ============================
  tabPanel(
    'Clusters',
    
    fluidRow(
      # Panel Lateral  -----------
      sidebarPanel(
        h4('Cluster de Series de Tiempo'),
        p(
          'Primero selecciona una de las Métricas
          definidas para series de tiempo.'
        ),
        selectInput(
          'vaz_clus_metric',
          label = 'Selecciona Métrica',
          selected = 'D_acf',
          list(
            'Correlación Cruzada' = 'D_ccor',
            'Autocorrelación' = 'D_acf',
            'Correlación de Pearson' = 'D_cor',
            'Correlación Temporal' = 'D_cort',
            'Métrica Euclidea' = 'D_euc',
            'Métrica de Fourier' = 'D_fourier',
            'Métrica Infinito' = 'D_ifnrm',
            'Métrica Manhatan' = 'D_manh',
            'Métrica de Minkwoski' = 'D_mink',
            'Autocorrelación Parcial' = 'D_pacf',
            'Periodograma' = 'D_per'
          )
        ),
        p('Luego elige un método de clusterización
          (agrupamiento).'),
        selectInput(
          'vaz_clus_metod',
          label = 'Selecciona Método',
          selected = 'clara',
          list(
            'K-Medias' = 'kmedias',
            'K-Medoid (CLARA)' = 'clara',
            'Cluster Gerárquico' = 'gerarquico'
          )
        ),
        p('Finalmente elige el número de clusters
          que quieres que se formen.'),
        sliderInput(
          'vaz_clus_k',
          label = 'Número de Clusters',
          min = 2,
          max = 8,
          value = 4
        ),
        actionButton(
          'vaz_clus_boton',
          label = 'Clusterizar',
          icon = icon('braille')
        ),
        hr(),
        h4('Gráfico de Series'),
        p(
          'Para graficar una o varias series,
          primero clusteriza las estaciones, luego
          seleccione los nombres de las estaciones
          correspondientes en la Tabla que
          se encuentra en la parte inferior derecha'
        ),
        hr(),
        #Link a pestaña ACP Funcional
        p(
          'Si desea puede seguir con el Análisis
          de Componentes Principales Funcional
          de las Series de Flujos en la pestaña',
          actionLink(inputId = "pestania_acpf", label = "ACP Funcional")
        )
        ),
      # Panel Principal -----------
      mainPanel(
        h3('Mapa de Estaciones Clusterizadas: Vazoes '),
        hr(),
        leafletOutput("mapa_cluster", width = "100%", height = "450px"),
        hr(),
        h4('Tabla de Estaciones por Cluster'),
        fluidRow(
          dataTableOutput(outputId = "tabla_cluster"),
          #, width = "50%")),
          hr(),
          h4("Grafico de las Series"),
          dygraphOutput('vaz_clu_grf')
        )
      ),
      hr()
        ),
    
    # Analisis Comp. Princip Funcional  =================
    tabPanel(
      "ACP Funcional",
      
      h3(align = "center", "Análisis de Componentes Principales Funcional"),
      
      p(
        'Primero realice el Análisis Clúster en la pestaña anterior (',
        actionLink(inputId = "pestania_cluster2", label = "Clusters"),
        ') fijando adecuadamente los parámetros. Luego, selecciona
        que Clúster deseas analizar usando ACP Funcional.'
      ),
      # Número de Clúster a Analizar
      selectInput(
        'n_clus_acpf2',
        label = 'Selecciona Clúster',
        selected = "1",
        choices = 1:4
      ),
      
      p(
        "En el siguiente gráfico se muestran las
        series de Flujos que componene el clúster,
        así como una listado de las mismas."
      ),
      #Grafico de Series Vazoes por Cluster
      tabsetPanel(
        #Tabla Vazoes por Cluster
        tabPanel("Listado", br(),
                 dataTableOutput(outputId = "tab_vaz_clus2")),
        #Grafico de Vazoes del Cluster
        tabPanel(
          "Gráfico",
          br(),
          br(),
          dygraphOutput(
            outputId = "graf_vaz_clus2",
            width = "98%",
            height = "300px"
          )
          
        )
      ),
      br(),
      
      #Resultados ACPF
      h4("Resultados del ACP Funcional"),
      p(
        "A continuación, se muestra un conjunto
        de gráficos resultado de haber relizado el
        ACP Funcional de las series de Flujos del
        Cluster. Es decir, la función media del
        proceso, las funciones propias, y el
        porcentaje que aporta cada componente a la
        variabilidad del proceso."
      ),
      #Grafico ACOF
      tabsetPanel(
        tabPanel("Gráfico Resumen",
                 
                 plotOutput(outputId = "graf_acpf1")),
        tabPanel("Gráficos de Presición",
                 
                 plotOutput(outputId = "graf_acpf2")),
        tabPanel("BoxPlot Funcional",
                 
                 plotOutput(outputId = "graf_acpf3"))
      )
      
      ),
    
    # MODELAMIENTO SARIMAX   ============================
    tabPanel(
      "SARIMAX",
      h3(align = "center", "Modelamiento SARIMAX"),
      p(
        "En esta sección modelaremos una serie de
        tiempo asociada a Flujos, usando como
        variables regresoras a variables Climáticas y
        las componentes principales del Clúster obtenidas
        a partir del ACP Funcional)."
    ),
    
    
    withMathJax(),
    p(
      "Se plantea un modelo \\(SARIMAX(p,d,q,P,D,Q) \\),
      que tiene la siguiente forma:"
    ),
    p(
      align = "center",
      "\\( \\varphi_p(L)\\Psi_P(L^s) \\bigtriangledown _s^D V_t =
      \\sum_{k=1}^{w}\\beta_k C_{kt} + \\phi_q(L) \\Phi_Q(L^s)e_t \\)"
    ),
    
    #Especificaciones
    p(
      "Donde \\(V_t\\) es el caudal estimado del clúster en el
      tiempo \\(t\\), \\(C_{kt}\\) son las variables de Clima
      de la estación \\(k\\) en el tiempo \\(t\\)."
    ),
    
    
    p(
      "Para ello primero seleccione el Clúster
      que desea analizar. Recuerde haber realizado
      primero el Análisis respectivo en la primera pestaña (",
      actionLink(inputId = "pestania_cluster3", label = "Clusters"),
      ').'
      ),
    #Seleccionar Numero de Cluster
    selectInput(
      'n_clus_acpf3',
      label = 'Selecciona Clúster',
      selected = "1",
      choices = 1:4
    ),
    p(
      "A continuación, seleccione la estación
      correspondiente a la serie de tiempo de
      Flujos que desea modelar."
    ),
    #Seleccionar Estacion Vazoe
    selectInput(
      'nomb_est_vaz3',
      label = 'Selecciona Estación',
      selected = "1",
      choices = 1:4
    ),
    #Grafico Estacion Seleccionada
    dygraphOutput(outputId = "graf_vaz_estacion", width = "98%"),
    p(
      "Luego, elije las variables regresoras del
      modelo, en este caso contamos con variables Climáticas."
    ),
    br(),
    # Variables Climáticas
    h4("Variables Regresoras"),
    p(
      "En la siguiente tabla se muestran las
      series climáticas asociadas a las estaciones
      de medición más cercanas a las estaciones
      donde se midieron los Flujos que componen
      el Clúster. Además, podemos encontrar
      la gráfica de dichas series, así como un mapa
      donde podemos observar las estaciones de
      medición de Flujos y sus correspondientes
      estaciones de medición de Clima."
    ),
    p(
      tags$b("Nota:"),
      "Es posible que sea necesario
      desestacionalizar las series de clima,
      antes de ser usadas en el modelo."
    ),
    checkboxInput("ruidoClimaBox",
                  label = "Desestacionalizar Series de Clima.",
                  value = FALSE),
    p(
      "Puede incluir en el modelo además, variables
      como la serie de Fujos representante
      del Clúster, así como las series que
      representan a las variables Climáticas:
      Precipitación, Temperatura Máxima,
      Temperatura Mínima, y Humedad (halladas a partir
      de ACP Funcional)."
    ),
    # Eleccion Variables Extras
    checkboxInput("flujoBox",
                  label = "Flujo del Clúster",
                  value = FALSE),
    checkboxInput("precipBox",
                  label = "Precipitación del Clúster",
                  value = FALSE),
    checkboxInput("tempMaxBox",
                  label = "Temperatura Máxima del Clúster",
                  value = FALSE),
    checkboxInput("tempMinBox",
                  label = "Temperatura Mínima del Clúster",
                  value = FALSE),
    checkboxInput("humedBox",
                  label = "Humedad del Clúster",
                  value = FALSE),
    
    #Pestañas
    tabsetPanel(
      tabPanel(
        "Variables Regresoras",
        br(),
        #Tabla Variables Clima del Clúster
        dataTableOutput(outputId = "tab_clim_clus3")
      ),
      tabPanel(
        "Gráfico",
        br(),
        p(
          "Primero selecciona las variables de
          la tabla anterior para que sean graficadas."
        ),
        #Grafico de las Series Climaticas
        dygraphOutput(
          outputId = "graf_clim_clus3",
          width = "98%",
          height = "400px"
        )
        
        ),
      tabPanel("Mapa",
               #Mapa de estaciones de Clima
               leafletOutput(outputId = "map_clim_clus3"))
      
    ),
    br(),
    p(
      "Nota: Si no selecciona ninguna
      de las variables de la tabla anterior, por
      defecto se consideran todas las variable climáticas."
    ),
    br(),
    
    #Parámetros del Modelo
    h4("Selección del Parámetros"),
    p(
      "A continuación puede elegir los parámetros
      \\( (p,d,q,P,D,Q)\\) del modelo (asociados
      a los retardos y diferencias)."
    ),
    
    fluidRow(
      column(
        4,
        selectInput(
          inputId = "par_p",
          label = "p",
          choices = 0:12,
          selected = 6
        ),
        selectInput(
          inputId = "par_P",
          label = "P",
          choices = 0:12,
          selected = 1
        )
      ),
      column(
        4,
        selectInput(
          inputId = "par_d",
          label = "d",
          choices = 0:3,
          selected = 0
        ),
        selectInput(
          inputId = "par_D",
          label = "D",
          choices = 0:3,
          selected = 1
        )
      ),
      column(
        4,
        selectInput(
          inputId = "par_q",
          label = "q",
          choices = 0:8,
          selected = 4
        ),
        selectInput(
          inputId = "par_Q",
          label = "Q",
          choices = 0:8,
          selected = 0
        )
      )
      
    ),
    br(),
    p(
      align = "center",
      actionButton(
        inputId = "boton_modelo",
        label = "Ejecutar Análisis",
        icon = icon("cog", lib = "glyphicon")
      )
    ),
    
    hr(),
    h3("Resultados", align = "center"),
    h4(textOutput(outputId = "estacion_modelada")),
    
    tabsetPanel(
      #Coeficientes Estimados
      tabPanel(
        "Coeficientes",
        br(),
        p(
          "En esta sección presentamos un
          resumen general del modelo estimado
          a partir de los parámetros antes fijados."
        ),
        verbatimTextOutput("coeficientes")
        ),
      #Residuos
      tabPanel(
        "Residuos",
        br(),
        p(
          "A continuación podemos ver el gráfico
          de los residuos, su distribución, así como
          la función de autocorrelación de los mismos."
        ),
        plotOutput("resid_graf")
        ),
      #Prediccion
      tabPanel("Predicción")
      
      
      
      ),
    
    br()
    
    
      )
    
    )
    )
