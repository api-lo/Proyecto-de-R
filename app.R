## app.R ##
library(shinydashboard)
library(bslib)
library(tidyverse)
library(rvest)
library(dplyr)
library(gifski)
library(ggplot2)
library(plotly)
library(tidyverse)
library(rvest)
library(gganimate)
library(gifski)
library(ggplot2)
library(RColorBrewer)
library(gapminder)
library(tidyverse)
library(dplyr)
library(dummies)
library(kableExtra)
library(scales)
library(lubridate)
library(igraphdata)
library(visNetwork)
library(igraph)
library(threejs)
library(shinyjs)

header <- dashboardHeader(title = "Terrae")


sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Dashboard", tabName = "Inicio", icon = icon("th")),
  menuItem("Conjunto de datos", tabName = "Dataset", icon = icon("table")),
  menuItem("Social network analysis", icon = icon("bar-chart-o"),
           menuSubItem("SNA de magnitud", tabName = "GENERAL_MAGNITUD"  ),
           menuSubItem("SNA de profundidad", tabName = "GENERAL_PROFUNDIDAD"  ),
           menuSubItem("SNA(magnitud y produndidad)",tabName = "SNA"),
           menuSubItem("SNA(Duración del evento)")),
  menuItem("Preguntas frecuentes", tabName = "Informacion", icon = icon("dashboard")),
  menuItem("Conceptos", tabName = "Conceptos", icon = icon("question")),
  menuItem("Código", tabName = "Conceptos", icon = icon("question"))
  
))

body <- dashboardBody(   useShinyjs(),  tags$script("document.title = 'Monitoreo de Terremotos'"),
                         tags$style(HTML(".fa-dashboard { font-size: 15px; }")),
                         tags$style(HTML(".fa-globe { font-size: 20px; }")),
                         tags$style(HTML(".fa-barcode { font-size: 20px; }")),
                         tags$style(HTML(".tab-content { padding-left: 20px; padding-right: 30px; }")) ,
                         tags$style(HTML(".fa-wrench { font-size: 15px; }")),
                         tags$style(HTML(".fa-refresh { font-size: 15px; }")),
                         tags$style(HTML(".fa-search { font-size: 15px; }")),
                         tags$style(HTML(".fa-comment { font-size: 20px; }")),
                         tags$style(HTML(".fa-share-alt { font-size: 20px; }")),
                         tags$style(HTML(".fa-envelope { font-size: 20px; }")),
                         tags$style(HTML(".fa-question-circle { font-size: 20px; }")),
                         tags$style(HTML(".fa-chevron-circle-down { font-size: 15px; }")),
                         tags$style(HTML(".fa-bell { font-size: 17px; }")),
                         tags$style(HTML(".fa-check { font-size: 14px; }")),
                         tags$style(HTML(".fa-times { font-size: 14px; }")),
                         tags$style( HTML("hr {border-top: 1px solid #000000;}") ),
                         tags$style( HTML(".shiny-output-error { visibility: hidden; }") ),
                         tags$style( HTML(".shiny-output-error:before { visibility: hidden; }") ),
                         tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
                         tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
                         tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}')),
                         
                         tabItems(
                           tabItem(tabName = "Inicio",
                                          h1("Cantidad de sismos  Fecha: ", Sys.Date()),
                                          fluidRow(
                                            valueBoxOutput("Cantidad_anio"),
                                            valueBoxOutput("Cantidad_mes"),
                                            valueBoxOutput("Cantidad_dia")
                                          ),
                                         
                                          
                                          h1("Gráficos  comparativos"),hr(),
                                          fluidRow(box(
                                            column(6, selectInput("regionInicio",
                                                                  "Regiones:",
                                                                  c("All",
                                                                    unique(as.character(tabla_base$Group))))),
                                            column(6, selectInput("anioInicio",
                                                                  "Años:",
                                                                  c("All",
                                                                    unique(as.character(tabla_base$Year))))))
                                          ),fluidRow(  box(width = "100%", title = "Gráfico según la magnitud", status = "warning", collapsible = TRUE,solidHeader = TRUE,
                                            column(9,plotlyOutput("graficoBarras_mes")),
                                            column(3,(tableOutput("datos_grafico_barra" ))))
                                          )
                                          ,fluidRow( 
                                            box(title ="Gráfico según la magnitud", status = "warning", collapsible = TRUE,solidHeader = TRUE
                                                     ,plotlyOutput("grafico_pastel_magnitud"),
                                                     tableOutput("tabla_magnitud")),
                                            box(title = "Gráfico según la profundidad ", status = "warning", collapsible = TRUE,solidHeader = TRUE
                                                   ,plotlyOutput("grafico_pastel_profundida"),
                                                    tableOutput("tabla_profundida"))
                                                ),
                                          fluidRow( h3("Por cada región"),selectInput("anioRegion",
                                                                                                "Años:",
                                                                                                c("All",
                                                                                                  unique(as.character(tabla_base$Year))))
                                                    ),
                                           fluidRow( box(width = "100%", title = "Gráfico según la magnitud", status = "warning", collapsible = TRUE,solidHeader = TRUE,
                                                         column(9,plotlyOutput("graficoRegiones")),
                                                         column(3,(tableOutput("datos_grafico_regiones" )))))
                         )
                         , tabItem( 
                           tabName = "Informacion",
                           
                         ), tabItem( 
                           tabName = "Dataset", 
                           h1("Tabla de datos telurica"),
                           fluidRow(column(4, selectInput("region",
                                                          "Regiones:",
                                                          c("All",
                                                            unique(as.character(tbl_clasificado_anio_actual$Group))))),
                                    column(4, selectInput("pais",
                                                          "Pais:",
                                                          c("All",
                                                            unique(as.character(tbl_clasificado_anio_actual$pais))))),
                                    column(4, selectInput("tipoM",
                                                          "Categoría:",
                                                          c("All",
                                                            unique(as.character(tbl_clasificado_anio_actual$tipoM)))))),
                                  (DT::dataTableOutput("datos"))
                                      ), 
                         tabItem(
                           tabName = "SNA",
                           h3("Redes según la categoría de la magnitud"),
                           hr(""),
                           fluidRow(column(4, selectInput("regionSNA",
                                                          "Regiones:",
                                                          c("All",
                                                            unique(as.character(tbl_clasificado_anio_actual$Group))))),
                           )
                           ,
                           fluidRow(
                             column(4,box(classs="ASDAS", width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Menor entre 1.0 - 3.9", status = "warning", solidHeader = TRUE, visNetworkOutput("snaMenor"))),
                             column(4,box( width = "100%",  collapsible = TRUE,collapsed = TRUE,title = "Ligero entre 4.0 - 4.9", status = "warning", solidHeader = TRUE, visNetworkOutput("snaLigero"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Moderado entre 5.0 -5.9", status = "warning", solidHeader = TRUE, visNetworkOutput("snaModerado"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Fuerte entre 6.0-6.9", status = "warning", solidHeader = TRUE, visNetworkOutput("snaFuerte"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Mayor entre 7.0-7.9", status = "warning", solidHeader = TRUE, visNetworkOutput("snaMayor"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Grandes mayor de 8.0", status = "warning", solidHeader = TRUE, visNetworkOutput("snaGran")))
                           ),
                           h3("Redes según la profundida"),
                           hr(),
                           fluidRow(
                             column(4,box( width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Superficial hasta 70 km", status = "primary", solidHeader = TRUE, visNetworkOutput("snaSuperficial"))),
                             column(4,box( width = "100%",  collapsible = TRUE,collapsed = TRUE,title = "Intermedio entre 70km y 300 km", status = "primary", solidHeader = TRUE,  visNetworkOutput("snaIntermedio"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Profundo mayor de 300km", status = "primary", solidHeader = TRUE,visNetworkOutput("snaProfundo")))
                             
                           )
                        
                         ), tabItem(
                                tabName = "GENERAL_MAGNITUD",
                                box(
                                  title = "Inputs",
                                  status = "warning",
                                  width = "100%", 
                                  "Descripción del contenido y utilidad del grafo"
                                ),
                                visNetworkOutput("graficoUno")
                              )
                         , tabItem(
                           tabName = "GENERAL_PROFUNDIDAD",
                           box(
                             title = "Inputs",
                             status = "warning",
                             width = "100%", 
                             "Descripción del contenido y utilidad del grafo"
                           ),
                           visNetworkOutput("graficoDos")
                         )
                         )
)

# WEB SCRAPING ------------------------------------------------------------------------------------------------------
library(rvest)
dt_sudamerica<- read_html("http://ds.iris.edu/ieb/evtable.phtml?caller=IEB&orderby=time-desc&src=usgs&limit=5000&maxlat=-0.014&minlat=-39.910&maxlon=-57.656&minlon=-89.283&sbl=1&pbl=1&caller=self&name=Western%20South%20America&zm=4&mt=ter&rgn=Western%20South%20America&title=IEB%20export%3A%205000%20earthquakes%20as%20a%20sortable%20table.&stitle=from%20the%20earliest%20to%20the%20latest%20available%2C%20all%20mags%2C%20all%20depths%2C%20with%20priority%20for%20most%20recent%2C%20limited%20to%205000%2C%20%20showing%20data%20from%20USGS%2C%20")
dt_centroamerica<- read_html("https://ds.iris.edu/ieb/evtable.phtml?caller=IEB&orderby=time-desc&src=usgs&limit=5000&maxlat=28.770&minlat=4.920&maxlon=-53.440&minlon=-118.480&sbl=1&pbl=1&caller=self&name=Central%20America&zm=4&mt=ter&rgn=Central%20America&title=IEB%20export%3A%205000%20earthquakes%20as%20a%20sortable%20table.&stitle=from%20the%20earliest%20to%20the%20latest%20available%2C%20all%20mags%2C%20all%20depths%2C%20with%20priority%20for%20most%20recent%2C%20limited%20to%205000%2C%20%20showing%20data%20from%20USGS%2C%20")
dt_sudesteasiatico<- read_html("https://ds.iris.edu/ieb/evtable.phtml?caller=IEB&orderby=time-desc&src=usgs&limit=5000&maxlat=37.720&minlat=9.800&maxlon=107.580&minlon=64.510&sbl=1&pbl=1&caller=self&name=S.E.%20Asia%20Region&zm=4&mt=ter&rgn=S.E.%20Asia%20Region&title=IEB%20export%3A%205000%20earthquakes%20as%20a%20sortable%20table.&stitle=from%20the%20earliest%20to%20the%20latest%20available%2C%20all%20mags%2C%20all%20depths%2C%20with%20priority%20for%20most%20recent%2C%20limited%20to%205000%2C%20%20showing%20data%20from%20USGS%2C%20")
dt_mediterraneo_oriental<- read_html("https://ds.iris.edu/ieb/evtable.phtml?caller=IEB&orderby=time-desc&src=usgs&limit=5000&maxlat=48.220&minlat=31.580&maxlon=45.880&minlon=7.560&sbl=1&pbl=1&caller=self&name=E.%20Mediterranean&zm=5&mt=ter&rgn=E.%20Mediterranean&title=IEB%20export%3A%205000%20earthquakes%20as%20a%20sortable%20table.&stitle=from%20the%20earliest%20to%20the%20latest%20available%2C%20all%20mags%2C%20all%20depths%2C%20with%20priority%20for%20most%20recent%2C%20limited%20to%205000%2C%20%20showing%20data%20from%20USGS%2C%20")


#------------------------------------------------------EXTRAER TABLAS ----------------------------------------------------------

tbl_sudamerica<- dt_sudamerica %>%
  html_table()
tbl_sudamerica<- tbl_sudamerica[[1]]
tbl_sudamerica$Group<-"Sudamerica"

tbl_centroamerica<- dt_centroamerica %>%
  html_table()
tbl_centroamerica<- tbl_centroamerica[[1]]
tbl_centroamerica$Group<-"Centro_America"

tbl_sudesteasiatico<- dt_sudesteasiatico %>%
  html_table()
tbl_sudesteasiatico<- tbl_sudesteasiatico[[1]]
tbl_sudesteasiatico$Group<-"Sudesteasiatico"

tbl_mediterraneo_oriental<- dt_mediterraneo_oriental %>%
  html_table()
tbl_mediterraneo_oriental<- tbl_mediterraneo_oriental[[1]]
tbl_mediterraneo_oriental$Group<-"Mediterraneo_oriental"

# ---------------------------Unión------------------------------
tabla_sis<- tbl_sudamerica
tabla_sis<-rbind(tabla_sis,tbl_centroamerica)
tabla_sis<-rbind(tabla_sis,tbl_sudesteasiatico)
tabla_sis<-rbind(tabla_sis,tbl_mediterraneo_oriental)
tabla_base <- subset(tabla_sis, select = -c(4,10,11))

# FECHA ACTUAL ******************************************************************************************************************
dia <- as.numeric(format(Sys.Date(),'%d'))
mes <- as.numeric(format(Sys.Date(),'%m'))
anio<- as.numeric(format(Sys.Date(),'%Y'))
# CONJUNTO DE DATOS PARA EL INICIO***********************************************************************************************
tabla_para_inicio <-tabla_base 

# CLASIFICACION DE DATOS -------------------------------------------------------------------------------------- 
# AÑO ACTUA
tbl_clasificado_anio_actual<-tabla_para_inicio %>% separate(8, c("region", "pais"), ", ", extra = "merge")
tbl_clasificado_anio_actual<- na.omit(tbl_clasificado_anio_actual)
tbl_clasificado_por_profundidad<-tbl_clasificado_anio_actual

tbl_clasificado_anio_actual$tipoM <-
  with(tbl_clasificado_anio_actual,
       ifelse(tbl_clasificado_anio_actual$Mag > 0 & tbl_clasificado_anio_actual$Mag <=3.9, "Menor",
              ifelse(tbl_clasificado_anio_actual$Mag >=4 & tbl_clasificado_anio_actual$Mag <=4.9, "Ligero",
                     ifelse(tbl_clasificado_anio_actual$Mag >=5 & tbl_clasificado_anio_actual$Mag <=5.9, "Moderado",
                            ifelse(tbl_clasificado_anio_actual$Mag >=6 & tbl_clasificado_anio_actual$Mag <=6.9, "Fuerte",
                                   ifelse(tbl_clasificado_anio_actual$Mag >=7 & tbl_clasificado_anio_actual$Mag <=7.9,"Mayor",
                                          ifelse(tbl_clasificado_anio_actual$Mag >=8, "Gran",0)))))))

tbl_clasificado_por_profundidad$tipoDepth.km <-
  with(tbl_clasificado_por_profundidad,ifelse(tbl_clasificado_por_profundidad$`Depth km` >= 0 & tbl_clasificado_por_profundidad$`Depth km` <=70, "Superficial",
                            ifelse(tbl_clasificado_por_profundidad$`Depth km` >=70 & tbl_clasificado_por_profundidad$`Depth km` <=300, "Intermedio",
                            ifelse(tbl_clasificado_por_profundidad$`Depth km` >=301,"Profundo","Mas produndos"))))

tbl_todo_los_datos_magnitud<-tbl_clasificado_anio_actual
tbl_todo_los_datos_profundidad<-tbl_clasificado_por_profundidad

tbl_clasificado_anio_actual<-tbl_clasificado_anio_actual %>% filter(Year==anio)
tbl_clasificado_por_profundidad<-tbl_clasificado_por_profundidad%>% filter(Year==anio)

# GRAFICO PASTEL DE LA CATEGORIA-----------------------------------------------------------------------------
# Pastel uno--------------------------------------------------------------------------------------------
#REDES SOCIAL NETWORCK ANALITY********************************************************************************
# REDES ACTUALES---------------------------------------------------------------------------------------------
tbl_redes_actual<- subset(tbl_clasificado_anio_actual, select = -c(1,2,3,5,6,7,8))
tbl_redes_actual<-subset(tbl_redes_actual, select = c(2,4,1,3))
datosAnalizar <- graph.data.frame(tbl_redes_actual, directed=T)
V(datosAnalizar)
E(datosAnalizar)
V(datosAnalizar)$label <- V(datosAnalizar)$name
V(datosAnalizar)$degree <- degree(datosAnalizar)
visIgraph(datosAnalizar)
#GRAFICO CON LOS NODOS TIPO MAG CON LIBERIA VISNETWORCK
xy<-as.data.frame(degree(datosAnalizar))
colnames(xy)<-c("n")

edges<-as_data_frame(datosAnalizar, what="edges")
nodes<-data.frame(V(datosAnalizar)$name)
colnames(nodes) <- c("pais")

dfx<-edges %>%
  group_by(from, to)%>%
  tally()

Pfx<-tbl_redes_actual %>%
  group_by(pais,Group)%>%
  tally()

for(i in 1:nrow(nodes))
{
  
  for(j in 1:nrow(Pfx))
  {
    if( nodes$pais[i]== Pfx$pais[j]){
      nodes$Group[i]<-Pfx$Group[j]
    }else if(nodes$pais[i]=="Menor" ||
             nodes$pais[i]=="Ligero" ||
             nodes$pais[i]=="Moderado" ||
             nodes$pais[i]=="Fuerte" || nodes$pais[i]=="Mayor" )
      
    {
      nodes$Group[i]<-"categoria"
    }
  }
}



nodes1 <- data.frame(id =nodes$pais,group=nodes$Group , value=xy$n, label=nodes$pais )
edges1 <- data.frame(from = c(dfx$from), to = c(dfx$to), value=c(dfx$n), label=c(dfx$n),title=c(dfx$n))


graficoUno<- visNetwork(nodes1,edges1,
                        layout = "layout_in_circle" )%>% visIgraphLayout() %>% 
  visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
  visOptions(selectedBy = "group",
             highlightNearest = TRUE,
             nodesIdSelection = TRUE)%>%
  visLegend(main = "Categoria de nodo", position = "right")%>%
  visInteraction(navigationButtons = TRUE)


# -------REDES ACTUALES---------------------------------------------------------------------------------------------
tbl_redes_profundidad<- subset(tbl_clasificado_por_profundidad, select = -c(1,2,3,5,6,7,8))
tbl_redes_profundidad<-subset(tbl_redes_profundidad, select = c(2,4,1,3))
datosAnalizar <- graph.data.frame(tbl_redes_profundidad, directed=T)
V(datosAnalizar)
E(datosAnalizar)
V(datosAnalizar)$label <- V(datosAnalizar)$name
V(datosAnalizar)$degree <- degree(datosAnalizar)

#GRAFICO CON LOS NODOS TIPO MAG CON LIBERIA VISNETWORCK
xy<-as.data.frame(degree(datosAnalizar))
colnames(xy)<-c("n")

edges<-as_data_frame(datosAnalizar, what="edges")
nodes<-data.frame(V(datosAnalizar)$name)
colnames(nodes) <- c("pais")

dfx<-edges %>%
  group_by(from, to)%>%
  tally()

Pfx<-tbl_redes_actual %>%
  group_by(pais,Group)%>%
  tally()

for(i in 1:nrow(nodes))
{
  
  for(j in 1:nrow(Pfx))
  {
    if( nodes$pais[i]== Pfx$pais[j]){
      nodes$Group[i]<-Pfx$Group[j]
    }else if(nodes$pais[i]=="Intermedio" ||
             nodes$pais[i]=="Superficial" ||
             nodes$pais[i]=="Profundo" || nodes$pais[i]=="Mas produndos"  )
      
    {
      nodes$Group[i]<-"categoria"
    }
  }
}



nodes1 <- data.frame(id =nodes$pais,group=nodes$Group , value=xy$n, label=nodes$pais )
edges1 <- data.frame(from = c(dfx$from), to = c(dfx$to), value=c(dfx$n), label=c(dfx$n),title=c(dfx$n))


graficoDos<- visNetwork(nodes1,edges1,
                        layout = "layout_in_circle" )%>% visIgraphLayout() %>% 
  visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
  visOptions(selectedBy = "group",
             highlightNearest = TRUE,
             nodesIdSelection = TRUE)%>%
  visLegend(main = "Categoria de nodo", position = "right")%>%
  visInteraction(navigationButtons = TRUE)

# ------------------Servidor-----------------

ui <- dashboardPage(header,sidebar,body)
server <- function(input, output) {
  
  #DATOS GENERALES DE DIA MES Y AÑO-----------------------------------------------------------------------
  output$Cantidad_anio <- renderValueBox({
    CANTIDAD_ANIOS<- nrow(tabla_para_inicio %>% filter(Year==anio))
    valueBox(
      paste0(CANTIDAD_ANIOS), "AÑO", icon = icon("list"),
      color = "red"
    )
  })
  output$Cantidad_mes <- renderValueBox({
    CANTIDAD_MES  <- nrow(tabla_para_inicio %>% filter(Year==anio & Month==mes ))
    valueBox(
      paste0(CANTIDAD_MES), "MES", icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$Cantidad_dia <- renderValueBox({
    CANTIDAD_DIA  <- nrow(tabla_para_inicio %>% filter(Year==anio & Month==mes & Day==dia))
    valueBox(
      paste0(CANTIDAD_DIA), "DÍA", icon = icon("list"),
      color = "green"
    )
  })
  # TABLAS*************************************************************************************************
  # ---------------------TABLA INFORMACIÓN GENERAL-------------------------------------
  output$datos<-DT::renderDataTable({
    data<-tbl_clasificado_anio_actual
    if(input$region  != "All" )
    {
      data <- data[data$Group == input$region,]
    }
    if(input$pais  != "All" )
    {
      data <- data[data$pais == input$pais,]
    }
    if(input$tipoM  != "All" )
    {
      data <- data[data$tipoM == input$tipoM,]
    }
    data
  })
  output$datos_grafico_barra<-renderTable(width ="100%",striped = TRUE,hover = TRUE,bordered=TRUE, {
     data<-tabla_base
     if(input$anioInicio  != "All" )
     {
       data <- data[data$Year == input$anioInicio,]
     }
     if( input$regionInicio  != "All" )
     {
       data <- data[data$Group ==  input$regionInicio,]
     }
     Informe_barra_mes<-data %>%                     
       group_by(Month) %>%     
       tally() 
     colnames(Informe_barra_mes)<-c("Meses","Cantidad")
     
     Informe_barra_mes  
  })
  
  
  output$graficoRegiones<-renderPlotly({
    data<-tbl_todo_los_datos_magnitud
    if( input$anioRegion  != "All" )
    {
      data <- data[data$Year ==  input$anioRegion,]
      
    
    }
    Informe_barra_mes<-data %>%                     
      group_by(Group) %>%     
      tally() 
    
    xvz<-ggplot(Informe_barra_mes,aes(x=Group,y=n,fill=n))+
      geom_bar(stat="identity", position="dodge")+
      labs(title = "Año actual", y="N terremoto", x="Meses", caption="Manos a la data")
    
    xvz
    
    
    
  })
  output$datos_grafico_regiones<-renderTable(width ="100%",striped = TRUE,hover = TRUE,bordered=TRUE, {
    data<-tbl_todo_los_datos_magnitud
    if(input$anioRegion  != "All" )
    {
      data <- data[data$Year == input$anioRegion,]
    }
    
    Informe_barra_mes<-data %>%                     
      group_by(Group) %>%     
      tally() 
    colnames(Informe_barra_mes)<-c("Región","Cantidad")
    
    Informe_barra_mes  
  })
  
  output$graficoBarras_mes <- renderPlotly({
    data<-tabla_base
    if(input$anioInicio  != "All" )
    {
      data <- data[data$Year == input$anioInicio,]
    }
    if( input$regionInicio  != "All" )
    {
      data <- data[data$Group ==  input$regionInicio,]
    }
    Informe_barra_mes<-data %>%                     
      group_by(Month) %>%     
      tally() 
    Informe_barra_mes
   
    Informe_barra_mes$Month <- 
      with(Informe_barra_mes,
           ifelse(Informe_barra_mes$Month == 1 , "En",
                  ifelse(Informe_barra_mes$Month ==2 , "Feb",
                         ifelse(Informe_barra_mes$Month ==3, "Mzo",
                                ifelse(Informe_barra_mes$Month ==4, "Abr",
                                       ifelse(Informe_barra_mes$Month ==5 ,"My",
                                              ifelse(Informe_barra_mes$Month ==6,"Jun",
                                                     ifelse(Informe_barra_mes$Month ==7,"Jul",
                                                            ifelse(Informe_barra_mes$Month ==8,"Ag",
                                                                   ifelse(Informe_barra_mes$Month ==9,"Sept",
                                                                          ifelse(Informe_barra_mes$Month ==10,"Oct",
                                                                                 ifelse(Informe_barra_mes$Month ==11,"Nov",
                                                                                        ifelse(Informe_barra_mes$Month ==12,"Dic","Null")))))))))))))
    
    xv<-ggplot(Informe_barra_mes,aes(x=Month,y=n,fill=n))+
      geom_bar(stat="identity", position="dodge")+
      labs(title = "Año actual", y="N terremoto", x="Meses", caption="Manos a la data")
    
     xv
    
    
   })
  output$grafico_pastel_magnitud <- renderPlotly({
    
    
    data<-tbl_todo_los_datos_magnitud
    if(input$anioInicio  != "All" )
    {
      data <- data[data$Year == input$anioInicio,]
    }
    if( input$regionInicio  != "All" )
    {
      data <- data[data$Group ==  input$regionInicio,]
    }
    
    Informe_magnitud<-data %>%                     
      group_by(tipoM) %>%     
      tally() 
    
    pastelMagnitud <- plot_ly( labels=Informe_magnitud$tipoM,values=Informe_magnitud$n,
                               textinfo='label+percent',
                               insidetextorientation='radial')
    pastelMagnitud <- pastelMagnitud %>% add_pie(hole = 0.6)
    pastelMagnitud <- pastelMagnitud %>% layout(title = ('Cantida de sismo del año Actual'),
                                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
    
  })
  output$grafico_pastel_profundida <- renderPlotly({
    
    
    data<-tbl_todo_los_datos_profundidad
    if(input$anioInicio  != "All" )
    {
      data <- data[data$Year == input$anioInicio,]
    }
    if( input$regionInicio  != "All" )
    {
      data <- data[data$Group ==  input$regionInicio,]
    }
    
    
    Informe_profundidad<-data %>%                     
      group_by(tipoDepth.km) %>%     
      tally() 
    
    pastelProfundidad <- plot_ly( labels=Informe_profundidad$tipoDepth.km,values=Informe_profundidad$n,
                                  textinfo='label+percent',
                                  insidetextorientation='radial')
    pastelProfundidad <- pastelProfundidad %>% add_pie(hole = 0.6)
    pastelProfundidad <- pastelProfundidad %>% layout(title = ('Cantida de sismo del año Actual'),
                                                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$tabla_magnitud<-renderTable(width ="100%",striped = TRUE,hover = TRUE,bordered=TRUE,{
    
    data<-tbl_todo_los_datos_magnitud
    if(input$anioInicio  != "All" )
    {
      data <- data[data$Year == input$anioInicio,]
    }
    if( input$regionInicio  != "All" )
    {
      data <- data[data$Group ==  input$regionInicio,]
    }

    Informe_magnitud<-data %>%
      group_by(tipoM) %>%
      tally()
    Informe_magnitud
    
  })


  output$tabla_profundida<-renderTable(width ="100%",striped = TRUE,hover = TRUE,bordered=TRUE,{
    
    data<-tbl_todo_los_datos_profundidad
    if(input$anioInicio  != "All" )
    {
      data <- data[data$Year == input$anioInicio,]
    }
    if( input$regionInicio  != "All" )
    {
      data <- data[data$Group ==  input$regionInicio,]
    }


    Informe_profundidad<-data %>%
      group_by(tipoDepth.km) %>%
      tally()
  })

  output$graficoUno<- renderVisNetwork({graficoUno})
  
  output$graficoDos<- renderVisNetwork({graficoDos})
  # GRÁFICOS PASTE*****************************************************************************************
  # -----------------------GRÁFICO DE TERREMOTOS EN MESES ANIO ACTUAL--------------------------------------
  # output$pastelAnioActual <- renderPlotly(
  #   pastelAnioActual
  # )
  # -----------------------GRÁFICO DE TERREMOTOS EN MESES ANIO PASADO--------------------------------------
  output$pastelAnioPasado <- renderPlotly(
    pastelAnioPasado
  )

  # -----------------------------SOCIAL NETWORK ANALITY PARA LA MAGNITUD--------------------------------------
  output$snaMenor <- renderVisNetwork({
    graficoSnaMenor<-NULL

    data <-  tbl_clasificado_anio_actual
    if(input$regionSNA  != "All" )
    {


      data <- data %>% filter(Group==input$regionSNA & tipoM =="Menor" )
      if(nrow(data)>0)
      {

        dataSnaM<- subset(data, select = -c(1,2,3,5,6,7,8))
        dataSnaM<-subset(dataSnaM, select = c(2,4,1,3))
        snaMenor <- graph.data.frame(dataSnaM, directed=T)
        nrNodos<-as.data.frame(degree(snaMenor))
        colnames(nrNodos)<-c("n")
        edgesM<-as_data_frame(snaMenor, what="edges")
        nodesM<-data.frame(V(snaMenor)$name)
        colnames(nodesM) <- c("pais")
        dfx<-edgesM %>%
          group_by(from, to)%>%
          tally()
        Pfx<-dataSnaM %>%
          group_by(pais,Group)%>%
          tally()
        for(i in 1:nrow(nodesM))
        {
          if(nodesM$pais[i]=="Menor" ||
             nodesM$pais[i]=="Ligero" ||
             nodesM$pais[i]=="Moderado" ||
             nodesM$pais[i]=="Fuerte" || nodesM$pais[i]=="Mayor" )
          {
            nodesM$Group[i]<-"categoria"
          }else
          {
            nodesM$Group[i]<-"pais"
          }

        }
        nodesM <- data.frame(id =nodesM$pais,group=nodesM$Group , value=nrNodos$n, label=nodesM$pais )
        edgesM <- data.frame(from = c(dfx$from), to = c(dfx$to), value=c(dfx$n), label=c(dfx$n),title=c(dfx$n))
        graficoSnaMenor<- visNetwork(nodesM,edgesM,
                                     layout = "layout_in_circle" )%>% visIgraphLayout() %>%
          visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
          visOptions(highlightNearest = TRUE,
                     nodesIdSelection = TRUE) %>%
          visLayout(randomSeed = 123)


      }else
      {

      }


    }
    else
    {


    }

    graficoSnaMenor

  })
  output$snaLigero <- renderVisNetwork({
    graficoSnaMenor<-NULL
    data <-  tbl_clasificado_anio_actual
    if(input$regionSNA  != "All" )
    {
      data <- data %>% filter(Group==input$regionSNA & tipoM =="Ligero" )
      dataSnaM<- subset(data, select = -c(1,2,3,5,6,7,8))
      dataSnaM<-subset(dataSnaM, select = c(2,4,1,3))
      snaMenor <- graph.data.frame(dataSnaM, directed=T)
      nrNodos<-as.data.frame(degree(snaMenor))
      colnames(nrNodos)<-c("n")
      edgesM<-as_data_frame(snaMenor, what="edges")
      nodesM<-data.frame(V(snaMenor)$name)
      colnames(nodesM) <- c("pais")
      dfx<-edgesM %>%
        group_by(from, to)%>%
        tally()
      Pfx<-dataSnaM %>%
        group_by(pais,Group)%>%
        tally()
      for(i in 1:nrow(nodesM))
      {
        if(nodesM$pais[i]=="Menor" ||
           nodesM$pais[i]=="Ligero" ||
           nodesM$pais[i]=="Moderado" ||
           nodesM$pais[i]=="Fuerte" || nodesM$pais[i]=="Mayor" )
        {
          nodesM$Group[i]<-"categoria"
        }else
        {
          nodesM$Group[i]<-"pais"
        }

      }
      nodesM <- data.frame(id =nodesM$pais,group=nodesM$Group , value=nrNodos$n, label=nodesM$pais )
      edgesM <- data.frame(from = c(dfx$from), to = c(dfx$to), value=c(dfx$n), label=c(dfx$n),title=c(dfx$n))
      graficoSnaMenor<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
    }
    graficoSnaMenor
  })
  output$snaModerado <- renderVisNetwork({
    graficoSnaMenor<-NULL
    data <-  tbl_clasificado_anio_actual
    if(input$regionSNA  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA & tipoM =="Moderado" )
      dataSnaM<- subset(data, select = -c(1,2,3,5,6,7,8))
      dataSnaM<-subset(dataSnaM, select = c(2,4,1,3))
      snaMenor <- graph.data.frame(dataSnaM, directed=T)
      nrNodos<-as.data.frame(degree(snaMenor))
      colnames(nrNodos)<-c("n")
      edgesM<-as_data_frame(snaMenor, what="edges")
      nodesM<-data.frame(V(snaMenor)$name)
      colnames(nodesM) <- c("pais")
      dfx<-edgesM %>%
        group_by(from, to)%>%
        tally()
      Pfx<-dataSnaM %>%
        group_by(pais,Group)%>%
        tally()
      for(i in 1:nrow(nodesM))
      {
        if(nodesM$pais[i]=="Menor" ||
           nodesM$pais[i]=="Ligero" ||
           nodesM$pais[i]=="Moderado" ||
           nodesM$pais[i]=="Fuerte" || nodesM$pais[i]=="Mayor" )
        {
          nodesM$Group[i]<-"categoria"
        }else
        {
          nodesM$Group[i]<-"pais"
        }

      }
      nodesM <- data.frame(id =nodesM$pais,group=nodesM$Group , value=nrNodos$n, label=nodesM$pais )
      edgesM <- data.frame(from = c(dfx$from), to = c(dfx$to), value=c(dfx$n), label=c(dfx$n),title=c(dfx$n))
      graficoSnaMenor<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
    }
    graficoSnaMenor
  })
  output$snaFuerte <- renderVisNetwork({
    graficoSnaMenor<-NULL
    data <-  tbl_clasificado_anio_actual
    if(input$regionSNA  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA & tipoM =="Fuerte" )
      dataSnaM<- subset(data, select = -c(1,2,3,5,6,7,8))
      dataSnaM<-subset(dataSnaM, select = c(2,4,1,3))
      snaMenor <- graph.data.frame(dataSnaM, directed=T)
      nrNodos<-as.data.frame(degree(snaMenor))
      colnames(nrNodos)<-c("n")
      edgesM<-as_data_frame(snaMenor, what="edges")
      nodesM<-data.frame(V(snaMenor)$name)
      colnames(nodesM) <- c("pais")
      dfx<-edgesM %>%
        group_by(from, to)%>%
        tally()
      Pfx<-dataSnaM %>%
        group_by(pais,Group)%>%
        tally()
      for(i in 1:nrow(nodesM))
      {
        if(nodesM$pais[i]=="Menor" ||
           nodesM$pais[i]=="Ligero" ||
           nodesM$pais[i]=="Moderado" ||
           nodesM$pais[i]=="Fuerte" || nodesM$pais[i]=="Mayor" )
        {
          nodesM$Group[i]<-"categoria"
        }else
        {
          nodesM$Group[i]<-"pais"
        }

      }
      nodesM <- data.frame(id =nodesM$pais,group=nodesM$Group , value=nrNodos$n, label=nodesM$pais )
      edgesM <- data.frame(from = c(dfx$from), to = c(dfx$to), value=c(dfx$n), label=c(dfx$n),title=c(dfx$n))
      graficoSnaMenor<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
    }
    graficoSnaMenor
  })
  output$snaMayor <- renderVisNetwork({
    graficoSnaMenor<-NULL
    data <-  tbl_clasificado_anio_actual
    if(input$regionSNA  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA & tipoM =="Mayor" )
      dataSnaM<- subset(data, select = -c(1,2,3,5,6,7,8))
      dataSnaM<-subset(dataSnaM, select = c(2,4,1,3))
      snaMenor <- graph.data.frame(dataSnaM, directed=T)
      nrNodos<-as.data.frame(degree(snaMenor))
      colnames(nrNodos)<-c("n")
      edgesM<-as_data_frame(snaMenor, what="edges")
      nodesM<-data.frame(V(snaMenor)$name)
      colnames(nodesM) <- c("pais")
      dfx<-edgesM %>%
        group_by(from, to)%>%
        tally()
      Pfx<-dataSnaM %>%
        group_by(pais,Group)%>%
        tally()
      for(i in 1:nrow(nodesM))
      {
        if(nodesM$pais[i]=="Menor" ||
           nodesM$pais[i]=="Ligero" ||
           nodesM$pais[i]=="Moderado" ||
           nodesM$pais[i]=="Fuerte" || nodesM$pais[i]=="Mayor" )
        {
          nodesM$Group[i]<-"categoria"
        }else
        {
          nodesM$Group[i]<-"pais"
        }

      }
      nodesM <- data.frame(id =nodesM$pais,group=nodesM$Group , value=nrNodos$n, label=nodesM$pais )
      edgesM <- data.frame(from = c(dfx$from), to = c(dfx$to), value=c(dfx$n), label=c(dfx$n),title=c(dfx$n))
      graficoSnaMenor<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
    }
    graficoSnaMenor
  })
  output$snaGran <- renderVisNetwork({
    graficoSnaMenor<-NULL
    data <-  tbl_clasificado_anio_actual
    if(input$regionSNA  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA & tipoM =="Gram" )
      dataSnaM<- subset(data, select = -c(1,2,3,5,6,7,8))
      dataSnaM<-subset(dataSnaM, select = c(2,4,1,3))
      snaMenor <- graph.data.frame(dataSnaM, directed=T)
      nrNodos<-as.data.frame(degree(snaMenor))
      colnames(nrNodos)<-c("n")
      edgesM<-as_data_frame(snaMenor, what="edges")
      nodesM<-data.frame(V(snaMenor)$name)
      colnames(nodesM) <- c("pais")
      dfx<-edgesM %>%
        group_by(from, to)%>%
        tally()
      Pfx<-dataSnaM %>%
        group_by(pais,Group)%>%
        tally()
      for(i in 1:nrow(nodesM))
      {
        if(nodesM$pais[i]=="Menor" ||
           nodesM$pais[i]=="Ligero" ||
           nodesM$pais[i]=="Moderado" ||
           nodesM$pais[i]=="Fuerte" || nodesM$pais[i]=="Mayor" )
        {
          nodesM$Group[i]<-"categoria"
        }else
        {
          nodesM$Group[i]<-"pais"
        }

      }
      nodesM <- data.frame(id =nodesM$pais,group=nodesM$Group , value=nrNodos$n, label=nodesM$pais )
      edgesM <- data.frame(from = c(dfx$from), to = c(dfx$to), value=c(dfx$n), label=c(dfx$n),title=c(dfx$n))
      graficoSnaMenor<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
    }
    graficoSnaMenor
  })

  #----------------------------SOCIAL NETWORK ANALITY PARA LA PROFUNDIDA
  output$snaSuperficial <- renderVisNetwork({
    snaSuperficial<-NULL
    data <-  tbl_clasificado_por_profundidad
    if(input$regionSNA  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA & tipoDepth.km =="Superficial" )
      dataSnaM<- subset(data, select = -c(1,2,3,4,5,6,8))
      dataSnaM<-subset(dataSnaM, select = c(2,4,1,3))
      snaMenor <- graph.data.frame(dataSnaM, directed=T)
      nrNodos<-as.data.frame(degree(snaMenor))
      colnames(nrNodos)<-c("n")
      edgesM<-as_data_frame(snaMenor, what="edges")
      nodesM<-data.frame(V(snaMenor)$name)
      colnames(nodesM) <- c("pais")
      dfx<-edgesM %>%
        group_by(from, to)%>%
        tally()
      Pfx<-dataSnaM %>%
        group_by(pais,Group)%>%
        tally()
      for(i in 1:nrow(nodesM))
      {
        if(nodesM$pais[i]=="Superficial" ||
           nodesM$pais[i]=="Intermedio" ||
           nodesM$pais[i]=="Profundo" )
        {
          nodesM$Group[i]<-"categoria"
        }else
        {
          nodesM$Group[i]<-"pais"
        }

      }
      nodesM <- data.frame(id =nodesM$pais,group=nodesM$Group , value=nrNodos$n, label=nodesM$pais )
      edgesM <- data.frame(from = c(dfx$from), to = c(dfx$to), value=c(dfx$n), label=c(dfx$n),title=c(dfx$n))
      graficoSnaMenor<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
      # shinyjs::hide(id =  "img2")
    }
    else
    {
      # shinyjs::show(id = "img2")
    }

    graficoSnaMenor
  })
  output$snaIntermedio <- renderVisNetwork({
    snaIntermedio<-NULL
    data <-  tbl_clasificado_por_profundidad
    if(input$regionSNA  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA & tipoDepth.km =="Intermedio" )
      dataSnaM<- subset(data, select = -c(1,2,3,4,5,6,8))
      dataSnaM<-subset(dataSnaM, select = c(2,4,1,3))
      snaMenor <- graph.data.frame(dataSnaM, directed=T)
      nrNodos<-as.data.frame(degree(snaMenor))
      colnames(nrNodos)<-c("n")
      edgesM<-as_data_frame(snaMenor, what="edges")
      nodesM<-data.frame(V(snaMenor)$name)
      colnames(nodesM) <- c("pais")
      dfx<-edgesM %>%
        group_by(from, to)%>%
        tally()
      Pfx<-dataSnaM %>%
        group_by(pais,Group)%>%
        tally()
      for(i in 1:nrow(nodesM))
      {
        if(nodesM$pais[i]=="Superficial" ||
           nodesM$pais[i]=="Intermedio" ||
           nodesM$pais[i]=="Profundo" )
        {
          nodesM$Group[i]<-"categoria"
        }else
        {
          nodesM$Group[i]<-"pais"
        }

      }
      nodesM <- data.frame(id =nodesM$pais,group=nodesM$Group , value=nrNodos$n, label=nodesM$pais )
      edgesM <- data.frame(from = c(dfx$from), to = c(dfx$to), value=c(dfx$n), label=c(dfx$n),title=c(dfx$n))
      graficoSnaMenor<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
      # shinyjs::hide(id =  "img2")
    }
    else
    {
      # shinyjs::show(id = "img2")
    }

    graficoSnaMenor
  })
  output$snaProfundo <- renderVisNetwork({
    snaProfundo<-NULL
    graficoSnaMenor<-NULL
    data <-  tbl_clasificado_por_profundidad
    if(input$regionSNA  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA & tipoDepth.km =="Profundo" )
      dataSnaM<- subset(data, select = -c(1,2,3,4,5,6,8))
      dataSnaM<-subset(dataSnaM, select = c(2,4,1,3))
      snaMenor <- graph.data.frame(dataSnaM, directed=T)
      nrNodos<-as.data.frame(degree(snaMenor))
      colnames(nrNodos)<-c("n")
      edgesM<-as_data_frame(snaMenor, what="edges")
      nodesM<-data.frame(V(snaMenor)$name)
      colnames(nodesM) <- c("pais")
      dfx<-edgesM %>%
        group_by(from, to)%>%
        tally()
      Pfx<-dataSnaM %>%
        group_by(pais,Group)%>%
        tally()
      for(i in 1:nrow(nodesM))
      {
        if(nodesM$pais[i]=="Superficial" ||
           nodesM$pais[i]=="Intermedio" ||
           nodesM$pais[i]=="Profundo" )
        {
          nodesM$Group[i]<-"categoria"
        }else
        {
          nodesM$Group[i]<-"pais"
        }

      }
      nodesM <- data.frame(id =nodesM$pais,group=nodesM$Group , value=nrNodos$n, label=nodesM$pais )
      edgesM <- data.frame(from = c(dfx$from), to = c(dfx$to), value=c(dfx$n), label=c(dfx$n),title=c(dfx$n))
      graficoSnaMenor<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)

    }
    else
    {

    }

    graficoSnaMenor

  })

}

shinyApp(ui, server)
