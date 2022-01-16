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
  menuItem("Intruducción", tabName = "Inicio", icon = icon("th")),
  menuItem("Project information", tabName = "Informacion", icon = icon("dashboard")),
  menuItem("Social network analysis", tabName = "SNA", icon = icon("bar-chart-o")),
  menuItem("Data set", tabName = "Conjunto de datos", icon = icon("table")),
  menuItem("Concepts", tabName = "Conceptos", icon = icon("question"))
  
))

body <- dashboardBody(   useShinyjs(),  tags$script("document.title = 'Monitoreo de Terremotos'"),
                         
                         ### Styles
                         
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
                         
                         tabItems(tabItem(tabName = "Inicio",
                                          ## columna uno
                                          h1("Cantidad de sismos  Fecha: ", Sys.Date()),
                                          fluidRow(
                                            valueBoxOutput("Cantidad_anio"),
                                            valueBoxOutput("Cantidad_mes"),
                                            valueBoxOutput("Cantidad_dia")
                                          ),
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
                                          (DT::dataTableOutput("datos")),
                                          h1("Gráficos  comparativos"),hr(),
                                          fluidRow(
                                            box(
                                              plotlyOutput("pastelAnioPasado"),
                                            ),
                                            box(
                                              plotlyOutput("pastelAnioActual")
                                            )
                                          )
                         ),   
                         tabItem(
                           tabName = "SNA",
                           h3("Sección de redes segun la categoría de la magnitud"),
                           p(""),
                           fluidRow(column(4, selectInput("regionSNA",
                                                          "Regiones:",
                                                          c("All",
                                                            unique(as.character(tbl_clasificado_anio_actual$Group))))),
                           )
                           ,
                           fluidRow(
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Menor", status = "warning", solidHeader = TRUE, visNetworkOutput("snaMenor"))),
                             column(4,box( width = "100%",  collapsible = TRUE,collapsed = TRUE,title = "Ligero", status = "warning", solidHeader = TRUE, visNetworkOutput("snaLigero"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Moderado", status = "warning", solidHeader = TRUE, visNetworkOutput("snaModerado"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Fuerte", status = "warning", solidHeader = TRUE, visNetworkOutput("snaFuerte"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Mayor", status = "warning", solidHeader = TRUE, visNetworkOutput("snaMayor"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Grandes", status = "warning", solidHeader = TRUE, visNetworkOutput("snaGran")))
                           ),
                           fluidRow(
                             # GRAFICO UNO
                             box(
                               title = "Inputs",
                               status = "warning",
                               width = "100%", 
                               "Descripción del contenido y utilidad del grafo"
                             ),visNetworkOutput("graficoUno"))
                         )
                         )
)

# WEB SCRAPING ******************************************************************************************************************
library(rvest)
dt_sudamerica<- read_html("http://ds.iris.edu/ieb/evtable.phtml?caller=IEB&orderby=time-desc&src=usgs&limit=5000&maxlat=-0.014&minlat=-39.910&maxlon=-57.656&minlon=-89.283&sbl=1&pbl=1&caller=self&name=Western%20South%20America&zm=4&mt=ter&rgn=Western%20South%20America&title=IEB%20export%3A%205000%20earthquakes%20as%20a%20sortable%20table.&stitle=from%20the%20earliest%20to%20the%20latest%20available%2C%20all%20mags%2C%20all%20depths%2C%20with%20priority%20for%20most%20recent%2C%20limited%20to%205000%2C%20%20showing%20data%20from%20USGS%2C%20")
dt_centroamerica<- read_html("https://ds.iris.edu/ieb/evtable.phtml?caller=IEB&orderby=time-desc&src=usgs&limit=5000&maxlat=28.770&minlat=4.920&maxlon=-53.440&minlon=-118.480&sbl=1&pbl=1&caller=self&name=Central%20America&zm=4&mt=ter&rgn=Central%20America&title=IEB%20export%3A%205000%20earthquakes%20as%20a%20sortable%20table.&stitle=from%20the%20earliest%20to%20the%20latest%20available%2C%20all%20mags%2C%20all%20depths%2C%20with%20priority%20for%20most%20recent%2C%20limited%20to%205000%2C%20%20showing%20data%20from%20USGS%2C%20")
dt_sudesteasiatico<- read_html("https://ds.iris.edu/ieb/evtable.phtml?caller=IEB&orderby=time-desc&src=usgs&limit=5000&maxlat=37.720&minlat=9.800&maxlon=107.580&minlon=64.510&sbl=1&pbl=1&caller=self&name=S.E.%20Asia%20Region&zm=4&mt=ter&rgn=S.E.%20Asia%20Region&title=IEB%20export%3A%205000%20earthquakes%20as%20a%20sortable%20table.&stitle=from%20the%20earliest%20to%20the%20latest%20available%2C%20all%20mags%2C%20all%20depths%2C%20with%20priority%20for%20most%20recent%2C%20limited%20to%205000%2C%20%20showing%20data%20from%20USGS%2C%20")
dt_mediterraneo_oriental<- read_html("https://ds.iris.edu/ieb/evtable.phtml?caller=IEB&orderby=time-desc&src=usgs&limit=5000&maxlat=48.220&minlat=31.580&maxlon=45.880&minlon=7.560&sbl=1&pbl=1&caller=self&name=E.%20Mediterranean&zm=5&mt=ter&rgn=E.%20Mediterranean&title=IEB%20export%3A%205000%20earthquakes%20as%20a%20sortable%20table.&stitle=from%20the%20earliest%20to%20the%20latest%20available%2C%20all%20mags%2C%20all%20depths%2C%20with%20priority%20for%20most%20recent%2C%20limited%20to%205000%2C%20%20showing%20data%20from%20USGS%2C%20")
# dt_norte_america<- read_html("http://ds.iris.edu/ieb/evtable.phtml?caller=IEB&orderby=time-desc&src=usgs&limit=5000&maxlat=55.826&minlat=15.877&maxlon=-52.689&minlon=-135.746&sbl=1&pbl=1&caller=self&zm=2&mt=ter&title=IEB%20export%3A%205000%20earthquakes%20as%20a%20sortable%20table.&stitle=from%20the%20earliest%20to%20the%20latest%20available%2C%20all%20mags%2C%20all%20depths%2C%20with%20priority%20for%20most%20recent%2C%20limited%20to%205000%2C%20%20showing%20data%20from%20USGS%2C%20")

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

# tbl_norte_america<- dt_norte_america %>%
#   html_table()
# tbl_norte_america<- tbl_norte_america[[1]]
# tbl_norte_america$Group<-"Norte_America"

# ---------------------------Unión------------------------------
tabla_sis<- tbl_sudamerica

tabla_sis<-rbind(tabla_sis,tbl_centroamerica)
tabla_sis<-rbind(tabla_sis,tbl_sudesteasiatico)
tabla_sis<-rbind(tabla_sis,tbl_mediterraneo_oriental)
# tabla_sis<-rbind(tabla_sis,tbl_norte_america)

tabla_base <- subset(tabla_sis, select = -c(4,10,11))

# FECHA ACTUAL ******************************************************************************************************************

dia <- as.numeric(format(Sys.Date(),'%d'))
mes <- as.numeric(format(Sys.Date(),'%m'))
anio<- as.numeric(format(Sys.Date(),'%Y'))

# CONJUNTO DE DATOS PARA EL INICIO***********************************************************************************************
tabla_para_inicio <-tabla_base %>% filter(Year==anio)
tabla_anio_pasado <-tabla_base %>% filter(Year==(anio-1))

# DATOS PARA PARA LA GRAFICAS PASTEL-----------------------------------------------
InformePastel_anio_pasado<-tabla_anio_pasado %>%                     
  group_by(Month) %>%     
  tally() 
InformePastel_anio_actual<-tabla_para_inicio %>%                     
  group_by(Month) %>%     
  tally() 

# GRAFICAS PATEL***********************************************************************************************

# Pastel uno--------------------------------------------------------------------------------------------
pastelAnioActual <- plot_ly( labels=InformePastel_anio_actual$Month,values=InformePastel_anio_actual$n, 
                             textinfo='label+percent',
                             insidetextorientation='radial')
pastelAnioActual <- pastelAnioActual %>% add_pie(hole = 0.6)
pastelAnioActual <- pastelAnioActual %>% layout(title = ('Cantida de sismo del año Actual'),
                                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# Pastel dos-----------------------------------------------------------------------------------------------
pastelAnioPasado <- plot_ly( labels=InformePastel_anio_pasado$Month,values=InformePastel_anio_pasado$n, 
                             textinfo='label+percent',
                             insidetextorientation='radial')
pastelAnioPasado <- pastelAnioPasado %>% add_pie(hole = 0.6)
pastelAnioPasado <- pastelAnioPasado %>% layout(title = ('Cantida de sismo del año pasado'),
                                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# CLASIFICACION DE DATOS -------------------------------------------------------------------------------------- 
# AÑO ACTUA
tbl_clasificado_anio_actual<-tabla_para_inicio %>% separate(8, c("region", "pais"), ", ", extra = "merge")
tbl_clasificado_anio_actual<- na.omit(tbl_clasificado_anio_actual)

tbl_clasificado_anio_actual$tipoM <- 
  with(tbl_clasificado_anio_actual,
       ifelse(tbl_clasificado_anio_actual$Mag > 0 & tbl_clasificado_anio_actual$Mag <=3.9, "Menor",
              ifelse(tbl_clasificado_anio_actual$Mag >=4 & tbl_clasificado_anio_actual$Mag <=4.9, "Ligero",
                     ifelse(tbl_clasificado_anio_actual$Mag >=5 & tbl_clasificado_anio_actual$Mag <=5.9, "Moderado",
                            ifelse(tbl_clasificado_anio_actual$Mag >=6 & tbl_clasificado_anio_actual$Mag <=6.9, "Fuerte",
                                   ifelse(tbl_clasificado_anio_actual$Mag >=7 & tbl_clasificado_anio_actual$Mag <=7.9,"Mayor",
                                          ifelse(tbl_clasificado_anio_actual$Mag >=8, "Gran",0)))))))

# AÑO PASADO
tbl_clasificado_anio_pasado<-tabla_anio_pasado %>% separate(8, c("region", "pais"), ", ", extra = "merge")
tbl_clasificado_anio_pasado<- na.omit(tbl_clasificado_anio_pasado)

tbl_clasificado_anio_pasado$tipoM <- 
  with(tbl_clasificado_anio_pasado,
       ifelse(tbl_clasificado_anio_pasado$Mag > 0 & tbl_clasificado_anio_pasado$Mag <=3.9, "Menor",
              ifelse(tbl_clasificado_anio_pasado$Mag >=4 & tbl_clasificado_anio_pasado$Mag <=4.9, "Ligero",
                     ifelse(tbl_clasificado_anio_pasado$Mag >=5 & tbl_clasificado_anio_pasado$Mag <=5.9, "Moderado",
                            ifelse(tbl_clasificado_anio_pasado$Mag >=6 & tbl_clasificado_anio_pasado$Mag <=6.9, "Fuerte",
                                   ifelse(tbl_clasificado_anio_pasado$Mag >=7 & tbl_clasificado_anio_pasado$Mag <=7.9,"Mayor",
                                          ifelse(tbl_clasificado_anio_pasado$Mag >=8, "Gran",0)))))))


# GRAFICO PASTEL DE LA CATEGORIA-----------------------------------------------------------------------------

# Grafico uno--------------------------- 

# grafico_categoria_actual<-tbl_clasificado_anio_actual %>%
#   group_by(pais) %>%
#   tally()
# 
# grafico_categoria_actual <- plot_ly( labels=grafico_categoria_actual$pais,values=grafico_categoria_actual$n, 
#                              textinfo='label+percent',
#                              insidetextorientation='radial')
# grafico_categoria_actual <- grafico_categoria_actual %>% add_pie(hole = 0.6)
# grafico_categoria_actual <- grafico_categoria_actual %>% layout(title = ('Cantida de sismo del año pasado'),
#                                                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                                                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
# 
# 
# 
# # Grafico dos---------------------------
# grafico_categoria_pasado<-tbl_clasificado_anio_pasado %>%
#   group_by(pais) %>%
#   tally()
# 
# grafico_categoria_pasado <- plot_ly( labels=grafico_categoria_pasado$pais,values=grafico_categoria_pasado$n, 
#                                     textinfo='label+percent',
#                                     insidetextorientation='radial')
# grafico_categoria_pasado <- grafico_categoria_pasado %>% add_pie(hole = 0.6)
# grafico_categoria_pasado <- grafico_categoria_pasado %>% layout(title = ('Cantida de sismo del año pasado'),
#                                                               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#                                                               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
# 



#REDES SOCIAL NETWORCK ANALITY********************************************************************************
# REDES ACTUALES---------------------------------------------------------------------------------------------
tbl_redes_actual<- subset(tbl_clasificado_anio_actual, select = -c(1,2,3,5,6,7,8))
tbl_redes_actual<-subset(tbl_redes_actual, select = c(2,4,1,3))
# tbl_net<-subset(tbl_redes_actual, select = c(1,2))
#Generacion de nodos y vertices con library(igraph)
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


 data <- toVisNetworkData(datosAnalizar)
 colnames(data$nodes)<-c("id","label","value")
 colnames(data$edges)<-c("from","to","label","Group","value")
 datanodes<-data$nodes
 dataedges<-data$edges
 dataedges$title<-data$edges$label

 visNetwork(data$nodes, dataedges)
 

ui <- dashboardPage(header,sidebar,body)
server <- function(input, output) {
  
  #DATOS GENERALES DE DIA MES Y AÑO-----------------------------------------------------------------------
  output$Cantidad_anio <- renderValueBox({
    CANTIDAD_ANIOS<- nrow(tabla_para_inicio)
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
  # ---------------------TABLA INFORMACIÓN GENERAL
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
  }
  )
  
  output$graficoUno<- renderVisNetwork({graficoUno}
    
  )
  # GRÁFICOS PASTE*****************************************************************************************
  # -----------------------GRÁFICO DE TERREMOTOS EN MESES ANIO ACTUAL--------------------------------------
  output$pastelAnioActual <- renderPlotly(
    pastelAnioActual
  )
  # -----------------------GRÁFICO DE TERREMOTOS EN MESES ANIO PASADO--------------------------------------      
  output$pastelAnioPasado <- renderPlotly(
    pastelAnioPasado
  )
  
  # SOCIAL NETWORK ANALITY*******************************************************************************************************
  output$snaMenor <- renderVisNetwork({
    graficoSnaMenor<-NULL
    data <-  tbl_clasificado_anio_actual
    if(input$regionSNA  != "All" )
    {
      # data <- data %>% filter(Group=="Sudamerica"   & tipoM =="Ligero" )
      data <- data %>% filter(Group==input$regionSNA & tipoM =="Menor" )
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
      # shinyjs::hide(id =  "img2")
    }
    else
    { 
      # shinyjs::show(id = "img2")
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
}

shinyApp(ui, server)
