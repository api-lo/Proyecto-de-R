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
library(leaflet)
library( shinycssloaders)

header <- dashboardHeader(title = "Pacha-Kuyuy" )


sidebar <- dashboardSidebar(collapsed =FALSE ,sidebarMenu(
  menuItem("Dashboard", tabName = "Inicio", icon = icon("th")),
  menuItem("Mapa", tabName = "Mapa", icon = icon("globe")),
  menuItem("Social network analysis", icon = icon("neuter"),
           menuSubItem("SNA de magnitud", icon=icon("caret-right"), tabName = "GENERAL_MAGNITUD"  ),
           menuSubItem("SNA de profundidad",icon=icon("caret-right"), tabName = "GENERAL_PROFUNDIDAD"  ),
           menuSubItem("SNA detalle magnitud", icon=icon("caret-right"), tabName = "SNAM"),
           menuSubItem("SNA detalle produndidad", icon=icon("caret-right"), tabName = "SNAP")  ),
  menuItem("Preguntas frecuentes", tabName = "Informacion", icon = icon("question")),
  menuItem("Conceptos", tabName = "Conceptos", icon = icon("align-justify")),
  menuItem("Código", tabName = "Conceptos", icon = icon("file-code-o"))
  
))

body <- dashboardBody(   useShinyjs(),  tags$script("document.title = 'Monitoreo de Terremotos'"),
                      
                      tags$head(tags$style(HTML('
                                       body
                                            {
                                                background: #cfcfcf;
                                            }
                                            .inner
                                            {
                                                display: flex;
                                                flex-direction: column-reverse;
                                            }
                                            .inner h3{
                                                font-size: 30px;
                                                font-weight: bold;
                                               
                                            }
                                            .inner p{
                                                font-size: 15px;
                                               
                                            }
                                           .titulo{
                                             font-weight: bold; 
                                             margin-bottom: 10px;
                                            }

                                              th
                                              {
                                                color: #5679F0;
                                              }
                                              .box-header
                                                {
                                                    background: rgb(76,113,240) !important;
                                                    color: #fff;
                                                    border-top-left-radius:10px;
                                                    border-top-right-radius:10px;
                                                    
                                                }
                                                .box{
                                                border-radius: 10px;
                                                border-top: 0px solid #d2d6de;
                                                }
                                                
                                                .bg-orange
                                                {
                                                background: #fff;
                                                padding: 10PX;
                                                border-radius: 15px;
                                                background: rgb(76,113,240) !important;
                                                background: linear-gradient(90deg, rgba(76,113,240,1) 0%,  rgba(116,144,241,7) 100%) !important;
                                                
                                                }
                                                .bg-green
                                                {
                                                
                                                background: #fff;
                                                padding: 10PX;
                                                border-radius: 15px;
                                                background: rgb(84,96,101)!important;
                                                background: linear-gradient(262deg, rgba(84,96,101,1) 0%, rgba(148,152,154,1) 100%)!important;
                                                }
                                                .bg-olive{
                                                  
                                                 
                                                background: #fff;
                                                padding: 10PX;
                                                border-radius: 15px;
                                                height:100%;
                                                background: rgb(23,191,65) !important;
                                                background: linear-gradient(90deg,  rgba(31,170,31,1)  0%, rgba(23,191,65,1) 100%)!important;
                                                
                                              
                                                }
                                                .bg-yellow{
                                                
                                                background: #fff;
                                                padding: 10PX;
                                                border-radius: 15px;
                                                height:100%;
                                                background: rgb(224,94,38); !important;
                                                background: linear-gradient(262deg, rgba(224,94,38,1) 0%, rgba(231,183,87,1) 100%) !important;
                                                
                                                }
                                                .tab-content { padding-left: 20px; padding-right: 30px;}
                                                .main-sidebar{background:#fff  !important;  color: #000  !important;}
                                                
                                                .sidebar-menu li a{
                                                color: #000  !important;
                                                }
                                                .sidebar-menu li a {
                                                color: #000  !important;
                                                font-weight: bold;
                                                }
                                                .menu>li:hover>a {
                                                    
                                                }
                                                .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
                                                    color: #fff !important;
                                                    background: rgb(76,113,240) !important;
                                                    background: linear-gradient(90deg, rgba(76,113,240,1) 0%,  rgba(116,144,241,7) 100%) !important;
                                                    border: 10px solid white !important;
                                                    border-radius: 20px !important;
                                                }
                                                      .skin-blue .sidebar-menu>li>.treeview-menu {
                                                                  margin: 0 1px;
                                                                  background: #e8eaeb !important;
                                                                 
                                                      }
                                                        .sidebar-menu .treeview-menu>li>a {
                                                          padding: 5px 5px 5px 15px;
                                                          display: block;
                                                          font-size: 14px;
                                                          font-weight: normal !important;
                                                          color: #7D8487 !important;
                                                        }
                                                       .sidebar-menu .treeview-menu>li:hover>a {
                                                           color: #fff !important;
                                                          border-radius: 20px !important;
                                                          font-style: normal !important;
                                                          background: rgb(84,96,101)!important;
                                                          background: linear-gradient(262deg, rgba(84,96,101,1) 0%, rgba(148,152,154,1) 100%)!important;
                                                       }
                                                        .skin-blue .main-header .navbar {
                                                        background: rgb(224,94,38); !important;
                                                     
                                                
                                                        }
                                                          .skin-blue .main-header .logo 
                                                        {
                                                      background: rgb(224,94,38); !important;
                                                      background: linear-gradient(262deg,rgba(224,94,38,1) 100%,  rgba(231,183,87,1) 0%) !important;
                                                          }  

                        '))),
                         tags$style(HTML('.navbar-custom-menu>.navbar-nav>li:last-child>.dropdown-menu { width:10px; font-size:10px; padding:1px; margin:1px;}')),
                         tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > h4 {width:0px; font-size:0px; padding:0px; margin:0px;}')),
                         tags$style(HTML('.navbar-custom-menu> .navbar-nav> li:last-child > .dropdown-menu > p {width:0px; font-size:0px; padding:0px; margin:0px;}')),
                         
                         tabItems(
                           tabItem(tabName = "Inicio",
                                          tags$div(class="titulo" ,"Dashboard"),
                                          fluidRow(
                                            column(9,
                                            valueBoxOutput("Cantidad_anio"),
                                            valueBoxOutput("Cantidad_mes"),
                                            valueBoxOutput("Cantidad_dia")),
                                            
                                            column(3,
                                                   valueBoxOutput("CantidadPais", width = "100%" )       
                                                   )
                                          ),
                                          fluidRow(
                                            column(3, selectInput("regionInicio",
                                                                  "Seleccione un región",
                                                                  c("All",
                                                                    unique(as.character(tabla_base$Group))))),
                                            column(3, selectInput("anioInicio",
                                                                  "Seleccione el año:",
                                                                  c("All",
                                                                    unique(as.character(tabla_base$Year)))))
                                          ),fluidRow(
                                            column(9, box(width = "100%",title ="Gráfico mensual de sismos", withSpinner(plotlyOutput("graficoBarras_mes")))),
                                            column(3,box(width = "100%",title ="Tabla mensual de sismos",(withSpinner(tableOutput("datos_grafico_barra" )))))
                                          )
                                          ,fluidRow( 
                                            box(title ="Reporte según el tipo de magnitud", status = "warning"
                                                     ,withSpinner(plotlyOutput("grafico_pastel_magnitud")),
                                                     tableOutput("tabla_magnitud")),
                                            box(title = "Reporte según el tipo de profundidad", status = "warning"
                                                   ,withSpinner(plotlyOutput("grafico_pastel_profundida")),
                                                    tableOutput("tabla_profundida"))
                                                ),
                                           fluidRow( box(width = "100%", title = "Cantidad de eventos por región",
                                                         selectInput("anioRegion",
                                                                    "Seleccione el año:",
                                                                    c("All",
                                                                    unique(as.character(tabla_base$Year)))),
                                                         column(9,plotlyOutput("graficoRegiones")),
                                                         column(3,(tableOutput("datos_grafico_regiones" )))))
                         )
                         , tabItem( 
                           tabName = "Informacion", 
                           fluidRow(
                             h4("¿Cuál es nuestro objetivo?"),
                             p("Tenemos por objetivo procesar los datos de eventos telúricos y presentar información mediante gráficos. Principalmente nuestro objetivo es identificar las zonas con mayor frecuencia de movimientos telúricos por medio del análisis de redes sociales (Social Network Analysis, SNA). Para el efecto, se utilizó el software RStudio y se organizó en tres fases: (1) utilizar web scraping para extraer en tiempo real los registros de movimientos telúricos de las Instituciones de Investigación Incorporadas para la Sismología o IRIS, (2) realizar la exploración y preprocesamiento del conjunto de datos, (3) construir las redes aplicando Social Network Analysis, (4) finalmente, crear este sitio web con shiny para presentar los resultados."),
                             h4("¿Qué es Social Network Analysis?"),
                             p("La característica que define el análisis de redes sociales (Social Network Analysis, SNA) es su enfoque en la estructura de las relaciones. El análisis se representa en una red que está formada por un conjunto finito de vértices y las relaciones, definidos en ellos."),
                             h4("¿Cuáles son las fuentes de datos?"),
                             p("Los datos de eventos de movimientos telúricos son extraídos en tiempo real de la página oficial de las Instituciones de Investigación Incorporadas para la Sismología o IRIS. IRIS es un consorcio de investigación a nivel de universidades que se dedica a investigar la Tierra y explorar su interior a través de la colección y la distribución de datos sismológicos."),
                             h4("¿Cuándo se actualizará los gráficos?"),
                             p("Los gráficos informativos y de redes se actualizan de forma automática cada vez que recargue esta página."),
                            
                           )
                           
                         ),
                         tabItem( 
                           tabName = "Mapa", 
                           leafletOutput("mymap", height = 1000)
                         ),
                         tabItem( 
                           tabName = "Dataset", 
                           h1("Tabla de datos de los movimientos telúricos"),
                           fluidRow(column(4, selectInput("region",
                                                          "Seleccione la región:",
                                                          c("All",
                                                            unique(as.character(tbl_clasificado_anio_actual$Group))))),
                                    column(4, selectInput("pais",
                                                          "Seleccione el país:",
                                                          c("All",
                                                            unique(as.character(tbl_clasificado_anio_actual$pais))))),
                                    column(4, selectInput("tipoM",
                                                          "Seleccione el tipo de magnitud:",
                                                          c("All",
                                                            unique(as.character(tbl_clasificado_anio_actual$tipoM)))))),
                                  (DT::dataTableOutput("datos"))
                                      ), 
                         tabItem(
                           tabName = "SNAM",
                           h3("Detalle de redes según el tipo de magnitud"),
                           hr(""),
                           fluidRow(column(4, selectInput("regionSNA",
                                                          "Seleccione la región:",
                                                          c("All",
                                                            unique(as.character(tbl_clasificado_anio_actual$Group))))),
                           )
                           ,
                           fluidRow(
                             column(4,box(classs="ASDAS", width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Menor entre 1.0 - 3.9", solidHeader = TRUE, visNetworkOutput("snaMenor"))),
                             column(4,box( width = "100%",  collapsible = TRUE,collapsed = TRUE,title = "Ligero entre 4.0 - 4.9", solidHeader = TRUE, visNetworkOutput("snaLigero"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Moderado entre 5.0 -5.9",  solidHeader = TRUE, visNetworkOutput("snaModerado"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Fuerte entre 6.0-6.9", solidHeader = TRUE, visNetworkOutput("snaFuerte"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Mayor entre 7.0-7.9", solidHeader = TRUE, visNetworkOutput("snaMayor"))),
                             column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Grandes mayor de 8.0", solidHeader = TRUE, visNetworkOutput("snaGran")))
                           ),
                         ), 
                         tabItem(
                           
                           tabName = "SNAP",
                                  h3("Detalle de redes según el tipo de profundida"),
                                  hr(""), 
                                  fluidRow(column(12, selectInput("regionSNA2",
                                                                 "Seleccione la región:",
                                                                 c("All",
                                                                   unique(as.character(tbl_clasificado_anio_actual$Group))))),
                                  
                                  ),
                                     fluidRow(
                                       column(4,box( width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Superficial hasta 70 km",solidHeader = TRUE, visNetworkOutput("snaSuperficial"))),
                                       column(4,box( width = "100%",  collapsible = TRUE,collapsed = TRUE,title = "Intermedio entre 70km y 300 km",solidHeader = TRUE,  visNetworkOutput("snaIntermedio"))),
                                       column(4,box(width = "100%", collapsible = TRUE,collapsed = TRUE, title = "Profundo mayor de 300km",  solidHeader = TRUE,visNetworkOutput("snaProfundo")))
                                       
                                     ),
                                )
                         
                         
                         , tabItem(
                                tabName = "GENERAL_MAGNITUD",
                                box(
                                  title = "SNA QUE REPRESENTA LA RELACIÓN DE MOVIMIENTOS TELÚRICOS EN CADA PAÍS CON EL TIPO DE MAGNITUD ",collapsible = TRUE,
                                  status = "warning",
                                  width = "100%", 
                                  "El gráfico de redes representa interconexiones entre los países con el tipo de magnitud. La presencia o ausencia de cada interconexión indica si hubo al menos un evento de ese tipo de magnitud en el país.",
                                  tags$div(tags$ul(
                                    tags$li(tags$span("Los nodos hacen referencia a los países y los tipos de magnitud")),
                                    tags$li(tags$span("Las interconexiones o edges hacen referencia a la relación entre los nodos de tipo de magnitud con los nodos de países, el grosor de la línea representa la cantidad de eventos")),
                                  )),
                                  p("El color de los nodos y líneas representa a qué región pertenece, en este caso los eventos de Sudamérica son de color azul, los de Centro América son de color amarillo, los del Sudeste Asiático son rojo, los del Mediterraneo Oriental son de color verde y los nodos del tipo de categoría están representados de color morado.")
                                  
                                ),
                                box(title = "SNA",status = "warning",width = "100%",visNetworkOutput("graficoUno"))
                              )
                         , tabItem(
                           tabName = "GENERAL_PROFUNDIDAD",
                           box(
                             title = "SNA QUE REPRESENTA LA RELACIÓN DE MOVIMIENTOS TELÚRICOS EN CADA PAÍS CON EL TIPO DE PROFUNDIDAD",collapsible = TRUE,
                             status = "warning",
                             width = "100%", 
                             "El gráfico de redes representa interconexiones entre los países con el tipo de profundidad. La presencia o ausencia de cada interconexión indica si hubo al menos un evento de ese tipo de profundidad en el país.",
                             tags$div(tags$ul(
                               tags$li(tags$span("Los nodos hacen referencia a los países y los tipos de profundidad. Los movimientos telúricos son de tipo SUPERFICIAL cuando un evento tuvo profundidad menor a 70 km, INTERMEDIO cuando la profundidad fue entre 71 - 300 km y PROFUNDO representa una profundidad mayor a 300 km.")),
                               tags$li(tags$span("Las interconexiones o edges hacen referencia a la relación entre los nodos de tipo de profundidad con los nodos de países, el grosor de la línea representa la cantidad de eventos")),
                             )),
                             p("El color de los nodos y líneas representa a qué región pertenece, en este caso los eventos de Sudamérica son de color azul, los de Centro América son de color amarillo, los del Sudeste Asiático son rojo, los del Mediterraneo Oriental son de color verde y los nodos del tipo de categoría están representados de color morado.")
                           ),
                           box(title = "SNA",status = "warning",width = "100%", visNetworkOutput("graficoDos"))
                          
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
for(i in 1:nrow(tabla_base))
{
  vector<-str_split(tabla_base$Region[i], ",", simplify = TRUE)
  vector<-as.data.frame(vector)
  tabla_base$pais[i]<-  vector[,ncol(vector)]
}


tbl_clasificado_anio_actual<-tabla_base [ , c(1,2,3,4,5,6,7,8,10,9)]
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
  visInteraction(navigationButtons = TRUE)

# ------------------Servidor-----------------

ui <- dashboardPage(header,sidebar,body)
server <- function(input, output) {
  
  
  # output$map <- renderLeaflet(
  #   
  #   leaflet(tabla_para_inicio) %>% addTiles() %>% addMarkers(
  #     clusterOptions = markerClusterOptions()
  #   )
  #   
  #   
  # )
  
  output$mymap <- renderLeaflet({
   
      # leaflet(tabla_para_inicio) %>% addTiles() %>% addMarkers(
      #   clusterOptions = markerClusterOptions(), label = tabla_para_inicio$Mag
      # )
    leaflet(tbl_todo_los_datos_magnitud) %>% addTiles() %>%
      # estilos mapa
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addCircleMarkers(
        clusterOptions = markerClusterOptions,
        radius = ifelse(tbl_todo_los_datos_magnitud$tipoM == "Menor", 4,
                        ifelse(tbl_todo_los_datos_magnitud$tipoM == "Ligero", 6,
                               ifelse(tbl_todo_los_datos_magnitud$tipoM == "Moderado", 8,
                                      ifelse(tbl_todo_los_datos_magnitud$tipoM == "Fuerte", 10,
                                             ifelse(tbl_todo_los_datos_magnitud$tipoM == "Mayor", 12,14))))),
        color = ifelse(tbl_todo_los_datos_magnitud$tipoM == "Menor", "#2ce622",
                       ifelse(tbl_todo_los_datos_magnitud$tipoM == "Ligero", "#ded416",
                              ifelse(tbl_todo_los_datos_magnitud$tipoM == "Moderado", "#1878c7",
                                     ifelse(tbl_todo_los_datos_magnitud$tipoM == "Fuerte", "#a216de",
                                            ifelse(tbl_todo_los_datos_magnitud$tipoM == "Mayor", "#e00909","#000000"))))),
        label = tbl_todo_los_datos_magnitud$Mag ,
        popup = paste0(
          "<b>Magnitud: </b>"
          , tbl_todo_los_datos_magnitud$Mag
          , "<br>"
          , "<b>Tipo Mag: </b>"
          , tbl_todo_los_datos_magnitud$tipoM
          , "<br>"
          , "<b>País: </b>"
          , tbl_todo_los_datos_magnitud$pais
          , "<br>"
          , "<b>Región: </b>"
          , tbl_todo_los_datos_magnitud$Region
          
        ),
        stroke = FALSE, fillOpacity = 1) %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(colors =c("#2ce622", "#ded416","#1878c7","#a216de","#e00909","#000000"),
                   labels = c("Menor", "Ligero", "Moderado","Fuerte", "Mayor", "Gran"),
                   opacity = 0.5)
    
  })
  #DATOS GENERALES DE DIA MES Y AÑO-----------------------------------------------------------------------
  output$Cantidad_anio <- renderValueBox({
    CANTIDAD_ANIOS<- nrow(tabla_para_inicio %>% filter(Year==anio))
    valueBox(
      paste0(CANTIDAD_ANIOS),  paste("Cantidad de sismos en el año:  ", anio, sep = " " ), icon =  icon("calendar"),color ="orange"
    )
    # valueBox(
    #   paste0(CANTIDAD_ANIOS), "AÑO", icon = icon("list")
    # )
  })
  output$Cantidad_mes <- renderValueBox({
    CANTIDAD_MES  <- nrow(tabla_para_inicio %>% filter(Year==anio & Month==mes ))
    valueBox(
      paste0(CANTIDAD_MES), paste( "Cantidad de sismos en el mes:  ", mes, sep = " " ), icon =  icon("calendar"),color ="green"
    )
  })
  output$Cantidad_dia <- renderValueBox({
    CANTIDAD_DIA  <- nrow(tabla_para_inicio %>% filter(Year==anio & Month==mes & Day==dia))
    valueBox(
      paste0(CANTIDAD_DIA), paste("Cantidad de sismos en el día: ", dia, sep=" "),icon =  icon("calendar"),color ="yellow"
    )
  })
  
  output$CantidadPais<- renderValueBox({
    nPais<-nrow(tabla_base[!duplicated(tabla_base$pais), ])
     
    valueBox(
      paste0(nPais), "Cantidad de paises analizados en la aplicación"  , icon =  icon("globe"),color ="olive"
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
     
     Informe_barra_mes$Meses <- 
       with(Informe_barra_mes,
            ifelse(Informe_barra_mes$Meses == 1 , "En",
                   ifelse(Informe_barra_mes$Meses ==2 , "Feb",
                          ifelse(Informe_barra_mes$Meses ==3, "Mzo",
                                 ifelse(Informe_barra_mes$Meses ==4, "Abr",
                                        ifelse(Informe_barra_mes$Meses ==5 ,"My",
                                               ifelse(Informe_barra_mes$Meses ==6,"Jun",
                                                      ifelse(Informe_barra_mes$Meses ==7,"Jul",
                                                             ifelse(Informe_barra_mes$Meses ==8,"Ag",
                                                                    ifelse(Informe_barra_mes$Meses ==9,"Sept",
                                                                           ifelse(Informe_barra_mes$Meses ==10,"Oct",
                                                                                  ifelse(Informe_barra_mes$Meses ==11,"Nov",
                                                                                         ifelse(Informe_barra_mes$Meses ==12,"Dic","Null")))))))))))))
     
     Informe_barra_mes  
  })
  
  
  output$graficoRegiones<-renderPlotly({
    data<-tbl_todo_los_datos_magnitud
    if( input$anioRegion  != "All" )
    {
      data <- data[data$Year ==  input$anioRegion,]
      
    
    }else { data <- data[data$Year == "2022",] }
    Informe_barra_mes<-data %>%                     
      group_by(Group) %>%     
      tally() 
    
    xvz<-ggplot(Informe_barra_mes,aes(x=Group,y=n,fill=n))+
      geom_bar(stat="identity", position="dodge")+
      labs(title = "", y="N terremoto", x="Regiones", caption="Manos a la data")
    
    xvz
    
    
    
  })
  output$datos_grafico_regiones<-renderTable(width ="100%",striped = TRUE,hover = TRUE,bordered=TRUE, {
    data<-tbl_todo_los_datos_magnitud
    if(input$anioRegion  != "All" )
    {
      data <- data[data$Year == input$anioRegion,]
    }else { data <- data[data$Year == "2022",] }
    
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
      labs(title = "", y="N terremoto", x="Meses", caption="Manos a la data")
    
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
    pastelMagnitud <- pastelMagnitud %>% layout(title = ('Gráfico con la cantidad de eventos'),
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
    pastelProfundidad <- pastelProfundidad %>% layout(title = ('Gráfico con la cantidad de eventos'),
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

    graficoSnaMenor

  })
  output$snaLigero <- renderVisNetwork({
    graficoSnaLigero<-NULL
    data <-  tbl_clasificado_anio_actual
    if(input$regionSNA  != "All" )
    {
      data <- data %>% filter(Group==input$regionSNA & tipoM =="Ligero" )
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
      graficoSnaLigero<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
      }else
      {
        
      }
      
      }
      
    graficoSnaLigero
  })
  output$snaModerado <- renderVisNetwork({
    graficoSnaModerado<-NULL
    data <-  tbl_clasificado_anio_actual
    if(input$regionSNA  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA & tipoM =="Moderado" )
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
      graficoSnaModerado<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
      }else
      {
        
      }
    }
    graficoSnaModerado
  })
  output$snaFuerte <- renderVisNetwork({
    graficoSnaFuerte<-NULL
    data <-  tbl_clasificado_anio_actual
    if(input$regionSNA  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA & tipoM =="Fuerte" )
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
      graficoSnaFuerte<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
      }else
      {
        
      }
    }
    graficoSnaFuerte
  })
  output$snaMayor <- renderVisNetwork({
    graficoSnaMayor<-NULL
    data <-  tbl_clasificado_anio_actual
    if(input$regionSNA  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA & tipoM =="Mayor" )
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
      graficoSnaMayor<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
      }else
      {
        
      }
    }
    graficoSnaMayor
   ## validate(
   ##   need(!is.null(input$regionSNA), "NO HAY DATOS")
   ## )
  })
  output$snaGran <- renderVisNetwork({
    graficoSnaGran<-NULL
    data <-  tbl_clasificado_anio_actual
    if(input$regionSNA  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA & tipoM =="Gran" )
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
      graficoSnaGran<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
      }else
      {
        
      }
    }
    graficoSnaGran
  })

  #----------------------------SOCIAL NETWORK ANALITY PARA LA PROFUNDIDA
  output$snaSuperficial <- renderVisNetwork({
    graficosnaSuperficial<-NULL
    data <-  tbl_clasificado_por_profundidad
    if(input$regionSNA2  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA2 & tipoDepth.km =="Superficial" )
      if(nrow(data)>0)
      {
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
      graficosnaSuperficial<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
      # shinyjs::hide(id =  "img2")
      }else
      {
        
      }
    }

    graficosnaSuperficial
  })
  output$snaIntermedio <- renderVisNetwork({
    graficoSnaIntermedio<-NULL
    data <-  tbl_clasificado_por_profundidad
    if(input$regionSNA2  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA2 & tipoDepth.km =="Intermedio" )
      if(nrow(data)>0)
      {
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
      graficoSnaIntermedio<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
      }else
      {
        
      }
      
      # shinyjs::hide(id =  "img2")
    }
    
    

    graficoSnaIntermedio
  })
  output$snaProfundo <- renderVisNetwork({
  
    graficoSnaProfundo<-NULL
    data <-  tbl_clasificado_por_profundidad
    if(input$regionSNA2  != "All" )
    {

      data <- data %>% filter(Group==input$regionSNA2 & tipoDepth.km =="Profundo" )
      if(nrow(data)>0)
      {
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
      graficoSnaProfundo<- visNetwork(nodesM,edgesM,
                                   layout = "layout_in_circle" )%>% visIgraphLayout() %>%
        visEdges( label = edges1$label, physics = FALSE) %>% visNodes(size =nodes1$value ) %>%
        visOptions(highlightNearest = TRUE,
                   nodesIdSelection = TRUE)
      }else
      {
       
      }
        

    }
    
    graficoSnaProfundo
    
  })
  
}

shinyApp(ui, server)
