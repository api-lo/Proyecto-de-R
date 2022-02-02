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


for(i in 1:nrow(tabla_base))
{
    vector<-str_split(tabla_base$Region[i], ",", simplify = TRUE)
    vector<-as.data.frame(vector)
    tabla_base$pais[i]<-  vector[,ncol(vector)]
}

tabla_base


