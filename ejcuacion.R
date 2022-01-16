library(tidyverse)
library(rvest)
library(gganimate)
library(gifski)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(gapminder)
library(tidyverse)
library(dplyr)
library(dummies)
library(kableExtra)
library(scales)
library(lubridate)
library(visNetwork)

datos<- read_html("https://n9.cl/ehsjz")
tabla1<- datos %>%
html_table()
  tabla1[1]
tabla_sis<- tabla1[[1]]
tabla_analizar <- subset(tabla_sis, select = -c(4,10,11))


# DIA
CANTIDAD_DIA  <- nrow(tabla_analizar %>% filter(Year=="2021" & Month=="12" & Day=="28" ))
# MES
CANTIDAD_MES  <- nrow(tabla_analizar %>% filter(Year=="2021" & Month=="12" ))
# ANIO
CANTIDAD_ANIOS  <- nrow(tabla_analizar %>% filter(Year=="2021"))

 
# Tabla_filtrada <- tabla_analizar %>% filter(Year==substr(Sys.Date(),0,4))
Tabla_filtrada <- tabla_analizar %>% filter(Year=="2021")

Tbl_cant_mes<-Tabla_filtrada %>%                     
  group_by(Month) %>%     
  tally() 


substr(Sys.Date(),0,4)

g_cant_mes <- Tbl_cant_mes %>%
  ggplot(aes(x = Month, y = n)) +
  geom_line(color =  "steelblue3",size=1) + geom_point(size=4, color="steelblue4")+
  scale_color_gradient(low="blue", high="red")+
  labs(title = "Sismo ", 
       subtitle = "", 
       y = "Cantidad", x = "Meses") + 
  theme_bw()
ggplotly(g_cant_mes)
  

terremotos<-unite(tabla_analizar, date,c(1,2,3),  sep = "-", remove = TRUE)
tabla_paisregion<-terremotos %>% separate(6, c("region", "pais"), ", ", extra = "merge")
tabla_paisregion<- na.omit(tabla_paisregion)


tabla_agrupado_<-tabla_paisregion %>%
  group_by(pais, date)%>%     
  tally() 


tabla_agrupado_$date <- as.Date(tabla_agrupado_$date)



auxGra<-subset(tabla_agrupado_, date> "2021-01-24" & date < "2021-12-31")

todo <- auxGra%>%
  ggplot(aes(x = date, y = n, color=pais)) +
  geom_line(size=1)+ 
  labs(title = "AMZN Line Chart", 
       subtitle = "Continuous Scale", 
       y = "Closing Price", x = "") + 
  theme_bw()
ggplotly(todo)



# tabla_paisregion<-tabla_analizar %>% separate(8, c("region", "pais"), ", ", extra = "merge")
tabla_paisregion<- na.omit(tabla_paisregion)
any(is.na(tabla_pais)) #COMPROBAR QUE SE ELIMINARON LOS DATOS



tabla_pais<- subset(tabla_paisregion, select = -c(1,2,3,4,5,6,7,8))
nuevo_df <- data.frame(Estados =c (tabla_pais))
new_df<-aggregate(nuevo_df$pais, nuevo_df, length)
colnames(new_df)[2]<-"Incidencias"
new_df


new_df<-new_df[-c(6,7),]
new_df
tbl_nodo<-new_df
tbl_nodo


tbl_vertice<-tabla_analizar %>% separate(8, c("region", "pais"), ", ", extra = "merge")
tbl_vertice<- na.omit(tbl_vertice)
tbl_vertice


tbl_vertice$tipoM <- with(tbl_vertice,ifelse(tbl_vertice$Mag >= 1 & tbl_vertice$Mag <=3.9, "Menor",
                                             ifelse(tbl_vertice$Mag >=4 & tbl_vertice$Mag <=4.9, "Ligero",
                                                    ifelse(tbl_vertice$Mag >=5 & tbl_vertice$Mag <=5.9, "Moderado",
                                                           ifelse(tbl_vertice$Mag >=6 & tbl_vertice$Mag <=6.9, "Fuerte",
                                                                  ifelse(tbl_vertice$Mag >=7 & tbl_vertice$Mag <=7.9,"Mayor",
                                                                         ifelse(tbl_vertice$Mag >=8, "Gran",0)))))))


f1 <- tbl_vertice %>% filter(Year=="2021" & Month=="12" )
tbl_analisis<- subset(f1, select = -c(1,2,3,4,5,6,7,8))
tbl_analisis<-subset(tbl_analisis, select = c(1,2))


library(igraph)

net <- graph.data.frame(tbl_analisis, directed=F)
V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)
plot(net)

tbl_count
nodoCompletos<-data.frame(V(net)$name)
visIgraph(net)

asdas <- graph_from_data_frame(d=tbl_analisis, vertices=nodoCompletos, directed=F)
plot(asdas)
data2 <- toVisNetworkData(net)

visNetwork(nodes = data2$nodes, edges = data2$edges)%>%
  visEdges(smooth = FALSE)


data2 <- toVisNetworkData(net)


E(FinalNetwork)$color <- ifelse(E(FinalNetwork)$tipoM == "Menor", "#c94f4f", 
                                ifelse(E(FinalNetwork)$tipoM == "Ligero", "#4b95c9",
                                       ifelse(E(FinalNetwork)$tipoM == "Moderado", "#4acf60",
                                              ifelse(E(FinalNetwork)$tipoM == "Fuerte", "#c7188d","black"))))
visIgraph(FinalNetwork)

data <- toVisNetworkData(net)
visNetwork(nodes = data$nodes, edges = data$edges,layout = "layout_in_circle", 
           physics = TRUE, smooth = FALSE)%>%visEdges(
             shadow = FALSE,
             color = list(color = "#0085AF", highlight = "#C62F4B")
           )    %>%  visIgraphLayout() 

data <- toVisNetworkData(net)



visIgraph(net) %>% 
  visNodes(color = list(background = "lightblue", 
                        border = "darkblue",
                        highlight = "yellow"),
           shadow = list(enabled = TRUE, size = 10))  %>%
  visLayout(randomSeed = 12) 







visIgraph(net  , idToLabel = TRUE) %>%   visIgraphLayout(layout = "layout_with_fr") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% 
  visInteraction(navigationButtons = TRUE)


tbl_analisis
tbl_count<-tbl_analisis %>%                     
  group_by(pais) %>%     
  tally() 




FinalNetwork <- graph_from_data_frame(d=finalConexiones, vertices=tbl_count, directed=F)
plot(FinalNetwork)

plot(FinalNetwork,layout=layout_with_fr, main="Force-directed",
     vertex.color = rainbow(52),
     vertex.size=tbl_count$n,
     edge.arrow.size=0.2,layout=layout.kamada.kawai)


  

    visNetwork(nodes = data$nodes, edges = data$edges)%>%
      visPhysics(stabilization = FALSE) %>% visIgraphLayout()

    
    visNetwork(nodes = data$nodes, edges = data$edges)%>%
      visEdges(smooth = FALSE)
    

  data <- toVisNetworkData(FinalNetwork)
  visNetwork(nodes = data$nodes, edges = data$edges,layout = "layout_in_circle",
             physics = TRUE, smooth = FALSE)%>%visEdges(
               shadow = FALSE,
               color = list(color = "#0085AF", highlight = "#C62F4B")
             )%>% visIgraphLayout()








V(FinalNetwork)$size <- strength(FinalNetwork)
par(mar=c(0,0,0,0)); plot(g)

E(FinalNetwork)$color <- ifelse(E(FinalNetwork)$tipoM == "Menor", "#c94f4f", 
                                ifelse(E(FinalNetwork)$tipoM == "Ligero", "#4b95c9",
                                       ifelse(E(FinalNetwork)$tipoM == "Moderado", "#4acf60",
                                              ifelse(E(FinalNetwork)$tipoM == "Fuerte", "#c7188d","black"))))



visIgraph(net, idToLabel = TRUE,layout = "layout_in_circle")


# 


library(igraph)
# MENOR
datos<-NULL 
tbl_Menor<-tbl_analisis %>% filter(tipoM=="Menor") %>% group_by(pais,tipoM) %>% slice(1)
aux_tbl<-full_join(tbl_Menor,tbl_Menor,by="tipoM")
aux_tbl<- subset(aux_tbl, select = c(1,3,2))


for (i in 1:nrow(aux_tbl)) {
  auxP1<-  aux_tbl$pais.x[i]
  auxP2<-  aux_tbl$pais.y[i]
  auxT<-  aux_tbl$tipoM[i]
  if(auxP1==auxP2)
  {
    
  }
  else
  {
    lista<- c(auxP1,auxP2)
    lista<-sort(lista, index.return = TRUE)
    lista<- data.frame(lista$x)
    aux<- data.frame("pais1"=lista[1,1],"pais2"=lista[2,1],"tipoM"=auxT)
    datos<-rbind(datos,aux)
  }
}

conexionesMenor<-datos %>% group_by(pais1,pais2) %>% slice(1)
paises<-aux_tbl %>% group_by(pais.x) %>% slice(1) 
nodosMenor<-subset(paises, select = -c(2,3))
colnames(nodosMenor) <- c("Pais")
MenorNetwork <- graph_from_data_frame(d=conexionesMenor, vertices=nodosMenor, directed=F)
plot(MenorNetwork)






# LIGERO
datos<-NULL 
tbl_Ligero<-tbl_analisis %>% filter(tipoM=="Ligero")  %>% group_by(pais,tipoM) %>% slice(1)
aux_tbl<-full_join(tbl_Ligero,tbl_Ligero,by="tipoM")
aux_tbl<- subset(aux_tbl, select = c(1,3,2))


for (i in 1:nrow(aux_tbl)) {
  auxP1<-  aux_tbl$pais.x[i]
  auxP2<-  aux_tbl$pais.y[i]
  auxT<-  aux_tbl$tipoM[i]
  if(auxP1==auxP2)
  {
    
  }
  else
  {
    lista<- c(auxP1,auxP2)
    lista<-sort(lista, index.return = TRUE)
    lista<- data.frame(lista$x)
    aux<- data.frame("pais1"=lista[1,1],"pais2"=lista[2,1],"tipoM"=auxT)
    datos<-rbind(datos,aux)
  }
}

conexionesLigero<-datos %>% group_by(pais1,pais2) %>% slice(1)
paises<-aux_tbl %>% group_by(pais.x) %>% slice(1) 
nodosLigero<-subset(paises, select = -c(2,3))
colnames(nodosLigero) <- c("Pais")
LigeroNetwork <- graph_from_data_frame(d=conexionesLigero, vertices=nodosLigero, directed=F)
plot(LigeroNetwork)







# MODERADO
datos<-NULL
tbl_Moderado<-tbl_analisis %>% filter(tipoM=="Moderado")  %>% group_by(pais,tipoM) %>% slice(1)
aux_tbl<-full_join(tbl_Moderado,tbl_Moderado,by="tipoM")
aux_tbl<- subset(aux_tbl, select = c(1,3,2))


for (i in 1:nrow(aux_tbl)) {
  auxP1<-  aux_tbl$pais.x[i]
  auxP2<-  aux_tbl$pais.y[i]
  auxT<-  aux_tbl$tipoM[i]
  if(auxP1==auxP2)
  {
    
  }
  else
  {
    lista<- c(auxP1,auxP2)
    lista<-sort(lista, index.return = TRUE)
    lista<- data.frame(lista$x)
    aux<- data.frame("pais1"=lista[1,1],"pais2"=lista[2,1],"tipoM"=auxT)
    datos<-rbind(datos,aux)
  }
}

conexionesModerado<-datos %>% group_by(pais1,pais2) %>% slice(1)
paises<-aux_tbl %>% group_by(pais.x) %>% slice(1) 
nodosModerado<-subset(paises, select = -c(2,3))
colnames(nodosModerado) <- c("Pais")
ModeradoNetwork <- graph_from_data_frame(d=conexionesModerado, vertices=nodosModerado, directed=F)
plot(ModeradoNetwork)









# FUERTE
datos<-NULL

tbl_Fuerte<-tbl_analisis %>% filter(tipoM=="Fuerte")  %>% group_by(pais,tipoM) %>% slice(1)
aux_tbl<-full_join(tbl_Fuerte,tbl_Fuerte,by="tipoM")
aux_tbl<- subset(aux_tbl, select = c(1,3,2))
if(nrow(aux_tbl)<=1)
{
  nodosFuerte<- data.frame("Pais"=aux_tbl[1,1])  
  colnames(nodosFuerte) <- c("Pais")
  conexionesFuerte<-aux_tbl
  colnames(conexionesFuerte) <- c("pais1","pais2","tipoM")
  conexionesFuerte[]
  
}else{

for (i in 1:nrow(aux_tbl)) {
  auxP1<-  aux_tbl$pais.x[i]
  auxP2<-  aux_tbl$pais.y[i]
  auxT<-  aux_tbl$tipoM[i]
  if(auxP1==auxP2)
  {
    
  }
  else
  {
    lista<- c(auxP1,auxP2)
    lista<-sort(lista, index.return = TRUE)
    lista<- data.frame(lista$x)
    aux<- data.frame("pais1"=lista[1,1],"pais2"=lista[2,1],"tipoM"=auxT)
    datos<-rbind(datos,aux)
  }
}
  conexionesFuerte<-datos %>% group_by(pais1,pais2) %>% slice(1)
  paises<-aux_tbl %>% group_by(pais.x) %>% slice(1) 
  nodosFuerte<-subset(paises, select = -c(2,3))
  colnames(nodosFuerte) <- c("Pais")
  FuerteNetwork <- graph_from_data_frame(d=conexionesFuerte, vertices=nodosFuerte, directed=F)
  plot(FuerteNetwork)
  
}



# 
# conexionesMenor nodosMenor
# conexionesLigero nodosLigero
# conexionesModerado nodosModerado
# conexionesFuerte nodosFuerte

finalConexiones<-NULL
finalNodos<-NULL


finalConexiones<-rbind(finalConexiones,conexionesMenor)
finalConexiones<-rbind(finalConexiones,conexionesLigero)
finalConexiones<-rbind(finalConexiones,conexionesModerado)
finalConexiones<-rbind(finalConexiones,conexionesFuerte)

finalNodos<-rbind(finalNodos,nodosMenor)
finalNodos<-rbind(finalNodos,nodosLigero)
finalNodos<-rbind(finalNodos,nodosModerado)
finalNodos<-rbind(finalNodos,nodosFuerte)
finalNodos<-finalNodos %>% group_by(Pais) %>% slice(1) 


FinalNetwork <- graph_from_data_frame(d=finalConexiones, vertices=finalNodos, directed=F)
plot(FinalNetwork)


# 
# plot(FinalNetwork,vertex.color = rainbow(52),
#      vertex.size=V(FinalNetwork)$,edge.arrow.size=0.2,layout=layout.fruchterman.reingold)


plot(net,vertex.color = rainbow(52), vertex.size=V(net)$degree*0.4,edge.arrow.size=0.2,layout=layout.graphopt)
plot(net,vertex.color = rainbow(52), vertex.size=V(net)$degree*0.4,edge.arrow.size=0.2,layout=layout.kamada.kawai)





E(FinalNetwork)$color <- ifelse(E(FinalNetwork)$tipoM == "Menor", "#c94f4f", 
                          ifelse(E(FinalNetwork)$tipoM == "Ligero", "#4b95c9",
                                 ifelse(E(FinalNetwork)$tipoM == "Moderado", "#4acf60",
                                        ifelse(E(FinalNetwork)$tipoM == "Fuerte", "#c7188d","black"))))


visIgraph(FinalNetwork, idToLabel = TRUE,layout = "layout_in_circle")

library(ggplot2)
library(ggnet)
plot(FinalNetwork, vertex.frame.color="white")


visIgraph(FinalNetwork, idToLabel = TRUE,layout = "layout_in_circle")%>%
    visNodes(size = 10) %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = TRUE)%>%
     visConfigure(enabled = TRUE)


network<-visIgraph(FinalNetwork)

custom_network <- visNetworkEditor(object = network)
custom_network

tbl_Mayor<-tbl_analisis %>% filter(tipoM=="Mayor") %>% group_by(pais,tipoM) %>% slice(1)
tbl_Gran<-tbl_analisis %>% filter(tipoM=="Gran") %>% group_by(pais,tipoM) %>% slice(1)











library(networkD3)


networkData <- data.frame(datos2,nodos)

# Plot
simpleNetwork(networkData, zoom = TRUE,linkDistance = 200)


Informe_uno<-tbl_analisis %>%                     
  group_by(tipoM) %>%     
  tally() 

Informe_dos<-tbl_analisis %>%                     
  group_by(pais) %>%     
  tally() 

gCanCategoriaP<-ggplot(Informe_dos,aes(x=pais,y=n,fill=pais))+
  geom_bar(stat="identity", position="dodge")+
  labs(title="SISMOS EN PAIS DE ESTE MES", 
       subtitle="(Cantidad de sismos)", y="Montos", x="Proveedor", caption="Manos a la data")

ggplotly(gCanCategoriaP)

gCanCategoriaM <- plot_ly( labels=Informe_uno$tipoM, values=Informe_uno$n, 
               textinfo='label+percent',
               insidetextorientation='radial')
gCanCategoriaM <- gCanCategoriaM %>% add_pie(hole = 0.6)
gCanCategoriaM <- gCanCategoriaM %>% layout(title = 'Cantidad por categoria 2021',
                                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

gCanCategoriaM<-ggplot(Informe_uno,aes(x=2, y=Informe_uno$n, fill=Informe_uno$tipoM))+
geom_bar(stat = "identity",color="white")+ geom_text(aes(label=Informe_uno$n),
position=position_stack(vjust=0.5),color="black",size=8)+ coord_polar(theta="y")+theme_void()+
labs(title="Gráfico de Pie")+xlim(0.5,2.5)
 

 
# DIA
CANTIDAD_DIA  <- nrow(tbl_vertice %>% filter(Year=="2021" & Month=="12" & Day=="28" ))
# MES
CANTIDAD_MES  <- nrow(tbl_vertice %>% filter(Year=="2021" & Month=="12" ))
# ANIO
CANTIDAD_ANIOS  <- nrow(tbl_vertice %>% filter(Year=="2021"))

library(visNetwork)
library(igraph)

tbl_analisis
nodes <- as.data.frame(tbl_analisis[1])
nodes$id <- nodes$label

edges <- as.data.frame(tbl_analisis[2])
graph <- graph_from_data_frame(edges, directed = FALSE)

visNetwork(nodes, edges)



library(igraph)
net <- graph.data.frame(tbl_repetido, directed=F)
V(net)
E(net)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)


hist(V(net)$degree,
     col="green",
     main='Histogram of node degree',
     ylab='Frequency',
     xlab='Degree of vertices')
#Network diagram
plot(net, vertex.color ="green", edge.arrow.size=0.3)
#Destacar los grados y los trazados
plot(net,vertex.color = rainbow(52), vertex.size=V(net)$degree*0.4,edge.arrow.size=0.2,layout=layout.fruchterman.reingold)
plot(net,vertex.color = rainbow(52), vertex.size=V(net)$degree*0.4,edge.arrow.size=0.2,layout=layout.graphopt)
plot(net,vertex.color = rainbow(52), vertex.size=V(net)$degree*0.4,edge.arrow.size=0.2,layout=layout.kamada.kawai)
#Hubs and authorities
hs <- hub_score(net)$vector
as <- authority.score(net)$vector
par(mfrow=c(1,2)) #to get one row, 2 columns and see 2 diagrams side by side
set.seed(123) #to get same configuration
plot(net,vertex.size=hs*30,main="Hubs",vertex.color=rainbow(52),edge.arrow.size=0.2,layout=layout.kamada.kawai)
plot(net,vertex.size=as*30,main="Authorities",vertex.color=rainbow(52),edge.arrow.size=0.2,layout=layout.kamada.kawai)



#Community detection

net <- graph.data.frame(tbl_analisis,directed=F)
cnet <- cluster_edge_betweenness(net)
p<-plot(cnet,net)
ggplotly(p)

data <- toVisNetworkData(igraph_network)
visNetwork(nodes = data$nodes, edges = data$edges, height = "500px")

network.vertex.names(net) = letters[1:10]
p <- ggnet2(net, node.size = 6, node.color = "black", edge.size = 1, edge.color = "grey")



# random graph
net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)

# vertex names
network.vertex.names(net) = letters[1:10]

p <- ggnet2(net, size = 6, color = rep(c("tomato", "steelblue"), 5))

ggplotly(p)

