library(leaflet)
library(rgdal)
library(sp)
library(shiny)

ogrInfo(".","Muni_2012gw")
mun<-readOGR(".","Muni_2012gw")  

ogrInfo(".","dest_2012gw")
dest<-readOGR(".","dest_2012gw")  


x<-c()

for (i in 1:9) {
  x[i]=length(mun[mun$CVE_ENT==paste("0",as.character(i),sep = ""),])
}

for (i in 10:32) {
  x[i]=length(mun[mun$CVE_ENT==i,])
}

y<-sort.list(sort.list(x))


b<-colorRampPalette(c("white","red"))(32)[y]

m <- leaflet() %>%
  addTiles() %>%  
  addPolygons(data=mun[mun$CVE_ENT=="20",],stroke = T,opacity = .2,popup=dest$NOM_ENT) %>% 
  addProviderTiles("CartoDB.Positron")
m 

m <- leaflet() %>%
  addTiles() %>%  
  addPolygons(data=dest,color = b,stroke = F,popup=dest$NOM_ENT) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addLegend(pal = colorNumeric(palette = colorRampPalette(c("white","red"))(32),domain = 1:32),values =1:32,title = "Cantidad de Municipios")

m 

#Implementado en shiny

ui <- fluidPage(
  leafletOutput("mymap"),
  verbatimTextOutput("stats")
)

server <- function(input, output, session) {
  
  nom<-data.frame(dest$NOM_ENT)
  numeroMunicipios<-cbind(nom,x,y)
  names(numeroMunicipios)<-c("Estado","Número de Municipios","Posición")
  
  output$stats <- renderPrint({
    numeroMunicipios
    
  })
  
  output$mymap<-renderLeaflet({
      leaflet() %>%
      addTiles() %>%
      addPolygons(data=dest,color = b,stroke = F,popup=dest$NOM_ENT) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addLegend(pal = colorNumeric(palette = colorRampPalette(c("red","white"))(32),domain = 1:32),values =1:32,title = "Cantidad de Municipios")
  })
  
}

shinyApp(ui, server)

