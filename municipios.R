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

x<-order(x)


b<-colorRampPalette(c("white","red"))(32)[x]

m <- leaflet() %>%
  addTiles() %>%  
  addPolygons(data=mun[mun$CVE_ENT=="20",],stroke = F,popup=dest$NOM_ENT) %>% 
  addProviderTiles("CartoDB.Positron")
m 

m <- leaflet() %>%
  addTiles() %>%  
  addPolygons(data=dest,color = b,stroke = F,popup=dest$NOM_ENT) %>% 
  addProviderTiles("CartoDB.Positron")
m 
