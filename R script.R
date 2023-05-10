library(mapview)
library(raster)
library(leaflet)
library(sp)
library(dplyr)
library(lattice)
library(leafpop)
library(viridis)
library(RColorBrewer)
library(htmltools)
library(leafem)
library(leaflet.extras)
library(leaflegend)



setwd("E:/Dropbox/01_Work/Nastava/grupa za agonomske podatke/datathon dogcityzg/layers")

parks <-shapefile("parks_updated.shp")
green_areas <-shapefile("green without leash.shp")
grooming <-shapefile("grooming.shp")
daycare <-shapefile("hotels.shp")
shops <-shapefile("shops.shp")
wastebins <-shapefile("trash.shp")
wastebins <-wastebins[,1:7]
vets <-shapefile("vets_3-11-21.shp")
districts<-shapefile("districts.shp")


icon_bins <- makeIcon(
  iconUrl = "004-poop.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 15, iconAnchorY = 15,)
icon_groom <- makeIcon(
  iconUrl = "002-dog.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 15, iconAnchorY = 15,)
icon_vet <- makeIcon(
  iconUrl = "022-first aid kit.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 15, iconAnchorY = 15,)
icon_shop <- makeIcon(
  iconUrl = "013-dog food.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 15, iconAnchorY = 15,)
icon_hotel <- makeIcon(
  iconUrl = "018-Dog house.png",
  iconWidth = 20, iconHeight = 20,
  iconAnchorX = 15, iconAnchorY = 15,)

binpal <- colorBin("BrBG", districts$Score, 7, pretty = TRUE)

map <-leaflet(options = leafletOptions(minZoom = 11))%>%
  setMaxBounds( lng1 = 15.77322
                , lat1 = 45.61397
                , lng2 = 16.23959
                , lat2 = 45.96915)%>%
  addProviderTiles(providers$CartoDB.Positron)%>% 
  addPolygons(data=districts, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~binpal(Score),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              label = paste("Dog friendly Score:",round(districts$Score, digits = 2)),
              popup = paste("<b> <center>DISTRICT</b></center>", "<br>",
                            "<b>Name</b>", districts$Name, "<br>",
                            "<b>Number of Vets:</b>", districts$Vets, "<br>",
                            "<b>Number of Petshops:</b>", districts$Shops, "<br>",
                            "<b>Number of Parks:</b>", districts$Parks, "<br>",
                            "<b>Number of Doggy waste bins:</b>", districts$Wastebins, "<br>",
                            "<b>% of district with green areas for dogs:</b>", districts$`green area`, "<br>",
                            "<b>Number of Groomers:</b>", districts$Grooming, "<br>",
                            "<b>Number of Pet hotels:</b>", districts$Daycare),
              group = "Districts")%>%                        
  groupOptions("Districts", zoomLevels = 1:11)%>% 
addPolygons(data= green_areas, color = "#444444", weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fillOpacity = 0.5,
            fillColor ="lightgreen",
            highlightOptions = highlightOptions(color = "darkgreen", weight = 2,
                                                bringToFront = TRUE),
            popup = paste("<b>Name</b>", green_areas$name, "<br>",
                          "<b>Size (m<sup>2</sup>):</b>", green_areas$area),
            group = "Green areas for dogs")%>%                        
  groupOptions("Green areas for dogs", zoomLevels = 12:20)%>% 
addPolygons(data=parks,color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.7,
              fillColor = parks$color,
              highlightOptions = highlightOptions(color = "darkgreen", weight = 2,
                                                  bringToFront = TRUE),
              popup = paste("<b>Name</b>", parks$Name, "<br>",
                            "<b>Size (m<sup>2</sup>):</b>", parks$Area, "<br>",
                            "<b>Content:</b>", parks$Content, "<br>",
                            "<b>Additional content:</b>", parks$Additional, "<br>",
                            "<b>Safety:</b>", parks$Danger),
            group = "Dog parks")%>%                        
  groupOptions("Dog parks", zoomLevels = 12:20)%>%
  addMarkers(data=vets,~X,~Y,icon = icon_vet,  popup = paste("<b>Name</b>", vets$Name, "<br>",
                                                             "<b>Address:</b>", vets$Adress, "<br>",
                                                             "<b>Working hours:</b>", vets$`Working ho`, "<br>",
                                                             "<b>Phone:</b>", vets$Phone, "<br>",
                                                             "<b>Web:</b>", vets$Web),group = "Vets")%>% 
  addMarkers(data=shops,~X,~Y,icon = icon_shop,  popup = paste("<b>Name</b>", shops$Name, "<br>",
                                                               "<b>Address:</b>", shops$Adress, "<br>",
                                                               "<b>Working hours:</b>", shops$`Working ho`, "<br>",
                                                               "<b>Phone:</b>", shops$Phone, "<br>",
                                                               "<b>Web:</b>", shops$Web),group = "Petshops")%>%
  addMarkers(data=wastebins,~X,~Y, icon = icon_bins, popup = paste("<b>Location</b>", wastebins$Location, "<br>",
                                                  "<b>Description:</b>", wastebins$Descriptio, "<br>",
                                                  "<b>District:</b>", wastebins$District, "<br>",
                                                  "<b>Neighborhood:</b>", wastebins$Neighborho),group = "Doggy waste bins")%>%                        
addMarkers(data=grooming,~Y,~X,icon = icon_groom,  popup = paste("<b>Name</b>", grooming$Name, "<br>",
                                                                 "<b>Address:</b>", grooming$Adress, "<br>",
                                                                 "<b>Working hours:</b>", grooming$`Working ho`, "<br>",
                                                                 "<b>Phone:</b>", grooming$Phone, "<br>",
                                                                 "<b>Web:</b>", grooming$Web),group = "Grooming salons")%>% 
addMarkers(data=daycare,~X,~Y,icon = icon_hotel,  popup = paste("<b>Name</b>", daycare$Name, "<br>",
                                                                "<b>Address:</b>", daycare$Adress, "<br>",
                                                                "<b>Working hours:</b>", daycare$`Working ho`, "<br>",
                                                                "<b>Phone:</b>", daycare$Phone, "<br>",
                                                                "<b>Web:</b>", daycare$Web),group = "Dog hotels")%>%
  groupOptions("Vets", zoomLevels = 12:20)%>%
  groupOptions("Petshops", zoomLevels = 12:20)%>%
  groupOptions("Doggy waste bins", zoomLevels = 12:20)%>%
  groupOptions("Grooming salons", zoomLevels = 12:20)%>%
  groupOptions("Dog hotels", zoomLevels = 12:20)%>%
leafem::addHomeButton(ext = extent(districts), group="Home")%>%
  addControlGPS(
    options = gpsOptions(
      position = "topleft",
      activate = TRUE, 
      autoCenter = TRUE,
      setView = TRUE))%>%
addLayersControl(
  overlayGroups = c("Districts", "Green areas for dogs", "Dog parks", "Vets","Petshops","Doggy waste bins","Grooming salons", "Dog hotels"),
  options = layersControlOptions(collapsed = FALSE)
)%>%
  addLegendImage(images = c("green.png","parks.png","parks ns.png","022-first aid kit.png","013-dog food.png","004-poop.png","002-dog.png","018-Dog house.png"),
                 labels = c("Green areas for dogs","Dog parks","Unsafe Dog parks", "Vets","Petshops","Doggy waste bins", "Grooming salons", "Dog hotels"),width = 20, height =20,
                 orientation = 'vertical',labelStyle = "font-size: 16px; vertical-align: middle;",
                 title = htmltools::tags$div('Legend',
                                             style = 'font-size: 24px; text-align: center;'),
                 position = 'topright')

map


