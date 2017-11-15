library(maptools)   # A package for building maps
library(ggplot2)    # To have ggplot2 graphic interface
library(plyr)       # To manipulate data
library(graphics)   # Set par()
library(REmap)      # Get the longitude and latitude of regions
library(rgdal)

setwd("C:/Users/Think/Desktop/map")

# Prepare your epidemiological data
mydata <- read.csv("D:/R practice/R for map/example.csv") 

# Method 1: Remap

province <- as.character(mydata$NAME)
province <- data.frame(get_geo_position (province))
str(province)
names(province)[3] <- c("NAME" )

china_data_REmap <- join(mydata,province,type="full",by="NAME")  

provinceC_REmap <- china_data_REmap[
  names(china_data_REmap) %in% c("NAME1", "Dct_nur")]

remapC(provinceC_REmap)


# Method 2: ggplot2

china_map <- readOGR("C:/Users/Think/Desktop/map/province_2004.shp")       
china_map1 <- china_map@data          
china_map1 <- data.frame(china_map1,id=seq(0:36)-1) 
china_map2 <- fortify(china_map)
china_map3 <- join(china_map2, china_map1, type="full",by="id")  
names(china_map3) [13]<- c("NAME")
china_map4 <- join(china_map3, mydata, type="full",by="NAME") 

# example 1
ggplot ()+
  geom_polygon(data=china_map4,aes(x=long,y=lat,group=group,fill=Dct_nur),colour="gray40")+
  scale_fill_gradient(name="Numbers of doctor \nand nurse, 1/1000",low="white",high="red")+
  geom_text(data =china_data_REmap,aes(x=lon,y=lat,label=province),colour="black",size=5,
            vjust=0,nudge_y=0.5)+
  labs(title ="Numbers of doctor nand nurse per 1000 persons in China")+
  coord_map ("polyconic")+
  ylim (18, 54)+
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 25),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = c(0.15,0.2)
  )

# example 2-1 : Bubble chart on map
ggplot ()+
  geom_polygon(data=china_map4,aes(x=long,y=lat,group=group,fill=Dct_nur),colour="gray40")+
  scale_fill_gradient(name="Numbers of doctor \nand nurse, 1/1000",low="white",high="red")+
  geom_point(data=china_data_REmap,aes(x=lon,y=lat,size=Population),fill="blue",colour="red",
             alpha=0.6,shape=21)+
  geom_text(data =china_data_REmap,aes(x=lon,y=lat,label=province),colour="black",size=5,
            vjust=0,nudge_y=0.5)+
  labs(title ="Numbers of doctor nand nurse per 1000 persons in China")+
  coord_map ("polyconic")+
  ylim (18, 54)+
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 25),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

# example 2-2 : Bar chart on map
ggplot ()+
  geom_polygon(data=china_map4,aes(x=long,y=lat,group=group,fill=Dct_nur),colour="gray40")+
  scale_fill_gradient(name="Numbers of doctor \nand nurse, 1/1000",low="white",high="red")+
  geom_errorbar(data=china_data_REmap,aes(x=lon, ymin=lat, ymax=lat + Population/1500 ),
                colour="blue",size=5, width=0,alpha=0.5)+
  geom_text(data =china_data_REmap,aes(x=lon,y=lat,label=province),colour="black",size=5,
            vjust=0,nudge_y=0.5)+
  labs(title ="Numbers of doctor nand nurse per 1000 persons in China")+
  ylim (18, 54)+
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 25),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

library(googleVis)

# Method 3: googleVis

G1 <- gvisGeoMap(mydata,
                 locationvar='NAME2', 
                 numvar='Dct_nur',
                 options=list(region='CN'))
plot(G1) 

G2 <- gvisGeoChart(mydata, 
                   locationvar='NAME2', 
                   colorvar='Dct_nur',
                   options=list(region='CN',
                                displayMode="regions",
                                resolution="provinces",
                                colorAxis="{colors: ['yellow','red']}" ))
plot(G2) 