library(tmap)
library(readr)
library(rgdal)
library(ggplot2)
library(ggmap)
library(maptools)
library(maps)
library(dplyr)
library(viridis)
library(mapproj)
library(spdep)
library(car)
library(spgwr)


chinaSHP <- readOGR("exe/gadm36_CHN_1.shp")# Read SHP file
qtm(chinaSHP)
dataWindow <- read_csv("cw/data.csv") # Read data

model <- lm(`open rate` ~ `Dry-bulb temperature`+`Wind speed`+`External relative humidity`
            +`Atmospheric pressure`,data=dataWindow) # OLS model
summary(model)
plot(model) # Plot result
durbinWatsonTest(model) #Autocorrelation

dataWindow$residuals <- residuals(model) 
qplot(dataWindow$residuals) + geom_histogram() 


#Function to plot residuals
bubblefunc <- function(myData,size,name,myBreaks){
  China <- map_data("world") %>% filter(region=="China")
  map <- 
    ggplot() +
    geom_polygon(data=China,aes(long, y = lat, group = group), fill="black", alpha=0.3) +
    geom_point(data=myData, aes(x=longitude, y = latitude, size=size, color=size, alpha=size), shape=20, stroke=FALSE) +
    scale_size_continuous(name=name,  trans="identity",range=c(1,12),breaks=myBreaks) +
    scale_alpha_continuous(name=name, trans="identity", range=c(0.1, 0.6),breaks=myBreaks) +
    scale_color_viridis(option="magma", trans="identity", name=name,breaks=myBreaks ) + 
    theme_void()  + coord_map() +  xlim(70,149)+ ylim(18,55)+
    guides( colour = guide_legend()) +
    ggtitle(paste("Window opening rate" ,name,"in China")) +
    theme(
      legend.position = c(0.9, 0.8),
      text = element_text(color = "#22211d",size=15), # Choose colour
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    )
  plot(map)
}
bubblefunc(dataWindow,dataWindow$residuals,"Residuals",c(-0.2,-0.1,0,0.1,0.3)) # Plot residuals


xy=dataWindow[,c(13,14)]
chinasf <- st_as_sf(x=xy,coords = c("longitude", "latitude"))
chinasp <- as(chinasf, "Spatial") # Transform to "spatial"

coordsW <- coordinates(chinasp) # calculate the centroids of all Wards in London
plot(coordsW)
knn_wards <- knearneigh(coordsW, k=4)# nearest neighbours
LWard_knn <- knn2nb(knn_wards)
plot(LWard_knn, coordinates(coordsW), col="blue") #Plot
#create a spatial weights matrix object from  weight
Lward.knn_4_weight <- nb2listw(LWard_knn, style="C")
#moran's I test on the residuals
moran.test(dataWindow$residuals, Lward.knn_4_weight)


#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(`open rate` ~ `Dry-bulb temperature`+`Wind speed`+`External relative humidity`
                     +`Atmospheric pressure`,data=dataWindow,coords=coordsW,adapt=T)
#run the gwr model
GWRModel <- gwr(`open rate` ~ `Dry-bulb temperature`+`Wind speed`+`External relative humidity`
                +`Atmospheric pressure`,data=dataWindow,coords=coordsW,adapt=GWRbandwidth,
                hatmatrix = TRUE,se.fit = TRUE)
GWRModel #print the results of the model
results<-as.data.frame(GWRModel$SDF)
names(results)


#attach coefficients to original dataframe
dataWindow$coeTemperature <- results$X.Dry.bulb.temperature.
dataWindow$coeWindspeed <- results$X.Wind.speed.
dataWindow$coeHumidity <- results$X.External.relative.humidity.
dataWindow$coePressure <- results$X.Atmospheric.pressure.


# Plot the coefficient
bubblefunc(dataWindow,dataWindow$coeTemperature,'coeTemperature',c(0.02,0.05, 0.06,0.07, 0.08))
bubblefunc(dataWindow,dataWindow$coeWindspeed,'coeWindspeed',c(-0.03,-0.01, 0,0.01, 0.03))
bubblefunc(dataWindow,dataWindow$coeHumidity,'coeHumidity',c(-0.005,-0.001, 0,0.001, 0.005))
bubblefunc(dataWindow,dataWindow$coePressure,'coePressure',c(-0.03,-0.01, 0,0.01, 0.03))

#statistically significant test
sigTest_Temp = abs(GWRModel$SDF$"`Dry-bulb temperature`") - 2 * GWRModel$SDF$"`Dry-bulb temperature`_se"
dataWindow$GWRTemp <- sigTest_Temp

sigTest_Wind = abs(GWRModel$SDF$"`Wind speed`") - 2 * GWRModel$SDF$"`Wind speed`_se"
dataWindow$GWRWind <- sigTest_Wind

sigTest_Humidity = abs(GWRModel$SDF$"`External relative humidity`") - 2 * GWRModel$SDF$"`External relative humidity`_se"
dataWindow$GWRHumidity <- sigTest_Humidity

sigTest_Pre = abs(GWRModel$SDF$"`Atmospheric pressure`") - 2 * GWRModel$SDF$"`Atmospheric pressure`_se"
dataWindow$GWRPre <- sigTest_Pre

# Plot the coefficient
bubblefunc(subset(dataWindow,dataWindow$GWRTemp>0),subset(dataWindow$coeTemperature,dataWindow$GWRTemp>0),'GWRTemp',c(0.02,0.05, 0.06,0.07, 0.08))
bubblefunc(subset(dataWindow,dataWindow$GWRWind>0),subset(dataWindow$coeWindspeed,dataWindow$GWRWind>0),'GWRWind',c(-0.1,-0.05,-0.03,-0.01, 0))
bubblefunc(subset(dataWindow,dataWindow$GWRHumidity>0),subset(dataWindow$coeHumidity,dataWindow$GWRHumidity>0),'GWRHumidity',c(-0.005,-0.001, 0,0.001, 0.005,0.01))
bubblefunc(subset(dataWindow,dataWindow$GWRPre>0),subset(dataWindow$coePressure,dataWindow$GWRPre>0),'GWRPre',c(-0.03,-0.01, 0,0.01, 0.03))

