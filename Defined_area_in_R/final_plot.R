library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(stringr)
library(tidyverse)
library(raster)#DBSCAN
library(fpc)#DBSCAN
library(dbscan)
library(ggplot2)
library(OpenStreetMap)
library(spdep)
library(caret)
library(car)
library(broom)
library(corrr)
library(spgwr)

##########################3
##后续分析
gz_final_area <- st_read(here::here("final_output", 
                                       "gz",
                                       "gz_area_withcentricity.shp"))

gz_final_station <- st_read(here::here("final_output", 
                                       "gz",
                                       "gz_station_withcentricity.shp"))

######Moran's I
coordsW <- gz_final_area%>%
  st_centroid()%>%
  st_geometry()

plot(gz_final_station,axes=TRUE)

#确定knn的k
set.seed(3033)
intrain <- createDataPartition(y = gz_final_station$Node_index, p= 0.7, list = FALSE)
training <- gz_final_station[intrain,]
testing <- gz_final_station[-intrain,]
str(training)
testing<- testing %>%
  st_drop_geometry()
training<- training %>%
  st_drop_geometry()
grid = expand.grid(.k = seq(2, 20, by = 1))

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(Node_index~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneGrid = grid)

knn_fit
plot2 <- plot(knn_fit)
plot2
ggsave('RMSE&K.png',dpi = 300)
knn.pred_new = predict(knn_fit,testing)
plot(knn.pred_new)

#KNN空间weight
knn_wards <-gz_final_station %>%
  knearneigh(., k=2)
LWard_knn <- knn_wards %>%
  knn2nb()
Lward.knn_2_weight <- LWard_knn %>%
  nb2listw(., style="C")

#Global Moran's I
Knn_Global_Node <- gz_final_station %>%
  pull(Node_index) %>%
  as.vector()%>%
  moran.test(., Lward.knn_2_weight)

Knn_Global_Node

Knn_Global_Place <- gz_final_station %>%
  pull(Place_inde) %>%
  as.vector()%>%
  moran.test(., Lward.knn_2_weight)

Knn_Global_Place

Knn_Global_Settlement <- gz_final_station %>%
  pull(Settlement) %>%
  as.vector()%>%
  moran.test(., Lward.knn_2_weight)

Knn_Global_Settlement

Knn_Global_Container <- gz_final_station %>%
  pull(Container_) %>%
  as.vector()%>%
  moran.test(., Lward.knn_2_weight)

Knn_Global_Container

##Geary's C
C_Global_Node <- 
  gz_final_station %>%
  pull(Node_index) %>%
  as.vector()%>%
  geary.test(., Lward.knn_2_weight)

C_Global_Node

#Local Moran's I
Knn_Local_Node <- gz_final_station %>%
  pull(Node_index) %>%
  as.vector()%>%
  localmoran(., Lward.knn_2_weight)%>%
  as_tibble()

Knn_Local_Node

Knn_Local_Place<- gz_final_station %>%
  pull(Place_inde) %>%
  as.vector()%>%
  localmoran(., Lward.knn_2_weight)%>%
  as_tibble()

Knn_Local_Place

Knn_Local_Settlement<- gz_final_station %>%
  pull(Settlement) %>%
  as.vector()%>%
  localmoran(., Lward.knn_2_weight)%>%
  as_tibble()

Knn_Local_Settlement

Knn_Local_Container<- gz_final_station %>%
  pull(Container_) %>%
  as.vector()%>%
  localmoran(., Lward.knn_2_weight)%>%
  as_tibble()

Knn_Local_Container

Knn_Local_centricity<- gz_final_station %>%
  pull(centricity) %>%
  as.vector()%>%
  localmoran(., Lward.knn_2_weight)%>%
  as_tibble()

Knn_Local_centricity

#Getis OrdG∗i
Gi_Local_Node <- gz_final_station %>%
  pull(Node_index) %>%
  as.vector()%>%
  localG(., Lward.knn_2_weight)

Gi_Local_Node 

#Summarize
gz_final_area_MG <- gz_final_area %>%
  mutate(Gi_Local_Node = as.numeric(Gi_Local_Node))

gz_final_area_MG <- gz_final_area_MG %>%
  mutate(Knn_Local_Node_I = as.numeric(Knn_Local_Node$Ii))%>%
  mutate(Knn_Local_Node_Iz =as.numeric(Knn_Local_Node$Z.Ii))%>%
  mutate(Knn_Local_Place_I = as.numeric(Knn_Local_Place$Ii))%>%
  mutate(Knn_Local_Place_Iz =as.numeric(Knn_Local_Place$Z.Ii))%>%
  mutate(Knn_Local_Settlement_I = as.numeric(Knn_Local_Settlement$Ii))%>%
  mutate(Knn_Local_Settlement_Iz =as.numeric(Knn_Local_Settlement$Z.Ii))%>%
  mutate(Knn_Local_Container_I = as.numeric(Knn_Local_Container$Ii))%>%
  mutate(Knn_Local_Container_Iz =as.numeric(Knn_Local_Container$Z.Ii))

gz_final_area_MG <- gz_final_area_MG %>%
  mutate(Knn_Local_centricity_I = as.numeric(Knn_Local_centricity$Ii))%>%
  mutate(Knn_Local_centricity_Iz =as.numeric(Knn_Local_centricity$Z.Ii))

summary(gz_final_area_MG$Knn_Local_Node_Iz)

#Summarize——for station
gz_final_station_MG <- gz_final_station %>%
  mutate(Gi_Local_Node = as.numeric(Gi_Local_Node))

gz_final_station_MG <- gz_final_station_MG %>%
  mutate(Knn_Local_Node_I = as.numeric(Knn_Local_Node$Ii))%>%
  mutate(Knn_Local_Node_Iz =as.numeric(Knn_Local_Node$Z.Ii))%>%
  mutate(Knn_Local_Place_I = as.numeric(Knn_Local_Place$Ii))%>%
  mutate(Knn_Local_Place_Iz =as.numeric(Knn_Local_Place$Z.Ii))%>%
  mutate(Knn_Local_Settlement_I = as.numeric(Knn_Local_Settlement$Ii))%>%
  mutate(Knn_Local_Settlement_Iz =as.numeric(Knn_Local_Settlement$Z.Ii))%>%
  mutate(Knn_Local_Container_I = as.numeric(Knn_Local_Container$Ii))%>%
  mutate(Knn_Local_Container_Iz =as.numeric(Knn_Local_Container$Z.Ii))

gz_final_area_MG <- gz_final_area_MG %>%
  mutate(Knn_Local_centricity_I = as.numeric(Knn_Local_centricity$Ii))%>%
  mutate(Knn_Local_centricity_Iz =as.numeric(Knn_Local_centricity$Z.Ii))

summary(gz_final_area_MG$Knn_Local_Node_Iz)
#plot Local Morans I
data(gz_final_area_MG)
osm_gz_final_area_MG <- read_osm(gz_final_area_MG, ext=1.1)
MoranColours<- rev(brewer.pal(8, "RdBu"))#创建调色板
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)#手动设计break
plot_gz_Node_M <- tm_shape(osm_gz_final_area_MG) + tm_rgb()+tm_shape(gz_final_area_MG) +
  tm_polygons("Knn_Local_Node_Iz",
              #legend.hist=TRUE,
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,alpha = .8)+
  tm_layout(title = "Local Moran's I of Node index in Guangzhou", title.position = c(0.55,0.95),title.size=.9)+
  tm_scale_bar(position = c(0,0.02), text.size = .4)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .8,
            frame=FALSE,
            earth.boundary = TRUE)+
  tm_credits("Data source from SIMITRI project.Created by author.", position=c(0.0,0.0),size=.7)+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.05)) +
  tm_shape(gz_line)+tm_lines("line_name",lwd=1.5,lty='solid',legend.col.show = FALSE)+
  tm_shape(gz_final_station)+tm_symbols(size="Node_index", col='black',legend.size.show = FALSE,scale=.3) 
plot_gz_Node_M 
tmap_save(plot_gz_Node_M , 'Local_Moran_Node_index_in_Guangzhou.png',dpi = 300)
tmap_save(plot_gz_Node_M , 'Local_Moran_Node_index_in_Guangzhou_test.png',dpi = 300)

plot_gz_Place_M <- tm_shape(gz_final_area_MG) +
  tm_polygons("Knn_Local_Place_Iz",
              #legend.hist=TRUE,
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,alpha = .8)+
  tm_layout(title = "Local Moran's I of Place index in Guangzhou", title.position = c(0.55,0.95),title.size=.9)+
  tm_scale_bar(position = c(0,0.02), text.size = .4)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .8,
            frame=FALSE,
            earth.boundary = TRUE)+
  tm_credits("Data source from SIMITRI project.Created by author.", position=c(0.0,0.0),size=.7)+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.05)) +
  tm_shape(gz_line)+tm_lines("line_name",lwd=1.5,lty='solid',legend.col.show = FALSE)+
  tm_shape(gz_final_station)+tm_symbols(size="Place_inde", col='black',legend.size.show = FALSE,scale=.3) 
plot_gz_Place_M 
tmap_save(plot_gz_Place_M , 'Local_Moran_Place_index_in_Guangzhou.png',dpi = 300)

plot_gz_Settlement_M <- tm_shape(gz_final_area_MG) +
  tm_polygons("Knn_Local_Settlement_Iz",
              #legend.hist=TRUE,
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,alpha = .8)+
  tm_layout(title = "Local Moran's I of Settlement index in Guangzhou", title.position = c(0.55,0.95),title.size=.9)+
  tm_scale_bar(position = c(0,0.02), text.size = .4)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .8,
            frame=FALSE,
            earth.boundary = TRUE)+
  tm_credits("Data source from SIMITRI project.Created by author.", position=c(0.0,0.0),size=.7)+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.05)) +
  tm_shape(gz_line)+tm_lines("line_name",lwd=1.5,lty='solid',legend.col.show = FALSE)+
  tm_shape(gz_final_station)+tm_symbols(size="Settlement", col='black',legend.size.show = FALSE,scale=.3) 
plot_gz_Settlement_M 
tmap_save(plot_gz_Settlement_M , 'Local_Moran_Settlement_index_in_Guangzhou.png',dpi = 300)

plot_gz_Container_M <- tm_shape(gz_final_area_MG) +
  tm_polygons("Knn_Local_Container_Iz",
              #legend.hist=TRUE,
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,alpha = .8)+
  tm_layout(title = "Local Moran's I of Container index in Guangzhou", title.position = c(0.55,0.95),title.size=.9)+
  tm_scale_bar(position = c(0,0.02), text.size = .4)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .8,
            frame=FALSE,
            earth.boundary = TRUE)+
  tm_credits("Data source from SIMITRI project.Created by author.", position=c(0.0,0.0),size=.7)+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.05)) +
  tm_shape(gz_line)+tm_lines("line_name",lwd=1.5,lty='solid',legend.col.show = FALSE)+
  tm_shape(gz_final_station)+tm_symbols(size="Container_", col='black',legend.size.show = FALSE,scale=.3) 
plot_gz_Container_M 
tmap_save(plot_gz_Container_M , 'Local_Moran_Container_index_in_Guangzhou.png',dpi = 300)

plot_gz_centricity_M <- tm_shape(gz_final_area_MG) +
  tm_polygons("Knn_Local_centricity_Iz",
              #legend.hist=TRUE,
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,alpha = .8)+
  tm_layout(title = "Local Moran's I of Centricity index in Guangzhou", title.position = c(0.55,0.95),title.size=.9)+
  tm_scale_bar(position = c(0,0.02), text.size = .4)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .8,
            frame=FALSE,
            earth.boundary = TRUE)+
  tm_credits("Data source from SIMITRI project.Created by author.", position=c(0.0,0.0),size=.7)+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.05)) +
  tm_shape(gz_line)+tm_lines("line_name",lwd=1.5,lty='solid',legend.col.show = FALSE)+
  tm_shape(gz_final_station)+tm_symbols(size="centricity", col='black',legend.size.show = FALSE,scale=.3) 
plot_gz_centricity_M 
tmap_save(plot_gz_centricity_M , 'Local_Moran_centricity_index_in_Guangzhou.png',dpi = 300)

#plot Gerit
plot_gz_Node_G <- tm_shape(gz_final_area_MG) +
  tm_polygons("Gi_Local_Node",
              #legend.hist=TRUE,
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,alpha = .8)+
  tm_layout(title = "Getis-Ord Gi* of Node index in Guangzhou", title.position = c(0.55,0.95),title.size=.9)+
  tm_scale_bar(position = c(0,0.02), text.size = .4)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .8,
            frame=FALSE,
            earth.boundary = TRUE)+
  tm_credits("Data source from SIMITRI project.Created by author.", position=c(0.0,0.0),size=.7)+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.05)) +
  tm_shape(gz_line)+tm_lines("line_name",lwd=1.5,lty='solid',legend.col.show = FALSE)+
  tm_shape(gz_final_station)+tm_symbols(size="Node_index", col='black',legend.size.show = FALSE,scale=.3) 
plot_gz_Node_G 
tmap_save(plot_gz_Node_G , 'GetisOrd_Node_index_in_Guangzhou.png',dpi = 300)


#GWR
#检验分布
ggplot(gz_final_area, aes(x=centricity)) +
  geom_histogram()##

ggplot(gz_final_area, aes(x=Node_index)) +
  geom_histogram()

ggplot(gz_final_area, aes(x=Place_inde)) +
  geom_histogram()

ggplot(gz_final_area, aes(x=Settlement)) +
  geom_histogram()

ggplot(gz_final_area, aes(x=Container_)) +
  geom_histogram()

symbox(~Place_inde, 
       gz_final_area, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

symbox(~Settlement, 
       gz_final_area, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

symbox(~Container_, 
       gz_final_area, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

###GWR
st_crs(gz_final_area) = 32650
gz_final_areaSP <- gz_final_area %>%
  as(., "Spatial")

st_crs(coordsW) = 32650
coordsWSP <- coordsW %>%
  as(., "Spatial")

GWRbandwidth <- gwr.sel(centricity ~
                          Node_index+Place_inde+
                          Settlement+Container_, 
                        data = gz_final_areaSP, 
                        coords=coordsWSP,
                        adapt=T)

gwr.model = gwr(centricity ~
                  Node_index+Place_inde+
                  Settlement+Container_, 
                data = gz_final_areaSP,
                coords=coordsWSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

gwr.model

results <- as.data.frame(gwr.model$SDF)
names(results)

gz_final_area_gwr <- gz_final_area %>%
  mutate(coef_N = results$Node_index,
         coef_P = results$Place_inde,
         coef_S = results$Settlement,
         coef_C = results$Container_)

tmaptools::palette_explorer()

plot_N=tm_shape(gz_final_area_gwr) +
  tm_polygons(col = "coef_N", 
              palette = "OrRd", 
              alpha = 0.8)
plot_N
tmap_save(plot_N , 'GWR_coef_Node_index_in_Guangzhou.png',dpi = 300)

plot_P=tm_shape(gz_final_area_gwr) +
  tm_polygons(col = "coef_P", 
              palette = "OrRd", 
              alpha = 0.8)
plot_P
tmap_save(plot_P , 'GWR_coef_Place_index_in_Guangzhou.png',dpi = 300)

plot_S=tm_shape(gz_final_area_gwr) +
  tm_polygons(col = "coef_S", 
              palette = "OrRd", 
              alpha = 0.8)
plot_S
tmap_save(plot_S , 'GWR_coef_Settlement_index_in_Guangzhou.png',dpi = 300)

plot_C=tm_shape(gz_final_area_gwr) +
  tm_polygons(col = "coef_C", 
              palette = "OrRd", 
              alpha = 0.8)
plot_C
tmap_save(plot_C , 'GWR_coef_Container_index_in_Guangzhou.png',dpi = 300)