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

gz_final_station_MG <- gz_final_station_MG %>%
  mutate(Knn_Local_centricity_I = as.numeric(Knn_Local_centricity$Ii))%>%
  mutate(Knn_Local_centricity_Iz =as.numeric(Knn_Local_centricity$Z.Ii))

summary(gz_final_station_MG$Knn_Local_Node_Iz)

########
quadrant <- vector(mode="numeric",length=nrow(Knn_Local_Node$Ii))

# centers the variable of interest around its mean
m.qualification <- gz_final_station_MG$Node_index - mean(gz_final_station_MG$Node_index)     

# centers the local Moran's around the mean
m.local <- gz_final_station_MG$Knn_Local_Node_I - mean(gz_final_station_MG$Knn_Local_Node_I)    

# significance threshold
signif <- 0.05 
local <- Knn_Local_Node[,c('Pr(z > 0)')]
local
# builds a data quadrant
gz_final_station_MG$Cluster_type[m.qualification >0 & m.local>0] <- 'H-H' 
gz_final_station_MG$Cluster_type[m.qualification <0 & m.local<0] <- 'L-L'    
gz_final_station_MG$Cluster_type[m.qualification <0 & m.local>0] <- 'L-H'
gz_final_station_MG$Cluster_type[m.qualification >0 & m.local<0] <- 'H-L'
gz_final_station_MG$Cluster_type[local>signif] <- 'Not significant'

#plot Local Morans I
data(gz_final_station_MG)
osm_gz_final_station_MG <- read_osm(gz_final_stationa_MG, ext=1.1)
MoranColours<- rev(brewer.pal(8, "RdBu"))#创建调色板
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)#手动设计break
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
# make some bbox magic
bbox_new <- st_bbox(gz_final_station_MG) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
#bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon


plot_gz_Node_M <- tm_shape(gz_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(gz_final_station_MG,bbox = bbox_new) +
  tm_bubbles(size="Node_index",scale=.9,col="Cluster_type",
              #legend.hist=TRUE,
              style="fixed",
              palette=MoranColours,
              midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(title = "Local Moran's I of Node index", title.position = c(.65,.95),title.size=.9)+
  tm_scale_bar(position = c(0,0.02), text.size = .4)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)+
  tm_credits("Data source from SIMITRI project.Created by author.", position=c(0.0,0.0),size=.7)+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.05), size=4)
plot_gz_Node_M

plot_gz_Place_M <- tm_shape(gz_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(gz_final_station_MG,bbox = bbox_new) +
  tm_bubbles(size="Place_inde",scale=.9,col="Knn_Local_Place_Iz",
             #legend.hist=TRUE,
             style="fixed",
             breaks=breaks1,
             palette=MoranColours,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(title = "Local Moran's I of Place index", title.position = c(.65,.95),title.size=.9)+
  tm_scale_bar(position = c(0,0.02), text.size = .4)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)+
  tm_credits("Data source from SIMITRI project.Created by author.", position=c(0.0,0.0),size=.7)+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.05), size=4)
plot_gz_Place_M

plot_gz_Settlement_M <- tm_shape(gz_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(gz_final_station_MG,bbox = bbox_new) +
  tm_bubbles(size="Settlement",scale=.9,col="Knn_Local_Settlement_Iz",
             #legend.hist=TRUE,
             style="fixed",
             breaks=breaks1,
             palette=MoranColours,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(title = "Local Moran's I of Settlement index", title.position = c(.55,.95),title.size=.9)+
  tm_scale_bar(position = c(0,0.02), text.size = .4)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)+
  tm_credits("Data source from SIMITRI project.Created by author.", position=c(0.0,0.0),size=.7)+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.05), size=4)
plot_gz_Settlement_M

plot_gz_Container_M <- tm_shape(gz_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(gz_final_station_MG,bbox = bbox_new) +
  tm_bubbles(size="Container_",scale=.9,col="Knn_Local_Container_Iz",
             #legend.hist=TRUE,
             style="fixed",
             breaks=breaks1,
             palette=MoranColours,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(title = "Local Moran's I of Container index", title.position = c(.55,.95),title.size=.9)+
  tm_scale_bar(position = c(0,0.02), text.size = .4)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)+
  tm_credits("Data source from SIMITRI project.Created by author.", position=c(0.0,0.0),size=.7)+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.05), size=4)
plot_gz_Container_M
tmap_save(plot_gz_Node_M , 'GZ_Node.png',dpi = 300)
tmap_save(plot_gz_Place_M , 'GZ_Place.png',dpi = 300)
tmap_save(plot_gz_Settlement_M , 'GZ_Settlement.png',dpi = 300)
tmap_save(plot_gz_Container_M , 'GZ_Container.png',dpi = 300)

gz_final_station_MG$centricity_abs <- abs(gz_final_station_MG$centricity)
plot_gz_centricity_M <- tm_shape(gz_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(gz_final_station_MG,bbox = bbox_new) +
  tm_bubbles(size='centricity_abs',scale=2,col="Knn_Local_centricity_Iz",
             #legend.hist=TRUE,
             style="fixed",
             breaks=breaks1,
             palette=MoranColours,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(title = "Local Moran's I of Mobility index", title.position = c(.55,.95),title.size=.9)+
  tm_scale_bar(position = c(0,0.02), text.size = .4)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)+
  tm_credits("Data source from SIMITRI project.Created by author.", position=c(0.0,0.0),size=.7)+
  #tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
  tm_compass(type = "8star", position = c(0.02, 0.05), size=4)
plot_gz_centricity_M
tmap_save(plot_gz_centricity_M , 'GZ_centricity.png',dpi = 300)

