###GWR
coordsW <- sz_final_station%>%
  st_centroid()%>%
  st_geometry()

st_crs(sz_final_station) = 4326
sz_final_stationSP <- sz_final_station %>%
  as(., "Spatial")

st_crs(coordsW) = 4326
coordsWSP <- coordsW %>%
  as(., "Spatial")

GWRbandwidth <- gwr.sel(centricity_abs ~
                          Node_index+Place_inde+
                          Settlement+Container_, 
                        data = gz_final_stationSP, 
                        coords=coordsWSP,
                        adapt=T)

gwr.model = gwr(centricity_abs ~
                  Node_index+Place_inde+
                  Settlement+Container_, 
                data = sz_final_stationSP,
                coords=coordsWSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

gwr.model

results <- as.data.frame(gwr.model$SDF)
names(results)

sz_final_station_gwr <- sz_final_station %>%
  mutate(coef_N = results$Node_index,
         coef_P = results$Place_inde,
         coef_S = results$Settlement,
         coef_C = results$Container_)

#N
sigTest = abs(gwr.model$SDF$"Node_index")-2 * gwr.model$SDF$"Node_index_se"

sz_final_station_gwr <- sz_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:230){
  if (sz_final_station_gwr$GWR_Sig[i]>0) {
    sz_final_station_gwr$coef_Node[i]=sz_final_station_gwr$coef_N[i]
  } else {sz_final_station_gwr$coef_Node[i]= 10
  }
}

brks <- c(-1,0,1,11)
MyPalette <- c("#01579B", "#B71C1C","#FAFAFA" )
Labels <- c('Negative correlation','Positive correlation','Not significant')
plot_N <- tm_shape(sz_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(sz_final_station_gwr) +
  tm_bubbles(size="Node_index",scale=.9,col="coef_Node",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  #tm_layout(title = "Local Moran's I of Node index", title.position = c(.65,.95),title.size=.9)+
  #tm_scale_bar(position = c(0,0.02), text.size = .4)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)
#tm_credits("Data source from SIMITRI project.Created by author.", position=c(0.0,0.0),size=.7)+
#tm_text(text = "NAME", size = .5, along.lines =T, remove.overlap=T,  auto.placement=F)+
#tm_compass(type = "8star", position = c(0.02, 0.05), size=4)
plot_N
tmap_save(plot_N , 'sz_Node.png',dpi = 300)

#P
sigTest = abs(gwr.model$SDF$"Place_inde")-2 * gwr.model$SDF$"Place_inde_se"

sz_final_station_gwr <- sz_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:230){
  if (sz_final_station_gwr$GWR_Sig[i]>0) {
    sz_final_station_gwr$coef_Place[i]=sz_final_station_gwr$coef_P[i]
  } else {sz_final_station_gwr$coef_Place[i]= 10
  }
}

plot_P <- tm_shape(sz_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(sz_final_station_gwr,bbox = bbox_new) +
  tm_bubbles(size="Place_inde",scale=.9,col="coef_Place",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)
plot_P
tmap_save(plot_P , 'sz_Place.png',dpi = 300)

#S
sigTest = abs(gwr.model$SDF$"Settlement")-2 * gwr.model$SDF$"Settlement_se"

sz_final_station_gwr <- sz_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:230){
  if (sz_final_station_gwr$GWR_Sig[i]>0) {
    sz_final_station_gwr$coef_Settlement[i]=sz_final_station_gwr$coef_S[i]
  } else {sz_final_station_gwr$coef_Settlement[i]= 10
  }
}

plot_S <- tm_shape(sz_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(sz_final_station_gwr) +
  tm_bubbles(size="Settlement",scale=.9,col="coef_Settlement",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)

plot_S
tmap_save(plot_S , 'sz_Settlement.png',dpi = 300)

#C
sigTest = abs(gwr.model$SDF$"Container_")-2 * gwr.model$SDF$"Container_se"

sz_final_station_gwr <- sz_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:230){
  if (sz_final_station_gwr$GWR_Sig[i]>0) {
    sz_final_station_gwr$coef_Container[i]=sz_final_station_gwr$coef_C[i]
  } else {sz_final_station_gwr$coef_Container[i]= 10
  }
}

plot_C <- tm_shape(sz_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(sz_final_station_gwr,bbox = bbox_new) +
  tm_bubbles(size="Container_",scale=.9,col="coef_Container",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)

plot_C
tmap_save(plot_C , 'sz_Container.png',dpi = 300)

###GWR
coordsW <- fs_final_station%>%
  st_centroid()%>%
  st_geometry()

st_crs(fs_final_station) = 4326
fs_final_stationSP <- fs_final_station %>%
  as(., "Spatial")

st_crs(coordsW) = 4326
coordsWSP <- coordsW %>%
  as(., "Spatial")

GWRbandwidth <- gwr.sel(centricity_abs ~
                          Node_index+Place_inde+
                          Settlement+Container_, 
                        data = fs_final_stationSP, 
                        coords=coordsWSP,
                        adapt=T)

gwr.model = gwr(centricity_abs ~
                  Node_index+Place_inde+
                  Settlement+Container_, 
                data = fs_final_stationSP,
                coords=coordsWSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

gwr.model

results <- as.data.frame(gwr.model$SDF)
names(results)

fs_final_station_gwr <- fs_final_station %>%
  mutate(coef_N = results$Node_index,
         coef_P = results$Place_inde,
         coef_S = results$Settlement,
         coef_C = results$Container_)

#N
sigTest = abs(gwr.model$SDF$"Node_index")-2 * gwr.model$SDF$"Node_index_se"

fs_final_station_gwr <- fs_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:200){
  if (fs_final_station_gwr$GWR_Sig[i]>0) {
    fs_final_station_gwr$coef_Node[i]=fs_final_station_gwr$coef_N[i]
  } else {fs_final_station_gwr$coef_Node[i]= 10
  }
}

brks <- c(-1,0,1,11)
MyPalette <- c("#01579B", "#B71C1C","#FAFAFA" )
Labels <- c('Negative correlation','Positive correlation','Not significant')
plot_N <- tm_shape(fs_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(fs_final_station_gwr) +
  tm_bubbles(size="Node_index",scale=.9,col="coef_Node",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)
plot_N
tmap_save(plot_N , 'fs_Node.png',dpi = 300)

#P
sigTest = abs(gwr.model$SDF$"Place_inde")-2 * gwr.model$SDF$"Place_inde_se"

fs_final_station_gwr <- fs_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:200){
  if (fs_final_station_gwr$GWR_Sig[i]>0) {
    fs_final_station_gwr$coef_Place[i]=fs_final_station_gwr$coef_P[i]
  } else {fs_final_station_gwr$coef_Place[i]= 10
  }
}

plot_P <- tm_shape(fs_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(fs_final_station_gwr,bbox = bbox_new) +
  tm_bubbles(size="Place_inde",scale=.9,col="coef_Place",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)
plot_P
tmap_save(plot_P , 'fs_Place.png',dpi = 300)

#S
sigTest = abs(gwr.model$SDF$"Settlement")-2 * gwr.model$SDF$"Settlement_se"

fs_final_station_gwr <- fs_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:200){
  if (fs_final_station_gwr$GWR_Sig[i]>0) {
    fs_final_station_gwr$coef_Settlement[i]=fs_final_station_gwr$coef_S[i]
  } else {fs_final_station_gwr$coef_Settlement[i]= 10
  }
}

plot_S <- tm_shape(fs_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(fs_final_station_gwr) +
  tm_bubbles(size="Settlement",scale=.9,col="coef_Settlement",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)

plot_S
tmap_save(plot_S , 'fs_Settlement.png',dpi = 300)

#C
sigTest = abs(gwr.model$SDF$"Container_")-2 * gwr.model$SDF$"Container_se"

fs_final_station_gwr <- fs_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:200){
  if (fs_final_station_gwr$GWR_Sig[i]>0) {
    fs_final_station_gwr$coef_Container[i]=fs_final_station_gwr$coef_C[i]
  } else {fs_final_station_gwr$coef_Container[i]= 10
  }
}

plot_C <- tm_shape(fs_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(fs_final_station_gwr,bbox = bbox_new) +
  tm_bubbles(size="Container_",scale=.9,col="coef_Container",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)

plot_C
tmap_save(plot_C , 'fs_Container.png',dpi = 300)

###GWR
coordsW <- dg_final_station%>%
  st_centroid()%>%
  st_geometry()

st_crs(dg_final_station) = 4326
dg_final_stationSP <- dg_final_station %>%
  as(., "Spatial")

st_crs(coordsW) = 4326
coordsWSP <- coordsW %>%
  as(., "Spatial")

GWRbandwidth <- gwr.sel(centricity_abs ~
                          Node_index+Place_inde+
                          Settlement+Container_, 
                        data = dg_final_stationSP, 
                        coords=coordsWSP,
                        adapt=T)

gwr.model = gwr(centricity_abs ~
                  Node_index+Place_inde+
                  Settlement+Container_, 
                data = dg_final_stationSP,
                coords=coordsWSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

gwr.model

results <- as.data.frame(gwr.model$SDF)
names(results)

dg_final_station_gwr <- dg_final_station %>%
  mutate(coef_N = results$Node_index,
         coef_P = results$Place_inde,
         coef_S = results$Settlement,
         coef_C = results$Container_)

#N
sigTest = abs(gwr.model$SDF$"Node_index")-2 * gwr.model$SDF$"Node_index_se"

dg_final_station_gwr <- dg_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:200){
  if (dg_final_station_gwr$GWR_Sig[i]>0) {
    dg_final_station_gwr$coef_Node[i]=dg_final_station_gwr$coef_N[i]
  } else {dg_final_station_gwr$coef_Node[i]= 10
  }
}

brks <- c(-1,0,1,11)
MyPalette <- c("#01579B", "#B71C1C","#FAFAFA" )
Labels <- c('Negative correlation','Positive correlation','Not significant')
plot_N <- tm_shape(dg_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(dg_final_station_gwr) +
  tm_bubbles(size="Node_index",scale=.9,col="coef_Node",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)
plot_N
tmap_save(plot_N , 'dg_Node.png',dpi = 300)

#P
sigTest = abs(gwr.model$SDF$"Place_inde")-2 * gwr.model$SDF$"Place_inde_se"

dg_final_station_gwr <- dg_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:200){
  if (dg_final_station_gwr$GWR_Sig[i]>0) {
    dg_final_station_gwr$coef_Place[i]=dg_final_station_gwr$coef_P[i]
  } else {dg_final_station_gwr$coef_Place[i]= 10
  }
}

plot_P <- tm_shape(dg_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(dg_final_station_gwr,bbox = bbox_new) +
  tm_bubbles(size="Place_inde",scale=.9,col="coef_Place",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)
plot_P
tmap_save(plot_P , 'dg_Place.png',dpi = 300)

#S
sigTest = abs(gwr.model$SDF$"Settlement")-2 * gwr.model$SDF$"Settlement_se"

dg_final_station_gwr <- dg_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:200){
  if (dg_final_station_gwr$GWR_Sig[i]>0) {
    dg_final_station_gwr$coef_Settlement[i]=dg_final_station_gwr$coef_S[i]
  } else {dg_final_station_gwr$coef_Settlement[i]= 10
  }
}

plot_S <- tm_shape(dg_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(dg_final_station_gwr) +
  tm_bubbles(size="Settlement",scale=.9,col="coef_Settlement",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)

plot_S
tmap_save(plot_S , 'dg_Settlement.png',dpi = 300)

#C
sigTest = abs(gwr.model$SDF$"Container_")-2 * gwr.model$SDF$"Container_se"

dg_final_station_gwr <- dg_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:200){
  if (dg_final_station_gwr$GWR_Sig[i]>0) {
    dg_final_station_gwr$coef_Container[i]=dg_final_station_gwr$coef_C[i]
  } else {dg_final_station_gwr$coef_Container[i]= 10
  }
}

plot_C <- tm_shape(dg_line)+
  tm_lines("line_name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(dg_final_station_gwr,bbox = bbox_new) +
  tm_bubbles(size="Container_",scale=.9,col="coef_Container",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)

plot_C
tmap_save(plot_C , 'dg_Container.png',dpi = 300)

###GWR
coordsW <- gba_final_station%>%
  st_centroid()%>%
  st_geometry()

st_crs(gba_final_station) = 4326
gba_final_stationSP <- gba_final_station %>%
  as(., "Spatial")

st_crs(coordsW) = 4326
coordsWSP <- coordsW %>%
  as(., "Spatial")

GWRbandwidth <- gwr.sel(centricity_abs ~
                          Node_index+Place_inde, 
                        data = gba_final_stationSP, 
                        coords=coordsWSP,
                        adapt=T)

gwr.model = gwr(centricity_abs ~
                  Node_index+Place_inde, 
                data = gba_final_stationSP,
                coords=coordsWSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

gwr.model

results <- as.data.frame(gwr.model$SDF)
names(results)

gba_final_station_gwr <- gba_final_station %>%
  mutate(coef_N = results$Node_index,
         coef_P = results$Place_inde)

#N
sigTest = abs(gwr.model$SDF$"Node_index")-2 * gwr.model$SDF$"Node_index_se"

gba_final_station_gwr <- gba_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:200){
  if (gba_final_station_gwr$GWR_Sig[i]>0) {
    gba_final_station_gwr$coef_Node[i]=gba_final_station_gwr$coef_N[i]
  } else {gba_final_station_gwr$coef_Node[i]= 10
  }
}

brks <- c(-1,0,1,11)
MyPalette <- c("#01579B", "#B71C1C","#FAFAFA" )
Labels <- c('Negative correlation','Positive correlation','Not significant')
plot_N <- tm_shape(gba)+tm_borders(lwd = 0.5) +tm_text(text="NAME_2",size=.8)+tm_shape(gba_line)+
  tm_lines("name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(gba_final_station_gwr) +
  tm_bubbles(size="Node_index",scale=.9,col="coef_Node",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)
plot_N
tmap_save(plot_N , 'gba_Node.png',dpi = 300)

#P
sigTest = abs(gwr.model$SDF$"Place_inde")-2 * gwr.model$SDF$"Place_inde_se"

gba_final_station_gwr <- gba_final_station_gwr %>%
  mutate(GWR_Sig = sigTest)

for (i in 1:200){
  if (gba_final_station_gwr$GWR_Sig[i]>0) {
    gba_final_station_gwr$coef_Place[i]=gba_final_station_gwr$coef_P[i]
  } else {gba_final_station_gwr$coef_Place[i]= 10
  }
}

plot_P <- tm_shape(gba)+tm_borders(lwd = 0.5) +tm_text(text="NAME_2",size=.8)+tm_shape(gba_line)+
  tm_lines("name",lwd=2,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(gba_final_station_gwr,bbox = bbox_new) +
  tm_bubbles(size="Place_inde",scale=.9,col="coef_Place",
             #legend.hist=TRUE,
             style="fixed",
             breaks=brks,
             palette=MyPalette,labels = Labels,
             midpoint=NA,alpha = .8,zindex=1)+
  tm_layout(legend.position = c(0.75,0), 
            legend.text.size=.75, 
            legend.title.size = .85,
            frame=FALSE,
            earth.boundary = TRUE)
plot_P
tmap_save(plot_P , 'gba_Place.png',dpi = 300)

major <- filter(gba, NAME_2 == "Guangzhou" |NAME_2 == "Shenzhen" |NAME_2 == "Foshan" |NAME_2 == "Dongguan" )
all <- filter(gba, NAME_2 == "Guangzhou" |NAME_2 == "Shenzhen" |NAME_2 == "Foshan" |NAME_2 == "Dongguan"
              |NAME_2 == "Zhaoqing" |NAME_2 == "Jiangmen" |NAME_2 == "Zhongshan" |NAME_2 == "Zhuhai" |NAME_2 == "Huizhou" )

plot <- tm_shape(gba)+tm_polygons()+tm_shape(all)+tm_polygons(col='lightblue',alpha=.2)+tm_shape(major)+tm_polygons(col='red',alpha=.2)+
  tm_shape(gba_line)+tm_lines(col='orange',lwd=1.2,lty='solid',legend.col.show = FALSE,zindex=2)+tm_shape(gba_final_station_gwr) +
  tm_bubbles(col='white',scale=.5,border.lwd = 1.5)+tm_shape(gba)+tm_borders(lwd = 0.5) +tm_text(text="NAME_2",size=.8)
plot
tmap_save(plot , 'study_area.png',dpi = 300)

dg <- tm_shape(dg_line)+
  tm_lines(col='orange',lwd=1,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(dg_final_station_gwr) +
  tm_bubbles(col = 'white',scale=.7,border.lwd = 2)
dg
tmap_save(dg , 'study_area_dg.png',dpi = 300)

fs <- tm_shape(fs_line)+
  tm_lines(col='orange',lwd=1,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(fs_final_station_gwr) +
  tm_bubbles(col = 'white',scale=.7,border.lwd = 2)
fs
tmap_save(fs , 'study_area_fs.png',dpi = 300)

gz <- tm_shape(gz_line)+
  tm_lines(col='orange',lwd=1,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(gz_final_station_gwr) +
  tm_bubbles(col = 'white',scale=.5,border.lwd = 2)
gz
tmap_save(gz , 'study_area_gz.png',dpi = 300)

sz <- tm_shape(sz_line)+
  tm_lines(col='orange',lwd=1,lty='solid',legend.col.show = FALSE,zindex=2)+
  tm_shape(sz_final_station_gwr) +
  tm_bubbles(col = 'white',scale=.5,border.lwd = 2)
sz
tmap_save(sz , 'study_area_sz.png',dpi = 300)