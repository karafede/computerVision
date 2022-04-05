

library(readr)
library(sp)
library(raster)
library(gstat)
library(rgdal)
library(RNetCDF)
library(ncdf4)
library(stringr)
library(htmlwidgets)
library(webshot)
library(leaflet)
library(rgeos)
library(ggvoronoi)
library(dplyr)
library(deldir)
library(gridGraphics)
library(sf)
library(sp)
library(raster)
library(gstat)
library(lubridate)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes) # theme_map()
library(inlmisc)
library(tidyr)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)

rm(list = ls())



setwd("D:/Federico/CityFLows/output_files/")
folder_trajectories <- "D:/Federico/CityFLows/objectdetection/output/speed_plots_roma3/"

## set forward motion speed
speed_FWD_motion <- 0.2  ## units [m/s]
# vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_camera_93_IDs_lato_lungo.csv")
# vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_camera_94_IDs_lato_corto_monodirectional.csv")
# vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_camera_94_IDs_lato_corto_bidirectional.csv")
# vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_camera_94_IDs_lato_corto_monodirectional_corr_stretto.csv")
# vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_camera_94_IDs_lato_corto_bidirectional_corr_stretto.csv")
vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_camera_94_IDs_lato_corto_bidirectional_corr_largo.csv")


## remove rows with NAs
vec_all <- vec_all[complete.cases(vec_all), ] 
vec_all <- vec_all %>% 
  filter_all(all_vars(!is.infinite(.)))




####--setup customized borders to make plot of the same size --####
xmin = min(vec_all$X)
ymin = min(vec_all$Y)
xmax = max(vec_all$X)
ymax = max(vec_all$Y)


box = c(xmin = xmin, ymin= ymin, xmax= xmax, ymax= ymax)
piazza = st_sfc(st_polygon(list(rbind(c(xmin,ymin), c(xmax,ymin), c(xmax,ymax),
                                      c(xmin, ymax), c(xmin,ymin)))))

piazza <- piazza %>% 
  st_set_crs(st_crs(32632))
plot(piazza)

vec_all <- vec_all %>%
  filter(speed >= speed_FWD_motion & speed <= 2.5)

vec_all <- vec_all %>%
  mutate(timedate = ymd_hms(timedate, tz = "UTC"))


vec_all <- as.vector(vec_all)
vec_all$id <- 1:nrow(vec_all)

coordinates(vec_all) <- c("X", "Y")
df <- as.data.frame(vec_all)

#######################################################################################
#######################################################################################
###### SPEED ##########################################################################

#Line parameters
line.length <- 2 #length of polylines representing wind in the map (meters)
arrow.length <- 0.7 #length of arrowhead leg (meters)
arrow.angle <- 120 #angle of arrowhead leg (degrees azimuth)

#Generate data frame with auxiliary coordinates
end.xy.df <- data.frame(end.x=NA,end.y=NA,end.arrow.x=NA,end.arrow.y=NA)


for (i in c(1:nrow(df))){
  
  #coordinates of end points for wind lines (the initial points are the ones where data was observed)
  if (df$angle_deg[i] <= 90) {
    end.x <- df$X[i] + (cos((90 - df$angle_deg[i]) * 0.0174532925) * 2*df$speed[i])      #### df$speed == line.length
  } else if (df$angle_deg[i] > 90 & df$angle_deg[i] <= 180) {
    end.x <- df$X[i] + (cos((df$angle_deg[i] - 90) * 0.0174532925) * 2*df$speed[i])
  } else if (df$angle_deg[i] > 180 & df$angle_deg[i] <= 270) {
    end.x <- df$X[i] - (cos((270 - df$angle_deg[i]) * 0.0174532925) * 2*df$speed[i])
  } else {end.x <- df$X[i] - (cos((df$angle_deg[i] - 270) * 0.0174532925) * 2*df$speed[i])}
  
  if (df$angle_deg[i] <= 90) {
    end.y <- df$Y[i] + (sin((90 - df$angle_deg[i]) * 0.0174532925) * 2*df$speed[i])
  } else if (df$angle_deg[i] > 90 & df$angle_deg[i] <= 180) {
    end.y <- df$Y[i] - (sin((df$angle_deg[i] - 90) * 0.0174532925) * 2*df$speed[i])
  } else if (df$angle_deg[i] > 180 & df$angle_deg[i] <= 270) {
    end.y <- df$Y[i] - (sin((270 - df$angle_deg[i]) * 0.0174532925) * 2*df$speed[i])
  } else {end.y <- df$Y[i] + (sin((df$angle_deg[i] - 270) * 0.0174532925) * 2*df$speed[i])}
  
  #coordinates of end points for arrowhead leg lines (the initial points are the previous end points)
  end.arrow.x <- end.x + (cos((df$angle_deg[i] + arrow.angle) * 0.0174532925) * arrow.length)
  end.arrow.y <- end.y - (sin((df$angle_deg[i] + arrow.angle) * 0.0174532925) * arrow.length)
  
  end.xy.df <- rbind(end.xy.df,c(end.x,end.y,end.arrow.x,end.arrow.y)) 
}

end.xy <- end.xy.df[-1,]
df <- data.frame(df,end.xy) #df with observed and auxiliary variables
# head(df,3)

#------------------------------
#Step 3 - Create an object of class `SpatialLinesDataFrame` 

lines <- data.frame(cbind(lng=c(df$X, df$end.x, df$end.arrow.x),
                          lat=c(df$Y, df$end.y, df$end.arrow.y),
                          id=c(rep(df$id,3))))

lines.list <- list()
library(sp)

for (i in c(1:max(lines$id))){
  line <- subset(lines,lines$id==i)
  line <- as.matrix(line[,c(1:2)])
  line <- Line(line) #object of class 'Line'
  lines.list[[i]] <- Lines(list(line), ID = i) #list of 'objects'Lines' 
}

sp.lines <- SpatialLines(lines.list) #object of class 'SpatialLines'
proj4string(sp.lines) <- CRS("+init=epsg:32632") #define CRS
sp.lines <- spTransform(sp.lines, CRS("+init=epsg:32632"))


rownames(df) = df$id
## Join SPEED variables (id, speed, direction and date) to object of class 'SpatialLines'
sp.lines.df <- SpatialLinesDataFrame(sp.lines, df[,c(1,6:7,2)]) #object of class 'SpatialLinesDataFrame'
# plot(sp.lines.df)

#### ---- Build GRID of 2x2 square meters....
extent = st_sfc(st_polygon(list(rbind(c(xmin,ymin), c(xmax,ymin), c(xmax,ymax),
                                      c(xmin, ymax), c(xmin,ymin)))))

## set CRS
extent <- extent %>% 
  st_set_crs(st_crs(sp.lines.df))

## define a regular grid of 2 meter resolution and add an Id to each cell
GRID <- st_make_grid(extent, cellsize = 0.7, square = T) %>%    # 0.7 good for camera_93 and camera_94
  st_as_sf() %>%
  mutate(cell_ID = row_number())


########################################################################################################
######### ------- plot AVERAGE SPEEDs and directions per CELL ------------- ############################

###################################
#### MORNING & EVENING HOURS ######
###################################


df <- df %>%
  mutate(hour = hour(timedate),
         minute = minute(timedate),
         second = second(timedate))

df_time_ALL <- df

##### show distribution of angles
p <- ggplot(df_time_ALL, aes(angle_deg, fill = "blue")) +
     geom_histogram() +  
     guides(fill=FALSE) +
     theme_bw() +
    theme( strip.text = element_text(size =20)) +
    theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
    theme(axis.text.x=element_text(size=20,face="bold", colour = "black")) +
    theme(axis.title.x = element_blank()) +                  # Remove x-axis label
    ylab("counts") +            # Set y-axis label
    xlab(expression(paste("angle (degrees)"))) +
    theme(axis.title.y = element_text(face="bold", colour="black", size=20),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
    theme(axis.title.x = element_text(face="bold", colour="black", size=20),
          axis.text.x  = element_text(angle=0, vjust=0.5, size=20, hjust = 0.5)) + 
    theme(axis.title.y = element_text(face="bold", colour="black", size=20),
          axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
    # ggtitle("distribution of directions bidirectional") + 
    # ggtitle("distribution of directions monodirectional") + 
    # ggtitle("distribution of directions monodirectional narrow corridor") + 
   # ggtitle("distribution of directions bidirectional narrow corridor") +
  ggtitle("distribution of directions bidirectional large corridor") +
    theme(plot.title = element_text(lineheight=.8, face="bold", size=22))
p

dpi = 96
# ggsave(filename=paste0(folder_trajectories,"directions_camera_94_lato_corto_monodirectional.png"),
#        width = 700 / dpi, height = 650 / dpi,
#        dpi = dpi)
# ggsave(filename=paste0(folder_trajectories,"directions_camera_94_lato_corto_bidirectional.png"),
#        width = 700 / dpi, height = 650 / dpi,
#        dpi = dpi)
# ggsave(filename=paste0(folder_trajectories,"directions_camera_94_lato_corto_corr_stretto_monodirectional.png"),
#        width = 700 / dpi, height = 650 / dpi,
#        dpi = dpi)
# ggsave(filename=paste0(folder_trajectories,"directions_camera_94_lato_corto_corr_stretto_bidirectional.png"),
#        width = 700 / dpi, height = 650 / dpi,
#        dpi = dpi)
ggsave(filename=paste0(folder_trajectories,"directions_camera_94_lato_corto_corr_largo_bidirectional.png"),
       width = 700 / dpi, height = 650 / dpi,
       dpi = dpi)



##### show distribution of angles
p <- ggplot(df_time_ALL, aes(speed, fill = "blue")) +
  geom_histogram() +  
  guides(fill=FALSE) +
  theme_bw() +
  theme( strip.text = element_text(size =20)) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=20,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                  # Remove x-axis label
  ylab("counts") +            # Set y-axis label
  xlab(expression(paste("speed(m/s)"))) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=20),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=20, hjust = 0.5)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  # ggtitle("distribution of speeds bidirectional") + 
  # ggtitle("distribution of speeds mondirectional") + 
  # ggtitle("distribution of speeds mondirectional narrow corridor") + 
  # ggtitle("distribution of speeds bidirectional narrow corridor") + 
  ggtitle("distribution of speeds bidirectional large corridor") + 
  theme(plot.title = element_text(lineheight=.8, face="bold", size=22))
p

dpi = 96
# ggsave(filename=paste0(folder_trajectories,"speeds_camera_94_lato_corto_monodirectional.png"),
#        width = 700 / dpi, height = 650 / dpi,
#        dpi = dpi)
# ggsave(filename=paste0(folder_trajectories,"speeds_camera_94_lato_corto_bidirectional.png"),
#        width = 700 / dpi, height = 650 / dpi,
#        dpi = dpi)
# ggsave(filename=paste0(folder_trajectories,"speeds_camera_94_lato_corto_corr_stretto_monodirectional.png"),
#        width = 700 / dpi, height = 650 / dpi,
#        dpi = dpi)
# ggsave(filename=paste0(folder_trajectories,"speeds_camera_94_lato_corto_corr_stretto_bidirectional.png"),
#        width = 700 / dpi, height = 650 / dpi,
#        dpi = dpi)
ggsave(filename=paste0(folder_trajectories,"speeds_camera_94_lato_corto_corr_largo_bidirectional.png"),
       width = 700 / dpi, height = 650 / dpi,
       dpi = dpi)




### loop every 1 minute -----------------------------------------
####-------------------------------------------------------------
### ----->>>>>> ###### -------------------------------------------------------------------------------------------
### ----->>>>>> ###### -------------------------------------------------------------------------------------------
df_time_ALL <- df_time_ALL %>%
  dplyr::mutate(time = format(ymd_hms(as.character(df_time_ALL$timedate, tz = "CET")),'%H:%M:%S'))
## order by time
df_time_ALL <- df_time_ALL[order(df_time_ALL$time),]
# df_time_ALL <- df_time_ALL[order(df_time_ALL$time, df_time_ALL$X, df_time_ALL$Y),]
df_time_ALL <- df_time_ALL[!duplicated(df_time_ALL[c("timedate","ID", "X", "Y")]),]
df_time_ALL$FLAG <- 0



##----PREVALENT directions OBSERVED from DISTRIBUTION of angles--------------

df_time_S <- df_time_ALL %>%
  filter(angle_deg >= 250 & angle_deg <= 280)
df_time_N <- df_time_ALL %>%
  filter(angle_deg >= 80 & angle_deg <= 120)
df_time_ALL <- rbind(df_time_S,
                     df_time_N)

### ----->>>>>> ###### -------------------------------------------------------------------------------------------
### ----->>>>>> ###### -------------------------------------------------------------------------------------------

df_time_ALL <- df_time_ALL %>%
  mutate(timePeriod_15sec = floor_date(timedate, "30seconds"))
df_time_ALL <- df_time_ALL %>%
  mutate(floor = second(timePeriod_15sec))
list_floor <- unique(as.list(df_time_ALL$floor))

# df_time_ALL <- df_time_ALL[1:10000, ]
list_hours <- unique(as.list(df_time_ALL$hour))
list_minutes <- unique(as.list(df_time_ALL$minute))

plot(piazza)

for (h in 1:length(list_hours)) {
  for (j in 1:length(list_minutes)) {
    for (f in 1:length(list_floor)) {
    print(paste0("hour:", list_hours[h], " @ ","minute:", j, " @ ","second:", list_floor[f]))
    df_time <- df_time_ALL %>%
      filter(hour == list_hours[h] & minute == list_minutes[j] & floor == list_floor[f])
    

df_time <- df_time_ALL
df_time$X_copy <- df_time$X
df_time$Y_copy <- df_time$Y

DF_sf <- st_as_sf(x = df_time,
                  coords = c("X", "Y")) %>%
  st_set_crs(32632)


GRID_direction <- st_intersection(DF_sf, GRID) 
GRID_direction <- as.data.frame(GRID_direction[, c("angle_deg", "end.x", "end.y","X_copy", "Y_copy", 
                                                   "end.arrow.x", "end.arrow.y", "speed", "cell_ID")])
GRID_direction <- as.data.frame(GRID_direction[, c("cell_ID", "speed", "angle_deg", "end.x", "end.y", 
                                                   "X_copy", "Y_copy", "end.arrow.x", "end.arrow.y")])
## join SPEED values to the GRID
GRID_A <- GRID %>%
  left_join(GRID_direction, by = c("cell_ID"))
GRID_A <- GRID_A %>%
  group_by(cell_ID) %>%
  summarise(mean_speed = mean(speed, na.rm = T),
            mean_angle = median(angle_deg, na.rm = T),
            inv_sd_angle = 1/sd(angle_deg, na.rm = T),
            mean_end_x = median(end.x, na.rm = T),
            mean_end_y = median(end.y, na.rm = T),
            mean_end_arrow_x = median(end.arrow.x, na.rm = T),
            mean_end_arrow_y = median(end.arrow.y, na.rm = T),
            mean_X = median(X_copy, na.rm = T),
            mean_Y = median(Y_copy, na.rm = T))

## remove rows with inf values
new_df <- as.data.frame(GRID_A)
new_df <-  new_df[, !names(new_df) %in% c("x")]
new_df <- new_df %>% 
  filter_all(all_vars(!is.infinite(.)))
### replace NAs values with 0
new_df <- new_df %>%
  mutate_at(vars(mean_speed), ~replace_na(., 0))


new_df_S <- new_df %>%
  filter(mean_angle >= 250 & mean_angle <= 280)
new_df_N <- new_df %>%
  filter(mean_angle >= 80 & mean_angle <= 120)
new_df <- rbind(new_df_S,
                new_df_N)


#########################################################
new_df <- new_df %>%
  filter(mean_speed <= 2.5,
         inv_sd_angle < 1)
#########################################################


#Step 3 - Create an object of class `SpatialLinesDataFrame` 

#Line parameters
arrow.length <- 0.1 #length of arrowhead leg (meters)
arrow.angle <- 120 #angle of arrowhead leg (degrees azimuth)
length <- 0.01

#Generate data frame with auxiliary coordinates
end.xy.df <- data.frame(end.x=NA,end.y=NA,end.arrow.x=NA,end.arrow.y=NA)


if (nrow(new_df) > 2) {

  for (i in (1:nrow(new_df))){
    
    
    # print(i)
    if (new_df$mean_angle[i] > 0 & new_df$mean_angle[i] < 90) {
      new_df$mean_angle[i] <- 90 - new_df$mean_angle[i]
    } else if (new_df$mean_angle[i] > 90 & new_df$mean_angle[i] < 180) {
      new_df$mean_angle[i] <- 90 - new_df$mean_angle[i]    ##################### <--------
    } else if  (new_df$mean_angle[i] > 180 & new_df$mean_angle[i] < 270) {
      new_df$mean_angle[i] <- 90 - new_df$mean_angle[i]
    } else if (new_df$mean_angle[i] > 270 & new_df$mean_angle[i] < 360) {
      diff <-  abs(270 - new_df$mean_angle[i])
      new_df$mean_angle[i] <-   270 + new_df$mean_angle[i] - 2*diff
    }
  }
  
  for (i in c(1:nrow(new_df))){
    
    #coordinates of end points for wind lines (the initial points are the ones where data was observed)
    if (new_df$mean_angle[i] <= 90) {
      end.x <- new_df$mean_X[i] + (cos((90 - new_df$mean_angle[i]) * 0.0174532925) * 30*length)    
    } else if (new_df$mean_angle[i] > 90 & new_df$mean_angle[i] <= 180) {
      end.x <- new_df$mean_X[i] + (cos((new_df$mean_angle[i] - 90) * 0.0174532925) * 30*length)
    } else if (new_df$mean_angle[i] > 180 & new_df$mean_angle[i] <= 270) {
      end.x <- new_df$mean_X[i] - (cos((270 - new_df$mean_angle[i]) * 0.0174532925) * 30*length)
    } else {end.x <- new_df$mean_X[i] - (cos((new_df$mean_angle[i] - 270) * 0.0174532925) * 30*length)}
    
    if (new_df$mean_angle[i] <= 90) {
      end.y <- new_df$mean_Y[i] + (sin((90 - new_df$mean_angle[i]) * 0.0174532925) * 30*length)
    } else if (new_df$mean_angle[i] > 90 & new_df$mean_angle[i] <= 180) {
      end.y <- new_df$mean_Y[i] - (sin((new_df$mean_angle[i] - 90) * 0.0174532925) * 30*length)
    } else if (new_df$mean_angle[i] > 180 & new_df$mean_angle[i] <= 270) {
      end.y <- new_df$mean_Y[i] - (sin((270 - new_df$mean_angle[i]) * 0.0174532925) * 30*length)
    } else {end.y <- new_df$mean_Y[i] + (sin((new_df$mean_angle[i] - 270) * 0.0174532925) * 30*length)}
    
    #coordinates of end points for arrowhead leg lines (the initial points are the previous end points)
    end.arrow.x <- end.x + (cos((new_df$mean_angle[i] + arrow.angle) * 0.0174532925) * arrow.length)
    end.arrow.y <- end.y - (sin((new_df$mean_angle[i] + arrow.angle) * 0.0174532925) * arrow.length)
    
    end.xy.df <- rbind(end.xy.df,c(end.x,end.y,end.arrow.x,end.arrow.y))
  }
  
  end.xy <- end.xy.df[-1,]
  df_new <- data.frame(new_df,end.xy) #df with observed and auxiliary variables
  # head(df,3)
  
  #------------------------------
  #Step 3 - Create an object of class `SpatialLinesDataFrame` 
  
  df_new$id <- 1:nrow(df_new)
  lines <- data.frame(cbind(lng=c(df_new$mean_X, df_new$end.x, df_new$end.arrow.x),
                            lat=c(df_new$mean_Y, df_new$end.y, df_new$end.arrow.y),
                            id=c(rep(df_new$id,3))))
  
  
  lines.list <- list()
  library(sp)
  
  for (i in c(1:max(lines$id))){
    line <- subset(lines,lines$id==i)
    line <- as.matrix(line[,c(1:2)])
    line <- Line(line) #object of class 'Line'
    lines.list[[i]] <- Lines(list(line), ID = i) #list of 'objects'Lines' 
  }
  
  
  mean_arrows <- SpatialLines(lines.list) #object of class 'SpatialLines'
  proj4string(mean_arrows) <- CRS("+init=epsg:32632") #define CRS
  mean_arrows <- spTransform(mean_arrows, CRS("+init=epsg:32632"))
  # plot(mean_arrows)
  

  plot(mean_arrows, add = T, pch = 16, cex = .01)
  # jpeg(filename=paste0(folder_trajectories, list_hours[h],"h_", j, "min_", list_floor[f],"secs_27Nov2021_", name_camera, "_",
  #                      time_of_day, ".jpg"),width = 800, height = 800, res = 1)
  # plot(piazza)
  # plot(mean_arrows, add = T, pch = 16, cex = .01)
  # dev.off()
  
     }
    }
  }
}


