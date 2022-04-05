

rm(list = ls())

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


setwd("D:/Federico/CityFLows/objectdetection/output/speed_plots_roma3/")
folder_speed_roma3 <- "D:/Federico/CityFLows/objectdetection/output/speed_plots_roma3/"

## set forward motion speed
speed_FWD_motion <- 0.2  ## units [m/s]

###-----------------****** IDs generated from FEDERICO KARAGULIAN from video processing with OpenCV -----******########
# DF <- read.csv('D:/Federico/CityFLows/objectdetection/camera_93_IDs_corridoio_lato_lungo_meters.csv')[-1]
# DF <- read.csv('D:/Federico/CityFLows/objectdetection/camera_94_IDs_corridoio_lato_corto_meters.csv')[-1]
# DF <- read.csv('D:/Federico/CityFLows/objectdetection/camera_94_IDs_corridoio_lato_corto_meters_bidirectional.csv')[-1]
# DF <- read.csv('D:/Federico/CityFLows/objectdetection/cam_94_lato_corto_monodirez_corr_stretto_meters.csv')[-1]
# DF <- read.csv('D:/Federico/CityFLows/objectdetection/cam_94_lato_corto_bidirez_corr_stretto_meters.csv')[-1]
DF <- read.csv('D:/Federico/CityFLows/objectdetection/cam_94_lato_corto_bidirez_corr_largo_meters.csv')[-1]

names(DF) <- c("X", "Y", "ID", "timedate", "frame")


# remove duplicates rows
DF <- DF[!duplicated(DF), ]


#### for monodirectional
DF <- DF %>%
  filter(frame < 1510)


### APPROXIMATION to transform PIXELS into ------->>>> METERS
## Aula seminari Roma3 (vista lato lungo: lunghezza corridoio: 5.4m x  larghezza corridoio: 7m)

#############################################################
#### -- setting for camera 93 --- ###########################
# DF$Y <- ((((DF$Y)/7.4 )/1) - 45)/2    # 7.4    -45
# DF$X <- ((DF$X)/100/4)*0.5  #0.6
###########--------------------------------##################
#############################################################


#############################################################
#### -- setting for camera 94 --- ###########################
## Aula seminari Roma3 (vista lato corto: lunghezza corridoio: 10.8m x  larghezza corridoio: 7.44m)
DF$Y <- (DF$Y)/40    # 25 
DF$X <- (DF$X)/100/3.5  ## 4
###########--------------------------------##################
#############################################################

DF <- DF %>%
  filter(Y > 0)


## count recurrency of IDs
COUNTS_ID <- DF %>%
  group_by(ID) %>%
  summarise(count_IDs = length(ID))


DF <- DF %>%
  merge(COUNTS_ID[,c("ID")], by = "ID")


####--setup customized borders to make plots of the same size --####
xmin = min(DF$X)
ymin = min(DF$Y)
xmax = max(DF$X)
ymax = max(DF$Y)


DF <- DF %>%
  mutate(year = year(timedate),
         month = month(timedate),
         day = day(timedate),
         hour = hour(timedate),
         minute = minute(timedate))

day_list <- as.list(DF$day)
day_list <- unique(day_list)

####----> loop over days
all_ID_speed = NULL

# TAG <- "camera_94_lato_corto_monodirezionale"
# TAG <- "camera_94_lato_corto_bidirezionale"
# TAG <- "camera_94_lato_corto_monodirezionale_corr_stretto"
# TAG <- "camera_94_lato_corto_bidirezionale_corr_stretto"
TAG <- "camera_94_lato_corto_bidirezionale_corr_largo"


## build a new data frame DF_people including speeds associated to each position
DF_people_speed = NULL


DF_people <- DF

## get list of all the IDs in the current timeslot
list_IDs <- unique(DF_people$ID)
i <- 2063
i <- 2001
i <- 1204
    
    
for (i in list_IDs) {
    # print(paste0("ID: ",i))
    distance <- 0
    DF_people_speed_ID = NULL
    
    
    ## select one trajectory
    DF_a <- DF_people %>%
      filter(ID == i)
    
    ## remove duplicated X, Y
    DF_a <- DF_a[!duplicated(DF_a[c("X", "Y")]),]
    
    DF_a <- DF_a %>%
      group_by(Y) %>%
      summarise(X = mean(X, na.rm = T),
                ID = ID,
                timedate= timedate,
                day = day,
                hour = hour,
                minute = minute, .groups = 'drop')
    DF_a <- as.data.frame(DF_a)
      
    
    ## sort DF_a by timedate
    # DF_a <- DF_a[order(DF_a$timedate),]
    

    p_t <- ggplot(DF_a, aes(x = X, y = Y, colour = ID, label=ID, group = ID)) +
      theme_bw() +
      geom_point(aes(X,Y,color=NULL, fill = ID), size = 2) +
      geom_path(aes(X,Y,color=NULL, fill = ID)) +
      # geom_text(aes(label=  paste0("ID: ", ID, "  -->",  round( minute(timedate), 0), ":", round( second(timedate), 0))     )   ,hjust=0, vjust=0) +
      geom_text(aes(label=  paste0(round( minute(timedate), 0), ":", round( second(timedate), 0))     )   ,hjust=0, vjust=0) +
      # scale_y_reverse() +
      # scale_x_reverse() +
      theme(legend.position = "none")
    # coord_flip()
    p_t
    
  
    
    # DF_a$timedate <- as.POSIXct(DF_a$timedate)
    ## add row number
    DF_a$index <- 1:nrow(DF_a)
    
    
    DF_a <- DF_a %>%
      dplyr::mutate(time = format(ymd_hms(as.character(DF_a$timedate, tz = "CET")),'%H:%M:%S'))
    DF_a <- as.data.frame(DF_a)
    

    if (nrow(DF_a) > 1) {
      
      ## compute speed without curvature (UNITS --> [m/s]), principal movement speed
      max_timedate <- max(strptime(DF_a[,"time"],format="%H:%M:%S"))
      min_timedate <-  min(strptime(DF_a[,"time"],format="%H:%M:%S"))
      # max_timedate <- max(strptime(DF_a[,"timedate"], format = "%Y-%m-%d %H:%M:%OS"))
      # min_timedate <- min(strptime(DF_a[,"timedate"], format = "%Y-%m-%d %H:%M:%OS"))
      op <- options(digits.secs=3)
      # options(op) #reset options
      
      
      duration_trajectory <- difftime(max_timedate,min_timedate)
      ## set units
      units(duration_trajectory) <- "secs"
      
      
      for (j in 1:nrow(DF_a))  {
        if (j != nrow(DF_a)) {
          ##---distance between two consecutive points (in meters)
          distance_j <- sqrt( ((DF_a[j+1, "X"] - DF_a[j, "X"])^2) +
                                ((DF_a[j+1, "Y"] - DF_a[j, "Y"])^2) )
          # print(paste0("distance: ", distance_j))
          distance = distance + distance_j
          
          
          ## speed [m/s]
          time1a <- strptime(DF_a[j+1, "time"],format="%H:%M:%S")
          time2a <- strptime(DF_a[j, "time"],format="%H:%M:%S")
          
          # ## calculate the speed along the single trajectory of each ID
          # ###-->> this is also the AVERAGE SPEEED OVER the QUARTER of HOUR --####
          DF_speed <- data.frame(timedate = DF_a[j, "timedate"],
                                 ID = i,
                                 X = DF_a[j, "X"],
                                 Y = DF_a[j, "Y"],
                                 speed = 0)

          names(DF_speed) <- c("timedate", "ID", "X", "Y", "speed")
          DF_people_speed_ID = rbind(DF_people_speed_ID, DF_speed)
        }
      }
      
    
      ## calculate the speed alone the singe trajectory of each ID
      ###-->> this is also the AVERAGE SPEEED OVER the QUARTER of HOUR --####
      speed_trajectory_quarter <- distance / as.numeric(duration_trajectory)  ## m/secs
      DF_people_speed_ID$speed <- speed_trajectory_quarter
   

      ### ----> get angle (in degrees) from ORIGIN --> DESTINATION
      len_path <- nrow(DF_people_speed_ID)
      
      ## sort data by timedate
      DF_people_speed_ID <- DF_people_speed_ID[order(DF_people_speed_ID$timedate),]
      
      DY <- DF_people_speed_ID[len_path ,"Y"] - DF_people_speed_ID[1 ,"Y"]
      DX <- DF_people_speed_ID[len_path ,"X"] - DF_people_speed_ID[1 ,"X"]
    
      slope <- DY/DX
      ## get angle from arcTg(slope)
      angle_rad <- atan((slope))  ## this is in radians
      angle_deg <- NISTradianTOdeg(angle_rad)   ## this in degree
      # print(paste0("----------------------------  ", angle_rad))
      
      if (angle_deg <= 0 & DX < 0 & !is.na(angle_deg)) {
        angle_deg = angle_deg + 180   
      } else if (DX < 0 &  DY < 0 & !is.na(angle_deg)) {
        angle_deg = angle_deg + 180  
      } else if (DX > 0 & DY <= 0 & !is.na(angle_deg)) {
        angle_deg = angle_deg + 360
      } else if (DX <= 0 & DY <= 0 & !is.na(angle_deg)) {
        angle_deg = angle_deg + 360
      }
      
      # print(paste0("----------------------------  ", angle_deg))
      DF_people_speed_ID$angle_deg <- angle_deg
      
      DF_people_speed_ID <- DF_people_speed_ID %>%
        filter( (speed < 2.5) ) %>%
        filter (speed > 0.2)
  
  
    } else {print("not enough data_2--------------->>>")}
    
    DF_people_speed <- rbind(DF_people_speed, DF_people_speed_ID)
    ## build a data frame with speed for each ID
    all_ID_speed <- rbind(all_ID_speed, DF_people_speed_ID)
    
}   ### this closes the for loop of the "list_IDs"

  
  
#####################################################
#####################################################
#####################################################
###--- SPEED plot with the right orientation---######
## convert data into a SpatialPolygon dataframe----##

### set dimension of cell size
#### --- monodirectional setting---- #################
######################################################
# cell_SIZE <- 1   ## (UNITS --> [m])
# all_ID_speed$X <- (all_ID_speed$X)*3
# all_ID_speed$Y  <- (all_ID_speed$Y)*0.7
# DF_people_speed$X <- (DF_people_speed$X)*3.5    #3
# DF_people_speed$Y <- (DF_people_speed$Y)*0.7   #0.5

######################################################
######################################################

cell_SIZE <- 1   ## (UNITS --> [m])
all_ID_speed$X <- (all_ID_speed$X)*3
all_ID_speed$Y  <- (all_ID_speed$Y)*1
DF_people_speed$X <- (DF_people_speed$X)*3.5    #3
DF_people_speed$Y <- (DF_people_speed$Y)*1   #0.5


if  (is.data.frame(DF_people_speed) == FALSE) {
  DF_people_speed =data.frame(X = 0,
                              Y = 0,
                              speed = 0)
} else {DF_c <- DF_people_speed %>%
  dplyr::select(X,Y, speed) }


DF_people_speed[is.na(DF_people_speed)] <- 0


## fill NA values with 0
DF_c[is.na(DF_c)] <- 0
## remove NAs in the speed column
DF_c <- DF_c[!is.na(DF_c$speed),]

#####-----> ####################################################
#####-----> ####################################################
### Make a Spatial Frame

DF_sf <- st_as_sf(x = DF_people_speed,                         
               coords = c("X", "Y")) %>%
  st_set_crs(32632)  
  
xmax = max(DF_people_speed$X)
ymax = max(DF_people_speed$Y)
extent = st_sfc(st_polygon(list(rbind(c(xmin,ymin), c(xmax,ymin), c(xmax,ymax),
                                      c(xmin, ymax), c(xmin,ymin)))))

## set CRS
extent <- extent %>% 
  st_set_crs(st_crs(DF_sf))

## width of corridor = 1.25 m (then we fix cell size @ 0.62)
## define a regular grid of 2 meter resolution and add an Id to each cell
GRID <- st_make_grid(extent, cellsize = 1, square = T) %>% 
  st_as_sf() %>%
  mutate(cell_ID = row_number())

GRID_speed <- st_intersection(DF_sf, GRID) 
GRID_speed <- as.data.frame(GRID_speed[, c("speed", "cell_ID")])
GRID_speed <- as.data.frame(GRID_speed[, c("cell_ID", "speed")])

## join SPEED values to the GRID
GRID_A <- GRID %>%
  left_join(GRID_speed, by = c("cell_ID"))
GRID_A <- GRID_A %>%
  group_by(cell_ID) %>%
  summarise(mean_speed = mean(speed, na.rm =T))

### replace NAs values with 0
GRID_A <- GRID_A %>%
  mutate_at(vars(mean_speed), ~replace_na(., 0))


if (mean(GRID_A$mean_speed, na.rm = T) > 0) {
  
plot_speed <- (GRID_A[, c("x", "mean_speed")]) %>% 
  st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
  ggplot(aes()) +
  theme_bw() +
  geom_sf(aes(fill = mean_speed)) + 
  scale_fill_gradient2(low = "grey", mid = "white", high = "blue", midpoint = .02, 
                       trans = "sqrt", name = "[m/s]") +    
  # scale_fill_gradient2(low = "grey", mid = "white", high = "blue", name = "[m/s]") +
  coord_sf(datum = st_crs(32632)) 
  # scale_y_reverse() 
plot_speed

## save plot
dpi = 96
ggsave(filename=paste0(folder_speed_roma3,TAG, ".png"),
       width = 700 / dpi, height = 650 / dpi,
       dpi = dpi)

}


###---> save all speed values
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_camera_94_IDs_lato_corto_monodirectional.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_camera_94_IDs_lato_corto_bidirectional.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_camera_94_IDs_lato_corto_monodirectional_corr_stretto.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_camera_94_IDs_lato_corto_bidirectional_corr_stretto.csv"), row.names=FALSE)
write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_camera_94_IDs_lato_corto_bidirectional_corr_largo.csv"), row.names=FALSE)


