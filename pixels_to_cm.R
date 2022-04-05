
rm(list = ls())

library(lubridate)
library(threadr)
library(stringr)
library(ggplot2)
library(dplyr)
library(openair)
library(pander)
library(ggrepel)
library(grid)
library(gridExtra)
library(broom)
library(tidyr)
library(RColorBrewer)
library(ggpmisc)
library(varhandle)


setwd("D:/Federico/CityFLows/objectdetection")
# DF <- read.csv("IDs_pixels_Piazza_Aosta_26Nov2021_new.csv")[-1]
DF <- read.csv("IDs_pixels_Piazza_Duca_Aosta_26Nov2021.csv")[-1]
DF <- DF %>%
  filter(X >= 0)
DF <- DF %>%
  dplyr::select(X, Y, ID, timestamp, frame_number)

##-- reverse Y axis---
offset_Y <- max(DF$Y)
offset_X <- max(DF$X)
DF$Y <- abs(DF$Y-offset_Y)
# DF$X <- abs(DF$X-offset_X)


#### view of the Piazza Duca D'Aosta is like a trapezoid

## conversion of Y --> dY == 62 meters (max:992 --- min:327)
# top dX == 60 + 0 + 11 + 41
# bottom dX = 0 + 12 meters
## conversion X ---> X + cateti (dx + sx) (min: -184, max:1901)

### ----- estimate angles of the camera view ------- ########################
## LEFT angle alpha
# https://www.youmath.it/domande-a-risposte/view/6645-calcolo-ipotenusa.html
alpha_sx <- ((atan(62/60))*180)/pi
# ipotenusa_sx <-  y / sin(alpha_sx* pi / 180)

# ipotenusa_sx <-  5 / sin(alpha_sx* pi / 180)
# cateto_sx <- ipotenusa_sx * cos(alpha_sx* pi / 180)

## RIGHT angle beta
beta_dx <- ((atan(62/41))*180)/pi
# ipotenusa_dx <-  y / sin(beta_dx* pi / 180)

#ipotenusa_dx <-  5 / sin(beta_dx* pi / 180)
# cateto_dx <- ipotenusa_sx * cos(beta_dx* pi / 180)


### along Y we can approximate a linear conversion from pixels to meters5
## pixel units m/pixels
meter_per_pixel_Y <- 62/(max(DF$Y)-min(DF$Y))  # 62
DF$Y_m <- ((DF$Y)*meter_per_pixel_Y)
offset_Y <- min(DF$Y_m)
DF$Y_m <- ((DF$Y)*meter_per_pixel_Y - offset_Y)



## along the X we should assign a new X that is not constant but changes with the perspectives...
## for each Y we need to add some meters to the X directions according to the angle of the camera view

## firstly do a linear conversion
# meter_per_pixel_X <- 12/(max(DF$X)-min(DF$X))  # max is 80 from 80 ----> 12 by each dY in pixels 672
# DF$X_m <- ((DF$X)*meter_per_pixel_X)



# increment <- 62 / (max(DF$Y)-min(DF$Y))  ## 62 meter is the depth of the square
# steps <- seq(12, 80, increment)   ## this is also the Y of the pixels



# for (i in steps) {
#   meter_per_pixel_X <- i/(max(DF$X)-min(DF$X)) 
#   print(meter_per_pixel_X)
# }


DF <- DF[order(DF$Y_m, decreasing = F), ]
DF$Y_round <- round(DF$Y_m, digits = 0)


INCREMENT <- 12
DF$X_m <- 0
for (y in seq(1:62)){
  print(y)
  INCREMENT <- INCREMENT + 1.7   ## play with this parameter....
  print(INCREMENT)
  DF[DF$Y_round == y, c("X_m")] <- (INCREMENT/(max(DF$X)-min(DF$X)))
}

DF$X_m <- (DF$X) * (DF$X_m)



  
# ### add meters on the left side
# DF$slope_sx <-  DF$Y_m / sin(alpha_sx* pi / 180)
# DF$add_sx <- DF$slope_sx * cos(alpha_sx* pi / 180)
# 
# ### add meters on the right side
# DF$slope_dx <-  DF$Y_m / sin(beta_dx* pi / 180)
# DF$add_dx <- DF$slope_dx * cos(beta_dx* pi / 180)
# 
# DF$X_m <- DF$X_m + DF$add_dx + DF$add_sx
# offset_X <- min(DF$X_m)
# DF$X_m <- (DF$X_m - offset_X)


# DF$X_m <- DF$X
# DF$Y_m <- DF$Y


### save file
DF <- DF %>%
  dplyr::select(X_m, Y_m, ID, timestamp, frame_number)

write.csv(DF, "IDs_meters_Piazza_Duca_Aosta_26Nov2021.csv")
  

