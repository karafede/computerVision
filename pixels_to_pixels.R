
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
# DF <- read.csv("IDs_pixels_Piazza_Duca_Aosta_26Nov2021.csv")[-1]
# DF <- read.csv("IDs_pixels_Piazza_Duca_Aosta_26Nov2021_new_new.csv")[-1]
# DF <- read.csv("camera_93_IDs_corridoio_lato_lungo_pixels.csv")[-1]
# DF <- read.csv("camera_94_IDs_corridoio_lato_corto_pixels.csv")[-1]
# DF <- read.csv("camera_94_IDs_bidirez_corridoio_lato_corto_pixels.csv")[-1]
# DF <- read.csv("cam_94_lato_corto_monodirez_corr_stretto_pixels.csv")[-1]
# DF <- read.csv("cam_94_lato_corto_bidirez_corr_stretto_pixels.csv")[-1]
# DF <- read.csv("cam_94_lato_corto_bidirez_corr_largo_pixels.csv")[-1]


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

DF$X_m <- DF$X
DF$Y_m <- DF$Y


### save file
DF <- DF %>%
  dplyr::select(X_m, Y_m, ID, timestamp, frame_number)

# write.csv(DF, "IDs_meters_Piazza_Duca_Aosta_26Nov2021.csv")
# write.csv(DF, "camera_93_IDs_corridoio_lato_lungo_meters.csv")
# write.csv(DF, "camera_94_IDs_corridoio_lato_corto_meters.csv")
# write.csv(DF, "camera_94_IDs_corridoio_lato_corto_meters_bidirectional.csv")
# write.csv(DF, "cam_94_lato_corto_bidirez_corr_stretto_meters.csv")
write.csv(DF, "cam_94_lato_corto_bidirez_corr_largo_meters.csv")

  

