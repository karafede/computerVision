

library(magick)
library(stringr)
# remotes::install_github("rstudio/chromote")
# remotes::install_github("rstudio/webshot2")
library(webshot2)
library(lubridate)
library(gtools)
library(animation)
library(purrr)
library(gifski)

rm(list = ls())
setwd("D:/Federico/CityFLows/objectdetection/output/prova_piazza_IDs")
# setwd("D:/Federico/CityFLows/objectdetection/output/prova")

# imgs <- list.files(pattern = ".jpg")
# ##---> sort files in alphabetical order....
# imgs <- mixedsort(imgs)
# 
# saveVideo({
#   for(img in imgs){
#     im <- magick::image_read(img)
#     plot(as.raster(im))
#   }
# })



# mixedsort(list.files(pattern = "*.jpg")) %>% 
#   map(image_read) %>% # reads each path file
#   image_join() %>% # joins image
#   image_animate(fps=10) %>% # animates
#   image_write("animation.gif") # write to current dir


file.jpg <-
  mixedsort(list.files(pattern = "*.jpg",
    ignore.case = TRUE,
    full.names = TRUE))

length(file.jpg)

l <- split(1:length(file.jpg),rep(1:round(length(file.jpg)/length(file.jpg)),each=length(file.jpg)))
l <- split(1:length(file.jpg),rep(1:round(length(file.jpg)/400),each=400))

for (i in l) {
  new.file.jpg <- file.jpg[i][1:400]
  new.file.jpg <- new.file.jpg[complete.cases(new.file.jpg)]
  m <- image_read(new.file.jpg)%>%
    image_scale('x500')%>%
    image_join()
  image_write_gif(m, paste0(tools::file_path_sans_ext(basename(new.file.jpg[1])),".gif"), delay=1/4)
  # image_write_gif(m,"animation_piazza.gif", delay=1/5)
}
  
length (l[[1]])

###########################################################################
###########################################################################
###########################################################################

# to make a movie.......
# to use with ImageMagik using the commanad line cmd in windows
# cd into the directory where there are the png files
## sort by name.....
# magick -delay 50 -loop 0 *.png MOVIE_density_Voronoi_I_Duca_AOSTA.gif
# magick -delay 50 -loop 0 *.png MOVIE_MEAN_SPEED_Duca_AOSTA.gif

###########################################################################
###########################################################################
###########################################################################

## to convert .gif into .mp4
# https://cloudconvert.com/gif-to-mp4

## to merge .mp4 videos
# https://clideo.com/it/merge-video




