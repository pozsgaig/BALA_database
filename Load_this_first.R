library(rstudioapi)
library(openxlsx)
require(stringr)
require(dplyr)
library(tidyverse)
require(tidyr)
require(lubridate)
library(uuid)



library(VoronoiPlus) # for voronoi treemaps
library(sf) # for voronoi treemaps
library(tidyterra) # for voronoi treemaps
library(terra) # for voronoi treemaps
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(wesanderson)
library(pals) # for order colours
library(ggforce) # for geom_circle
library(cowplot) # for get_legend
library(gridExtra) # for adding legend to large collection of ggplots
library(ggpattern) # for pattern fill of graph objects
library(ggnewscale) # to allow adding multiple legends


library(RMySQL) # for databasing
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


#### necessary functions ####

NA20<- function(x)
{
  x[is.na(x)]<-0
  x}



# generates voronoid maps based on the combination of several groups
# df: the data frame containing all variables
# variable: numeric variable along which the map will be summarised
# fgroups: a character vector showing the columns along which the grouping will happen
# shape: what the general shape of the voronoid map should be. Default is a circle.
voron_tree<-function(df, variable, fgroups, iter = 50, shape){
  for(n in 1:length(fgroups)){
    gr = fgroups[n]
    
    if(missing(shape)) {
      shape <- terra::buffer(terra::vect(cbind(0, 0),
                                         crs = "+proj=utm +zone=1"), 1, 30)
    }
    # df = data_4_voronoid
    # gr = "class"
    # variable = "weight"
    # shape = start_shape
    if(n==1){
      dat<-df %>% group_by(!!sym(gr)) %>%
        summarise(weight = sum(!!sym(variable))) %>% 
        as.data.frame()
      
      res<- voronoi_map(values = dat[,"weight"], groups = dat[, gr], iter = iter, accuracy = 0.0001,
                        shape = shape)
      assign("dat1", dat)
      assign("res1", res)
      if(length(fgroups)==1){return(res)} else
        
      {res<-NULL}} else
      {
        #oldres<-res
        #oldgr = "order"
        #gr = "ID"
        oldres<-get(paste0("res", n-1))
        oldgr = fgroups[n-1]
        
        
        for(x in unique(oldres$groups)){
          # print(oldres$groups)
          # print(oldgr)
          # print(gr)
          
          newdat<-df %>% filter(!!sym(oldgr) == x) %>% 
            group_by(!!sym(gr)) %>% 
            summarise(weight = sum(!!sym(variable))) %>% 
            as.data.frame()
          # print(newdat)
          
          out<-voronoi_map(values = newdat$weight, 
                           groups = newdat[,gr], iter = 50,
                           shape = oldres$geom[which(oldres$groups==x)])
          
          
          if(is.null(res)){res<-list(geom = out$geom, 
                                     sites = out$sites, 
                                     areas = out$areas, 
                                     groups = out$groups, 
                                     values = out$values)} else{
                                       
                                       a <- geom(res$geom, wkt=TRUE)
                                       b <- geom(out$geom, wkt=TRUE)
                                       ab<-c(a,b)
                                       
                                       
                                       res$geom<- terra::vect(ab, "polygons")
                                       # res$geom<- terra::union(res$geom, out$geom)                             
                                       res$sites<-terra::union(res$sites, out$sites)
                                       # kt[[1]]$shape<-terra::union(kt[[1]]$shape, 
                                       #                            kt[[n]]$shape)
                                       
                                       res$areas<-c(res$areas, out$areas)
                                       res$groups<-c(as.character(res$groups), as.character(out$groups))
                                       res$values<-c(res$values, out$values)
                                     }
          
          # print(sapply(res, length))
          
        }
        
        
        if(n==length(fgroups)){
          res<-structure(res, class = "voronoi_map")
          cat("Process has finished.")
          return(res)} else
          {assign(paste0("res", n), res)
            res<-NULL}
        
        
      }
  }}

load("all_BALA_data.RDA")
