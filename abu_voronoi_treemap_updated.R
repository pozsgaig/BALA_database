#### Voronoid maps for all data ####
data_4_voronoid<-BALA_all_data %>% 
  filter(!MF=="NPI") %>% 
  group_by(class, order, ind_nonind) %>% 
  summarise(weight = log(1+sum(total_abundance)))

data_4_voronoid$ind_nonind[data_4_voronoid$ind_nonind=="Indeterminate"]<-"UnK"
data_4_voronoid$ind_nonind[data_4_voronoid$ind_nonind=="NonInd"]<-"Exo"

data_4_voronoid$ID<-paste(data_4_voronoid$order, data_4_voronoid$ind_nonind)
data_4_voronoid<-data_4_voronoid[!apply(data_4_voronoid, 1, FUN = function(x) any(is.na(x))), ]


# defining a square as a starting shape
start_shape<-vect(rbind(c(-1,-1), c(-1,1), c(1,1), c(1,-1)), "polygons",
                  crs = "+proj=utm +zone=1")


# the main map, containing all groupings
qq<-voron_tree(data_4_voronoid, "weight", fgroups = c("class", "order", "ID"), iter = 500, shape = start_shape) 

# the parent groups to facilitate colouring
qq1<-voron_tree(data_4_voronoid, "weight", fgroups = "class", iter = 500, shape = start_shape) 

# setting both maps to the same crs
crs(qq$geom)<-crs(qq1$geom)


# setting up colours for classes

cols<-c(wes_palette("Rushmore1", 4)[4],
        wes_palette("BottleRocket2", 5)[4],
        wes_palette("Rushmore1", 4)[3],
        wes_palette("BottleRocket1", 6)[6])

classcols<-structure(cols,
                     names = unique(data_4_voronoid$class)) # order is to maximise darkness


# making a unique class-order dataframe
temptax<-select(data_4_voronoid, class, order) %>% 
  unique()

# adding class colour
temptax$col<-as.character(classcols[temptax$class])

# calculating alpha for each order, in each class. 
temptax$alpha<-unlist(sapply(unique(temptax$class), function(x){
  n = nrow(temptax[temptax$class==x, "class"])
  c(seq(0.2, 0.7, length.out =n))
}))

# adding alpha to class colours
temptax$final_col<-alpha(temptax$col, temptax$alpha)
temptax<-as.data.frame(temptax)

ordcols<-structure(temptax$final_col, names= temptax$order)
ind_nonind_pch<-structure(c(24,3,23), names=unique(data_4_voronoid$ind_nonind))
# ind_nonind_pch<-structure(c(8,"A",3), names=unique(data_4_voronoid$ind_nonind))

s <- sf::st_as_sf(qq$geom)
s$ind_nonind<-data_4_voronoid$ind_nonind
s$order<-data_4_voronoid$order
s$class<-data_4_voronoid$class

centroid<-st_as_sf(st_centroid(s$geometry))
centroid$order<-data_4_voronoid$order
centroid$class<-data_4_voronoid$class
centroid$ind_nonind<-data_4_voronoid$ind_nonind

gc()

# classes are plotted in separate layers to allow grouped legends
all_islands_plot<- ggplot(s) +
  geom_sf(data = filter(s, class == "Insecta"), 
                  aes(fill = order,
                      color = class)) +
  geom_sf(data = filter(centroid, class == "Insecta"), size = 1.5, stroke =1, 
          aes(pch = ind_nonind, color = class, fill = order)) +
  scale_fill_manual(values = ordcols, name = "Insecta",
                    guide = guide_legend(ncol = 2))+
  new_scale_fill()+
  geom_sf(data = filter(s, class == "Arachnida"), 
                  aes(fill = order,
                      color = class)) +
  geom_sf(data = filter(centroid, class == "Arachnida"), size = 1.5, stroke =1, 
          aes(pch = ind_nonind, color = class, fill = order)) +
  scale_fill_manual(values = ordcols, name = "Arachnida")+
  new_scale_fill()+
  geom_sf(data = filter(s, class == "Chilopoda"), 
                  aes(fill = order,
                      color = class)) +
  geom_sf(data = filter(centroid, class == "Chilopoda"), size = 1.5, stroke =1, 
          aes(pch = ind_nonind, color = class, fill = order)) +
  scale_fill_manual(values = ordcols, name = "Chilopoda")+
  new_scale_fill()+
  geom_sf(data = filter(s, class == "Diplopoda"), 
                  aes(fill = order,
                      color = class)) +
  geom_sf(data = filter(centroid, class == "Diplopoda"), size = 1.5, stroke =1, 
          aes(pch = ind_nonind, color = class, fill = order)) +
  scale_fill_manual(values = ordcols, name = "Diplopoda")+
  geom_spatvector(data = qq1$geom, fill = NA, aes(color = qq1$groups),
                  linewidth = 1.5)+
  scale_color_manual(values = classcols, guide = "none")+
  
  scale_shape_manual(values = ind_nonind_pch, guide = "none")+ 
  theme(legend.text=element_text(size=15),
        legend.title=element_text(size=17),
        legend.margin = margin(5,0,5,0),
        legend.box.spacing = unit(0, "pt"),
        legend.justification="left")
  

# saving out legend
leg<-get_legend(all_islands_plot)

# copying it to keep names and last list element
leg2<-leg
# looping through the legend grobs and setting colour, line width etc
leg2$grobs<-lapply(1:(length(leg$grobs)-1), function(x){
  g<-leg$grobs[[x]]
  col<-as.character(classcols[c("Insecta", "Chilopoda", "Diplopoda", "Arachnida")][x])
  g$grobs[[1]]$gp$col<-col
  g$grobs[[1]]$gp$lwd<-4
  g$grobs[[1]]$width[1]<-unit(24, "char")
  g$grobs[[1]]$x[1]<-unit(-10, "mm")
  g$grobs[[1]]$hjust<-0
  g
})

# copying last list element back
leg2$grobs[[5]]<-leg$grobs[[5]]

# resetting names
names(leg2$grobs)<-names(leg$grobs)



#### Voronoid maps for islands separately ####


data_4_island_voronoid<-BALA_all_data %>% 
  filter(!MF=="NPI") %>%
  group_by(island, class, order, ind_nonind) %>% 
  summarise(weight = log(1+sum(total_abundance)))

data_4_island_voronoid$ind_nonind[data_4_island_voronoid$ind_nonind=="Indeterminate"]<-"UnK"
data_4_island_voronoid$ind_nonind[data_4_island_voronoid$ind_nonind=="NonInd"]<-"Exo"

data_4_island_voronoid$ID<-paste(data_4_island_voronoid$order, data_4_island_voronoid$ind_nonind)
data_4_island_voronoid<-data_4_island_voronoid[!apply(data_4_island_voronoid, 1, FUN = function(x) any(is.na(x))), ]



island_voron_map_list<-lapply(unique(data_4_island_voronoid$island), function(x){
  cat("\n")
  print(x)
  cat("\n")
  # x = "FAI"
  dat<-data_4_island_voronoid[data_4_island_voronoid$island==x,]
  # the main map, containing all groupings
  qq<-voron_tree(dat, "weight", fgroups = c("class", "order", "ID"), iter = 500, shape = start_shape) 
  
  # the parent groups to facilitate colouring
  qq1<-voron_tree(dat, "weight", fgroups = "class", iter = 500, shape = start_shape) 
  
  # setting both maps to the same crs
  crs(qq$geom)<-crs(qq1$geom)
  
  
  s <- sf::st_as_sf(qq$geom)
  s$ind_nonind<-dat$ind_nonind
  s$order<-dat$order
  s$class<-dat$class
  
  centroid<-st_as_sf(st_centroid(s$geometry))
  centroid$order<-dat$order
  centroid$class<-dat$class
  centroid$ind_nonind<-dat$ind_nonind
  
  
  label_coords<-as.data.frame(terra::geom(qq$sites))[,c("x", "y")]
  label_coords$label<-qq$groups
  
  # windows()
  ggplot(s) +
    geom_sf(data = s, aes(fill = order,
                        color = class)) +
    geom_sf(data = centroid, size = 1.5, stroke =1, 
            aes(pch = ind_nonind, color = class, fill = order)) +
    geom_spatvector(data = qq1$geom, fill = NA, aes(color = qq1$groups),
                    linewidth = 1.5)+
    scale_fill_manual(values = ordcols)+
    scale_color_manual(values = classcols, guide = "none")+
    # geom_text(data = label_coords, aes(x = x, y = y, label = label))+
    scale_shape_manual(values = ind_nonind_pch, guide = "none")+
    theme_void()+
    theme(legend.position = "none",
          plot.margin = margin(1,0.1,0.1,0.1, "cm"))
  
})


full_plot<-all_islands_plot+
  theme_void()+
  theme(legend.position = "none",
        plot.margin = margin(1,0.1,0.1,0.1, "cm"))

island_voron_map_list[[9]]<-full_plot

plot_all<-ggarrange(plotlist = island_voron_map_list, ncol = 3, nrow = 3,
                    labels = c(unique(data_4_island_voronoid$island), "All data"),
                    label.x = 0.35, vjust = 3)

plot_2_save<-grid.arrange(plot_all, leg2,
                          ncol = 2,
                          widths = c(3,1))


ggsave(filename = "../Plots/abu_voron_maps_updated.pdf", plot_2_save, width = 15, height = 10)
ggsave(filename = "../Plots/abu_voron_maps_updated.png", plot_2_save, width = 15, height = 10)
