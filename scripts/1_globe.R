
# credentials and data loading --------------------------------------------

# Create leaflet map and visualization ------------------------------------
treeIcons <- iconList(
  beech = makeIcon(iconUrl = "www/beech.png", iconWidth = 9, iconHeight = 12),
  spruce = makeIcon(iconUrl = "www/spruce.png", iconWidth = 9, iconHeight = 12),
  thermophilic = makeIcon(iconUrl = "www/fraxinus.png", iconWidth = 9, iconHeight = 12)
)

plotMap <- leaflet.df %>%
  leaflet() %>%
  addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", options=tileOptions(opacity=0.6)) %>%
  addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}", options = tileOptions(opacity = 0.5)) %>%
  #addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(~longitude,
             ~latitude,
             icon = ~treeIcons[foresttype],
             popup =  ~paste('<b>Plot_ID:</b> ', plot_name, '<br><b>Species:</b> ', foresttype, '<br><b>Inventory:</b> ', inventory, '<br><b>Altitude:</b> ', altitude_m))

#-- aspect
asp <- leaflet.df %>%
  mutate(foresttype = case_when(
    foresttype %in% "spruce" ~ "Picea abies",
    foresttype %in% "beech" ~ "Fagus sylvatica",
    foresttype %in% "thermophilic" ~ "Fraxinus ornus")) %>%
  ggplot() +
  geom_histogram(aes(aspect, fill = foresttype), binwidth = 10) +
  coord_polar(start = 0)+
  gstyle + theme(legend.position="none") +
  xlab("Aspect") + ylab("Total count")+
  scale_x_continuous(breaks=seq(0, 359, by=90), expand=c(0,0), lim=c(0, 360))

#-- slope
alt <- leaflet.df  %>%
  mutate(foresttype = case_when(
    foresttype %in% "spruce" ~ "Picea abies",
    foresttype %in% "beech" ~ "Fagus sylvatica",
    foresttype %in% "thermophilic" ~ "Fraxinus ornus")) %>%
  ggplot() +
  geom_histogram(aes(altitude_m, fill = foresttype), binwidth = 50) +
  gstyle + theme(legend.position="none") +
  xlab("Altitude m a.s.l.") + ylab("Total count")


plotInfo <- plot_grid(asp, alt, nrow = 2, ncol = 1)
