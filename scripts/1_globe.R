
# credentials and data loading --------------------------------------------

# Create leaflet map and visualization ------------------------------------
#-- map
map_header <- paste("<table>
                    <tr>
                    <td><strong>Site_ID - </strong></td>
                    <td>",plot.unique.df$plotid,"</td>
                    </tr>
                    <tr>
                    <td><strong>Species - </strong></td>
                    <td>",plot.unique.df$foresttype,"</td>
                    </tr>
                    <tr>
                    <td><strong>Altitude - </strong></td>
                    <td>",plot.unique.df$altitude_m,"</td>
                    </tr>
                    </table>", sep="")

treeIcons <- iconList(
  beech = makeIcon(iconUrl = "www/beech.png", iconWidth = 9, iconHeight = 12),
  spruce = makeIcon(iconUrl = "www/spruce.png", iconWidth = 9, iconHeight = 12)
)

plotMap <- plot.unique.df %>%
  leaflet() %>%
  addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", options=tileOptions(opacity=0.6)) %>%
  addTiles("http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}", options = tileOptions(opacity = 0.5)) %>%
  #addProviderTiles(providers$CartoDB.Positron) %>% 
  addMarkers(~lng,
             ~lat,
             icon = ~treeIcons[foresttype],
             popup =  map_header)

#-- aspect
asp <- plot.unique.df %>%
  mutate(foresttype = ifelse(foresttype %in% "spruce","Picea abies", "Fagus sylvatica")) %>%
  ggplot() +
  geom_histogram(aes(aspect, fill = foresttype), binwidth = 10) +
  coord_polar(start = 0)+
  gstyle + theme(legend.position="none") +
  xlab("Aspect") + ylab("Total count")+
  scale_x_continuous(breaks=seq(0, 359, by=90), expand=c(0,0), lim=c(0, 360))

#-- slope
alt <- plot.unique.df  %>%
  mutate(foresttype = ifelse(foresttype %in% "spruce","Picea abies", "Fagus sylvatica")) %>%
  ggplot() +
  geom_histogram(aes(altitude_m, fill = foresttype), binwidth = 50) +
  gstyle + theme(legend.position="none") +
  xlab("Altitude m a.s.l.") + ylab("Total count")


plotInfo <- plot_grid(asp, alt, nrow = 2, ncol = 1)
