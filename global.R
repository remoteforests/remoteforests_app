# Libraries and global settings ------------------------------------------
library(shiny)
library(shinyBS)
library(shinythemes)
library(crosstalk)

library(pool)
library(DBI)

library(DT)
library(leaflet)
library(ggplot2)
library(cowplot)

library(dplyr)
library(tidyr)
library(tibble)
library(purrr)

# Connections -------------------------------------------------------------
source('pw.R')


# Global data to be loaded ------------------------------------------------

# Plot level
plot.df <- tbl(KEL, 'plot') %>% filter(!foresttype %in% "managed") %>%
  select(plot_id = id, date, plot_name = plotid, census, country, location, stand, standshort, subplot,
    longitude = lng, latitude = lat, foresttype, plotsize, dbh_min, plottype, foresttype, altitude_m, slope, aspect) %>%
  collect()
  
leaflet.df <- plot.df %>%
  group_by(plot_name) %>%
  summarise(longitude = mean(longitude, na.rm = T),
            latitude = mean(latitude, na.rm = T),
            altitude_m = mean(altitude_m, na.rm = T),
            aspect = mean(aspect, na.rm = T),
            foresttype = first(foresttype),
            inventory = paste0(date, collapse = ', '))

# Tree table
tree.df <- tbl(KEL, 'ring') %>%
  group_by(core_id) %>%
  summarise(age = n(),
            year_min = min(year, na.rm = T)) %>%
  inner_join(tbl(KEL, 'core') %>% select(core_id = id, tree_id), by = 'core_id') %>%
  group_by(tree_id) %>%
  summarise(age = max(age, na.rm = T),
            year_min = min(year_min, na.rm = T)) %>%
  left_join(tbl(KEL, 'tree') %>%
      filter(onplot != 0) %>% 
      select(plot_id, tree_id = id, treen, treetype, x_m, y_m, status, census, growth, layer, species, dbh_mm, height_m, decay), .,
    by = 'tree_id') %>%
  collect() %>%
  mutate(species = if_else(!species %in% c("Abies alba", "Picea abies", "Fagus sylvatica", "Sorbus aria", "Fraxinus ornus", "Quercus petraea", "Ostrya carpinifolia"), 'Others', species),
    status = cut(status, c(-Inf, 0, 9, Inf), c('stump', 'alive', 'dead')),
    growth = recode(growth, `0` = 'supressed', `1` = 'released', .default = "NA" ),
    layer = recode(layer, `11` = 'upper', `12` = 'middle', `13` = 'lower', .default = "NA" ))

# Global functions --------------------------------------------------------
toDT <- function(x){
  #' @description create a data table from teh data.frame
  x %>%
    datatable(escape = FALSE,
              rownames = FALSE,
              #extensions = 'Buttons',
              list(#dom = 'Bft',
                   scrollX = TRUE,
                   scrollY = "450px",
                   #buttons = c('csv', 'copy'),
                   pageLength = nrow(x))
    )
}

#-----/ FUNCTIONS
circleFun <- function( r = 12.5 ){
  #' @description Function to create the sircle.
  #' @return a data frame with x and y coordinate
  #' @param r A radius of a sircle
  
  tt <- seq(0,2*pi,length.out = 100)
  xx <- r * cos(tt)
  yy <- r * sin(tt)
  return(data.frame(X = xx, Y = yy))
}

paste_col <- function( x ){ 
  #' @description merge multiple values, separate by ';'
  unique(x) %>% paste(., collapse = '; ')
}

# Figure style ------------------------------------------------------------
base_size <- 7
gstyle <- list(
  theme_bw(base_size = base_size),
  theme(axis.text = element_text(size = base_size *1.2, angle = 0,  colour = "grey10"),
        axis.title=element_text(size=base_size*1.4),
        axis.ticks.length=unit(base_size*-0.15, "mm"),
        axis.ticks = element_line(size = base_size * 0.05),
        axis.text.x = element_text(margin=margin(2,0,1,0,"mm")),
        axis.text.y = element_text(margin=margin(0,2,0,1,"mm"))) ,
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_rect(colour = "black")) ,
  theme(strip.background = element_rect(colour = "white", fill = "white", size = base_size*1.1),
        strip.text.x = element_text(colour = "black", angle = 0, size = base_size*1.1,
                                    hjust = 0.5, vjust = 0.5)),
  theme(legend.key.size =  unit(base_size*1, "mm"), legend.key.height =  unit(base_size*1, "mm")),
  theme(legend.text = element_text(size=base_size * 1.2),
        legend.title = element_text(size=base_size * 1.5)),
  theme(legend.key = element_rect(colour = 'white', fill = 'white', linetype='dashed', size =0.1)),
  theme(legend.justification=c(1,1), legend.position=c(1,1)),
  scale_fill_manual(values = c("Fagus sylvatica" = "#da2c3a",
                               "Picea abies" = "#78c2ef",
                               "Abies alba" = "#7CAE00",
                               "Sorbus aria" = "#18f272", 
                               "Fraxinus ornus" = "#c67d10", 
                               "Quercus petraea" = "#d941e1", 
                               "Ostrya carpinifolia" = "#755de9",
                               "Others" = "grey50",
                               'NA' = "grey50",
                               'upper' = "#da2c3a",
                               'middle' = "#78c2ef",
                               'lower' = "#7CAE00",
                               'released' = "#da2c3a",
                               'supressed' = "#7CAE00",
                               'alive' = "#7CAE00",
                               'dead' = "#da2c3a",
                               'stump' =  "grey50")),
  scale_color_manual(values = c("Fagus sylvatica" = "#da2c3a",
                               "Picea abies" = "#78c2ef",
                               "Abies alba" = "#7CAE00",
                               "Sorbus aria" = "#18f272", 
                               "Fraxinus ornus" = "#c67d10", 
                               "Quercus petraea" = "#d941e1", 
                               "Ostrya carpinifolia" = "#755de9",
                               "Others" = "grey50",
                               'NA' = "grey50",
                               'upper' = "#da2c3a",
                               'middle' = "#78c2ef",
                               'lower' = "#7CAE00",
                               'released' = "#da2c3a",
                               'supressed' = "#7CAE00",
                               'alive' = "#7CAE00",
                               'dead' = "#da2c3a",
                               'stump' =  "grey50")),
  scale_shape_manual("Status",values = c("dead" = 21,
                                         "alive" = 19,
                                         "stump" = 4)))