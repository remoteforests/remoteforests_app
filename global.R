# Libraries and global settings ------------------------------------------
library(shiny)
library(shinyBS)
library(shinythemes)
library(tidyverse)
library(DBI)
library(leaflet)
library(DT)
library(ggvis)
library(cowplot)
library(pool)
#library(dbplot)

# Connections -------------------------------------------------------------
source('pw.R')


# Global data to be loaded ------------------------------------------------

#-/ Tables connections
plot.sql <- tbl(KEL, 'plot')
tree.sql <- tbl(KEL, 'tree')
tree.shiny <- tbl(KEL, sql('SELECT * FROM shiny.tree'))

#-/ Static data
plot.df <- plot.sql %>% collect()
plot.unique.df <- plot.df %>% group_by(plotid) %>% arrange(desc(date)) %>% slice(1)




# Global functions --------------------------------------------------------

toDT <- function(x){
  #' @description create a data table from teh data.frame
  x %>%
    datatable(escape = FALSE,
              rownames = FALSE,
              extensions = 'Buttons',
              list(dom = 'Bft',
                   scrollX = TRUE,
                   scrollY = "450px",
                   pageLength = nrow(x),
                   buttons = c('csv', 'copy'))
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
