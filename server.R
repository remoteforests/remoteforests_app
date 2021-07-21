
# Additional scripts and data ---------------------------------------------
source("scripts/1_globe.R",local = T)

# the main server function ------------------------------------------------
shinyServer(
  function(input, output,session) {
    
    # Dynamic variables -------------------------------------------------------
    # data_val <- reactiveValues(
    #   # stand.df = NULL
    #   plot.info.df = NULL,
    #   # tree.df = NULL,
    #   # tree.shiny = NULL,
    #   # regeneration.df = NULL,
    #   # deadwood.df = NULL,
    #   # gplot = NULL
    # )

    # General data overview ---------------------------------------------------
    output$gMap <- renderLeaflet(plotMap)
    output$gInf <- renderPlot(plotInfo)
    
    output$plotTable <- DT::renderDataTable(
      plot.df %>% select(plot_id = plot_name, inventory = date, country, location, stand, standshort, foresttype, plotsize, dbh_min, altitude_m, slope, aspect) %>% toDT()
    )

    # stand level tab ----------------------------------------------------------    
    source("scripts/2_stand.R",local = T)    
    
    # Plot level tab ----------------------------------------------------------    
    source("scripts/3_plot.R",local = T)
    
    # Session end -------------------------------------------------------------
    session$onSessionEnded(function(){poolClose(KEL)})
    session$onSessionEnded(stopApp)
  }
)