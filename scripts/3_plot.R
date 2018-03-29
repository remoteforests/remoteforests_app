observeEvent(input$get_plot_b, {

    PL_in <- input$pl_selplot_in


  withProgress(message = 'Preparing the data', detail = 'This may take a while ...', value = 0.3, {
    
  #--/ Summary of plot level data
  data_val$plot.info.df <- plot.df %>%
    filter(plotid %in% PL_in) %>%
    summarize_at(vars(country, location, plotid, date, plotsize, dbh_min, foresttype, altitude_m, slope, aspect),
                 funs(paste_col)) %>%
    t() %>%
    as.data.frame() %>%
    set_names('Value') %>%
    rownames_to_column('Parameter')
  
  incProgress(0.4)
  
  # plot the figures
  tree.shiny.df <- tree.shiny %>%
    filter(plotid %in% PL_in) %>%
    collect() 
  
  tree.gg.df <- tree.shiny.df %>% filter(date == max(date))
  data_val$tree.shiny <- tree.shiny.df
  
  
  if(  nrow(filter(tree.gg.df, !is.na(x_m))) > 1){
    position_gplot <- tree.gg.df %>% 
      filter(!is.na(x_m),
             !onplot %in% 0) %>%
      ggplot() +
      geom_point( aes(x_m, y_m, 
                      size = dbh_mm, 
                      color = species,
                      shape = status)) +
      geom_point(data = tree.gg.df %>% filter(!is.na(age)),
                 aes(x_m, y_m), color = 'grey30', size = 1) +
      scale_size_continuous("DBH (mm)", 
                            limits = c(0,1400),
                            breaks=c(0,400, 600, 1400),
                            range = c(2,7)) +
      gstyle +
      geom_point( aes(0,0), shape = 3, color = "red",size = 3) +
      geom_path(data = circleFun(r = 12.5), aes(x = X, y = Y), color = "black", size = 0.3)+
      geom_path(data = circleFun(r = 17.84), aes(x = X, y = Y), color = "black", size = 0.3)+
      geom_path(data = circleFun(r = 21.85), aes(x = X, y = Y), color = "black", size = 0.3)+
      geom_text( aes(x_m+0.5, y_m+0.5 , label = treen), size = 3, color = "grey20")+
      ggtitle(PL_in) +
      theme(legend.position='right')
  } else {
    position_gplot <- ggplot()
  }
  
  if ( nrow(filter(tree.gg.df, !is.na(dbh_mm))) > 1 ){
    dbh_gplot <- tree.gg.df %>%
      ggplot() +
      geom_histogram(aes(dbh_mm, fill = species), binwidth = 50) +
      coord_cartesian(xlim = c(0, 800)) +
      gstyle + xlab('DBH (mm)') + ylab('Count') +
      theme(legend.position = "none")
  } else {
    dbh_gplot <- ggplot()
  }
  
  if ( nrow(filter(tree.gg.df, !is.na(year_min))) > 1 ){
    age_gplot <- tree.gg.df %>%
      ggplot() +
      geom_histogram(aes(year_min, fill = species), binwidth = 10) +
      coord_cartesian(xlim = c(1700, 2000)) +
      gstyle + xlab('Calendar year') + ylab('Count') +
      theme(legend.position = "none")
  } else {
    age_gplot <- ggplot()
  }
  
  age_dbh_gg <- plot_grid(dbh_gplot, age_gplot, nrow = 1, ncol = 2)
  tree_plot_gg <- plot_grid(position_gplot, age_dbh_gg, nrow = 2, ncol = 1, rel_heights = c(4,2))
  
  incProgress(0.6)
  
  pl_id <- tbl(KEL, 'plot') %>% filter(plotid == PL_in) %>% pull(id)
  
  #--/ Download data
  data_val$tree.df <- KEL %>% tbl("tree") %>% filter(plot_id == pl_id) %>% collect()
  
  incProgress(0.7)
  data_val$regeneration.df <- KEL %>% tbl("regeneration") %>% filter(plot_id == pl_id) %>% collect()
  
  incProgress(0.8)
  data_val$deadwood.df <- KEL %>% tbl("deadwood") %>% filter(plot_id == pl_id) %>% collect()
  
  
  incProgress(0.9)
  # #---/ Plot tab
  output$plotInfoT <- renderTable(
    data_val$plot.info.df
  )
  
  output$treeMap <- renderPlot({
    tree_plot_gg
  })

  #----/ Tree level data
  output$treeDT <- DT::renderDataTable(
    data_val$tree.df %>% toDT()
  )
  output$regenerationDT <- DT::renderDataTable(
    data_val$regeneration.df %>% toDT()
  )
  output$deadwoodDT <- DT::renderDataTable(
    data_val$deadwood.df %>% toDT()
  )
  
  output$ageDT <- DT::renderDataTable(
    data_val$tree.shiny %>% toDT()
  )
  
  }) #End of progress bar
})

