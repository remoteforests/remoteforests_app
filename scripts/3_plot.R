observeEvent(input$get_plot_b, {
  
  plot_id_in <- filter(plot.df, plot_name %in% input$pl_selplot_in) %>% pull(plot_id) %>% unique()
  #plot_id_in <- c(1276,  745)
  
  withProgress(message = 'Preparing the data', detail = 'This may take a while ...', value = 0.3, {
    
    # Plot level summary ------------------------------------------------------
    plot.info.df <- plot.df %>%
      filter(plot_id %in% plot_id_in) %>%
      summarize_at(vars(country, location, plot_id, date, plotsize, dbh_min, foresttype, altitude_m, slope, aspect),
        funs(paste_col)) %>%
      t() %>%
      as.data.frame() %>%
      set_names('Value') %>%
      rownames_to_column('Parameter')
    
    
    incProgress(0.4)
    # Tree level summary ------------------------------------------------------
    tree.info.df <- plot.df %>%
      filter(plot_id %in% plot_id_in) %>%
      filter(date == min(date)) %>%
      distinct(plot_id) %>%
      inner_join(tree.df, by = 'plot_id')
    

    # GGplot ------------------------------------------------------------------
    
    if( nrow(filter(tree.info.df, !is.na(x_m))) > 1){
      position_gplot <- tree.info.df %>% 
        filter(!is.na(x_m)) %>%
        ggplot() +
        geom_point( aes(x_m, y_m, 
                      size = dbh_mm, 
                      color = species,
                      shape = status)) +
        geom_point(data = tree.info.df %>% filter(!is.na(age)),
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
    
    # DBH ------------------------------------------------------------------
  
    if ( nrow(filter(tree.info.df, !is.na(dbh_mm))) > 1 ){
      dbh_gplot <- tree.info.df %>%
        ggplot() +
        geom_histogram(aes(dbh_mm, fill = species), binwidth = 50) +
        coord_cartesian(xlim = c(0, 800)) +
        gstyle + xlab('DBH (mm)') + ylab('Number of trees') +
        theme(legend.position = "none")
      } else {
        dbh_gplot <- ggplot()
      }
  
    # Age ------------------------------------------------------------------
    if ( nrow(filter(tree.info.df, !is.na(year_min))) > 1 ){
      age_gplot <- tree.info.df %>%
        ggplot() +
        geom_histogram(aes(year_min, fill = species), binwidth = 10) +
        coord_cartesian(xlim = c(1700, 2000)) +
        gstyle + xlab('Calendar year') + ylab('Number of trees') +
        theme(legend.position = "none")
    } else {
      age_gplot <- ggplot()
    }
  
    age_dbh_gg <- plot_grid(dbh_gplot, age_gplot, nrow = 1, ncol = 2)
    tree_plot_gg <- plot_grid(position_gplot, age_dbh_gg, nrow = 2, ncol = 1, rel_heights = c(4,2))
  
    incProgress(0.6)
    
    #--/ Download data
    tree.sel <- KEL %>% tbl("tree") %>% filter(plot_id %in% plot_id_in) %>% collect()
    
    incProgress(0.7)
    regeneration.sel <- KEL %>% tbl("regeneration") %>% filter(plot_id %in% plot_id_in) %>% collect()
    
    incProgress(0.8)
    deadwood.sel <- KEL %>% tbl("deadwood") %>% filter(plot_id %in% plot_id_in) %>% collect()
    
  
    incProgress(0.9)
    # #---/ Plot tab
    output$plotInfoT <- renderTable(
      plot.info.df
    )
    
    output$treeMap <- renderPlot({
      tree_plot_gg
    })
  
    #----/ Tree level data
    output$treeDT <- DT::renderDataTable(
      tree.sel %>% toDT()
    )
    output$regenerationDT <- DT::renderDataTable(
      regeneration.sel %>% toDT()
    )
    output$deadwoodDT <- DT::renderDataTable(
      deadwood.sel %>% toDT()
    )
  
  }) #End of progress bar
})

