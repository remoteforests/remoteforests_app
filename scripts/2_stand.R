plotHist <- function(data, fill){
  data %>%
    mutate(n = 100 * n / sum(n)) %>%
    ggplot() +
    geom_col(aes_string('x', 'n', fill = fill)) +
    gstyle + 
    ylab('Proportion (%)')
}

#--/ Selecting the stands
output$st_date_ui <- renderUI({
  sliderInput("st_date_in", "Date:",
              min = min(plot.df$date), max = max(plot.df$date), step = 1,
              value = range(2010, max(plot.df$date)), sep = "")
})

output$st_foresttype_ui <- renderUI({
  selectizeInput("st_foresttype_in", "Forest type:", 
                 choices = plot.df %>% filter(date %in% sequence(input$st_date_in)) %>% pull(foresttype) %>% unique(),
                 selected = 'spruce',
                 multiple = T)
})

output$st_country_ui <- renderUI({
  selectizeInput("st_country_in", "Country:", 
                 choices = plot.df %>% filter(date %in% sequence(input$st_date_in), foresttype %in% input$st_foresttype_in) %>% pull(country) %>% unique(),
                 selected = c('Ukraine', 'Slovakia', 'Romania'),
                 multiple = T)
})


output$st_stand_ui <- renderUI({
  
  standid <- plot.df %>% filter(date %in% sequence(input$st_date_in),  foresttype %in% input$st_foresttype_in,
                                country %in% input$st_country_in) %>% pull(stand) %>% unique()
  selectizeInput("st_stand_in", "Stand:", 
                 choices = standid, selected = standid,
                 multiple = T)
})


#--/ Visualizing the data

observeEvent(input$st_get_data_b,{
  
  withProgress(message = 'Preparing the data', detail = 'This may take a while ...', value = 0.2, {
    
    datesql <- sequence(input$st_date_in)
    standsql <- input$st_stand_in
    
    tree.sel <- plot.sql %>% 
      filter(date %in% datesql, stand %in% standsql) %>%
      select(date, plotid) %>%
      inner_join(.,
                 tree.shiny,
                 by = c('date', 'plotid'))
    
    
    incProgress(0.5)
    # #-- tree bins
    tree.bind <- tree.sel %>%
      filter(!is.na(dbh_mm)) %>%
      group_by(species,
               layer,
               growth,
               status,
               x = !! db_bin(dbh_mm, binwidth = 50)) %>%
      tally() %>%
      collect() %>%
      ungroup()
    
    
    # age
    incProgress(0.7)
    tree.age <- tree.sel %>%
      filter(!is.na(age)) %>%
      group_by(species, layer, growth, status,
               x = !! db_bin(age, binwidth = 5)) %>%
      tally() %>%
      collect() %>%
      ungroup()
    
    
    
    
  })
  
  observe({
    histcol <- input$st_dbh_hist_col_in
    
    output$st_dbh_alive <- renderPlot(tree.bind %>% filter(status %in% 'alive') %>%
                                        plotHist(fill = histcol)  + ggtitle('Alive') +
                                        scale_x_continuous('DBH (mm)', breaks=seq(0, 1200, by=200), limits=c(0, 1200)))
    
    output$st_dbh_dead <- renderPlot(tree.bind %>% filter(status %in% 'dead') %>%
                                       plotHist(fill = histcol)  + ggtitle('Dead') +
                                       scale_x_continuous('DBH (mm)', breaks=seq(0, 1200, by=200), limits=c(0, 1200)))
    
    output$st_age_alive <- renderPlot(tree.age %>% filter(status %in% 'alive') %>%
                                        plotHist(fill = histcol)  + ggtitle('Alive') +
                                        scale_x_continuous('Age (years)', breaks=seq(0, 400, by=50), limits=c(0, 400)))
    
    output$st_age_dead <- renderPlot(tree.age %>% filter(status %in% 'dead') %>%
                                       plotHist(fill = histcol)  + ggtitle('Dead') +
                                       scale_x_continuous('Age (years)', breaks=seq(0, 400, by=50), limits=c(0, 400)))
  })
  
  
})