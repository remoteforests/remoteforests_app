plotHist <- function(data, fill){
  data %>%
    mutate(n = 100 * n / sum(n)) %>%
    ggplot() +
    geom_col(aes_string('x', 'n', fill = fill)) +
    gstyle + 
    ylab('Proportion (%)')
}

# "spruce"
# 1. Selectize and subset the data ----------------------------------------
output$st_foresttype_ui <- renderUI({
  selectizeInput("st_foresttype_in", "Forest type:", 
    choices = plot.df %>% pull(foresttype) %>% unique(),
    selected = 'spruce',
    multiple = T)
})

output$st_country_ui <- renderUI({
  selectizeInput("st_country_in", "Country:", 
    choices = plot.df %>% filter(foresttype %in% input$st_foresttype_in) %>% pull(country) %>% unique(),
    selected = c('Ukraine', 'Slovakia', 'Romania'),
    multiple = T)
})


output$st_stand_ui <- renderUI({
  standid <- plot.df %>% filter(foresttype %in% input$st_foresttype_in, country %in% input$st_country_in) %>% pull(stand) %>% unique() 
  
  selectizeInput("st_stand_in", "Stand:", 
    choices = standid, selected = standid,
    multiple = T)
})



# 2. Make the reactive data collection ------------------------------------
stand.df <- eventReactive(input$st_get_data_b,{
  
    st_stand_in <- input$st_stand_in
    #st_stand_in <- c('Smrekovice', 'Hlinna')
    
    #stand.df <- 
    plot.df %>%
      filter(stand %in% st_stand_in) %>%
      distinct(plot_id) %>%
      inner_join(tree.df %>% select(plot_id, tree_id, status, growth, layer, species, dbh_mm, age) %>% filter(!is.na(dbh_mm), !status %in% 'stump'), 
        by = 'plot_id')
})


# Plot the histograms -----------------------------------------------------

observe({
  histcol <- input$st_dbh_hist_col_in
  #histcol <- 'species'
  
  output$st_dbh <- renderPlot(
    stand.df() %>%
      ggplot()+
      geom_histogram(aes_string('dbh_mm', fill = histcol), breaks = seq(0, 1200, 50)) +
      facet_wrap(~status, nrow = 1) +
      scale_x_continuous('DBH (mm)', breaks=seq(0, 1200, by=200), limits=c(0, 1200)) +
      ylab('Number of trees') +
      gstyle
    )
  
  output$st_age <- renderPlot(
    stand.df() %>%
      ggplot()+
      geom_histogram(aes_string('age', fill = histcol), breaks = seq(0, 400, 10)) +
      facet_wrap(~status, nrow = 1) +
      scale_x_continuous('Age (years)', breaks=seq(0, 400, by=50), limits=c(0, 400)) +
      ylab('Number of trees') +
      gstyle
  )
})
