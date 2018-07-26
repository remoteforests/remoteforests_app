# main ui body ------------------------------------------------------------
shinyUI(
  bootstrapPage(theme = shinytheme("readable"),
                
                # The general header settings and styles ----------------------------------
                tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
                HTML('<img id = "RFlogo" src="logo.png"</img>'),

                br(),
                
                tabsetPanel(
                  
                  tabPanel('Globe', icon=icon("globe", lib = "font-awesome"),
                           fluidRow(
                             h4('General data overview'),
                             p('Below the general data overiview about all available plots are presented'),
                             
                             HTML('<p>For more information and access details please contact <a href="mailto:svobodam@fld.czu.cz">M.Svoboda</a> or <a href="mailto:trotsiuk@fld.czu.cz">V.Trotsiuk</a></p>')
                           ),
                           fluidRow(
                             column(8, leafletOutput('gMap')),
                             column(4, plotOutput('gInf')) 
                           ),
                           
                           br(), br(), br(),
                           
                           fluidRow(
                             DT::dataTableOutput('plotTable', width="100%")
                           )
                           
                  ), # END Globe
                  
                  
                  tabPanel('Stand', icon=icon("map-o", lib = "font-awesome"),
                           sidebarPanel(width = 3,
                                        p('Please filtr out the stands you would like to visualize.'),
                                        uiOutput('st_foresttype_ui'),
                                        uiOutput('st_country_ui'),
                                        uiOutput('st_stand_ui'),
                                        actionButton('st_get_data_b', 'Get data', icon = icon('refresh', lib = "font-awesome"))
                           ),
                           
                           mainPanel(
                             selectizeInput("st_dbh_hist_col_in", "Group by:", 
                                            choices = c('species', 'layer', 'growth'),selected = 'species'),
                             
                             h3('DBH distributions'),
                             fluidRow(plotOutput('st_dbh', height = '300px')),
                             h3('Age distributions'),
                             fluidRow(plotOutput('st_age', height = '300px'))
                             
                           )
                           ),
                  
                  tabPanel('Plot', icon=icon("tree", lib = "font-awesome"),
                           
                           fluidRow(
                             column(3, 
                                    selectizeInput('pl_selplot_in', 'Select plot', choices = leaflet.df$plot_name, selected = 'UKR_GR1_012', multiple = F),
                                    actionButton('get_plot_b', 'Get plot data', icon = icon('refresh', lib = "font-awesome")),
                                    tableOutput('plotInfoT')),
                             column(9, plotOutput('treeMap', height = '850px',  width = '690px'))
                           ),
                           
                           tabsetPanel(
                             tabPanel('Tree', DT::dataTableOutput('treeDT')),
                             tabPanel('Regeneration', DT::dataTableOutput('regenerationDT')),
                             tabPanel('Deadwood', DT::dataTableOutput('deadwoodDT'))
                           )
                           
                           
                  ) # END PLOT
                  
                ) # END TABSET
                

  )
)
