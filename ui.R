
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)


shinyUI(navbarPage("Алтел: Базовые станции", id="nav",

  # Sidebar with a slider input for number of bins
  tabPanel("Интерактивная Карта", value='map',
     div(class="outer",
         
         tags$head(
           # Include our custom CSS
           includeCSS("static/css/styles.css"),
           includeScript('static/js/main.js')
         ),
         span(textOutput('hi')),
         leafletMap("map", width="100%", height="100%",
                    initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                    initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
                    options=list(
                      center = KZ_CNTR,
                      zoom = 6,
                      maxBounds = list(KZ_BND_UPPERLEFT, KZ_BND_DOWNRIGHT) # Show US only
                    )
         ),
         # Shiny versions prior to 0.11 should use class="modal" instead.
         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
             draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
             width = 330, height = "auto",
             h2("KPI Explorer"),
             selectInput("city", "Города", c(city_list)),
             checkboxGroupInput("tech", "Поколение сети", c("LTE"="lte", '2G'='2g', '3G'='3g'), selected=c('lte', '2g', '3g')),
             checkboxGroupInput("status", "Статус", c("ОК"="ok", 'off-the-air'='bad', 'Требует Внимания'='normal'), selected=c('ok', 'bad', 'normal')),
             p("Summary"),
             tableOutput('summary')
         )
         
     )
  ),
  tabPanel("Показатели", value='kpi',
     fluidRow(
       column(6,
              dateRangeInput('input.dtRange',
                 label = paste('Задайте период (dd/mm/yy)'),
                 start = Sys.Date() - 30*3, end = Sys.Date(),
                 min = Sys.Date() - 30*4, max = Sys.Date(),
                 separator = " - ", format = "dd/mm/yy",
                 startview = 'year', language = 'ru', weekstart = 1
              ))
     ),
     fluidRow(
       column(3,
              selectInput("input.cities", "Города", city_list, multiple=TRUE)
       ),
       column(3,
              selectInput("input.tech", "Поколение сети", c("All technology"="", '2G'='2g', '3G'='3g', 'LTE'='lte'), multiple=TRUE)
       )
     ),
     hr(),
     fluidRow(
       column(8,
              checkboxGroupInput('colvis', 'Показать/Скрыть KPI', c('Проц. успешных вызовов'='success_call_rate',
                                                                    'Проц. сбоев'='fail_rate',
                                                                    'Доступность'='cell_avail',
                                                                    'Макс. кол-во активных абон.'='max_active_abon',
                                                                    'Проц. перегрузки'='overload_rate'), selected=c('success_call_rate', 'fail_rate', 'cell_avail', 'max_active_abon', 'overload_rate'), inline=TRUE)
              ),
       column(4, class='text-right',
              downloadButton('downloadData', 'Загрузить в CSV'))
     ),
     dataTableOutput('kpi_tbl'),
     
     mainPanel(class="col-sm-12",
       h2('Графики По Основным Показателям'),
       uiOutput('plots')
     )
     
  )
))
