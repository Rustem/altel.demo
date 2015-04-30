
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)


bs <- tbl_df(base_stations)
kpi_output <- arrange(bind_rows(
  mutate(kpi_2g, technology='2g'),
  mutate(kpi_3g, technology='3g'),
  mutate(kpi_lte, technology='lte')
), year, month, day)

KPIs <- c('fail_rate'='Проц. сбоев, %',
          'success_call_rate'='Проц. успешных вызовов, %',
          'cell_avail'='Доступность, %',
          'max_active_abon'='Макс. кол-во активных абон., ед.',
          'overload_rate'='Проц. перегрузки, %')

showBSPopup <- function(map, bsID, lat, lng) {
  popup <- filter(bs, id == bsID)
  tech <- switch(popup$technology,
         '2g'='2G',
         '3g'='3G',
         'lte'='LTE')
  content <- as.character(tagList(
    tags$p("ID: ", substr(popup$id, 1, 10), class='lead'), tags$br(),
    tags$span(sprintf("Модель/Производитель: %s/%s", popup$model, popup$manufacturer)), tags$br(),
    tags$span(sprintf("Подключенные абоненты: %s", popup$connAbons * 1000)), tags$br(),
    tags$span(sprintf("Поколение сети: %s", tech)), tags$br()
  ))
  map$showPopup(lat, lng, content, bsID)
}

map2Color <- function(statuses) {
  mapper <- function(status) {
    return(switch(status,
           ok='#33FF66',
           normal='#FFFF00',
           bad='#FF0000'))
  }
  return(unlist(Map(mapper, statuses)))
}

shinyServer(function(input, output, session) {

  map <- createLeafletMap(session, "map")
    
  baseStationsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(cities[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    rv <- filter(bs,
           lat >= latRng[1] & lat <= latRng[2],
           lon >= lngRng[1] & lon <= lngRng[2])
    rv
  })
  
  baseStations <- reactive({
    rv <- baseStationsInBounds()
    if(!is.null(input$tech)) {
      rv <- filter(rv, technology %in% input$tech)
    } else {
      rv <- filter(rv, FALSE)  
    }
    if(!is.null(input$status)) {
      chosenColors <- map2Color(input$status)
      rv <- filter(rv, color %in% chosenColors)
    } else {
      rv <- filter(rv, FALSE)
    }
  })
  
  output$summary <- renderTable({
    curBS <- baseStations()
    if(!any('color' %in% colnames(curBS)))
      return()
    by_status <- summarise(group_by(curBS, color), count=n())
    alive_bs <- summarise(filter(by_status, color == '#FFFF00' | color == '#33FF66'), count=sum(count))
    all_bs <- summarise(by_status, count=sum(count))
    if(all_bs$count != 0)
      cell_avail <- round(alive_bs$count / all_bs$count * 100, 1)
    else
      cell_avail <- 0
    data.frame('Доступность сети'=paste(cell_avail, '%', sep=''),
               'Подключ абоненты'=summarise(curBS, count=sum(connAbons))$count)
  })
  
  chunksize <- 10
  # session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
  # integration; without it, the addCircle commands arrive in the browser
  # before the map is created.
  session$onFlushed(once=TRUE, function() {
    paintObjs <- observe({
      curBS <- baseStations()
      # Clear existing circles before drawing
      map$clearMarkers()
      if(nrow(curBS) == 0) {
        return()
      }
      for (from in seq.int(1, nrow(curBS), chunksize)) {
        to <- min(nrow(curBS), from + chunksize)
        curChunk <- curBS[from:to,]
        try(
          map$addCircleMarker(
            curChunk$lat, curChunk$lon, 10,
            curChunk$id,
            list(stroke=FALSE, fill=TRUE),
            list(color=curBS$color[from:to])
          )
        )
      }
    })
    
    # TIL this is necessary in order to prevent the observer from
    # attempting to write to the websocket after the session is gone.
    session$onSessionEnded(paintObjs$suspend)
  })
  
  # set map view on a city
  observe({
    city_det <- filter(cities, city == input$city)
    if(is.null(city_det))
      return()
    map$setView(city_det$lat, city_det$lon, city_det$zoom)
  })
  
  clickObj <- observe({
    event <- input$map_marker_click
    if(is.null(event))
      return()
    map$clearPopups()
    isolate({
      showBSPopup(map, event$id, event$lat, event$lng)
  #    map$fitBounds(event$lat - 0.5, event$lon - 0.5, event$lat + 0.5, event$lon + 0.5)
    })
  })
  session$onSessionEnded(clickObj$suspend)
  
  observe({
    stillSelected <- isolate(input$input.cities[input$input.cities %in% city_list])
    updateSelectInput(session, 'input.cities', choices=city_list,
                      selected=stillSelected)
  })
  
  observe({
    stillSelected <- isolate(input$input.tech[input$input.tech %in% c('2g', '3g', 'lte')])
    updateSelectInput(session, 'input.tech', choices=c('2G'='2g', '3G'='3g', 'LTE'='lte'),
                      selected=stillSelected)
  })
  
  dtRange <- reactive({
    if(is.null(input$input.dtRange)) {
      return()
    }
    from_dt <- as.integer(unlist(strsplit(strftime(input$input.dtRange[1], '%Y-%m-%d'), '-')))
    to_dt <- as.integer(unlist(strsplit(strftime(input$input.dtRange[2], '%Y-%m-%d'), '-')))
    rbind(data.frame(year=from_dt[1], month=from_dt[2], day=from_dt[3]),
      data.frame(year=to_dt[1], month=to_dt[2], day=to_dt[3]))
  })
  
  observe({
    if(is.null(input$goto)) {
      return()
    }
    isolate({
      map$clearPopups()
      targetId <- input$goto$id
      rv <- filter(bs, id == targetId)
      dist <- 0.5
      showBSPopup(map, rv$id, rv$lat, rv$lon)
      map$fitBounds(rv$lat - dist, rv$lon - dist, rv$lat + dist, rv$lon + dist)
    })
  })
  targetBSId <- reactive({
    if(is.null(input$gotoKPI)) {
      return()
    }
    input$gotoKPI$id
  })
  tbl <- reactive({
    bsIds <- filter(bs, is.null(input$input.cities) | city %in% input$input.cities)
    targetId <- targetBSId()
    if(!is.null(targetId)) {
      bsIds <- bsIds[bsIds$id == targetId]
    }
    
    rawIds <- bsIds$id
    tbl_output <- kpi_output %>%
      filter(id %in% rawIds) %>%
      filter(is.null(input$input.tech) | technology %in% input$input.tech) %>%
      mutate(date = strftime(ISOdate(year, month, day), '%d/%m/%y'))
    
    if(!is.null(dtRange())) {
      dtRng <- dtRange()
      fromDt <- dtRng[1,]
      toDt <- dtRng[2,]
      
      tbl_output <- filter(tbl_output,
                           ((year > fromDt$year) | (year == fromDt$year & month > fromDt$month) | (year == fromDt$year & month == fromDt$month & day >= fromDt$day)) &
                             ((year < toDt$year) | (year == toDt$year & month < toDt$month) | (year == toDt$year & month == toDt$month & day <= toDt$day)))
    }
    tbl_output <- tbl_output[c('id', 'date', input$colvis)]
    tbl_output
  })
  output$`kpi_tbl` <- renderDataTable({
    tbl_output <- tbl()
    tbl_output <- tbl_output %>%
      mutate(Action = paste('<a class="go-map" href="" data-id="', id, '">Карта</a>', sep=""))
    tbl_output <- plyr::rename(tbl_output,
                               replace=c('date'='Дата', 'fail_rate'='Проц. сбоев', 'success_call_rate'='Проц. успешных вызовов', 'cell_avail'='Доступность', 'max_active_abon'='Макс. кол-во активных абон.','overload_rate'='Проц. перегрузки'),
                               warn_missing=FALSE)
    tbl_output
  }, escape=FALSE)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('report-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      tbl_output <- tbl()
      tbl_output <- plyr::rename(tbl_output,
                                 replace=c('date'='Дата', 'fail_rate'='Проц. сбоев', 'success_call_rate'='Проц. успешных вызовов', 'cell_avail'='Доступность', 'max_active_abon'='Макс. кол-во активных абон.','overload_rate'='Проц. перегрузки'),
                                 warn_missing=FALSE)
      write.csv(tbl_output, file)
    }
  )
  
  output$plots <- renderUI({
    if(is.null(input$colvis)) {
      return()
    }
    dtRng <- dtRange()
    plot_list <- lapply(1:length(input$colvis), function(i) {
      plotname <- paste('plot_', input$colvis[i], sep='')
      column(6, plotOutput(plotname, height=280, width=300))
    })
    fluidRow(do.call(tagList, plot_list))
  })
  observe({
    if(is.null(input$colvis)) {
      return()
    }
    max_plots <- length(input$colvis)
    by_day = group_by(tbl(), date)
    plot_data <- summarise_each(by_day,
              funs(mean), -date, -id)
    colors <- brewer.pal(max_plots, "Spectral")
    for(i in 1:max_plots) {
      local({
        local_i <- i
        plotname <- paste('plot_', input$colvis[i], sep='')
        plot_date <- plot_data[c('date', input$colvis[local_i])]
        xrange <- range(plot_date$date)
        yrange <- range(plot_date[[input$colvis[local_i]]])
        output[[plotname]] <- renderPlot({
          p <- ggplot(plot_data, aes_string(x='date', y=input$colvis[local_i]))
          p + geom_line(colour=colors[local_i], size=1, aes(group=1)) +
              scale_x_discrete(labels = abbreviate) + 
              xlab('Дата (по дням)') +
              ylab(KPIs[input$colvis[local_i]])
        })
      })
    }
  })
})


