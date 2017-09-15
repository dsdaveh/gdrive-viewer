library(tidyverse)
library(stringr)
library(googledrive)
library(googlesheets)
library(lubridate)
library(data.tree)
library(cowplot)
library(tictoc)
theme_set(theme_gray())

library(shiny)

theme_empty <- function() {
    theme_bw() %+replace%
        theme(
            line = element_blank(),
            rect = element_blank(),
            strip.text = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            plot.background = element_rect(fill="gray96", colour=NA)
        )
}

# https://stackoverflow.com/questions/4245196/do-not-ignore-case-in-sorting-character-strings
sortC <- function(...) {
    a <- Sys.getlocale("LC_COLLATE")
    on.exit(Sys.setlocale("LC_COLLATE", a))
    Sys.setlocale("LC_COLLATE", "C")
    sort(...)
}
sortClast <- function(x) {
    CAPS <- grep("^[A-Z]", x)
    c(sort(x[-CAPS]), sort(x[CAPS]))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Google Drive Viewer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         actionButton("authenticate", label = "Authenticate"), #TODO add GDrive icon
         dateRangeInput("date_filter", "Date Range", start = '2001-01-01', end = today()),
         numericInput("n_types", "Max MIME types", 6, min = 1, step = 1),
         radioButtons("view_type", "Plot Type", choices = c('Doc count', 'Doc size')),
         actionButton("trend", label = "Plot Trend")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    #Inputs:
    # view_type: size | count
    # since: date filter, default = NA
    # until: date filter, default = today
    # [alt] last_n_days: integer > 0 ... implicit since/until
    # [alt] last_n_docs: integer > 0 ... implicit since/until
    # n_types: integer > 0 ... top n_types are listed explicitly, the rest become 'Other'
    # ignore_types: character
    
    files <- reactive({ 
        cat('files()', input$authenticate, '\n')
        cached = FALSE #dev only
        cache_all <- '../all.rds'
        if (file.exists(cache_all) && cached) {
            files <- readRDS(cache_all)
        } else {
            cat('drive_find...')
            tic()
            files <- drive_find()
            saveRDS(files, cache_all)
            toc()
        }
        print(nrow(files))
        files %>% 
            drive_reveal(what = 'mime_type') %>%
            googledrive:::promote('createdTime') %>% 
            googledrive:::promote('quotaBytesUsed') %>% 
            mutate(q_size = as.numeric(quotaBytesUsed)) %>%
            arrange(createdTime) %>%
            mutate(create_date = as.Date(ymd_hms(createdTime)),
                   `Cumulative Doc Count` = row_number(createdTime),
                   `Cumulative File Size` = cumsum(q_size))
    })
        
    observeEvent(input$authenticate, {
        cat('authenticate\n')
        drive_auth(reset = TRUE, cache = FALSE)
        cat('set prog')
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Get file listing", value = 0.1)
        cat('auth files')
        files()
        progress$set(message = "Create Plot", value = 0.4)
        output$distPlot <- plot_trend
        Sys.sleep(5)
        progress$set(message = "Complete", value = 1)
    })
    
    plot_none <- renderPlot({
        ggplot(data.frame()) + 
            geom_text(aes(0,0), label = 'Click "Authenticate" to\n load Drive listing', size=10) + 
            theme_empty()
    })
    
    plot_trend <- renderPlot({
        cat('plot_trend:\n')
        
        files_cum <- files() 
        
        file_types <- files_cum %>%
            group_by(mime_type) %>%
            summarize(count = n(),
                      total_size = sum(q_size)) %>%
            arrange( -count)

        top_ftype <- input$n_types
        top_mime_types <- file_types %>%
            top_n(top_ftype, count) %>%
            select(mime_type) %>% .[[1]]

        files_otherized <- files_cum %>%
            mutate(mime_type = ifelse(mime_type %in% top_mime_types, mime_type, 'Other')) 
        
        cum_types <- files_otherized %>%
            mutate(create_date = as.Date(ymd_hms(createdTime))) %>%
            #filter(create_date > ymd('2017-01-01')) %>%
            group_by(create_date, mime_type) %>%
            summarize(count = n(),
                      total_size = sum(q_size)) %>%
            ungroup() %>%
            complete(create_date, mime_type, fill = list(count = 0, total_size = 0)) %>%
            group_by(mime_type) %>% arrange(create_date) %>%
            mutate( cum_count = cumsum(count),
                    cum_size = cumsum(total_size))
        cum_all <- cum_types %>% ungroup() %>%
            group_by(create_date) %>%
            summarize( day_count = sum(count),
                       day_size = sum(total_size)) %>%
            ungroup() %>% arrange(create_date) %>%
            mutate( count_all = cumsum(day_count),
                    size_all = cumsum(day_size))
        
        #todo ... this coule be done better with !!
        if (input$view_type == 'Doc count') {
            plot_cum <- cum_types %>%
                ggplot(aes(create_date, cum_count)) +
                geom_area(aes(fill = mime_type)) +
                geom_line(data = files_cum, aes(create_date, `Cumulative Doc Count`, group=1)) +
                geom_point(data = files_cum, aes(create_date, `Cumulative Doc Count`, group=1)) +
                ggtitle('Growth Trend by File Count')
        } else {
            plot_cum <- cum_types %>%
                ggplot(aes(create_date, cum_size)) +
                geom_point(data = files_cum, aes(create_date, `Cumulative File Size`, group=1)) +
                geom_area(aes(fill = mime_type)) +
                geom_line(data = files_cum, aes(create_date, `Cumulative File Size`, group=1)) + 
                ggtitle('Growth Trend by File Size')
        }
        
        plot_cum   
    })
    
    output$distPlot <- plot_none
}

# Run the application 
shinyApp(ui = ui, server = server)

