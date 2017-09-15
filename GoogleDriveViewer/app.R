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

user <- drive_user()
is_me <- function() is.null(user) || user$emailAddress == 'dhurst79@gmail.com'
ggsub_me <- 'This is a snapshot of my Drive. To view your own, click "Authenticate"\nNote: Authentication is done through Google, and no credentials are cached on the server'

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Google Drive Viewer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
       sidebarPanel(
           wellPanel(
               actionButton("authenticate", label = "Authenticate"), #TODO add GDrive icon
               dateRangeInput("date_filter", "Date Range", start = '2001-01-01', end = today()),
               numericInput("n_types", "Max MIME types", 6, min = 1, step = 1),
               radioButtons("view_type", "Plot Type", choices = c('Doc count', 'Doc size')),
               actionButton("trend", label = "Plot Trend")),
           wellPanel(
               withTags({
                   div(class="header", checked=NA,
                       p("Download and run locally from",
                       a(href="https://github.com/dsdaveh/gdrive-viewer", "GitHub"))
                   )
               })),
           wellPanel(
               withTags({
                   div(class="header", checked=NA,
                       h4("Project Construction Notes"),
                       li("Authenticate not running on server"),
                       li("Date range not functional"),
                       li("Other functionality coming...")
                   )
               }))
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
        cached = is_me()
        cache_all <- 'all.rds'
        if (cached) {
            files <- readRDS(cache_all)
        } else {
            cat('drive_find...')
            tic()
            files <- drive_find()
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
        user <- drive_user()

        progress <- shiny::Progress$new()
        on.exit(progress$close())
        
        progress$set(message = "Get file listing", value = 0.1)
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
        ggsub_line <- ifelse(is_me(), ggsub_me, "")
        
        progress_plot <- shiny::Progress$new()
        on.exit(progress_plot$close())
        
        progress_plot$set(message = "Get file listing", value = 0.1)
        files_cum <- files() 
        
        progress_plot$set(message = "Analyze MIME types", value = 0.5)
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
        
        progress_plot$set(message = "Prepare plot data", value = 0.7)
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
        
        progress_plot$set(message = "Plot data", value = 0.7)
        #todo ... this coule be done better with !!
        if (input$view_type == 'Doc count') {
            plot_cum <- cum_types %>%
                ggplot(aes(create_date, cum_count)) +
                geom_area(aes(fill = mime_type)) +
                geom_line(data = files_cum, aes(create_date, `Cumulative Doc Count`, group=1)) +
                geom_point(data = files_cum, aes(create_date, `Cumulative Doc Count`, group=1)) +
                ggtitle('Growth Trend by File Count', subtitle = ggsub_line)
        } else {
            plot_cum <- cum_types %>%
                ggplot(aes(create_date, cum_size)) +
                geom_point(data = files_cum, aes(create_date, `Cumulative File Size`, group=1)) +
                geom_area(aes(fill = mime_type)) +
                geom_line(data = files_cum, aes(create_date, `Cumulative File Size`, group=1)) + 
                ggtitle('Growth Trend by File Size', subtitle = ggsub_line)
        }
        
        plot_cum   
    })
    
    output$distPlot <- plot_trend
}

# Run the application 
shinyApp(ui = ui, server = server)

