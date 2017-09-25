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
library(shinydashboard)

sessionInfo()

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

tiny_mime <- function(x) {
    x %>%
        str_replace('^application/', 'app../') %>%
        str_replace('/vnd.google-apps.', '/..google..')
}

#link github issue shorthand
lgi <- function(i) sprintf("https://github.com/dsdaveh/gdrive-viewer/issues/%d", i)

#helpers
is_me <- function(u) is.null(u) || u$emailAddress == 'o9e4g9@u.northwestern.edu' #'dhurst79@gmail.com'
ggsub_me <- 'This is a snapshot of my Drive. To view your own, click "Authenticate"\nNote: Authentication is done through Google, and no credentials are cached on the server'

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "Google Drive Viewer", titleWidth = 200),
    
    dashboardSidebar(
        actionButton("authenticate", label = "Authenticate"), #TODO add GDrive icon
        dateRangeInput("date_filter", "Date Range", start = '2001-01-01', end = today()),
        numericInput("n_types", "Max MIME types", 6, min = 1, step = 1),
        radioButtons("view_type", "Plot Type", choices = c('Doc count', 'Doc size')),
        
        withTags({
            div(class="header", checked=NA,
                p("Download and run locally from",
                  a(href="https://github.com/dsdaveh/gdrive-viewer", "GitHub"))
            )
        }),
        withTags({
            div(class="header", checked=NA,
                h4("Project Construction Notes"),
                li(a("Authenticate not running on server", href = lgi(1))),
                li(tags$del(a("Date range not functional", href = lgi(2)))),
                li(tags$del(a("Add MIME type selection", href = lgi(4)))),
                li(a("Add Sharing info", href = lgi(5))),
                li(a("Optimize reactivity", href = lgi(8))),
                li(span("Other functionality coming... Have suggestion?",
                        a("Create an issue", href = "https://github.com/dsdaveh/gdrive-viewer/issues")))
            )
        })
    ),
        
    dashboardBody(
        fluidRow( 
            valueBoxOutput('fileCount'),
            valueBoxOutput('folderCount'),
            valueBoxOutput('quotaSize')),
        fluidRow(
            radioButtons("filterValues", "Filter Summary by Selected MIME Types", inline = TRUE, 
                         choices = c('Yes', 'No'), selected = 'No')),
        fluidRow(
            box( width = 12, 
                 plotOutput("distPlot"),
                 checkboxGroupInput("ignore_mime", label = "Ignore MIME types", choices = '...waiting for plot...', inline = TRUE ),
                 actionButton("update_trend", label = "Update Trend Plot"))
        )
    )
    
)

server <- function(input, output, session) {
   
    #Inputs:
    # view_type: size | count
    # since: date filter, default = NA
    # until: date filter, default = today
    # [alt] last_n_days: integer > 0 ... implicit since/until
    # [alt] last_n_docs: integer > 0 ... implicit since/until
    # n_types: integer > 0 ... top n_types are listed explicitly, the rest become 'Other'
    # ignore_mime: character
    
    drive_deauth(verbose = FALSE)
    user <- drive_user()
    
    #files() gets the drive file metadata (from googledrive or cache)
    find_results <- reactive({
        cat('find_results()', input$authenticate, '\n')
        cached = is_me(user)
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
        files
    })
    
    files <- reactive({ 
        cat('files()', input$authenticate, '\n')
        
        #add additional metadata
        find_results() %>% 
            drive_reveal(what = 'mime_type') %>%
            googledrive:::promote('createdTime') %>% 
            googledrive:::promote('quotaBytesUsed') %>% 
            mutate(q_size = as.numeric(quotaBytesUsed)) %>%
            arrange(createdTime) %>%
            mutate(create_date = as.Date(ymd_hms(createdTime))) %>%
            filter(create_date >= ymd(format(input$date_filter[1])),
                   create_date <= ymd(format(input$date_filter[2]))) %>%
            mutate(`Cumulative Doc Count` = row_number(createdTime),
                   `Cumulative File Size` = cumsum(q_size))
    })
    
    files_mime_filter <- reactive({
        cat('files_mime_filter()\n')
        
        mime_filter <- ''
        if (input$filterValues == 'Yes') mime_filter <- input$ignore_mime

        files() %>%
            mutate(mime_type = ifelse(mime_type %in% mime_types(), mime_type, '__Other__')) %>%
            filter(! mime_type %in% mime_filter) %>%
            mutate(`Cumulative Doc Count` = row_number(createdTime),
                   `Cumulative File Size` = cumsum(q_size))
    })
        
    mime_types <- reactive({
        cat('mime_types()\n')
        
        file_types <- files() %>%
            group_by(mime_type) %>%
            summarize(count = n(),
                      total_size = sum(q_size)) %>%
            arrange( -count)
        
        top_ftype <- input$n_types
        top_mime_types <- file_types %>%
            top_n(top_ftype, count) %>%
            select(mime_type) %>% .[[1]]
        
        #add __Other__ if appropriate
        top_mime_types
    })
        
    observeEvent(input$authenticate, {
        cat('authenticate\n')
        drive_auth(reset = TRUE, cache = FALSE) #, use_oob = TRUE)
        user <<- drive_user()

        progress <- shiny::Progress$new()
        on.exit(progress$close())
        
        progress$set(message = "Get file listing", value = 0.1)
        files()
        progress$set(message = "Create Plot", value = 0.4)
        output$distPlot <- plot_trend
        progress$set(message = "Complete", value = 1)
    })
    
    plot_none <- renderPlot({
        ggplot(data.frame()) + 
            geom_text(aes(0,0), label = 'Click "Authenticate" to\n load Drive listing', size=10) + 
            theme_empty()
    })
    
    plot_trend <- renderPlot({
        trigger_update <- input$update_trend
        cat('plot_trend:\n')
        ggsub_line <- ifelse(is_me(user), ggsub_me, sprintf("Logged in %s", user$emailAddress))
        
        progress_plot <- shiny::Progress$new()
        on.exit(progress_plot$close())
        
        progress_plot$set(message = "Get file listing", value = 0.1)
        files_cum <- files() 

        progress_plot$set(message = "Analyze MIME types", value = 0.5)
        files_otherized <- files_cum %>%
            mutate(mime_type = ifelse(mime_type %in% mime_types(), mime_type, '__Other__')) %>%
            filter(! mime_type %in% input$ignore_mime) %>%
            mutate(`Cumulative Doc Count` = row_number(createdTime),
                   `Cumulative File Size` = cumsum(q_size))
        
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
                geom_line(data = files_otherized, aes(create_date, `Cumulative Doc Count`, group=1)) +
                geom_point(data = files_otherized, aes(create_date, `Cumulative Doc Count`, group=1)) +
                ggtitle('Growth Trend by File Count', subtitle = ggsub_line)
        } else {
            plot_cum <- cum_types %>%
                ggplot(aes(create_date, cum_size)) +
                geom_point(data = files_otherized, aes(create_date, `Cumulative File Size`, group=1)) +
                geom_area(aes(fill = mime_type)) +
                geom_line(data = files_otherized, aes(create_date, `Cumulative File Size`, group=1)) + 
                ggtitle('Growth Trend by File Size', subtitle = ggsub_line)
        }
        
        plot_cum   
    })
    
    #TODO remove
    # ignore_mimes <- reactive({
    #     cat(paste(input$ignore_mime, '\n'))
    #     input$ignore_mime
    # })
    
    observe({
        updateCheckboxGroupInput(session, "ignore_mime", label = "Ignore",
                                 choices = c('__Other__', mime_types()),
                                 selected = input$ignore_mime, inline = TRUE )
        
    })
    
    output$fileCount <- renderValueBox({
        valueBox(files() %>% nrow(), "File Count")
    })
    output$folderCount <- renderValueBox({
        valueBox(files() %>% filter(str_detect(mime_type, 'google-apps.folder')) %>% nrow(), "Folder Count")
    })
    output$quotaSize <- renderValueBox({
        qsize <- sum(files_mime_filter()$q_size)
        print(c('qsize', qsize))
        if (qsize < 1e5) {
            valueBox(sprintf('%g KB', round(qsize / 1e3, 1)), "Size")
        } else if (qsize < 1e8) {
            valueBox(sprintf('%g MB', round(qsize / 1e6, 1)), "Size")
        } else {
            valueBox(sprintf('%g GB', round(qsize / 1e9, 1)), "Size")
        }
    })
    output$distPlot <- plot_trend
}


# Run the application 
shinyApp(ui = ui, server = server)

