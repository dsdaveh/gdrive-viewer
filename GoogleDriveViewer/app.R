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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Google Drive Viewer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         actionButton("authenticate", label = "Authenticate"), #TODO add GDrive icon
         dateRangeInput("date_filter", "Date Range", end = today()),
         numericInput("n_types", "Max MIME types", 10, min = 1, step = 1),
         radioButtons("ignore_types", "Plot variable", choices = c('Doc count', 'Doc size'))
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
    
    # files_cum <- files %>% 
    #     arrange(createdTime) %>%
    #     mutate(create_date = as.Date(ymd_hms(createdTime)),
    #            `Cumulative Doc Count` = row_number(createdTime),
    #            `Cumulative File Size` = cumsum(q_size)) 
    # 
    # 
    # cum_types <- files_otherized %>% 
    #     mutate(create_date = as.Date(ymd_hms(createdTime))) %>%
    #     #filter(create_date > ymd('2017-01-01')) %>%
    #     group_by(create_date, mime_type) %>%
    #     summarize(count = n(),
    #               total_size = sum(q_size)) %>% 
    #     ungroup() %>%
    #     complete(create_date, mime_type, fill = list(count = 0, total_size = 0)) %>%
    #     group_by(mime_type) %>% arrange(create_date) %>%
    #     mutate( cum_count = cumsum(count),
    #             cum_size = cumsum(total_size)) 
    # cum_all <- cum_types %>% ungroup() %>%
    #     group_by(create_date) %>%
    #     summarize( day_count = sum(count),  
    #                day_size = sum(total_size)) %>%
    #     ungroup() %>% arrange(create_date) %>%
    #     mutate( count_all = cumsum(day_count),
    #             size_all = cumsum(day_size))
    # 
    # plot_cum_counts <- cum_types %>%
    #     ggplot(aes(create_date, cum_count)) +
    #     geom_area(aes(fill = mime_type)) +
    #     geom_line(data = files_cum, aes(create_date, `Cumulative Doc Count`, group=1)) + 
    #     geom_point(data = files_cum, aes(create_date, `Cumulative Doc Count`, group=1))
    # plot_cum_counts
    
   output$distPlot <- renderPlot({

      # draw the histogram with the specified number of bins
      hist(rnorm(1:100))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

