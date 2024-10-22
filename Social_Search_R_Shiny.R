##### SOCIAL SEARCH R SHINY APPLICATION #####
##### CODE WRITTEN AND MAINTAINED BY JOSEPH MENA #####

library(shiny)
library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)
library(shinythemes)
library(shinycssloaders)
library(ggplot2)
library(tools)
library(tm)
library(wordcloud)
library(wordcloud2)
library(memoise)
library(colourpicker)

ui <- navbarPage(theme=shinytheme("flatly"), strong("Social Search"),
                 tabPanel("Themes",
                
                # Application title
                titlePanel(strong("Trending Themes in Online Conversation")),
                
                
                sidebarLayout(
                  sidebarPanel(
                    
                    helpText("For a successful marketing mix, knowing the thematic categories in which online users post the most can be a highly valuable resource.")
                    ,
                    
                    helpText("Simply enter a retailer or a brand to see the associated conversation topics and click search!")
                    ,
              
                    textInput("dgorcompetitor", "Search Retailer or Brand:", ""),

                    dateRangeInput('dateRange1',
                                   label = 'Choose Date Range:',
                                   start = Sys.Date() - 365, end = Sys.Date(), startview = "decade", min    = "2009-01-01",
                                   max    = Sys.Date()  
                    ),
                    actionButton("button", "Search")
              
                    
                  ),
                  
                  mainPanel(
                    
                    conditionalPanel(
                      condition = "input.button > 0", radioButtons(inputId = "radiobuttons", "Select type of plot:", c("Barchart", "Wordcloud", "Table", "Download Link"), selected = "Barchart", inline = TRUE)
                    ),

                     
                    conditionalPanel(
                      condition = "input.radiobuttons == 'Barchart'", plotOutput("bars", width = "720px", height="450px")),
                    conditionalPanel(
                      condition = "input.radiobuttons == 'Table'", tableOutput("table")),
                    conditionalPanel(
                      condition = "input.radiobuttons == 'Wordcloud'", wordcloud2Output("cloud")),
                    conditionalPanel(
                      condition = "input.button > 0 && input.radiobuttons == 'Download Link'", strong(helpText("Click below to download CSV data of top themes associated with your search which can then be opened with Excel")), downloadButton("downloadData1", "Download CSV"))
                    
                  )
                  
                )
                
),

        tabPanel("Brands",
         
         # Application title
         titlePanel(strong("Trending Brands in Online Conversation")),
         
         
         sidebarLayout(
           sidebarPanel(
             
             helpText("For successful promotions, knowing which brands are buzzing in word of mouth is key.")
             ,
             
             helpText("Simply enter a retailer or brand to see the brands most associated with them in social media and click search!")
             ,

             textInput("dgorcompetitor2", "Search Retailer or Brand:", ""),
             #selectInput("dateselect2", "Select time window:", choices=c("Past 30 days" = "1%20month%20ago", "Past 3 months" = "3%20months%20ago", "Past 6 months" = "6%20months%20ago", "Past Year" = "1%20year%20ago", "Past 2 years" = "2%20years%20ago", "Past 3 years" = "3%20years%20ago"), selected = ("Past Year" = "1%20year%20ago")),
             dateRangeInput('dateRange2',
                            label = 'Choose Date Range:',
                            start = Sys.Date() - 365, end = Sys.Date(), startview = "decade", min    = "2009-01-01",
                            max    = Sys.Date()
             ),
             actionButton("button2", "Search")

             
           ),
           
           mainPanel(
             
             conditionalPanel(
               condition = "input.button2 > 0", radioButtons(inputId = "radiobuttons2", "Select type of plot:", c("Barchart", "Wordcloud", "Table", "Download Link"), selected = "Barchart", inline = TRUE)
             ),
             
             # output for barchart of most discussed brands with loading animation
             conditionalPanel(
               condition = "input.radiobuttons2 == 'Barchart'", plotOutput("bars2", width = "720px", height="450px")), #%>% withSpinner(color="#37384b")
             conditionalPanel(
               condition = "input.radiobuttons2 == 'Table'", tableOutput("table2")),
             conditionalPanel(
               condition = "input.radiobuttons2 == 'Wordcloud'", wordcloud2Output("cloud2")), #%>% withSpinner(color="#37384b")
             conditionalPanel(
               condition = "input.button2 > 0 && input.radiobuttons2 == 'Download Link'", strong(helpText("Click below to download CSV data of the top brands associated with your search which can then be opened with Excel")), downloadButton("downloadData2", "Download CSV"))
             
             
           )
           
         )
         
),


        tabPanel("Topic Keywords",
         
         # Application title
         titlePanel(strong("Trending Topic Keywords in Online Conversation")),
         
         
         sidebarLayout(
           sidebarPanel(
             
             helpText("For successful marketing strategy, knowing which topic keywords are most used among your audience is key to best reaching them.")
             ,
             
             #helpText("Discover the themes being most discussed across social media by users who make posts mentioning whichever topic you wish.  This will show relevant interests associated with your search term and unlock creative potential for advertising and marketing efforts to best reach your audience.")
             #,
             helpText("Simply enter a retailer or a brand to see the topic keywords associated with them and click search!")
             ,
    
             textInput("dgorcompetitor3", "Search Retailer or Brand:", ""),
             #helpText("Optionally, you can also search for a brand within this retailer, or simply search the brand by itself"),
             #textInput("brandselection3", "and/or Search Brand:", ""),
             dateRangeInput('dateRange3',
                            label = 'Choose Date Range:',
                            start = Sys.Date() - 365, end = Sys.Date(), startview = "decade", min    = "2009-01-01",
                            max    = Sys.Date()
             ),
             #selectInput("dateselect3", "Select time window:", choices=c("Past 30 days" = "1%20month%20ago", "Past 3 months" = "3%20months%20ago", "Past 6 months" = "6%20months%20ago", "Past Year" = "1%20year%20ago", "Past 2 years" = "2%20years%20ago", "Past 3 years" = "3%20years%20ago"), selected = ("Past Year" = "1%20year%20ago")),
             actionButton("button3", "Search")

          
           ),
           
           mainPanel(
             
             conditionalPanel(
               condition = "input.button3 > 0", radioButtons(inputId = "radiobuttons3", "Select type of plot:", c("Barchart", "Wordcloud", "Table", "Download Link"), selected = "Barchart", inline = TRUE)
             ),
             
             # output for barchart of most discussed brands with loading animation
             conditionalPanel(
               condition = "input.radiobuttons3 == 'Barchart'", plotOutput("bars3", width = "720px", height="450px")), #%>% withSpinner(color="#37384b")
             conditionalPanel(
               condition = "input.radiobuttons3 == 'Table'", tableOutput("table3")),
             conditionalPanel(
               condition = "input.radiobuttons3 == 'Wordcloud'", wordcloud2Output("cloud3")), #%>% withSpinner(color="#37384b")
             conditionalPanel(
               condition = "input.button3 > 0 && input.radiobuttons3 == 'Download Link'", strong(helpText("Click below to download CSV data of top topic keywords associated with your search which can then be opened with Excel")), downloadButton("downloadData3", "Download CSV"))
             
            
           )
           
         )
         
)

)  


server <- function(input,output,session)  {

#themes/ source interests    
  observeEvent(input$button,{
    
    showModal(modalDialog("Loading data...This shouldn't take long...", footer=NULL))
    
    #data derived from API call
    
    #components of API request to concatenate, including authentication and parameters
    base <- "https://atlas.infegy.com/api/v2/"
    endpoint <- "interests"
    api_key <- ### THIRD PARTY PRIVATE KEY ###

    query <- reactive(gsub(" ","%20", input$dgorcompetitor))

    start_date <- reactive(input$dateRange1[1])
    
    end_date <- reactive(input$dateRange1[2])
    
    #end_date <- "end_date=now"
    countries <- "countries=US"
    limit <- "limit=1000"
    
    #full request
    call1 <- paste0(base,endpoint,"?",api_key,"&","query=",query(),"&","start_date=",start_date(),"&","end_date=",end_date(),"&",countries,"&",limit)
    
    #use httr's GET function to request infegy API
    get_query_source_interests <- GET(call1)
    
    #use httr's content function to deserialize/convert raw data from call into JSON format
    get_query_source_interests_text <- httr::content(get_query_source_interests, "text")
    
    #parse the JSON using the jsonlite package
    get_query_source_interests_json <- fromJSON(get_query_source_interests_text, flatten = TRUE)
    
    #converting output component of source_interests into data frame
    get_query_source_interests_df <- as.data.frame(get_query_source_interests_json$output)   
    
    library(tidyverse)
    library(magrittr)
    
    
    get_query_cleaned_source_interests_df <- get_query_source_interests_df %>%
      dplyr::mutate(name = gsub(".*\\|","", name)) %>%
      filter(distribution <= .4) %>%
      filter(ratio >= 1.3) %>%
      slice(1:30) %>%
      select(name,sources) %>%
      filter(!name %in% c('Crime','Disasters','Celebrity Deaths','Celebrity Scandal','Divorce','Crime and Mystery Movies','Bereavement','Atheism')) 
    
      
    library(ggplot2)
    
    
    color_range_number <- length(unique(get_query_cleaned_source_interests_df$name))
    color <- colorRampPalette(brewer.pal(9,"GnBu")[5:9])(color_range_number)[factor(get_query_cleaned_source_interests_df$name)]
    
    
    output$cloud <- renderWordcloud2({
      # wordcloud2(demoFreqC, size=input$size)
      wordcloud2(get_query_cleaned_source_interests_df, rotateRatio = 0, color = color, shape = 'circle')
    })
    
    
    output$table <- renderTable(get_query_cleaned_source_interests_df)
    
    
    output$bars <-
      renderPlot(ggplot(get_query_cleaned_source_interests_df%>%head(18), aes(x = reorder(name, sources), y = sources))+
                   geom_col(fill = "lightblue")+
                   coord_flip()+
                   xlab("Theme")+
                   ylab("# of Users Discussing Theme")+
                   ggtitle(paste("Online Users Discussing",tools::toTitleCase(input$dgorcompetitor),"Are Also Most Interested in These Themes"))
      )
    
    
    output$downloadData1 <- downloadHandler(
      filename = function() {
        paste(input$dgorcompetitor,"_top_themes_in_social_search", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(get_query_cleaned_source_interests_df, file, row.names = FALSE)
      }
    )
    
    removeModal()
    
  })
  
  
#brands  
  observeEvent(input$button2,{
    
    showModal(modalDialog("Loading data...This shouldn't take long...", footer=NULL))
    
    
    #data derived from API call
    
    #components of API request to concatenate, including authentication and parameters
    base <- "https://atlas.infegy.com/api/v2/"
    endpoint <- "entities"
    api_key <- ### THIRD PARTY PRIVATE KEY ###
    
    query2 <- reactive(gsub(" ","%20", input$dgorcompetitor2))
  
    start_date2 <- reactive(input$dateRange2[1])
    
    end_date2 <- reactive(input$dateRange2[2])
    
    #end_date <- "end_date=now"
    countries <- "countries=US"
    limit <- "limit=1000"
    
    #full request
    call2 <- paste0(base,endpoint,"?",api_key,"&","query=",query2(),"&","start_date=",start_date2(),"&","end_date=",end_date2(),"&",countries,"&",limit)
    
    #use httr's GET function to request infegy API
    get_query_entities <- GET(call2)
    
    #use httr's content function to deserialize/convert raw data from call into JSON format
    get_query_entities_text <- httr::content(get_query_entities, "text")
    
    #parse the JSON using the jsonlite package
    get_query_entities_json <- fromJSON(get_query_entities_text, flatten = TRUE)
    
    #converting output component of entities into data frame
    get_query_entities_df <- as.data.frame(get_query_entities_json$output)   
    
    library(tidyverse)
    library(magrittr)
    
    
    get_query_brand_entities_df <- get_query_entities_df %>%
      filter(entity_type=="brand") %>%
      select(name,documents) %>%
      filter(name != paste(tools::toTitleCase(input$dgorcompetitor2)))
     
    
    
    library(ggplot2)
    
    
    color_range_number <- length(unique(get_query_brand_entities_df$name))
    color <- colorRampPalette(brewer.pal(9,"GnBu")[5:9])(color_range_number)[factor(get_query_brand_entities_df$name)]
    
    
    output$cloud2 <- renderWordcloud2({
      # wordcloud2(demoFreqC, size=input$size)
      wordcloud2(get_query_brand_entities_df, rotateRatio = 0, color = color, shape = 'circle')
    })
    
    
    output$table2 <- renderTable(get_query_brand_entities_df)
    
    
    output$bars2 <-
      renderPlot(ggplot(get_query_brand_entities_df%>%head(18), aes(x = reorder(name, documents), y = documents))+
                   geom_col(fill = "lightblue")+
                   coord_flip()+
                   xlab("Brand")+
                   ylab("# of Users Discussing Brand")+
                   ggtitle(paste("Online Users Discussing",tools::toTitleCase(input$dgorcompetitor2),"Are Also Most Interested in These Brands"))
      )
    
    
    output$downloadData2 <- downloadHandler(
      filename = function() {
        paste(input$dgorcompetitor2,"_top_associated_brands_in_social_search", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(get_query_brand_entities_df, file, row.names = FALSE)
      }
    )
    

    removeModal()
    
  })
  
  
#positive topic keywords
  observeEvent(input$button3, {
    
    showModal(modalDialog("Loading data...This shouldn't take long...", footer=NULL))
    
    
    #data derived from API call
    
    #components of API request to concatenate, including authentication and parameters
    base <- "https://atlas.infegy.com/api/v2/"
    endpoint <- "positive-topics"
    api_key <- ### THIRD PARTY PRIVATE KEY ###
    
    query3 <- reactive(gsub(" ","%20", input$dgorcompetitor3))
    
    start_date3 <- reactive(input$dateRange3[1])
    
    end_date3 <- reactive(input$dateRange3[2])
    countries <- "countries=US"
    limit <- "limit=5000"
    
    #full request
    call3 <- paste0(base,endpoint,"?",api_key,"&","query=",query3(),"&","start_date=",start_date3(),"&","end_date=",end_date3(),"&",countries,"&",limit)
    
    #use httr's GET function to request infegy API
    get_query_positive_topics <- GET(call3)
    
    #use httr's content function to deserialize/convert raw data from call into JSON format
    get_query_positive_topics_text <- httr::content(get_query_positive_topics, "text")
    
    #parse the JSON using the jsonlite package
    get_query_positive_topics_json <- fromJSON(get_query_positive_topics_text, flatten = TRUE)
    
    #converting output component of positive topics into data frame
    get_query_positive_topics_df <- as.data.frame(get_query_positive_topics_json$output)   
    
    library(tidyverse)
    library(magrittr)
    
    get_query_cleaned_positive_topics_df <- get_query_positive_topics_df %>%
      filter(positive_intensity >= 20) %>%
      arrange(desc(positive_documents)) %>%
      select(topic,positive_documents)
      
    
    library(ggplot2)
    
    
    color_range_number <- length(unique(get_query_cleaned_positive_topics_df$topic))
    color <- colorRampPalette(brewer.pal(9,"GnBu")[5:9])(color_range_number)[factor(get_query_cleaned_positive_topics_df$topic)]
    
    
    output$cloud3 <- renderWordcloud2({
      # wordcloud2(demoFreqC, size=input$size)
      wordcloud2(get_query_cleaned_positive_topics_df, rotateRatio = 0, color = color, shape = 'circle')
    })
    
    
    
    output$table3 <- renderTable(get_query_cleaned_positive_topics_df)

    
    
    output$bars3 <-
      renderPlot(ggplot(get_query_cleaned_positive_topics_df%>%head(18), aes(x = reorder(topic, positive_documents), y = positive_documents))+
                   geom_col(fill = "lightblue")+
                   coord_flip()+
                   xlab("Topic Keyword")+
                   ylab("# of Users Discussing Topic Keyword")+
                   ggtitle(paste("Online Users Discussing",tools::toTitleCase(input$dgorcompetitor3),"Are Also Most Interested in These Topic Keywords"))
      ) 
    
    
    output$downloadData3 <- downloadHandler(
      filename = function() {
        paste(input$dgorcompetitor3,"_top_topics_in_social_search", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(get_query_cleaned_positive_topics_df, file, row.names = FALSE)
      }
    )
    
    removeModal()
    
  })
  
  
  
}   



shinyApp(ui = ui, server = server)


#remove(list = ls())

