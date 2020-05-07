library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(shiny)
#install.packages('text2vec')
library(text2vec)

#install.packages("textmineR")
library(textmineR)
library(ggplot2)
library(tidytext)

library(tm)
#install.packages("textmineR")
library(textmineR)
#install.packages("qdap")
#library(qdap)



# The list of valid books
states <<- list("New York" = "tweets_NY",
                "New Jersey" = "tweets_NJ",
                "Massachusetts" = "tweets_MA",
                "Illinois" = "tweets_IL",
                "California" = "tweets_CA",
                "Pennsylvania" = "tweets_PA",
                "Michigan" = "tweets_MI",
                "Florida" = "tweets_FL",
                "Texas" = "tweets_TX",
                "Connecticut" = "tweets_CT",
                "Georgia" = "tweets_GA",
                "Louisiana" = "tweets_LA",
                "Maryland" = "tweets_MD",
                "Indiana" = "tweets_IN",
                "Ohio" = "tweets_OH",
                "Virginia" = "tweets_VA",
                "Colorado" = "tweets_CO",
                "Washington" = "tweets_WA",
                "Tennessee" = "tweets_TN",
                "North Carolina" = "tweets_NC",
                "Iowa" = "tweets_IA",
                "Arizona" = "tweets_AZ",
                "Missouri" = "tweets_MO",
                "Wisconsin" = "tweets_WI")



getTermMatrix <- memoise(function(state) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(state %in% states))
    stop("Unknown state")
  
  
  #tweets <- eval(parse(text=sprintf("%s", state)))
  tweets <- read.csv(sprintf("./%s.csv", state))
  
  tweets_text <- tibble(tweet = 1:nrow(tweets), text = tweets$text)
  
  stopwords2 <- data_frame(word=c("coronavirus","covid","wont","im","didnt","dont","ive","doesnt"))
  
  tweets_text$text <- sub("RT.*:", "", tweets_text$text)
  tweets_text$text <- sub("@.* ", "", tweets_text$text)
  tweets_text$text <- gsub(" ?(f|ht)(tp)(s?)(://t.co)[|/](.*)", "", tweets_text$text)
  
  #tweets_text$text <- tolower(tweets_text$text)
  #tweets_text$text <-replace_abbreviation(tweets_text$text)
  
  #tweets_text$text <-replace_contraction(tweets_text$text)
  
  text_cleaning_tokens <- tweets_text %>% 
    tidytext::unnest_tokens(word, text)
  text_cleaning_tokens$word <- gsub('[[:digit:]]+', '', text_cleaning_tokens$word)
  text_cleaning_tokens$word <- gsub('[[:punct:]]+', '', text_cleaning_tokens$word)
  text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>% 
    anti_join(stop_words)%>% 
    anti_join(stopwords2)
  
  tokens <- text_cleaning_tokens %>% filter(!(word==""))
  tokens <- tokens %>% mutate(ind = row_number())
  tokens <- tokens %>% group_by(tweet) %>% mutate(ind = row_number()) %>%
    tidyr::spread(key = ind, value = word)
  tokens [is.na(tokens)] <- ""
  tokens <- tidyr::unite(tokens, text,-tweet,sep =" " )
  tokens$text <- trimws(tokens$text)
  
  
  dtm <- CreateDtm(tokens$text, 
                   doc_names = tokens$tweet, 
                   ngram_window = c(1,1))
  #explore the basic frequency
  tf <- TermDocFreq(dtm = dtm)
  original_tf <- tf %>% select(term, term_freq,doc_freq)
  rownames(original_tf) <- 1:nrow(original_tf)
  # Eliminate words appearing less than 2 times or in more than half of the
  # documents
  vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
  later_tf <- original_tf %>% filter(tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2)
  later_tf
  #dtm = dtm
  
  #m = as.matrix(dtm)
  
  #sort(rowSums(m), decreasing = TRUE)
})


ui <- fluidPage(
  titlePanel("COVID-19 Tweets Word Cloud for Different States"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a state:",
                  choices = states), ##########需要在global中set list
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 100,  value = 10)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    #v <- terms()
    #wordcloud_rep(v$term, v$term_freq, scale=c(4,1),
    # min.freq = input$freq, max.words=input$max,
    #  colors=brewer.pal(8, "Dark2"))
    wordcloud_rep(terms()$term, terms()$term_freq, scale = c(4,1),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}

shinyApp(ui = ui, server = server)
