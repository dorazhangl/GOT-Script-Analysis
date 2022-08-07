library(shiny)
library(tidyverse)
library(kableExtra)
library(readr)
library(shinythemes)
library(RSQLite)
library(tm)
library(SnowballC)
library(wordcloud)
library(syuzhet)
library(quanteda)
library(stringr)
library(plotly)


####################################
####################################
####
####  UI
####
####################################
####################################

ui <- fluidPage(
  theme=shinytheme("darkly"),
  h1('Game of Thrones Script Analysis'),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("character",
                  label="Choose Character",
                  choices=c("Arya Stark","Bran Stark","Brienne","Bronn","Catelyn Stark",
                            "Cersei Lannister","Daenerys Targaryen","Davos","Eddard Stark",
                            "Jaime Lannister","Joffrey Lannister","Jon Snow","Jorah Mormont",
                            "Margaery Tyrell","Petyr Baelish","Ramsay Bolton","Robb Stark",
                            "Sam","Sansa Stark","Stannis Baratheon","Theon Greyjoy",
                            "Tyrion Lannister","Tywin Lannister","Varys"),
                  multiple = FALSE),
      selectInput("season",
                  label="Choose Season",
                  choices=c("Season 1","Season 2","Season 3","Season 4","Season 5",
                            "Season 6","Season 7","Season 8"),
                  multiple = FALSE)
    ),  
    mainPanel(h3("Sentiment Analysis"),
              h5("Sentiment score is calculated as how many words associated with each emotion per line in the script using the NRC Emotion Lexicon."),
              plotlyOutput('plot1'),
              h3("Most Spoken Words"),
              plotOutput('plot2'))
    
  ))

####################################
####################################
####
####  SERVER
####
####################################
####################################

server <- function(input,output) {
  script <- read_csv("Game_of_Thrones_Script.csv")
  script$Episode<-sub('Episode','',script$Episode)
  class(script$Episode) = "integer"
  script$Name<-str_to_title(script$Name)
  df<-reactive({script%>%
      filter(Name == input$character,Season==input$season)})
  
  d<-reactive({get_nrc_sentiment(df()$Sentence)})
  
  plot_1<-reactive({cbind(df(),d())%>%
      group_by(Episode,`Episode Title`)%>%
      summarise(Anger=mean(anger),Anticipation=mean(anticipation), Disgust=mean(disgust),
                Fear=mean(fear), Joy=mean(joy), Sadness=mean(sadness), Surprise=mean(surprise),
                Trust=mean(trust))%>%
      pivot_longer(!c(Episode,`Episode Title`),names_to="Sentiment",values_to = "Score")%>%
      filter(Score>0)%>%
      ggplot(aes(Episode,Score,color=Sentiment, group=Sentiment))+
      scale_x_continuous(breaks=seq(min(df()$Episode),max(df()$Episode),by=1))+
      geom_line()+
      geom_point(aes(text=paste("Episode Title:",`Episode Title`,
                                "<br>Sentient:",Sentiment,
                                "<br>Score:",Score)))})
  plotly_1<-reactive({ggplotly(plot_1(),tooltip = 'text')})
  output$plot1 <-renderPlotly({plotly_1()})
  
  d0<- reactive({df()$Sentence})
  d1 <- reactive({Corpus(VectorSource(d0()))})
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  d2 <- reactive({tm_map(d1(), toSpace, "/")})
  d3 <- reactive({tm_map(d2(), toSpace, "@")})
  d4<- reactive({tm_map(d3(), toSpace, "\\|")})
  
  # Convert the text to lower case
  d5 <- reactive({tm_map(d4(), content_transformer(tolower))})
  # Remove numbers
  d6 <- reactive({tm_map(d5(), removeNumbers)})
  # Remove english common stopwords
  d7 <- reactive({tm_map(d6(), removeWords, stopwords("english"))})
  # Remove punctuations
  d8 <- reactive({tm_map(d7(), removePunctuation)})
  # Eliminate extra white spaces
  d9 <- reactive({tm_map(d8(), stripWhitespace)})
  
  # Text stemming (reduces words to their root form)
  d10 <- reactive({tm_map(d9(), stemDocument)})
  # Remove additional stopwords
  d11 <- reactive({tm_map(d10(), removeWords, c("you","the","and","your"))})
  # Term-document matrix
  dtm <- reactive({TermDocumentMatrix(d11())})
  m <- reactive({as.matrix(dtm())})
  v <- reactive({sort(rowSums(m()),decreasing=TRUE)})
  total <- reactive({data.frame(word = names(v()),freq=v())})
  #Generate wordcloud
  cloud<-reactive({wordcloud(total()$word, total()$freq,
                             random.order=FALSE, rot.per=0.3,colors=brewer.pal(8,"Dark2"))})
  output$plot2<-renderPlot({cloud()})
}
shinyApp(ui,server)
