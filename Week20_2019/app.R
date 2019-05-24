library(shiny)
library(plotly) # for interactive plots
library(tidyverse)
library(tm) # for text mining
library(wordcloud) # word-cloud generator
library(RColorBrewer) # color palettes
library(shinycustomloader) # for loading symbols

## Importing data -----
nobel_winners <-
  read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv"
  ) %>%
  mutate_if(is.character, as.factor) # converting all character variables to factors

## Function to prepare text to generate wordcloud -----
# (Original source of code 
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know)
cloud_prep_function <- function(txt) {
  docs <- Corpus(VectorSource(txt))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  return(d)
}


# Define UI  -----
ui <- fluidPage(
  
  # App title -----
  titlePanel(strong("Nobel Prize Winners and Gender")),
  h2(
    "Comparison of the number of male and female Nobel Prize winners over time"
  ),
  
  # First row containing slider, line plot and bar plot -----
  fluidRow(
    column(2,
           wellPanel(
             h4("Far fewer women than men have been awarded a Nobel Prize."),
             h4(
               "The disparity between genders has only slightly decreased over time."
             ),
             h4(
               "The distribution of awards between categories also differs between genders."
             ),
             h4(strong(
               "Use the slide below to explore how this changes over time."
             )),
             
             # Slider to select year for plots in top row -----
             sliderInput(
               inputId = "year",
               label = "Year",
               min = 1901,
               max = 2016,
               value = c(1901, 2016),
               step = 1,
               sep = "" # to remove comma seperator
             )
           )
    ),
    column(
      4,
      
      # inserting a plotly line chart -----
      withLoader(
        plotlyOutput( 
          outputId = "line"
        ), 
        type="html", 
        loader="loader6")
      
    ),
    column(6,
           
           # inserting a bar chart with loading symbol -----
           withLoader(plotOutput(outputId = "bar"), type="html", loader="loader6")
    )
  ),
  h2(
    "Comparison of popular words used in the award motivation for females and males"
  ),
  
  # Second row containing selection boxes and two word clouds -----
  fluidRow(
    column(2,
           wellPanel(
             h4(
               "The word clouds highlight the most popular words used in
                        the descriptions of why the recipients were awarded their Prize."
             ),
             h4(
               strong(
                 "Choose different categories below to compare popular words for a specific prize category."
               )
             ),
             selectInput('prize_cat', 'Prize Category', c("All", as.character(unique(nobel_winners$category)))
             ),
             h4(
               "'Discovery', 'discoveries' and 'work' were frequently used for both females and males."
             ),
             h4(
               "'Rights' and 'struggle' were frequently used for females but not males."
             )
           )
    ),
    column(
      5,
      h3("Female", align = "center"),
      
      # Inserting word cloud for words associated with females (including loading symbol)
      withLoader(
        plotOutput(
          outputId = "cloud_female",
          width = "100%",
          height = "550px"
        ), 
        type="html", 
        loader="loader6")
      
    ),
    column(
      5,
      h3("Male", align = "center"),
      
      # Inserting word cloud for words associated with males (including loading symbol)
      withLoader(
        plotOutput(
          outputId = "cloud_male",
          width = "100%",
          height = "550px"
        ), 
        type="html", 
        loader="loader6")
      
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Bar chart -----
  output$bar <- renderPlot({
    nobel_count <-
      filter(nobel_winners,
             between(prize_year, input$year[1], input$year[2])) %>%
      group_by(gender, category) %>%
      summarise(count_prize = n()) %>%
      na.omit()
    
    ggplot(data = nobel_count, aes(x = category, y = count_prize, fill = gender)) +
      geom_col() +
      facet_wrap( ~ gender) +
      theme_bw() +
      theme(text = element_text(size = 25),
            legend.position = "left") +
      labs(y = "Number of Nobel Prizes",
           x = "Prize Category",
           fill = "Gender") +
      scale_fill_manual(values = c("#ff7f00", "#377eb8")) +
      coord_flip()
    
    
  })
  
  # Interactive line chart -----
  output$line <-
    renderPlotly({
      nobel_winners_group <-
        filter(nobel_winners,
               between(prize_year, input$year[1], input$year[2])) %>%
        group_by(prize_year, gender) %>%
        summarise(count_prize = n()) %>%
        na.omit()
      
      nobel_winners_group_male <-
        filter(nobel_winners_group, gender == "Male")
      nobel_winners_group_female <-
        filter(nobel_winners_group, gender == "Female")
      
      #Need to add zeros for years with data for males but not females
      add_zero <-
        filter(nobel_winners,
               gender == "Male",
               between(prize_year, input$year[1], input$year[2])) %>%
        select(prize_year) %>%
        unique %>%
        mutate(gender = as.factor("Female"), count_prize = 0)
      
      add_zero <-
        anti_join(add_zero, nobel_winners_group_female, by = "prize_year") # selecting columns that should be zero
      nobel_winners_group_female <-
        merge(add_zero,
              nobel_winners_group_female,
              all.x = TRUE,
              all.y = TRUE)
      
      p <- plot_ly(
        x = nobel_winners_group_male$prize_year,
        y = nobel_winners_group_male$count_prize,
        type = "scatter",
        mode = "markers",
        fill = "tonexty",
        name = 'Male'
      ) %>%
        add_trace(
          x = nobel_winners_group_female$prize_year,
          y = nobel_winners_group_female$count_prize,
          type = "scatter",
          mode = "markers",
          fill = "tozeroy",
          name = 'Female'
        ) %>%
        layout(
          xaxis = list(title = "Year"),
          yaxis = list(title = "Number of Nobel Prizes")
        )
      hide_legend(p)
    })
  
  # Female word clouds -----
  output$cloud_female <- renderPlot({
    if (input$prize_cat == "All") {
      txt_female <- filter(nobel_winners, gender == "Female") %>%
        select(motivation) %>%
        .$motivation %>%
        as.character
    } else{
      txt_female <-
        filter(nobel_winners,
               category == input$prize_cat,
               gender == "Female") %>%
        select(motivation) %>%
        .$motivation %>%
        as.character
    }
    
    cloud_data_female <- cloud_prep_function(txt_female)
    
    set.seed(1234)
    col_orange <- brewer.pal(8, "Oranges")
    wordcloud(
      words = cloud_data_female$word,
      freq = cloud_data_female$freq,
      min.freq = 1,
      max.words = Inf,
      random.order = FALSE,
      rot.per = 0,
      colors = col_orange[4:8],
      fixed.asp = FALSE
    )
  })
  
  # Male word clouds -----
  output$cloud_male <- renderPlot({
    if (input$prize_cat == "All") {
      txt_male <- filter(nobel_winners, gender == "Male") %>%
        select(motivation) %>%
        .$motivation %>%
        as.character
    } else{
      txt_male <-
        filter(nobel_winners,
               category == input$prize_cat,
               gender == "Male") %>%
        select(motivation) %>%
        .$motivation %>%
        as.character
    }
    
    
    cloud_data_male <- cloud_prep_function(txt_male)
    set.seed(1234)
    col_blue <- brewer.pal(8, "Blues")
    wordcloud(
      words = cloud_data_male$word,
      freq = cloud_data_male$freq,
      min.freq = 1,
      max.words = Inf,
      random.order = FALSE,
      rot.per = 0,
      colors = col_blue[4:8],
      fixed.asp = FALSE
    )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
