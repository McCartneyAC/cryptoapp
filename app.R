library(shiny)
library(tidyverse)
library(shinythemes)
ui <- fluidPage( theme = shinytheme ("flatly"), 
                 titlePanel("Frequency Decryption: A Broken Algorithm"),
                 
                 sidebarLayout(
                   
                   sidebarPanel(
                     textAreaInput(inputId = "ins", 
                                   label="insert cipher text here", 
                                   width = "99%", 
                                   height = "500px",
                                   placeholder = "ciphertext")
                   ),
                   
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Decrypted Text",
                                tags$div(
                                  tags$h3("Your decrypted text"),
                                  tags$p((textOutput(outputId = "outs")))
                                )), 
                                
                       tabPanel("Plots", plotOutput("c3"), plotOutput("c1")), 
                                
                       tabPanel("About", 
                                         tags$div(class="header", checked=NA,
                                                  tags$h3("How does this algorithm work?"),
                                                  tags$p("And why doesn't it work?"),
                                                  tags$br(),
                                                  tags$p("The decryption algorithm is based on the frequency 
                                                         with which letters appear in English speech and prose.
                                                         Letters like E, T, A, and O are much more common than 
                                                         J, X, Q, and Z, and their frequencies follow a distribution.
                                                         This is actually true for every language, and is one of the
                                                         ways in which linguists determine whether an unknown language
                                                         is real or a hoax. What this algorithm does is take your 
                                                         ciphertext and re-sort it based on letter frequency, then
                                                         transposes those letters based on the frequency in the 
                                                         English language."),
                                                  tags$p("So if the most common letter in your ciphertext is Q, then 
                                                         it will be replaced with the most common letter in English, E."),
                                                  tags$p("So why doesn't it work? Because the frequencies given are 
                                                         based on standard English speech overall-- even encrypted text
                                                         with tens of thousands of words still doesn't always approximate
                                                         the actual frequencies of letters in English speech."),
                                                  tags$p("If you use this tool with a big enough sample, you'll find 
                                                         whispers of real words start to become apparent, but even
                                                         common words like 'the' will still end up looking silly, like
                                                         'tde'."),
                                                  tags$p("Note, however, that this kind of decryption only works for simple
                                                         substitutions ciphers. Nothing that became en vogue after about 1850
                                                         will be decipherable. Your credit card information is safe."),
                                                  tags$p("What if you don't have ciphertext? Try", tags$a(href="http://rot13.com/", "Rot13."),
                                                  tags$p(tags$a(href="https://en.wikipedia.org/wiki/Zipf%27s_law", "For further reading."))
                                                  )
                                         
                                         
                                                  ))
                                                  )
                   )
                   ))
                     





server <- function(input, output){

  freq.sort<-function(x){
    req(input$ins)
    english.by.frequency <- "etaoinshrdlcumwfgypbvkjxqz" 
    cleaned <-  gsub(pattern = "\\W|\\d", replace = "",  tolower(x))
    letter.frequencies <- substring(cleaned, 1:nchar(cleaned),1:nchar(cleaned))
    tab <- sort(table(letter.frequencies), decreasing = TRUE)
    ordered_characters <- dimnames(tab)$letter.frequencies
    ordered_in_a_string <- paste(ordered_characters, collapse = "")
    excess    <- (26 - nchar(ordered_in_a_string))
    addendum1 <- rep.int("_", excess)
    addendum2 <- paste(addendum1, collapse="")
    final     <- paste(ordered_in_a_string, addendum2, sep="")
    cipher <- chartr(final,english.by.frequency,tolower(x)) 
    finaltext <- cat(cipher)
    if(nchar(x) < 50) stop("Try it with a little more text. It works best when it has more data.")
    return(finaltext)
    }


  
  output$outs <- renderPrint({
    freq.sort(input$ins)
  })
  #######################
  # code for the Eng graph
  lets<-c("e", "t", "a", "o", "i", "n", "s", "h", "r", "d", "l", "c", "u", "m",	"w", "f", "g", "y", "p", "b",	"v", "k", "j", "x", "q", "z")
  percs<-c(12.702, 9.056, 8.167, 7.507,	6.966,	6.749,	6.327,	6.094,	5.987,	4.253,	4.025,	2.782,	2.758,	2.406,	2.36,	2.228,	2.015,	1.974,	1.929,	1.492,	0.978,	0.772,	0.153,	0.15,	0.095,	0.074)
  
  eng<-data.frame(lets,percs)
  
  output$c3 <- renderPlot({
    ggplot(eng, aes(x = reorder(lets, -percs), y = percs, fill = percs)) + 
      geom_col() + 
      scale_fill_gradient(low = "#DCCD59", high = "#2F3316")+
      labs(x = "letter",
           y = "frequency",
           title = "Frequency of Letters in Standard English",
           subtitle = "By Percent") 
  })
  ############# end #############
  ##### creates the data frame of letter frequencies
  create_data_frame<-function(x){
    req(input$ins)
    cleaned <-  gsub(pattern = "\\W|\\d", replace = "",  tolower(x))
    let.freqs <- substring(cleaned, 1:nchar(cleaned),1:nchar(cleaned))
    tab <- sort(table(let.freqs), decreasing = TRUE)
    dat <- as.data.frame(tab) 
    return(dat)
  }
  #####################################################################
  dat1<-reactive({create_data_frame(input$ins)})
  
  
  output$c1 <- renderPlot({
    ggplot(dat1(), aes(let.freqs, Freq, fill = Freq))+
      geom_col() + 
      scale_fill_gradient(low = "#C35521", high = "#3F1F11")+
      labs(x = "letter",
           y = "frequency",
           title = "Frequency of Letters in your Encrypted text",
           subtitle = "By Absolute Count")
  })
}


shinyApp(ui = ui, server = server)
