library(shiny)
library(tidyverse)
#library(readtext)
#library(here)
#library(shinythemes)

return_abbrevs <- function(txt, rgx2, max_len=Inf){
  # 1. split up the text into individual words by common delimiters
  txt.splt <- str_split(txt, pattern = "\\s|,|;|:") %>% unlist() 
  # 2. grab the words/phrases that match the regex
  hits <- grep(pattern = rgx2, x = txt.splt) 
  
  res <- txt.splt[hits] %>% 
    str_squish() %>% 
    str_replace_all("[^[:alnum:]|\\-|\\.]", "") %>% 
    unique() %>%  # get all unique values
    sort() #sort the list alphabetically

  ## if 2 terms differ by only a terminal period(s) (b/c it's at the end of a sentence) 
  ## and the term has no other periods, return a character with no periods
  regex_term_period = "^[^\\.]+\\.+$"
  res_term_per_idx <- grep(pattern = regex_term_period, x = res)

  if(length(res_term_per_idx) > 0){
    res[res_term_per_idx] <- str_replace_all(res[res_term_per_idx], "\\.+$", "")
  }
  res2 <- res[nchar(res) <= max_len]
  res3 <- unique(res2) %>% sort()
  return(res3)
}

ui <- fluidPage(
  theme = shinythemes::shinytheme("yeti"),
   titlePanel("abbreviatoR - Generate an Alphabetized List of Abbreviations from Text"),
  tabsetPanel(
    tabPanel("Main", 
      sidebarLayout(
          sidebarPanel(
          
          textInput(inputId = "txt",
                    label = "Paste any text here (can be copied/pasted straight from word, excel, etc.)"
                    ),
          checkboxInput(inputId="docx",
                        label="Check if pasting from .docx with in-text citations",
                        value=FALSE),
          textInput(inputId = "regex",
                    value = "([a-zA-Z]\\.){2,}|[A-Z]{2,}|[[A-Z]{1,}\\w*[A-Z]{1,}]{2,}",
                    label = "Regex (feel free to modify)"
                    ),
          textInput(inputId = "ml",
                    value=Inf,
                    label="Max Abbreviation length (set to 'Inf' for no limit)"),
          hr(),
          h4("About this App"),
          p("AbbreviatoR was developed in response to a need to create a list of abbreviations for my thesis. I used regular expressions to gather abbreviations but thought that this could make a more generally useful app."),
          HTML("Did this app save you time? If so, you can <a href='https://ko-fi.com/W7W0Z4N0' target='_blank'><img height='36' style='border:0px;height:30px;' src='https://az743702.vo.msecnd.net/cdn/kofi1.png?v=2' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>"),
          h4("About the Author"),
          HTML("If you'd like to stay in touch or learn more about my work, please follow me at the links below or <a href=\"https://dpique.rbind.io/\" target='_blank'> visit my homepage</a>."),
          HTML("<a href=\"https://twitter.com/dpique12?ref_src=twsrc%5Etfw\" class=\"twitter-follow-button\" data-show-count=\"false\">Follow @dpique12</a><script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>"),
          HTML("<a class=\"github-button\" href=\"https://github.com/dpique\" aria-label=\"Follow @dpique on GitHub\">Follow @dpique</a><script async defer src=\"https://buttons.github.io/buttons.js\"></script>"),
          ),
          
          mainPanel(
            br(),
            h3("Results"),
            htmlOutput("txt2")
          )
   ))
  )
)

server <- function(input, output) {
   
  output$txt2 <- renderUI({
    if(input$docx){
      #remove the in text citation part
      res <- str_split(input$txt, pattern = "\\.json") %>% 
        unlist() %>% 
        str_split(pattern = "ADDIN ") %>%
        unlist() %>%
        str_replace_all(paste0("CSL_CITATION", ".*","csl-citation"), "") %>%
        return_abbrevs(rgx2 = input$regex, max_len=as.numeric(input$ml))
    } else {
      res <- return_abbrevs(txt = input$txt, rgx2 = input$regex, max_len=as.numeric(input$ml))
    }
    len_res <- length(res)
    HTML(paste0("<h4>", len_res, " abbreviations </h4><br/>"), paste(res, collapse = '<br/>'))
  })
}

shinyApp(ui = ui, server = server)

