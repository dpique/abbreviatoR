library(shiny)
library(tidyverse)
library(readtext)
library(here)
library(shinythemes)

return_abbrevs <- function(txt, rgx2, max_len=Inf){
  txt.splt <- str_split(txt, pattern = "\\s|,|;|:") %>% unlist()
  #print(txt.splt)
  hits <- grep(pattern = rgx2, x = txt.splt)
  #print("hits")
  #print(hits)
  res <- txt.splt[hits] %>% str_squish() %>% str_replace_all("[^[:alnum:]|\\-|\\.]", "") %>% unique() %>% sort()
  #print("res")
  #print(res)
  ## update this so that if 2 terms differ by only a terminal period(s) 
  ## and the term has no other periods, return char with no periods
  regex_term_period = "^[^\\.]+\\.+$"
  res_term_per_idx <- grep(pattern = regex_term_period, x = res)
  #print("res_term_per_idx")
  #print(res_term_per_idx)
  
  if(length(res_term_per_idx) > 0){
    res[res_term_per_idx] <- str_replace_all(res[res_term_per_idx], "\\.+$", "")
  }
  res2 <- res[nchar(res) <= max_len]
  return(unique(res2) %>% sort())
}

ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme = shinytheme("yeti"),
   titlePanel("abbreviatoR - Generate an Alphabetized List of Abbreviations from Text"),
   #tags$h3(""),
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
                    label="Max Abbrev length (set to 'Inf' for no limit)")),
          mainPanel(
            br(),
            p("Results:"),
            htmlOutput("txt2")
          )
   )), tabPanel("About",
         tags$h4("Author: Daniel Pique"), 
         p("AbbreviatoR was developed in response for a need to create a list of abbreviations for my thesis. I used a regular expression in sublime text editor but then thought that this could make a more generally useful app."),
         HTML("<a href=\"https://twitter.com/dpique12?ref_src=twsrc%5Etfw\" class=\"twitter-follow-button\" data-show-count=\"false\">Follow @dpique12</a><script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>"),
         HTML("<a class=\"github-button\" href=\"https://github.com/dpique\" aria-label=\"Follow @dpique on GitHub\">Follow @dpique</a><script async defer src=\"https://buttons.github.io/buttons.js\"></script>")
         #tags$a(href="http://dpique.rbind.io", icon(name = "home" , class = "fa-2x"))
   )
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
    HTML(paste(res, collapse = '<br/>'))
  })
}

shinyApp(ui = ui, server = server)

