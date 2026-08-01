library(shiny)
library(bslib)
library(DBI)
library(pool)
library(tidyverse)
library(RPostgres)

options(sass.cache = tempdir())

if (file.exists("../supersecrets.r")) {

  source("../supersecrets.r")

}

tank <- dbPool(drv = Postgres(),
               dbname = Sys.getenv("db_name3"),
               host = Sys.getenv("db_ip"),
               port = as.numeric(Sys.getenv("db_port")),
               user = Sys.getenv("db_user"),
               password = Sys.getenv("db_pass"),
               bigint = "numeric")

get_random_comment <- function() {

  qry <- "SELECT comment_id, post_id, parent_id, body, score, subreddit, created_utc, 
                 gemini_scored_at, valence, social_intent, outlook, gemini_reasoning, 
	               model_version, batch_id
          FROM reddit.comments
          WHERE valence IS NOT NULL
          ORDER BY RANDOM() LIMIT 1;"
  
  cmnt <- dbGetQuery(tank, qry)
  
  parent_id <- cmnt %>% pull(parent_id)
  ischild <- str_sub(parent_id, 1, 3) == "t1_"
  
  if (ischild) {
    
    parent_id_no <- str_sub(parent_id, 4)
    parent_qry <- "SELECT body FROM reddit.comments WHERE comment_id = $1"
    parent_text <- dbGetQuery(tank, parent_qry, params = list(parent_id_no)) %>% pull(body)
    
  } else {
    
    parent_text <- ""
    
  }
  
  post_qry <- "SELECT title, selftext FROM reddit.posts WHERE post_id = $1"
  post_info_raw <- dbGetQuery(tank, post_qry, params = list(cmnt$post_id))
  
  post_info <- post_info_raw %>%
    mutate(st1 = str_sub(selftext, 1, 250),
           st2 = str_sub(selftext, -250, -1),
           snipped_text = ifelse(nchar(selftext) > 500, str_c(st1, '\n[...sniped for length...]\n', st2), selftext)) %>%
    select(title, snipped_text)
  
  final_frame <- cmnt %>%
    mutate(parent_comment = parent_text,
           post_title = post_info$title,
           post_body_snipped = post_info$snipped_text) %>%
    rename(comment_text = body,
           comment_post_time = created_utc,
           comment_score = score,
           reasoning = gemini_reasoning,
           scoring_model = model_version,
           scored_at = gemini_scored_at)
  
  return(final_frame)
  
}

ui <- page_sidebar(
  
  tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-wrap: break-word; }"))),
  
  title = "Reddit Sentiment Explorer",
  theme = bs_theme(version = 5, preset = "darkly"),
  
  sidebar = sidebar(
    title = "Controls",
    actionButton("next_btn", "🎲 Pull Random Comment", class = "btn-primary"),
    hr(),
    p("I pull comments from 48 unique subreddits, picked for diversity of content and analytical interest."),
    p("Comments are harvested 72 hours after post tagging to ensure maturity.")
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    
    card(
      card_header("Reddit Context"),
      card_body(
        p(tags$b("Subreddit: "), textOutput("subreddit", inline = TRUE)),
        p(tags$b("Post Time: "), textOutput("comment_post_time", inline = TRUE)),
        p(tags$b("Post Title: "), textOutput("post_title", inline = TRUE)),
        tags$b("Post Body (Snipped):"),
        verbatimTextOutput("post_body_snipped"),
        tags$b("Parent Comment:"),
        verbatimTextOutput("parent_comment"),
        p(tags$b("Target Comment Score: "), textOutput("comment_score", inline = TRUE)),
        tags$b("Target Comment:"),
        verbatimTextOutput("comment_text")
      )
    ),
    
    card(
      card_header("Sentiment Analysis Details"),
      card_body(
        tags$b("Valence:"), textOutput("valence", inline = TRUE),
        tags$b("Social Intent:"), textOutput("social_intent", inline = TRUE),
        tags$b("Outlook:"), textOutput("outlook", inline = TRUE),
        hr(),
        tags$b("Reasoning:"),
        verbatimTextOutput("reasoning"),
        hr(),
        tags$b("Metadata:"), br(),
        tags$small(
          "Model: ", textOutput("scoring_model", inline = TRUE), br(),
          "Batch ID: ", textOutput("batch_id", inline = TRUE), br(),
          "Scored At: ", textOutput("scored_at", inline = TRUE)
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  current_data <- eventReactive(input$next_btn, {
    get_random_comment()
  }, ignoreNULL = FALSE)
  
  output$subreddit <- renderText({ current_data()$subreddit })
  output$post_title <- renderText({ current_data()$post_title })
  output$comment_post_time <- renderText({ as.character(current_data()$comment_post_time) })
  output$comment_score <- renderText({ current_data()$comment_score })
  output$post_body_snipped <- renderText({ current_data()$post_body_snipped })
  output$parent_comment <- renderText({ current_data()$parent_comment })
  output$comment_text <- renderText({ current_data()$comment_text })
  
  output$valence <- renderText({ current_data()$valence })
  output$social_intent <- renderText({ current_data()$social_intent })
  output$outlook <- renderText({ current_data()$outlook })
  output$reasoning <- renderText({ current_data()$reasoning })
  
  output$scoring_model <- renderText({ current_data()$scoring_model })
  output$batch_id <- renderText({ current_data()$batch_id })
  output$scored_at <- renderText({ as.character(current_data()$scored_at) })
  
}

shinyApp(ui, server)