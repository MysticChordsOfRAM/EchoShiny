library(shiny)
library(dplyr)
library(pool)
library(glue)
library(bslib)
library(tidyverse)
library(RPostgres)

if (file.exists("../supersecrets.r")) {
  
  source("../supersecrets.r")
  
}

message('-------------------------------')
message('DEBUGGING ISSUE')
val <- Sys.getenv('db_ip')
message(paste("RAW VALUE: ", val))
message(paste("NUMERIC VALUE: ", as.numeric(val)))
message(paste("TYPE OF VALUE: ", class(val)))
message(paste('NCHARS: ', nchar(val)))
message('-------------------------------')

mdict <- tibble(MO = 1:12,
                MN = month.name)

tank <- dbPool(drv = Postgres(),
               dbname = Sys.getenv("db_name2"),
               host = Sys.getenv("db_ip"),
               port = as.numeric(Sys.getenv("db_port")),
               user = Sys.getenv("db_user"),
               password = Sys.getenv("db_pass"),
               bigint = "numeric")

form_tables <- c("dor.f2", "dor.f9", "dor.f10", "dor.f11")
qry_p1 <- vector("character", length(form_tables))

for (ii in 1:length(form_tables)){
  
  qry_px <- str_c("SELECT '", form_tables[ii], "' as form_name, ",
                  "MIN(subj_month) as min_date, ",
                  "MAX(subj_month) as max_date FROM ", form_tables[ii],
                  collapse = "")
  
  qry_p1[ii] <- qry_px
  
}

qry_p2 <- str_c(qry_p1, collapse = " UNION ALL ")

constnts <- dbGetQuery(tank, qry_p2)
cntys <- dbGetQuery(tank, "SELECT DISTINCT(county) FROM dor.f10") %>% pull(county)
industries <- dbGetQuery(tank, "SELECT DISTINCT(kcode_desc) FROM dor.f10") %>% pull(kcode_desc)
sectors <- dbGetQuery(tank, "SELECT DISTINCT(sector) FROM dor.f11") %>% pull(sector)

tlate <- tibble(choices = c("Form 2 - State Tax Collections",
                            "Form 9 - Sales Tax Summary Data",
                            "Form 10 - Sales Tax by Industry Sector",
                            "Form 11 - Sales Tax by Economic Sector"),
                tables = form_tables)

std_spread <- c('Gross Sales', 'Taxable Sales', 'Tax Collections')
metric_opts <- list('dor.f2' = NULL,
                    'dor.f9' = std_spread,
                    'dor.f10' = std_spread,
                    'dor.f11' = std_spread)

filter_opts <- list('dor.f2' = NULL,
                    'dor.f9' = NULL,
                    'dor.f10' = list(modes = c("County", "Industry"),
                                     data = list("County" = cntys,
                                                 "Industry" = industries),
                                     cols = list("County" = "county",
                                                 "Industry" = "kcode_desc")),
                    'dor.f11' = list(modes = c("County", "Sector"),
                                     data = list("County" = cntys,
                                                 "Sector" = sectors),
                                     cols = list("County" = "county",
                                                 "Sector" = "sector")))

ui <- fluidPage(

  tags$html(lang = "en"),
  theme = bs_theme(bootswatch = "sandstone"),
  
  titlePanel("DOR WEBFORM PROTOTYPE DASHBOARD"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "form_choice",
        label = "Step 1: Select Web Form",
        choices = tlate$choices,
        selected = character(0)
      ),
      
      uiOutput("date_ui"),
      
      uiOutput("options_ui"),
      
      hr(),
      
      actionButton(
        inputId = "compute",
        label = "Compute Form",
        icon = icon("calculator"),
        class = "btn-primary"
      ),
      
      br(), br(),
      
      uiOutput("download_ui")
    ),
    
    mainPanel(
      tags$h3("Form Output"),
      tags$p("The generated table will appear below after computation."),
      tableOutput("result_table")
    )
  )
)

server <- function(input, output, session) {
  
  form_constraints <- reactive({
    req(input$form_choice)
    
    tbl_choice <- tlate %>%
      filter(choices == input$form_choice) %>%
      pull(tables)
    
    lbnd <- constnts %>% filter(form_name == tbl_choice) %>% pull(min_date)
    ubnd <- constnts %>% filter(form_name == tbl_choice) %>% pull(max_date)
    
    form_set <- list(table = tbl_choice,
                     min_date = lbnd,
                     max_date = ubnd,
                     metrics = metric_opts[[tbl_choice]],
                     filters = filter_opts[[tbl_choice]])
    
    return(form_set)
  
  })
  
  output$date_ui <- renderUI({
    req(form_constraints())
    config <- form_constraints()
    
    def_end <- config$max_date
    def_start <- def_end - 365
    
    tagList(
      tags$br(),
      dateRangeInput(
        inputId = "date_range",
        label = "Step 2: Select Date Range",
        start = def_start,
        end = def_end,
        min = config$min_date,
        max = config$max_date,
        format = "mm/yyyy",
        startview = "year"
      )
    )
  })
  
  output$options_ui <- renderUI({
    req(form_constraints())
    config <- form_constraints()
    
    ui_list <- list(tags$br())
    
    if (!is.null(config$metrics)) {
      
      ui_list <- append(ui_list, list(
        radioButtons("form_opts", "Step 3a: Select Metric", config$metrics)
      ))
      
    }
    
    if (!is.null(config$filters)) {
      
      current_mode <- input$filter_mode %||% config$filters$modes[1]
      
      if (!current_mode %in% config$filters$modes) {
        
        current_mode <- config$filters$modes[1]
        
      }
      
      ui_list <- append(ui_list, list(
        tags$br(),
        radioButtons(
          inputId = "filter_mode",
          label = "Step 3b: Filter By",
          choices = config$filters$modes,
          selected = current_mode,
          inline = TRUE
        )
      ))
      
      list_choices <- config$filters$data[[current_mode]]
      
      ui_list <- append(ui_list, list(
        selectInput(
          inputId = "filter_specific",
          label = str_c("Step 3c: Select ", current_mode),
          choices = list_choices,
          selectize = FALSE,
          size = 10
        )
      ))
    }
    
    tagList(
      ui_list
      )
  })

  computed_data <- eventReactive(input$compute, {
    req(input$form_choice, input$date_range)
    config <- form_constraints()
    
    days_diff <- as.numeric(difftime(input$date_range[2], input$date_range[1], units = "days"))
    
    validate(
      need(days_diff <= 731, "Error: You cannot select a range larger than 24 months. Please adjust your dates.")
    )
    
    lbnd <- input$date_range[1] %>% floor_date('month') %>% as.character()
    ubnd <- input$date_range[2] %>% floor_date('month') %>% as.character()
    
    qry <- glue_sql("SELECT * FROM {DBI::SQL(config$table)}
                     WHERE subj_month BETWEEN {lbnd} AND {ubnd}",
                    .con = tank)
    
    if (!is.null(config$metrics)) {
      req(input$form_opts)
      qry <- glue_sql("{qry} AND measure = {input$form_opts}", .con = tank)
    }
    
    if (!is.null(config$filters)) {
      req(input$filter_mode, input$filter_specific)
      
      if (!input$filter_mode %in% config$filters$modes) {
        validate(need(FALSE, "Refreshing options..."))
      }

      sql_col <- config$filters$cols[[input$filter_mode]]
      
      if (is.null(sql_col)) return(NULL)
      
      qry <- glue_sql("{qry} AND {`sql_col`} = {input$filter_specific}", .con = tank)
    }
    
    table_data <- dbGetQuery(tank, qry)
    
    if (nrow(table_data) == 0) {
      
      return(NULL)
      
    } else if (config$table == 'dor.f2') {
      
      table_clean <- table_data %>%
        mutate(subj_month = ymd(subj_month),
               MO = month(subj_month),
               YR = year(subj_month)) %>%
        left_join(mdict) %>%
        mutate(LMN = str_c(MN, " ", YR),
               amt = scales::comma(round(amt, 2))) %>%
        arrange(lineid, subj_month) %>%
        select(-MO, -MN, -YR, -subj_month) %>%
        pivot_wider(names_from = LMN, values_from = amt) %>%
        relocate(lineid, line_name) %>%
        rename(` ` = lineid, Tax = line_name)
      
      return(table_clean)
      
    } else if (config$table == 'dor.f9') {
      
      table_clean <- table_data %>%
        mutate(subj_month = ymd(subj_month),
               MO = month(subj_month),
               YR = year(subj_month)) %>%
        left_join(mdict) %>%
        mutate(LMN = str_c(MN, " ", YR),
               amt = scales::comma(round(amt, 2))) %>%
        arrange(ctcode, subj_month) %>%
        select(-MO, -MN, -YR, -subj_month, -measure) %>%
        pivot_wider(names_from = LMN, values_from = amt) %>%
        relocate(ctcode, county) %>%
        rename(` ` = ctcode, County = county)
        
      return(table_clean)
      
    } else if (config$table == 'dor.f10') {
      
      table_clean <- table_data %>%
        mutate(subj_month = ymd(subj_month),
               MO = month(subj_month),
               YR = year(subj_month),
               kcode = as.numeric(kcode)) %>%
        left_join(mdict) %>%
        mutate(LMN = str_c(MN, " ", YR),
               amt = scales::comma(round(amt, 2)))
      
      if (input$filter_mode == 'County') {
        
        table_final <- table_clean %>%
          arrange(kcode, subj_month) %>%
          select(-MO, -MN, -YR, -subj_month, -measure, -county, -ctcode) %>%
          pivot_wider(names_from = LMN, values_from = amt) %>%
          relocate(kcode, kcode_desc) %>%
          mutate(kcode = as.character(kcode)) %>%
          rename(` ` = kcode, `Kind Code` = kcode_desc)
        
        return(table_final)
        
      } else {
        
        table_final <- table_clean %>%
          arrange(ctcode, subj_month) %>%
          select(-MO, -MN, -YR, -subj_month, -measure, -kcode, -kcode_desc) %>%
          pivot_wider(names_from = LMN, values_from = amt) %>%
          relocate(ctcode, county) %>%
          rename(` ` = ctcode, County = county)
        
        return(table_final)
        
      }
      
    } else if (config$table == 'dor.f11') {
      
      table_clean <- table_data %>%
        mutate(subj_month = ymd(subj_month),
               MO = month(subj_month),
               YR = year(subj_month)) %>%
        left_join(mdict) %>%
        mutate(LMN = str_c(MN, " ", YR),
               amt = scales::comma(round(amt, 2)))
      
      if (input$filter_mode == 'County') {
        
        table_final <- table_clean %>%
          arrange(sector_id, subj_month) %>%
          select(-MO, -MN, -YR, -subj_month, -measure, -county, -ctcode) %>%
          pivot_wider(names_from = LMN, values_from = amt) %>%
          relocate(sector_id, sector) %>%
          mutate(sector_id = as.character(sector_id)) %>%
          rename(` ` = sector_id, Sector = sector)
        
        return(table_final)
        
      } else {
        
        table_final <- table_clean %>%
          arrange(ctcode, subj_month) %>%
          select(-MO, -MN, -YR, -subj_month, -measure, -sector, -sector_id) %>%
          pivot_wider(names_from = LMN, values_from = amt) %>%
          relocate(ctcode, county) %>%
          rename(` ` = ctcode, County = county)
        
        return(table_final)
        
      }
      
    }
    
  })
  

  output$result_table <- renderTable({
    computed_data()
  })
  

  output$download_ui <- renderUI({
    req(computed_data())
    
    downloadButton("download_data", "Download Table")
  })
  
  # Handle the actual file download
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", input$form_choice), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(computed_data(), file, row.names = FALSE)
    }
  )
  
 # session$onSessionEnded(function() {
 #   
 #   print("Session Ended. Closing Pool")
 #   poolClose(tank)
 #   
 # })
}

shinyApp(ui = ui, server = server)
