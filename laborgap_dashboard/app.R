library(shiny)
library(DBI)
library(pool)
library(bslib)
library(RPostgres)
library(janitor)
library(tidyverse)
library(plotly)

if (file.exists("../supersecrets.r")) {
  
  source("../supersecrets.r")
  
}

tank <- dbPool(drv = Postgres(),
               dbname = Sys.getenv("db_name1"),
               host = Sys.getenv("db_ip"),
               port = as.numeric(Sys.getenv("db_port")),
               user = Sys.getenv("db_user"),
               password = Sys.getenv("db_pass"),
               bigint = "numeric")

cleanup <- function(tbl) {
  
  cleaned_tbl <- tbl %>%
    rename_with(~str_replace_all(.x, "_", " ")) %>%
    rename_with(~str_to_upper(.x)) %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0)),
           across(where(is.character), ~replace_na(.x, "0")))
  
  return(cleaned_tbl)
  
}

theme_black <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#1e1e1e", color = NA), # Dark Grey
      panel.background = element_rect(fill = "#1e1e1e", color = NA),
      text = element_text(color = "white"),
      axis.text = element_text(color = "white"),
      panel.grid = element_line(color = "#444444") # Subtle grid lines
    )
}

mdict <- tibble(MO = 1:12, MN = month.name)
models <- dbGetQuery(tank, "SELECT * FROM wiki.model_runs WHERE run_date IS NOT NULL") %>%
  mutate(run_date = ymd(run_date),
         MO = month(run_date)) %>%
  left_join(mdict) %>%
  mutate(run_prd = str_c(MN, " ", year(run_date)))

naics_descs <- dbGetQuery(tank, "SELECT * FROM wiki.jolts_naics")
 
ui <- fluidPage(
  theme = bs_theme(bootswatch = "cyborg"),
  titlePanel("Laborgap Explorer"),
  
  ##--------------------------------------------------------------------------##
  ## Side Bar                                                                 ##
  ##--------------------------------------------------------------------------##
  
  sidebarLayout(
    sidebarPanel(
      h3("Model Run"),
      selectInput(
        inputId = "selection",
        label = "Select a Model Run: ",
        choices = models$run_name
      ),
      hr(),
      textOutput("model_date"),
      hr(),
      textOutput("model_description"),
      hr(),
      downloadButton(outputId = 'outputraw', label = 'Download Student Level Data'),
      textOutput("outputrawsize"),
      hr(),
      downloadButton(outputId = 'claimsraw', label = 'Download Aggregate Claims Data'),
      textOutput("claimsrawsize"),
      hr()
    ),
    
    ##------------------------------------------------------------------------##
    ## Main Panel                                                             ##
    ##------------------------------------------------------------------------##
    mainPanel(
      tabsetPanel(
        
        ## Table 1 -----------------------------------------------------------##
        
        tabPanel("Table 1",
          tabsetPanel(type = "pills",
            tabPanel("Graduates",
              h3('Number of Graduates'),
              p("Counts the number of students who graduated at each degree level from each school group."),
              tableOutput(outputId = "t1a")),
            tabPanel("Migration",
              h3("Number of Marketplace Migrants"),
              p("Counts the number of students who migrated in our model, by degree level and school group."),
              tableOutput(outputId = "t1b")),
            tabPanel("Degree Offerings",
              h3("Unique Degrees Offered"),
              p("Counts the number of unique CIP Codes used by graduates, by degree level and school group."),
              tableOutput(outputId = "t1c")),
            tabPanel("Median Salary",
              h3("Median Salary Attained"),
              p("Calculated the median attained salary by degree level and school group."),
              tableOutput(outputId = "t1d")),
            tabPanel("Choice Index",
              h3("Mean Choice Index"),
              p("Shows the average number of iterations the model took to place graduates in a job (lower is better)."),
              tableOutput(outputId = "t1e")),
            tabPanel("Salary Attainment",
              h3("Mean Salary Attainment"),
              p("Attained salary as a percent of highest possible salary - averaged across degree level and school group."),
            tableOutput(outputId = "t1f")),
            tabPanel("In-Major Matches",
              h3("Percent In-Major Matches"),
              p("The percent of students who matched to a job using the ED crosswalk."),
              tableOutput(outputId = "t1g"))
            )
        ),
        
        ## Table 2 -----------------------------------------------------------##
        
        tabPanel("Table 2",
          tabsetPanel(type = "pills",
                      
            ## Table 2-A .....................................................##
                      
            tabPanel("Overall",
                     h3('Florida Top Degree Programs'),
                     fluidRow(
                       column(width = 6,
                              h4("Associates Degrees"),
                              tableOutput("t2a1")),
                       column(width = 6,
                              h4("Bachelors Degrees"),
                              tableOutput("t2a2"))),
                     fluidRow(
                       column(width = 6,
                              h4("Masters Degrees"),
                              tableOutput("t2a3")),
                       column(width = 6,
                              h4("Doctoral Degrees"),
                              tableOutput("t2a4")))
                     ),
            
            ## Table 2-B .....................................................##
            
            tabPanel("SUS",
                     h3("SUS Top Degree Programs"),
                     fluidRow(
                       column(width = 6,
                              h4("Associates Degrees"),
                              tableOutput("t2b1")),
                       column(width = 6,
                              h4("Bachelors Degrees"),
                              tableOutput("t2b2"))),
                     fluidRow(
                       column(width = 6,
                              h4("Masters Degrees"),
                              tableOutput("t2b3")),
                       column(width = 6,
                              h4("Doctoral Degrees"),
                              tableOutput("t2b4")))),
            
            ## Table 2-C .....................................................##
            
            tabPanel("ICUF",
                     h3("ICUF Top Degree Programs"),
                     fluidRow(
                       column(width = 6,
                              h4("Associates Degrees"),
                              tableOutput("t2c1")),
                       column(width = 6,
                              h4("Bachelors Degrees"),
                              tableOutput("t2c2"))),
                     fluidRow(
                       column(width = 6,
                              h4("Masters Degrees"),
                              tableOutput("t2c3")),
                       column(width = 6,
                              h4("Doctoral Degrees"),
                              tableOutput("t2c4")))),
            
            ## Table 2-D .....................................................##
            
            tabPanel("FCS",
                     h3("FCS Top Degree Programs"),
                     fluidRow(
                       column(width = 6,
                              h4("Associates Degrees"),
                              tableOutput("t2d1")),
                       column(width = 6,
                              h4("Bachelors Degrees"),
                              tableOutput("t2d2"))),
                     fluidRow(
                       column(width = 6,
                              h4("Masters Degrees"),
                              tableOutput("t2d3")),
                       column(width = 6,
                              h4("Doctoral Degrees"),
                              tableOutput("t2d4")))),
            
            ## Table 2-E .....................................................##
            
            tabPanel("Other",
                     h3("Other Schools Top Degree Programs"),
                     fluidRow(
                       column(width = 6,
                              h4("Associates Degrees"),
                              tableOutput("t2e1")),
                       column(width = 6,
                              h4("Bachelors Degrees"),
                              tableOutput("t2e2"))),
                     fluidRow(
                       column(width = 6,
                              h4("Masters Degrees"),
                              tableOutput("t2e3")),
                       column(width = 6,
                              h4("Doctoral Degrees"),
                              tableOutput("t2e4"))))
            )
          ),
        
        ## Table 4 -----------------------------------------------------------##
        
        tabPanel("Table 4",
          tabsetPanel(type = "pills",
            tabPanel("Total",
              h3("Total Salary by Industry"),
              tableOutput(outputId = "t4a")),
            tabPanel("Median",
              h3("Median Salary by Industry"),
              tableOutput(outputId = "t4b"))
          )
        ),
        
        ## Table 5 -----------------------------------------------------------##
        
        tabPanel("Table 5",
          tabsetPanel(type = "pills",
            tabPanel("Nursing Graduates",
              h3('Graduates in Nursing Programs by Degree Level and School Group'),
              tableOutput(outputId = "t5a")),
            tabPanel("ICUF Job Placement",
              h3("Job Openings Claimed by ICUF Nursing Graduates"),
              tableOutput(outputId = "t5b")),
            tabPanel("Nursing Salary",
              h3("Median Salary of ICUF Nursing Graduates"),
              tableOutput(outputId = "t5c")),
            tabPanel("Nursing Laborgap",
              h3("Nursing Openings in Florida's Job Market"),
              tableOutput(outputId = "t5d")
            )
          )
        ),
        
        ## Table 6 -----------------------------------------------------------##
        
        tabPanel("Table 6",
          tabsetPanel(type = "pills",
            tabPanel("Total",
              h3("Total Salary by Degree Program"),
              tableOutput(outputId = "t6a")),
            tabPanel("Median",
              h3("Median Salary by Degree Program"),
              tableOutput(outputId = "t6b"))
          )
        ),
        
        ## Table 7 -----------------------------------------------------------##
        
        tabPanel(
          "Table 7",
          h3("Hot Spots!"),
          tableOutput("t7")),
        
        ## Table 8 -----------------------------------------------------------##
        
        tabPanel("Table 8",
          tabsetPanel(type = "pills",
            tabPanel("Choice Index",
              h3("Average Choice Index by Degree Program"),
              tableOutput(outputId = "t8a")),
            tabPanel("In-Major Match",
              h3("Percent of Graduates Employed In-Major"),
              tableOutput(outputId = "t8b"))
          )
        ),
        
        ## Table 9 -----------------------------------------------------------##
        
        tabPanel(
          "Table 9",
          h3("In-Major vs Out-of-Major Salary"),
          tableOutput("t9")),
        
        ## Table 10 ----------------------------------------------------------##
        
        tabPanel("Table 10",
          tabsetPanel(type = "pills",
            tabPanel("Industry",
              tableOutput(outputId = "t10a")),
            tabPanel("Job Area",
              tableOutput(outputId = "t10b")
            )
          )
        ),
        
        ## Table 11 ----------------------------------------------------------##
        
        tabPanel("Table 11",
          tabsetPanel(type = "pills",
            tabPanel(
              "Top Program",
              h3("Top Degree Prgram Feeding Each Industry"),
              tableOutput(outputId = "t11a")
            ),
            tabPanel(
              "Top 10 Programs",
              h3("Top 10 Degree Programs Feeding Each Industry"),
              
              ## Row 1 .......................................................##       
              
              fluidRow(
                column(
                  width = 4,
                  h4("Accomodation and Food Services"),
                  tableOutput(outputId = "t11b1")
                  ),
                column(
                  width = 4,
                  h4("Health Care and Social Assistance"),
                  tableOutput(outputId = "t11b2")
                  ),
                column(
                  width = 4,
                  h4("Professional Business Services"),
                  tableOutput(outputId = "t11b3")
                )
              ),
              
              ## Row 2 .......................................................##       
              
              fluidRow(
                column(
                  width = 4,
                  h4("Arts, Entertainment, and Recreation"),
                  tableOutput(outputId = "t11b4")
                ),
                column(
                  width = 4,
                  h4("Information"),
                  tableOutput(outputId = "t11b5")
                ),
                column(
                  width = 4,
                  h4("Real Estate and Rental and Leasing"),
                  tableOutput(outputId = "t11b6")
                )
              ),
              
              ## Row 3 .......................................................##       
              
              fluidRow(
                column(
                  width = 4,
                  h4("Construction"),
                  tableOutput(outputId = "t11b7")
                ),
                column(
                  width = 4,
                  h4("Mining and Logging"),
                  tableOutput(outputId = "t11b8")
                ),
                column(
                  width = 4,
                  h4("Retail Trade"),
                  tableOutput(outputId = "t11b9")
                )
              ),
              
              ## Row 4 .......................................................##       
              
              fluidRow(
                column(
                  width = 4,
                  h4("Durable Goods Manufacturing"),
                  tableOutput(outputId = "t11b10")
                ),
                column(
                  width = 4,
                  h4("Nondurable Goods Manufacturing"),
                  tableOutput(outputId = "t11b11")
                ),
                column(
                  width = 4,
                  h4("Transportation, Warehousing, and Utilities"),
                  tableOutput(outputId = "t11b12")
                )
              ),
              
              ## Row 5 .......................................................##       
              
              fluidRow(
                column(
                  width = 4,
                  h4("Finance and Insurance"),
                  tableOutput(outputId = "t11b13")
                ),
                column(
                  width = 4,
                  h4("Other Services"),
                  tableOutput(outputId = "t11b14")
                ),
                column(
                  width = 4,
                  h4("Wholesale Trade"),
                  tableOutput(outputId = "t11b15")
                )
              ),
              
              ## Row 6 .......................................................##       
              
              fluidRow(
                column(
                  width = 4,
                  h4("Government"),
                  tableOutput(outputId = "t11b16")
                ),
                column(
                  width = 4,
                  h4("Private Education Services"),
                  tableOutput(outputId = "t11b17")
                )
              )
            )
          )
        ),
        
        ## Table 12 ----------------------------------------------------------##
        
        tabPanel("Table 12",
          tabsetPanel(type = "pills",
            tabPanel("Graph 1",
                     fluidRow(
                       column(
                         width = 12,
                         align = "center",
                         numericInput(
                           inputId = "nobs1",
                           label = "Number of CIPs to Plot: ",
                           value = 50,
                           width = '200px'
                         ),
                         tags$div(style = "margin-bottom: 20px;")
                         )
                       ),
                     plotlyOutput(outputId = 'g1',
                                  height = '800px',
                                  width = '95%')
            ),
            tabPanel("Graph 2",
                     fluidRow(
                       column(
                         width = 12,
                         align = "center",
                         numericInput(
                           inputId = "nobs2",
                           label = "Number of CIPs to Plot: ",
                           value = 50,
                           width = '200px'
                         ),
                         tags$div(style = "margin-bottom: 20px;")
                       )
                     ),
                     plotlyOutput(outputId = 'g2',
                                  height = '800px',
                                  width = '95%')
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ##--------------------------------------------------------------------------##
  ## TABLE 1                                                                  ##
  ##--------------------------------------------------------------------------##
  
  TABLE1A <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>%
      count(expertise, expertise_desc, school_group) %>% 
      collect() %>%
      arrange(expertise, school_group) %>%
      select(-expertise) %>%
      pivot_wider(names_from = school_group, 
                  values_from = n) %>%
      adorn_totals(c("row", "col")) %>% 
      mutate(across(where(is.numeric), scales::comma)) %>% 
      rename(`Degree Level` = expertise_desc) %>% 
      mutate(across(everything(), ~replace_na(.x, "")))
    
    return(datapull)
    
  })
  
  TABLE1B <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(str_sub(note, 1, 2) == "M:")  %>%
      count(expertise, expertise_desc, school_group) %>% 
      collect() %>%
      arrange(expertise, school_group) %>%
      select(-expertise) %>%
      pivot_wider(names_from = school_group, 
                  values_from = n) %>%
      adorn_totals(c("row", "col")) %>% 
      mutate(across(where(is.numeric), scales::comma)) %>% 
      rename(`Degree Level` = expertise_desc) %>% 
      mutate(across(everything(), ~replace_na(.x, "")))
    
    return(datapull)
    
  })
  
  TABLE1C <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>%
      group_by(expertise, expertise_desc, school_group) %>% 
      summarise(n = n_distinct(cipcode)) %>%
      collect() %>%
      arrange(expertise, school_group) %>%
      select(-expertise) %>%
      pivot_wider(names_from = school_group, 
                  values_from = n, 
                  values_fill = 0) %>%
      adorn_totals(c("row", "col")) %>% 
      mutate(across(where(is.numeric), scales::comma)) %>% 
      rename(`Degree Level` = expertise_desc) %>% 
      mutate(across(everything(), ~replace_na(.x, "")))
    
    return(datapull)
    
  })
  
  TABLE1D <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(!is.na(salary))
    
    body_data <- base_lazy %>% 
      group_by(expertise, expertise_desc, school_group) %>%
      summarise(amt = median(salary, na.rm = TRUE)) %>% 
      arrange(expertise, school_group) %>%
      ungroup %>%
      select(-expertise) %>%
      collect() %>% 
      pivot_wider(names_from = school_group, values_from = amt)
    
    mns <- base_lazy %>% 
      group_by(school_group) %>% 
      summarise(amt = median(salary, na.rm = TRUE)) %>% 
      collect() %>% 
      pivot_wider(names_from = school_group, values_from = amt) %>% 
      mutate(expertise_desc = 'MEDIAN')
    
    rms <- base_lazy %>% 
      group_by(expertise_desc, expertise) %>%
      summarise(MEDIAN = median(salary, na.rm = TRUE)) %>%
      arrange(expertise) %>%
      ungroup %>%
      select(-expertise) %>% 
      collect()
    
    total_median <- base_lazy %>% 
      summarise(x = median(salary, na.rm = TRUE)) %>% 
      pull(x)
    
    final_tbl <- body_data %>% 
      arrange(desc(ICUF)) %>% 
      bind_rows(mns) %>% 
      left_join(rms) %>% 
      mutate(MEDIAN = replace_na(MEDIAN, total_median)) %>% 
      mutate(across(where(is.numeric), scales::comma)) %>% 
      mutate(across(everything(), ~replace_na(.x, ""))) %>%
      rename(`Degree Level` = expertise_desc)
    
    return(final_tbl)
    
  })
  
  TABLE1E <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(!is.na(choice_index))
    
    body_data <- base_lazy %>% 
      group_by(expertise, expertise_desc, school_group) %>%
      summarise(amt = mean(choice_index, na.rm = TRUE)) %>% 
      arrange(expertise, school_group) %>%
      ungroup %>%
      select(-expertise) %>%
      collect() %>% 
      pivot_wider(names_from = school_group, values_from = amt)
    
    mns <- base_lazy %>% 
      group_by(school_group) %>% 
      summarise(amt = mean(choice_index, na.rm = TRUE)) %>% 
      collect() %>% 
      pivot_wider(names_from = school_group, values_from = amt) %>% 
      mutate(expertise_desc = 'MEAN')
    
    rms <- base_lazy %>% 
      group_by(expertise_desc, expertise) %>%
      summarise(MEAN = mean(choice_index, na.rm = TRUE)) %>%
      arrange(expertise) %>%
      ungroup %>%
      select(-expertise) %>% 
      collect()
    
    total_meam <- base_lazy %>% 
      summarise(x = mean(choice_index, na.rm = TRUE)) %>% 
      pull(x)
    
    final_tbl <- body_data %>% 
      arrange(desc(ICUF)) %>% 
      bind_rows(mns) %>% 
      left_join(rms) %>% 
      mutate(MEAN = replace_na(MEAN, total_meam)) %>% 
      mutate(across(where(is.numeric), scales::comma)) %>% 
      mutate(across(everything(), ~replace_na(.x, ""))) %>%
      rename(`Degree Level` = expertise_desc)
    
    return(final_tbl)
    
  })
  
  TABLE1F <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(!is.na(salary_attainment))
    
    body_data <- base_lazy %>% 
      group_by(expertise, expertise_desc, school_group) %>%
      summarise(amt = mean(salary_attainment, na.rm = TRUE)) %>% 
      arrange(expertise, school_group) %>%
      ungroup %>%
      select(-expertise) %>%
      collect() %>% 
      pivot_wider(names_from = school_group, values_from = amt)
    
    mns <- base_lazy %>% 
      group_by(school_group) %>% 
      summarise(amt = mean(salary_attainment, na.rm = TRUE)) %>% 
      collect() %>% 
      pivot_wider(names_from = school_group, values_from = amt) %>% 
      mutate(expertise_desc = 'MEAN')
    
    rms <- base_lazy %>% 
      group_by(expertise_desc, expertise) %>%
      summarise(MEAN = mean(salary_attainment, na.rm = TRUE)) %>%
      arrange(expertise) %>%
      ungroup %>%
      select(-expertise) %>% 
      collect()
    
    total_meam <- base_lazy %>% 
      summarise(x = mean(salary_attainment, na.rm = TRUE)) %>% 
      pull(x)
    
    final_tbl <- body_data %>% 
      arrange(desc(ICUF)) %>% 
      bind_rows(mns) %>% 
      left_join(rms) %>% 
      mutate(MEAN = replace_na(MEAN, total_meam)) %>% 
      mutate(across(where(is.numeric), scales::comma)) %>% 
      mutate(across(everything(), ~replace_na(.x, ""))) %>%
      rename(`Degree Level` = expertise_desc)
    
    return(final_tbl)
    
  })
  
  TABLE1G <- reactive({
    
    run_selection <- input$selection
    
    vrsn <- models %>% filter(run_name == run_selection) %>% pull(edition)
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    if (vrsn == "probabalistic") {
      
      base_tbl <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>%
        filter(!is.na(hire_probability)) %>%
        mutate(target_metric = ifelse(hire_probability == 1000, 1, 0))
      
    } else { 
      
      base_tbl <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>%
        filter(!is.na(in_field)) %>%
        mutate(target_metric = ifelse(in_field, 1, 0))
    }
    
    body <- base_tbl %>% 
      group_by(expertise, expertise_desc, school_group) %>% 
      summarise(amt = mean(target_metric, na.rm = TRUE)) %>% 
      collect() %>% 
      arrange(expertise, school_group) %>% 
      ungroup() %>% 
      select(-expertise) %>% 
      pivot_wider(names_from = school_group, values_from = amt)
    
    cols <- base_tbl %>% 
      group_by(school_group) %>% 
      summarise(amt = mean(target_metric, na.rm = TRUE)) %>% 
      collect() %>% 
      pivot_wider(names_from = school_group, values_from = amt) %>% 
      mutate(expertise_desc = 'MEAN')
    
    rows <- base_tbl %>% 
      group_by(expertise_desc) %>% 
      summarise(MEAN = mean(target_metric, na.rm = TRUE)) %>% 
      collect()
    
    grand_tot <- base_tbl %>% 
      summarise(x = mean(target_metric, na.rm = TRUE)) %>% 
      pull(x)
    
    final_tbl <- body %>% 
      bind_rows(cols) %>% 
      left_join(rows) %>%
      mutate(MEAN = replace_na(MEAN, grand_tot),
             across(where(is.numeric), ~scales::percent(round(.x, 2)))) %>% 
      mutate(across(everything(), ~replace_na(.x, ""))) %>%
      rename(`Degree Level` = expertise_desc)
    
    return(final_tbl)
    
  })
  
  ##--------------------------------------------------------------------------##
  ## TABLE 2                                                                  ##
  ##--------------------------------------------------------------------------##
  
  TABLE2A1 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 1)
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2A2 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 2)
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2A3 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 3)
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2A4 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 4)
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2B1 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 1, school_group == 'SUS')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2B2 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 2, school_group == 'SUS')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2B3 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 3, school_group == 'SUS')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2B4 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 4, school_group == 'SUS')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2C1 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 1, school_group == 'ICUF')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2C2 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 2, school_group == 'ICUF')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2C3 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 3, school_group == 'ICUF')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    return(datapull)
    
  })
  
  TABLE2C4 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 4, school_group == 'ICUF')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2D1 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 1, school_group == 'FCS')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2D2 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 2, school_group == 'FCS')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2D3 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 3, school_group == 'FCS')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    if (pop_size == 0) {
      
      datapull <- tibble(` ` = 'None')
      
    } else {
      
      datapull <- base_lazy %>% 
        count(cipdesc) %>% 
        arrange(desc(n)) %>% 
        head(10) %>%
        collect() %>% 
        mutate(PCT = round(n / pop_size, 3),
               ORD = row_number(), 
               n = scales::comma(n),
               PCT = scales::percent(PCT)) %>% 
        relocate(ORD) %>% 
        rename(Program = cipdesc,
               Grads = n,
               ` ` = ORD)
      
    }
    
    return(datapull)
    
  })
  
  TABLE2D4 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 4, school_group == 'FCS')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    if (pop_size == 0) {
      
      datapull <- tibble(` ` = 'None')
      
    } else {
      
      datapull <- base_lazy %>% 
        count(cipdesc) %>% 
        arrange(desc(n)) %>% 
        head(10) %>%
        collect() %>% 
        mutate(PCT = round(n / pop_size, 3),
               ORD = row_number(), 
               n = scales::comma(n),
               PCT = scales::percent(PCT)) %>% 
        relocate(ORD) %>% 
        rename(Program = cipdesc,
               Grads = n,
               ` ` = ORD)
      
    }
    
    return(datapull)
    
  })
  
  TABLE2E1 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 1, school_group == 'OTHER')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2E2 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 2, school_group == 'OTHER')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2E3 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 3, school_group == 'OTHER')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE2E4 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(expertise == 4, school_group == 'OTHER')
    
    pop_size <- base_lazy %>% count() %>% pull()
    
    datapull <- base_lazy %>% 
      count(cipdesc) %>% 
      arrange(desc(n)) %>% 
      head(10) %>%
      collect() %>% 
      mutate(PCT = round(n / pop_size, 3),
             ORD = row_number(), 
             n = scales::comma(n),
             PCT = scales::percent(PCT)) %>% 
      relocate(ORD) %>% 
      rename(Program = cipdesc,
             Grads = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  ##--------------------------------------------------------------------------##
  ## TABLE 4                                                                  ##
  ##--------------------------------------------------------------------------##
  
  TABLE4A <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(salary)) %>%
      group_by(naics, school_group) %>%
      summarise(amt = sum(salary)) %>%
      arrange(naics, school_group) %>%
      collect() %>%
      pivot_wider(names_from = school_group, values_from = amt, values_fill = 0)%>%
      adorn_totals(c("row", "col")) %>%
      mutate(across(where(is.numeric), scales::comma)) %>%
      left_join(naics_descs) %>%
      relocate(naics, naics_desc) %>%
      mutate(naics_desc = replace_na(naics_desc, ""))
    
    return(datapull)
    
  })
  
  TABLE4B <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(!is.na(salary))
    
    body_data <- base_lazy %>% 
      group_by(naics, school_group) %>%
      summarise(amt = median(salary, na.rm = TRUE)) %>% 
      arrange(naics, school_group) %>%
      collect() %>% 
      pivot_wider(names_from = school_group, values_from = amt, values_fill = 0)
    
    mns <- base_lazy %>% 
      group_by(school_group) %>% 
      summarise(amt = median(salary, na.rm = TRUE)) %>% 
      collect() %>% 
      pivot_wider(names_from = school_group, values_from = amt, values_fill = 0) %>% 
      mutate(naics = 'MEDIAN')
    
    rms <- base_lazy %>% 
      group_by(naics) %>%
      summarise(MEDIAN = median(salary, na.rm = TRUE)) %>%
      collect()
    
    total_median <- base_lazy %>% 
      summarise(x = median(salary, na.rm = TRUE)) %>% 
      pull(x)
    
    final_tbl <- body_data %>% 
      arrange(desc(ICUF)) %>% 
      bind_rows(mns) %>% 
      left_join(rms) %>% 
      mutate(MEDIAN = replace_na(MEDIAN, total_median)) %>% 
      mutate(across(where(is.numeric), scales::comma)) %>% 
      mutate(across(everything(), ~replace_na(.x, ""))) %>%
      left_join(naics_descs) %>%
      relocate(naics, naics_desc) %>%
      mutate(naics_desc = replace_na(naics_desc, "")) %>%
      rename(NAICS = naics,
             Industry = naics_desc)
    
    
    return(final_tbl)
    
  })
  
  ##--------------------------------------------------------------------------##
  ## TABLE 5                                                                  ##
  ##--------------------------------------------------------------------------##
  
  TABLE5A <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(l2_desc == 'Registered Nursing, Nursing Administration, Nursing Research and Clinical Nursing')%>%
      count(expertise, expertise_desc, school_group) %>%
      arrange(expertise, school_group) %>%
      select(-expertise) %>%
      collect() %>%
      pivot_wider(names_from = school_group, values_from = n, values_fill = 0) %>%
      adorn_totals(c("row", "col")) %>%
      mutate(across(where(is.numeric), scales::comma)) %>%
      rename(`Degree Level` = expertise_desc) %>% 
      mutate(across(everything(), ~replace_na(.x, "")))
    
    return(datapull)
    
  })
  
  TABLE5B <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(l2_desc == 'Registered Nursing, Nursing Administration, Nursing Research and Clinical Nursing',
             school_group == 'ICUF')%>%
      count(expertise, expertise_desc, soc_title) %>%
      arrange(expertise) %>%
      select(-expertise) %>%
      collect() %>%
      mutate(soc_title = replace_na(soc_title, "Migrated")) %>%
      pivot_wider(names_from = expertise_desc, values_from = n) %>%
      arrange(soc_title == 'Migrated') %>%
      adorn_totals(c("row", "col")) %>%
      mutate(across(where(is.numeric), scales::comma)) %>%
      rename(`Detailed SOC` = soc_title) %>% 
      mutate(across(everything(), ~replace_na(.x, "")))
    
    return(datapull)
    
  })
  
  TABLE5C <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(!is.na(salary),
             l2_desc == 'Registered Nursing, Nursing Administration, Nursing Research and Clinical Nursing',
             school_group == 'ICUF')
    
    body_data <- base_lazy %>% 
      group_by(expertise, expertise_desc, soc_title) %>%
      summarise(amt = median(salary, na.rm = TRUE)) %>% 
      arrange(expertise, soc_title) %>%
      ungroup %>%
      select(-expertise) %>%
      collect() %>% 
      pivot_wider(names_from = expertise_desc, values_from = amt)
    
    mns <- base_lazy %>% 
      group_by(soc_title) %>% 
      summarise(MEDIAN = median(salary, na.rm = TRUE)) %>% 
      collect()
    
    rms <- base_lazy %>% 
      group_by(expertise_desc, expertise) %>%
      summarise(amt = median(salary, na.rm = TRUE)) %>%
      arrange(expertise) %>%
      select(-expertise) %>% 
      collect() %>%
      pivot_wider(names_from = expertise_desc, values_from = amt, values_fill = 0) %>%
      mutate(soc_title = 'MEDIAN')
    
    total_median <- base_lazy %>% 
      summarise(x = median(salary, na.rm = TRUE)) %>% 
      pull(x)
    
    final_tbl <- body_data %>% 
      bind_rows(rms) %>% 
      left_join(mns) %>% 
      mutate(MEDIAN = replace_na(MEDIAN, total_median),
             across(where(is.numeric), scales::comma)) %>% 
      mutate(across(everything(), ~replace_na(.x, ""))) %>%
      rename(`Job Title` = soc_title)
    
    return(final_tbl)
    
  })
  
  TABLE5D <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    claims_table <- models %>% filter(run_name == run_selection) %>% pull(claims_table)
    
    output_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name))
    claims_lazy <- tbl(tank, DBI::Id(schema = "rec", table = claims_table))
    
    soc_filter <- output_lazy %>% 
      filter(l2_desc == 'Registered Nursing, Nursing Administration, Nursing Research and Clinical Nursing',
             school_group == 'ICUF') %>% 
      filter(!is.na(soc_title)) %>% 
      select(soc_title)
    
    datapull <- claims_lazy %>% 
      semi_join(soc_filter, by = "soc_title") %>% 
      group_by(soc_title) %>% 
      summarise(across(c(soc_openings, claimed, laborgap, icuf_claimed), sum)) %>% 
      collect() %>% 
      adorn_totals("row") %>% 
      mutate(across(where(is.numeric), scales::comma)) %>% 
      rename(`Detailed SOC` = soc_title,
             `SOC Openings` = soc_openings,
             Claimed = claimed,
             `Labor Gap` = laborgap,
             `ICUF Claimed` = icuf_claimed) %>% 
      mutate(across(everything(), ~replace_na(.x, "")))
    
    return(datapull)
    
  })
  
  ##--------------------------------------------------------------------------##
  ## TABLE 6                                                                  ##
  ##--------------------------------------------------------------------------##
  
  TABLE6A <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(salary)) %>%
      group_by(l1_desc, school_group) %>%
      summarise(amt = sum(salary)) %>%
      arrange(l1_desc, school_group) %>%
      collect() %>%
      pivot_wider(names_from = school_group, values_from = amt) %>%
      arrange(desc(ICUF)) %>%
      adorn_totals(c("row", "col")) %>%
      mutate(across(where(is.numeric), scales::comma)) %>%
      mutate(across(everything(), ~replace_na(.x, ""))) %>%
      rename(`Degree Program` = l1_desc)
    
    return(datapull)
    
  })
  
  TABLE6B <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(!is.na(salary))
    
    body_data <- base_lazy %>% 
      group_by(l1_desc, school_group) %>% 
      summarise(amt = median(salary, na.rm = TRUE)) %>% 
      collect() %>% 
      pivot_wider(names_from = school_group, values_from = amt)
    
    mns <- base_lazy %>% 
      group_by(school_group) %>% 
      summarise(amt = median(salary, na.rm = TRUE)) %>% 
      collect() %>% 
      pivot_wider(names_from = school_group, values_from = amt) %>% 
      mutate(l1_desc = 'MEDIAN')
    
    rms <- base_lazy %>% 
      group_by(l1_desc) %>% 
      summarise(MEDIAN = median(salary, na.rm = TRUE)) %>% 
      collect()
    
    total_median <- base_lazy %>% 
      summarise(x = median(salary, na.rm = TRUE)) %>% 
      pull(x)
    
    final_tbl <- body_data %>% 
      arrange(desc(ICUF)) %>% 
      bind_rows(mns) %>% 
      left_join(rms, by = "l1_desc") %>% 
      mutate(MEDIAN = replace_na(MEDIAN, total_median)) %>% 
      mutate(across(where(is.numeric), scales::comma)) %>% 
      mutate(across(everything(), ~replace_na(.x, ""))) %>% 
      rename(`Degree Program` = l1_desc)
    
    return(final_tbl)
    
  })
  
  ##--------------------------------------------------------------------------##
  ## TABLE 7                                                                  ##
  ##--------------------------------------------------------------------------##
  
  TABLE7 <- reactive({
    
    run_selection <- input$selection
    
    tbls <- models %>% select(run_name, output_table)
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    claims_name <- models %>% filter(run_name == run_selection) %>% pull(claims_table)
    
    lg_lazy <- tbl(tank, DBI::Id(schema = "rec", table = claims_name)) %>% 
      filter(laborgap > 0) %>% 
      group_by(soc_title) %>% 
      summarise(lg = sum(laborgap, na.rm = TRUE))
    
    sal_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(!is.na(salary)) %>% 
      group_by(soc_title, cipdesc) %>% 
      summarise(med_sal = median(salary, na.rm = TRUE))
    
    joined_data <- sal_lazy %>% 
      inner_join(lg_lazy, by = "soc_title") %>% 
      mutate(potential = med_sal * lg)
    
    top_5_socs <- joined_data %>% 
      group_by(soc_title) %>% 
      summarise(max_pot = max(potential, na.rm = TRUE)) %>% 
      arrange(desc(max_pot)) %>% 
      head(5)
    
    final_table <- joined_data %>% 
      semi_join(top_5_socs, by = "soc_title") %>% 
      collect() %>% 
      arrange(desc(potential)) %>% 
      rename(`Job Title` = soc_title,
             `Degree Field` = cipdesc,
             `Median Salary` = med_sal,
             `Labor Gap` = lg,
             `Economic Potential` = potential) %>% 
      mutate(across(where(is.numeric), scales::comma))
    
    return(final_table)
    
  })
  
  ##--------------------------------------------------------------------------##
  ## TABLE 8                                                                  ##
  ##--------------------------------------------------------------------------##
  
  TABLE8A <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(choice_index)) %>%
      group_by(l1_desc, school_group) %>%
      summarise(amt = round(mean(choice_index), 3)) %>%
      arrange(l1_desc, school_group) %>%
      collect() %>%
      pivot_wider(names_from = school_group, values_from = amt) %>%
      arrange(ICUF) %>%
      mutate(across(where(is.numeric), as.character)) %>%
      mutate(across(everything(), ~replace_na(.x, ""))) %>%
      rename(`Degree Program` = l1_desc)
    
    return(datapull)
    
  })
  
  TABLE8B <- reactive({
    
    run_selection <- input$selection
    
    vrsn <- models %>% filter(run_name == run_selection) %>% pull(edition)
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    if (vrsn == "probabalistic") {
      
      base_tbl <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>%
        filter(!is.na(hire_probability)) %>%
        mutate(target_metric = ifelse(hire_probability == 1000, 1, 0))
      
    } else { 
      
      base_tbl <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>%
        filter(!is.na(in_field)) %>%
        mutate(target_metric = ifelse(in_field, 1, 0))
    }
    
    body <- base_tbl %>% 
      group_by(l1_desc, school_group) %>% 
      summarise(amt = mean(target_metric, na.rm = TRUE)) %>% 
      collect() %>% 
      arrange(l1_desc, school_group) %>% 
      pivot_wider(names_from = school_group, values_from = amt)
    
    cols <- base_tbl %>% 
      group_by(school_group) %>% 
      summarise(amt = mean(target_metric, na.rm = TRUE)) %>% 
      collect() %>% 
      pivot_wider(names_from = school_group, values_from = amt) %>% 
      mutate(l1_desc = 'MEAN')
    
    rows <- base_tbl %>% 
      group_by(l1_desc) %>% 
      summarise(MEAN = mean(target_metric, na.rm = TRUE)) %>% 
      collect()
    
    grand_tot <- base_tbl %>% 
      summarise(x = mean(target_metric, na.rm = TRUE)) %>% 
      pull(x)
    
    final_tbl <- body %>% 
      bind_rows(cols) %>% 
      left_join(rows) %>%
      mutate(MEAN = replace_na(MEAN, grand_tot),
             across(where(is.numeric), ~scales::percent(round(.x, 2)))) %>% 
      mutate(across(everything(), ~replace_na(.x, ""))) %>%
      rename(`Degree Field` = l1_desc)
    
    return(final_tbl)
    
  })
  
  ##--------------------------------------------------------------------------##
  ## TABLE 9                                                                  ##
  ##--------------------------------------------------------------------------##
  
  TABLE9 <- reactive({
    
    run_selection <- input$selection
    
    vrsn <- models %>% filter(run_name == run_selection) %>% pull(edition)
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    base_lazy <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>% 
      filter(!is.na(salary))
    
    if (vrsn == "probabalistic") {
      
      prepared_data <- base_lazy %>% 
        filter(!is.na(hire_probability)) %>% 
        mutate(in_major_col = hire_probability == 1000)
      
    } else {

      prepared_data <- base_lazy %>% 
        filter(!is.na(in_field)) %>% 
        mutate(in_major_col = in_field )
    }
    
    calc_median <- function(data, ...) {
      
      withmed <- data %>% 
        group_by(...) %>% 
        summarise(amt = median(salary, na.rm = TRUE)) %>% 
        collect()
      
      return(withmed)
    
    }
    
    detailed <- prepared_data %>% 
      calc_median(expertise, expertise_desc, school_group, in_major_col)
    
    overall <- prepared_data %>% 
      calc_median(school_group, in_major_col) %>% 
      mutate(expertise_desc = "Overall")
    
    final_tbl <- detailed %>% 
      bind_rows(overall) %>% 
      arrange(expertise, in_major_col, school_group) %>% 
      pivot_wider(names_from = school_group, values_from = amt) %>% 
      mutate(across(where(is.numeric), scales::comma)) %>% 
      rename(`Degree Level` = expertise_desc,
             `Employed In Major` = in_major_col) %>% 
      mutate(across(everything(), ~replace_na(.x, ""))) %>%
      ungroup %>%
      select(-expertise)
    
    return(final_tbl)
    
  })
  
  ##--------------------------------------------------------------------------##
  ## TABLE 10                                                                 ##
  ##--------------------------------------------------------------------------##
  
  TABLE10A <- reactive({
    
    run_selection <- input$selection
    
    claims_name <- models %>% filter(run_name == run_selection) %>% pull(claims_table)
    
    laborgaps <- tbl(tank, DBI::Id(schema = "rec", table = claims_name)) %>%
      group_by(naics, naics_industry) %>%
      summarise(across(c(laborgap, soc_openings, claimed, icuf_claimed),
                       ~sum(.x, na.rm = TRUE))) %>%
      mutate(no_icuf_lg = icuf_claimed + laborgap) %>%
      collect() %>%
      relocate(naics, naics_industry,
               soc_openings, claimed, icuf_claimed, laborgap, no_icuf_lg) %>%
      adorn_totals("row") %>%
      mutate(icuf_effect = round((no_icuf_lg / laborgap) - 1, 3),
             across(soc_openings:no_icuf_lg, scales::comma),
             icuf_effect = scales::percent(icuf_effect)) %>%
      rename(NAICS = naics,
             Industry = naics_industry,
             `Job Openings` = soc_openings, 
             `Modeled Claims` = claimed,
             `ICUF Claims` = icuf_claimed,
             `Labor Gap` = laborgap,
             `Labor Gap w. No ICUF` = no_icuf_lg,
             `ICUF Effect on Labor Market` = icuf_effect)
  
    return(laborgaps)
    
  })
  
  TABLE10B <- reactive({
    
    run_selection <- input$selection
    
    claims_name <- models %>% filter(run_name == run_selection) %>% pull(claims_table)
    
    laborgaps <- tbl(tank, DBI::Id(schema = "rec", table = claims_name)) %>%
      group_by(major_title) %>%
      summarise(across(c(laborgap, soc_openings, claimed, icuf_claimed),
                       ~sum(.x, na.rm = TRUE))) %>%
      mutate(no_icuf_lg = icuf_claimed + laborgap) %>%
      collect() %>%
      relocate(major_title, 
               soc_openings, claimed, icuf_claimed, laborgap, no_icuf_lg) %>%
      adorn_totals("row") %>%
      mutate(icuf_effect = round((no_icuf_lg / laborgap) - 1, 3),
             across(soc_openings:no_icuf_lg, scales::comma),
             icuf_effect = scales::percent(icuf_effect)) %>%
      rename(`Job Area` = major_title,
             `Job Openings` = soc_openings, 
             `Modeled Claims` = claimed,
             `ICUF Claims` = icuf_claimed,
             `Labor Gap` = laborgap,
             `Labor Gap w. No ICUF` = no_icuf_lg,
             `ICUF Effect on Labor Market` = icuf_effect)
    
    return(laborgaps)
    
  })
  
  ##--------------------------------------------------------------------------##
  ## TABLE 11                                                                 ##
  ##--------------------------------------------------------------------------##
  
  TABLE11A <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), school_group == 'ICUF') %>%
      count(naics_industry, l1_desc) %>%
      group_by(naics_industry) %>%
      slice_max(order_by = n, n = 1) %>%
      collect() %>%
      mutate(n = scales::comma(n)) %>%
      rename(Industry = naics_industry,
             `Top Program` = l1_desc,
             `ICUF Claims` = n)
    
    return(datapull)
    
  })
  
  TABLE11B1 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Accommodation and food services') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B2 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Health care and social assistance') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B3 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Professional and business services') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B4 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Arts, entertainment, and recreation') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B5 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Information') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B6 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Real estate and rental and leasing') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B7 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Construction') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B8 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Mining and logging') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B9 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Retail trade') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B10 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Durable goods manufacturing') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B11 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Nondurable goods manufacturing') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B12 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Transportation, warehousing, and utilities') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B13 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Finance and insurance') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B14 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Other services') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B15 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Wholesale trade') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B16 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Government') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  TABLE11B17 <- reactive({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>%
      filter(!is.na(naics_industry), 
             school_group == 'ICUF', 
             naics_industry == 'Private educational services') %>%
      count(l1_desc) %>%
      slice_max(order_by = n, n = 10) %>%
      collect() %>%
      mutate(n = scales::comma(n),
             ORD = row_number()) %>%
      relocate(ORD) %>%
      rename(`Program` = l1_desc,
             `ICUF Claims` = n,
             ` ` = ORD)
    
    return(datapull)
    
  })
  
  ##--------------------------------------------------------------------------##
  ## TABLE 12                                                                 ##
  ##--------------------------------------------------------------------------##
  
  ##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\##
  ## RENDER                                                                   >#
  ##//////////////////////////////////////////////////////////////////////////##
  
  output$t1a <- renderTable({
    
    TABLE1A()
    
  }, striped = TRUE)
  
  output$t1b <- renderTable({
    
    TABLE1B()
    
  }, striped = TRUE)
  
  output$t1c <- renderTable({
    
    TABLE1C()
    
  }, striped = TRUE)
  
  output$t1d <- renderTable({
    
    TABLE1D()
    
  }, striped = TRUE)
  
  output$t1e <- renderTable({
    
    TABLE1E()
    
  }, striped = TRUE)
  
  output$t1f <- renderTable({
    
    TABLE1F()
    
  }, striped = TRUE)
  
  output$t1g <- renderTable({
    
    TABLE1G()
    
  }, striped = TRUE)
  
  output$t2a1 <- renderTable({
    
    TABLE2A1()
    
  }, striped = TRUE)
  
  output$t2a2 <- renderTable({
    
    TABLE2A2()
    
  }, striped = TRUE)
  
  output$t2a3 <- renderTable({
    
    TABLE2A3()
    
  }, striped = TRUE)
  
  output$t2a4 <- renderTable({
    
    TABLE2A4()
    
  }, striped = TRUE)
  
  output$t2b1 <- renderTable({
    
    TABLE2B1()
    
  }, striped = TRUE)
  
  output$t2b2 <- renderTable({
    
    TABLE2B2()
    
  }, striped = TRUE)
  
  output$t2b3 <- renderTable({
    
    TABLE2B3()
    
  }, striped = TRUE)
  
  output$t2b4 <- renderTable({
    
    TABLE2B4()
    
  }, striped = TRUE)
  
  output$t2c1 <- renderTable({
    
    TABLE2C1()
    
  }, striped = TRUE)
  
  output$t2c2 <- renderTable({
    
    TABLE2C2()
    
  }, striped = TRUE)
  
  output$t2c3 <- renderTable({
    
    TABLE2C3()
    
  }, striped = TRUE)
  
  output$t2c4 <- renderTable({
    
    TABLE2C4()
    
  }, striped = TRUE)
  
  output$t2d1 <- renderTable({
    
    TABLE2D1()
    
  }, striped = TRUE)
  
  output$t2d2 <- renderTable({
    
    TABLE2D2()
    
  }, striped = TRUE)
  
  output$t2d3 <- renderTable({
    
    TABLE2D3()
    
  }, striped = TRUE)
  
  output$t2d4 <- renderTable({
    
    TABLE2D4()
    
  }, striped = TRUE)
  
  output$t2e1 <- renderTable({
    
    TABLE2E1()
    
  }, striped = TRUE)
  
  output$t2e2 <- renderTable({
    
    TABLE2E2()
    
  }, striped = TRUE)
  
  output$t2e3 <- renderTable({
    
    TABLE2E3()
    
  }, striped = TRUE)
  
  output$t2e4 <- renderTable({
    
    TABLE2E4()
    
  }, striped = TRUE)
  
  output$t4a <- renderTable({
    
    TABLE4A()
    
  }, striped = TRUE)
  
  output$t4b <- renderTable({
    
    TABLE4B()
    
  }, striped = TRUE)
  
  output$t5a <- renderTable({
    
    TABLE5A()
    
  }, striped = TRUE)
  
  output$t5b <- renderTable({
    
    TABLE5B()
    
  }, striped = TRUE)
  
  output$t5c <- renderTable({
    
    TABLE5C()
    
  }, striped = TRUE)
  
  output$t5d <- renderTable({
    
    TABLE5D()
    
  }, striped = TRUE)
  
  output$t6a <- renderTable({
    
    TABLE6A()
    
  }, striped = TRUE)
  
  output$t6b <- renderTable({
    
    TABLE6B()
    
  }, striped = TRUE)
  
  output$t7 <- renderTable({
    
    TABLE7()
    
  }, striped = TRUE)
  
  output$t8a <- renderTable({
    
    TABLE8A()
    
  }, striped = TRUE)
  
  output$t8b <- renderTable({
    
    TABLE8B()
    
  }, striped = TRUE)
  
  output$t9 <- renderTable({
    
    TABLE9()
    
  }, striped = TRUE)
  
  output$t10a <- renderTable({
    
    TABLE10A()
    
  }, striped = TRUE)
  
  output$t10b <- renderTable({
    
    TABLE10B()
    
  }, striped = TRUE)
  
  output$t11a <- renderTable({
    
    TABLE11A()
    
  }, striped = TRUE)
  
  output$t11b1 <- renderTable({
    
    TABLE11B1()
    
  }, striped = TRUE)
  
  output$t11b2 <- renderTable({
    
    TABLE11B2()
    
  }, striped = TRUE)
  
  output$t11b3 <- renderTable({
    
    TABLE11B3()
    
  }, striped = TRUE)
  
  output$t11b4 <- renderTable({
    
    TABLE11B4()
    
  }, striped = TRUE)
  
  output$t11b5 <- renderTable({
    
    TABLE11B5()
    
  }, striped = TRUE)
  
  output$t11b6 <- renderTable({
    
    TABLE11B6()
    
  }, striped = TRUE)
  
  output$t11b7 <- renderTable({
    
    TABLE11B7()
    
  }, striped = TRUE)
  
  output$t11b8 <- renderTable({
    
    TABLE11B8()
    
  }, striped = TRUE)
  
  output$t11b9 <- renderTable({
    
    TABLE11B9()
    
  }, striped = TRUE)
  
  output$t11b10 <- renderTable({
    
    TABLE11B10()
    
  }, striped = TRUE)
  
  output$t11b11 <- renderTable({
    
    TABLE11B11()
    
  }, striped = TRUE)
  
  output$t11b12 <- renderTable({
    
    TABLE11B12()
    
  }, striped = TRUE)
  
  output$t11b13 <- renderTable({
    
    TABLE11B13()
    
  }, striped = TRUE)
  
  output$t11b14 <- renderTable({
    
    TABLE11B14()
    
  }, striped = TRUE)
  
  output$t11b15 <- renderTable({
    
    TABLE11B15()
    
  }, striped = TRUE)
  
  output$t11b16 <- renderTable({
    
    TABLE11B16()
    
  }, striped = TRUE)
  
  output$t11b17 <- renderTable({
    
    TABLE11B17()
    
  }, striped = TRUE)
  
  output$g1 <- renderPlotly({
    
    run_selection <- input$selection
    
    vrsn <- models %>% filter(run_name == run_selection) %>% pull(edition)
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    if (vrsn == "probabalistic") {
      
      base_tbl <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>%
        filter(!is.na(hire_probability)) %>%
        mutate(target_metric = ifelse(hire_probability == 1000, 1, 0))
      
    } else { 
      
      base_tbl <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>%
        filter(!is.na(in_field)) %>%
        mutate(target_metric = ifelse(in_field, 1, 0))
    }
    
    plot_data_lazy <- base_tbl %>%
      filter(school_group == 'ICUF') %>% 
      group_by(cipdesc) %>% 
      summarise(agsal = sum(salary, na.rm = TRUE),
                n = n(),
                inF = sum(target_metric, na.rm = TRUE),
                medsal = median(salary, na.rm = TRUE)) %>% 
      mutate(inf_pct = inF / n) %>% 
      arrange(desc(agsal)) %>% 
      head(input$nobs1)
    
    plot_data <- plot_data_lazy %>% 
      collect()
    
    g <- ggplot(plot_data, aes(x = medsal, y = n, size = agsal, fill = inf_pct, text = cipdesc)) + 
      geom_point(pch = 21, color = 'black', alpha = 0.8) + 
      scale_fill_gradient(high = 'goldenrod', low = 'royalblue', 
                          name = 'PCT Working \nin Major', 
                          labels = scales::percent) + 
      scale_size(range = c(4, 20), guide = 'none') + 
      scale_y_continuous(labels = scales::comma) + 
      scale_x_continuous(labels = scales::comma) + 
      coord_cartesian(ylim = c(0, 4200)) +
      labs(title = str_c('Top ', input$nobs1, ' CIPs By Aggregate Salary Attainment'),
           subtitle = 'Size: Total earnings',
           x = 'Median Salary', y = 'Graduate Count') + 
      theme_black()
    
    ggplotly(g, tooltip = "text") %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$g2 <- renderPlotly({
    
    run_selection <- input$selection
    
    vrsn <- models %>% filter(run_name == run_selection) %>% pull(edition)
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    if (vrsn == "probabalistic") {
      
      base_tbl <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>%
        filter(!is.na(hire_probability)) %>%
        mutate(target_metric = ifelse(hire_probability == 1000, 1, 0))
      
    } else { 
      
      base_tbl <- tbl(tank, DBI::Id(schema = "rec", table = table_name)) %>%
        filter(!is.na(in_field)) %>%
        mutate(target_metric = ifelse(in_field, 1, 0))
    }
    
    plot_data <- base_tbl %>% 
      filter(school_group == 'ICUF') %>% 
      group_by(cipcode, cipdesc) %>% 
      summarise(agsal = sum(salary, na.rm = TRUE),
                n = n(),
                inF = sum(target_metric, na.rm = TRUE),
                medsal = median(salary, na.rm = TRUE)) %>% 
      filter(n <= 500) %>% 
      arrange(desc(agsal)) %>% 
      head(input$nobs2) %>% 
      collect() %>% 
      mutate(inf_pct = inF / n)
    
    g <- ggplot(plot_data, aes(x = medsal, y = n, size = agsal, fill = inf_pct, text = cipdesc)) +
      geom_point(pch = 21, color = 'black') +
      scale_fill_gradient(high = 'gold', low = 'royalblue', labels = scales::percent, name = "In Major %") +
      scale_size(range = c(4, 40), guide = 'none') +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      labs(title = str_c('Top ', input$nobs2, ' CIPs By Aggregate Salary Attainment, Small Programs Only'),
           subtitle = 'Size: Total earnings',
           x = 'Median Salary', y = 'Graduate Count') +
      theme_black()
    
    ggplotly(g, tooltip = "text") %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$model_description <- renderText({
    
    desc_text <- models %>% filter(run_name == input$selection) %>% pull(run_summary)
    
    return(desc_text)
    
  })
  
  output$model_date <- renderText({
    
    date_text <- models %>% filter(run_name == input$selection) %>% pull(run_prd)
    
    return(str_c("Model Published ", date_text))
    
  })
  
  output$outputraw <- downloadHandler(
    
    filename = function() {
      
      run_selection <- input$selection
      
      nme = str_c(run_selection, "_stdnts_", Sys.Date(), ".csv")
      
      return(nme)
      
    },
    
    content = function(file) {
      
      run_selection <- input$selection
      
      table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
      
      datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>% collect()
      
      readr::write_csv(datapull, file)
      
    }
    
  )
  
  output$claimsraw <- downloadHandler(
    
    filename = function() {
      
      run_selection <- input$selection
      
      nme = str_c(run_selection, "_claims_", Sys.Date(), ".csv")
      
      return(nme)
      
    },
    
    content = function(file) {
      
      run_selection <- input$selection
      
      table_name <- models %>% filter(run_name == run_selection) %>% pull(claims_table)
      
      datapull <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>% collect()
      
      readr::write_csv(datapull, file)
      
    }
    
  )
  
  output$outputrawsize <- renderText({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(output_table)
    
    sze <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>% 
      count() %>% 
      pull()
    
    sze <- round((sze * 500) / (1024^2), 2)
    
    return(str_c("~", sze, " MB"))
    
  })
  
  output$claimsrawsize <- renderText({
    
    run_selection <- input$selection
    
    table_name <- models %>% filter(run_name == run_selection) %>% pull(claims_table)
    
    sze <- tbl(tank, DBI::Id(schema = 'rec', table = table_name)) %>% 
      count() %>% 
      pull()
    
    sze <- round((sze * 500) / (1024^2), 2)
    
    return(str_c("~", sze, " MB"))
    
  })
  
  ##==========================================================================##
  ## SESSION END                                                              ##
  ##==========================================================================##
  
  onStop(function() {
    poolClose(tank)
  })
  
}

shinyApp(ui = ui, server = server)