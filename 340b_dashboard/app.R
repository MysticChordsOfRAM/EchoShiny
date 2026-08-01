library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(plotly)
library(RSQLite)
library(DBI)

con <- dbConnect(SQLite(), "./data/340b_data.db", flags = SQLITE_RO)

incomes <- dbGetQuery(con, "SELECT * FROM incomes")

samples <- dbGetQuery(con, "SELECT * FROM samples")

sample_contents <- dbGetQuery(con, "SELECT * FROM sample_contents")

pop_data <- dbGetQuery(con, "SELECT * FROM population") %>% mutate(orgfldt = as_date(orgfldt))

dbDisconnect(con)
  
current <- incomes %>% filter(yr == max(yr)) %>% select(geofips, income)
ymin <- incomes %>% pull(income) %>% min %>% {. * 0.95}
ymax <- incomes %>% pull(income) %>% max %>% {. * 1.05}

la_shape <- readRDS("./data/la_shape.rds")

ui <- navbarPage(
  title = "340b Underlying Data Explorer",
  tabPanel("Parish Income Map",
           sidebarLayout(
             sidebarPanel(
               width = 4,
               h3("Parish Per Capita Income"),
               p("Select a Parish to see it's income over time"),
               hr(),
               leafletOutput("LEAFMAP", height = "500px"),
               br(),
               actionButton("clear", "Clear All Selections", class = "btn-danger")
             ),
             mainPanel(
               width = 8,
               plotlyOutput("INCOMEPLOT", height = '600px')
             )
           )
         ),
  
  tabPanel("Sample Comparison",
           fluidPage(
             fluidRow(
               column(width = 4, offset = 4,
                      br(),
                      div(style = "text-align: center;",
                          selectizeInput("SAMPLESELECT",
                                         "Select a Sample: ",
                                         choices = NULL,
                                         multiple = FALSE,
                                         width = '100%')
                          )
                      )
             ),
             hr(),
             tabsetPanel(
               id = "sample_tabs",
               
               # Geography Sub-Tab
               tabPanel("Geography",
                        br(),
                        fluidRow(
                          column(6, plotlyOutput("geo_plot_1")),
                          column(6, plotlyOutput("geo_plot_2"))
                        )
               ),
               
               # Finances Sub-Tab
               tabPanel("Finances",
                        br(),
                        fluidRow(
                          column(6, plotOutput("fin_plot_1", width = "800px", height = "600px")),
                          column(6, plotOutput("fin_plot_2", width = "800px", height = "600px"))
                        )
               ),
               
               # Time Sub-Tab
               tabPanel("Time",
                        br(),
                        fluidRow(
                          column(6, plotlyOutput("time_plot_1")),
                          column(6, plotlyOutput("time_plot_2"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected_parishes <- reactiveVal(c())
  
  observeEvent(input$LEAFMAP_shape_click, {
    req(input$LEAFMAP_shape_click$id)
    
    current_id <- input$LEAFMAP_shape_click$id
    existing_list <- selected_parishes()
    
    if (current_id %in% existing_list) {
      new_list <- setdiff(existing_list, current_id)
    } else {
      new_list <- c(existing_list, current_id)
    }
    
    selected_parishes(new_list)
    
  })
  
  observe({
    updateSelectizeInput(session, "SAMPLESELECT", choices = samples$present, server = TRUE)
  })
  
  selected_sample_data <- reactive({
    req(input$SAMPLESELECT)
    sample_name <- samples %>% filter(present == input$SAMPLESELECT) %>% pull(grp_name)
    sample_members <- sample_contents %>% filter(grp_name == sample_name) %>% pull(pacer_no)
  })
  
  observeEvent(input$clear, {
    selected_parishes(c())
  })
  
  observe({
    proxy <- leafletProxy("LEAFMAP")
    proxy %>% clearGroup("highlight")
    
    selected_shapes <- la_shape %>%
      filter(GEOID %in% selected_parishes())
    
    if (nrow(selected_shapes) > 0) {
      proxy %>%
        addPolygons(
          data = selected_shapes,
          fillColor = 'red',
          weight = 3,
          color = 'white',
          group = 'highlight'
        )
    }
  })
  
  
  output$LEAFMAP <- renderLeaflet({
    
    leaflet(la_shape) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -91.9623, lat = 30.9843, zoom = 7) %>%
      addPolygons(
        layerId = ~GEOID,
        fillColor = "#74add1",
        weight = 1,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~parish
      )
  })
  
  output$INCOMEPLOT <- renderPlotly({
    req(length(selected_parishes()) > 0)
    
    plot_data <- incomes %>%
      filter(geofips %in% selected_parishes())
    
    p <- ggplot(plot_data, aes(x = yr, y = income, color = geoname, group = geoname)) + 
      geom_line(linewidth = 1) + 
      geom_point(size = 1) + 
      scale_y_continuous(labels = scales::dollar, limits = c(ymin, ymax)) + 
      scale_color_brewer(palette = "Set1", name = "Parish") +
      theme_bw() + 
      labs(title = "Personal Income Per Capital For Louisiana Parishes")
    
    ggplotly(p) %>%
      layout(hovermode = 'closest')
    
  })
  
  output$geo_plot_1 <- renderPlotly({
    
    req(selected_sample_data())
    
    pop_ord <- pop_data %>%
      group_by(d1cnty) %>%
      tally() %>%
      arrange(n) %>%
      rowid_to_column()
    
    popCum <- pop_ord %>%
      mutate(cs = cumsum(n),
             POP_cumPct = cs / sum(n)) %>%
      select(d1cnty, POP_cumPct)
    
    sampCum <- pop_data %>%
      filter(pacer_no %in% selected_sample_data()) %>%
      group_by(d1cnty) %>%
      tally() %>%
      rename(ns = n) %>%
      left_join(pop_ord) %>%
      arrange(rowid) %>%
      mutate(cs = cumsum(ns),
             SAMP_cumPct = cs / sum(ns)) %>%
      select(d1cnty, SAMP_cumPct)
      
      dd <- full_join(popCum, sampCum) %>%
        fill(SAMP_cumPct) %>%
        mutate(delt = abs(POP_cumPct - SAMP_cumPct),
               across(POP_cumPct:delt, ~round(.x, 2)))
      
      p <- ggplot(dd, aes(x = SAMP_cumPct, y = POP_cumPct, color = delt)) + 
        geom_abline(intercept = 0, slope = 1, color = 'grey55') +
        geom_point(size = 1) + 
        scale_y_continuous(labels = scales::percent, limits = c(0,1)) + 
        scale_x_continuous(labels = scales::percent, limits = c(0,1)) + 
        scale_color_viridis_b(name = 'Deviance') + 
        labs(title = "Cumulative Proportional Representaion of LA Parishes",
             x = "Chosen Sample", y = "Population") + 
        theme_bw()
      
      ggplotly(p,
               width = 800,
               height = 600,) %>% 
        layout(
          xaxis = list(
            scaleanchor = "y", 
            scaleratio = 1,
            tickformat = ".0%"
          ),
          yaxis = list(
            tickformat = ".0%"
          ),

          autosize = FALSE,
          hovermode = 'closest'
          )
  })
  
  output$geo_plot_2 <- renderPlotly({
    
    req(input$SAMPLESELECT)
    
    sample_counts <- pop_data %>%
      filter(pacer_no %in% selected_sample_data()) %>%
      mutate(d1cnty = as.character(d1cnty)) %>%
      group_by(d1cnty) %>%
      tally(name = "sample_count")
    
    map_data <- la_shape %>%
      left_join(sample_counts, by = c("GEOID" = "d1cnty")) %>%
      mutate(sample_count = replace_na(sample_count, 0))
    
    p <- ggplot(map_data) + 
      geom_sf(aes(fill = sample_count,
                  text = str_c(parish, " Parish",
                               "<br>Sample Count: ", sample_count)),
                  color = "white", size = 0.1) +
      scale_fill_viridis_c(option = "plasma", name = "Cases in Sample") + 
      theme_void() + 
      labs(title = "Where do Debtors live in this sample?")
    
    ggplotly(p, width = 800, height = 600, tooltip = "text") %>% layout(autosize = FALSE)
    
  })
  
  output$fin_plot_1 <- renderPlot({
    
    req(input$SAMPLESELECT)
    
    balance_sheet <- pop_data %>%
      select(pacer_no, totassts, totlblts) %>%
      mutate(insamp = pacer_no %in% selected_sample_data())

    nosamp <- balance_sheet %>% filter(!insamp)

    issamp <- balance_sheet %>% filter(insamp)
    
    ggplot() + 
      geom_jitter(data = nosamp, aes(x = totassts, y = totlblts), color = "grey35", size = 1, alpha = 0.4) +
      geom_jitter(data = issamp, aes(x = totassts, y = totlblts), color = "orangered", size = 2, alpha = 0.8) + 
      scale_x_continuous(labels = scales::dollar) +
      scale_y_continuous(labels = scales::dollar) +
      labs(title = 'Assets and Liabilities of Debtors',
           x = "Total Assets at time of Bankruptcy", y = "Total Liabilities at time of Bankruptcy") + 
      coord_cartesian(xlim = c(0, 1500000), ylim = c(0, 1500000)) +
      theme_bw()
    
  })
  
  output$fin_plot_2 <- renderPlot({
    
    req(input$SAMPLESELECT)
    
    balance_sheet <- pop_data %>%
      select(pacer_no, avgmnthi, avgmnthe) %>%
      mutate(insamp = pacer_no %in% selected_sample_data())
    
    nosamp <- balance_sheet %>% filter(!insamp)

    issamp <- balance_sheet %>% filter(insamp)

    ggplot(balance_sheet, aes(x = avgmnthi, y = avgmnthe, color = insamp, size = insamp, alpha = insamp)) + 
      geom_jitter(data = nosamp, aes(x = avgmnthi, y = avgmnthe), color = "grey35", size = 1, alpha = 0.4) +
      geom_jitter(data = issamp, aes(x = avgmnthi, y = avgmnthe), color = "orangered", size = 2, alpha = 0.8) + 
      scale_x_continuous(labels = scales::dollar) +
      scale_y_continuous(labels = scales::dollar) +
      labs(title = 'Income and Expenses of Debtors',
           x = "Average Monthly Income at time of Bankruptcy", y = "Average Monthly Expenses at time of Bankruptcy") + 
      coord_cartesian(xlim = c(0, 13500), ylim = c(0, 13500)) +
      theme_bw()
  
  })
  
  output$time_plot_1 <- renderPlotly({
    req(input$SAMPLESELECT)
    
    sample_ids <- selected_sample_data()
    
    pop_time <- pop_data %>% select(orgfldt) %>% mutate(Group = "Population")
    samp_time <- pop_data %>% filter(pacer_no %in% sample_ids) %>% 
      select(orgfldt) %>% mutate(Group = "Sample")
    
    combined_time <- bind_rows(pop_time, samp_time)
    
    p1 <- ggplot(combined_time, aes(x = orgfldt, fill = Group, color = Group)) +
      geom_density(alpha = 0.3) + 
      scale_fill_manual(values = c("Sample" = "red", "Population" = "black")) +
      scale_color_manual(values = c("Sample" = "red", "Population" = "black")) +
      scale_y_continuous(labels = scales::percent) +
      theme_bw() +
      labs(title = "Normalized Filing Trends (Shape Comparison)",
           x = "Filing Date", y = "Relative Density")
    
    ggplotly(p1)
  })
  
  output$time_plot_2 <- renderPlotly({
    req(input$SAMPLESELECT)
    
    sample_ids <- selected_sample_data()
    
    pop_monthly <- pop_data %>%
      mutate(month = floor_date(orgfldt, "month")) %>%
      count(month, name = "pop_n") %>%
      mutate(pop_pct = pop_n / sum(pop_n))
    
    samp_monthly <- pop_data %>%
      filter(pacer_no %in% sample_ids) %>%
      mutate(month = floor_date(orgfldt, "month")) %>%
      count(month, name = "samp_n") %>%
      mutate(samp_pct = samp_n / sum(samp_n))
    
    time_comp <- full_join(pop_monthly, samp_monthly, by = "month") %>%
      replace_na(list(pop_pct = 0, samp_pct = 0))
    
    p2 <- ggplot(time_comp, aes(x = month)) +
      geom_line(aes(y = pop_pct, color = "Population"), size = 1) +
      geom_line(aes(y = samp_pct, color = "Sample"), size = 1) +
      scale_color_manual(values = c("Sample" = "red", "Population" = "black")) +
      scale_y_continuous(labels = scales::percent) +
      theme_bw() +
      labs(title = "Monthly Filing Intensity (% of Total)",
           x = "Month", y = "% of Total Filings", color = "Group")
    
    ggplotly(p2) %>% layout(hovermode = "closest")
  })
  
}

shinyApp(ui, server)
