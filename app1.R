library(shiny)
library(shinythemes)
library(RCurl)
library(ggplot2)
library(data.table)
library(DT)

not_sel <- "Not selected"

#another option : ggvis
draw_plot_1 <- function(data_input, num_var_1, num_var_2, fact_var){
  if(fact_var!=not_sel){
    data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
  }
  if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
      geom_point()
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2)) +
      geom_point(color="blue") +
      geom_smooth(method=lm)
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_1)) +
      geom_violin()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_2)) +
      geom_violin()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1)) +
      geom_histogram()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_2)) +
      geom_histogram(fill="maroon")
  }
  else if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var)) +
      geom_bar(fill="maroon")
  }
}

####### Summary stat ########

create_num_var_table <- function(data_input, num_var){
  if(num_var != not_sel){
    col <- data_input[,get(num_var)]
    if (length(col)>5000) col_norm <- sample(col,5000) else col_norm <- col
    norm_test <- shapiro.test(col_norm)
    statistic <- c("mean", "median", "5th percentile", "95th percentile",
                   "Shapiro statistic", "Shapiro p-value","Max","Min","Variance")
    value <- c(round(mean(col),2), round(median(col),2),
               round(quantile(col, 0.05),2), round(quantile(col, 0.95),2),
               norm_test$statistic, norm_test$p.value,max(col),min(col),var(col))
    data.table(statistic, value)
  }
}

create_fact_var_table <- function(data_input, fact_var){
  if(fact_var != not_sel){
    freq_tbl <- data_input[,.N, by = get(fact_var)]
    freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
    freq_tbl
  }
}

########## Combined stat ##########

create_combined_table <- function(data_input, num_var_1, num_var_2, fact_var){
  if(fact_var != not_sel){
    if(num_var_1 != not_sel & num_var_2 != not_sel){
      res_tbl <- data_input[,.(correlation = cor(get(num_var_1), get(num_var_2))), by = fact_var]
    }
    else if(num_var_1 != not_sel & num_var_2 == not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_1))), by = fact_var]
    }
    else if(num_var_1 == not_sel & num_var_2 != not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_2))), by = fact_var]
    }
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel){
    res_tbl <- data.table(
      statistic = c("correlation"),
      value = c(cor(
        data_input[,get(num_var_1)],
        data_input[,get(num_var_2)])))
    if (res_tbl$value == -1) {
      print("A perfect downhill (negative) linear relation between" & num_var_1 & "and" & num_var_2 & ".")
    }
    else if (res_tbl$value == -0.7) {
      print("A strong downhill (negative) linear relationship between" & num_var_1 & "and" & num_var_2 & ".")
    }
    else if (res_tbl$value == - 0.5) {
      print("A moderate downhill (negative) relationship between" & num_var_1 & "and" & num_var_2 & ".")
    }
    else if (res_tbl$value == - 0.3) {
      print(" A weak downhill (negative) linear relationship between" & num_var_1 & "and" & num_var_2 & "." )
    }
    else if (res_tbl$value == 0) {
      print("There is no linear relationship between" & num_var_1 & "and" & num_var_2 & ".")
    }
    else if (res_tbl$value == 0.3) {
      print("A weak uphill (positive) linear relationship between" & num_var_1 & "and" & num_var_2 & "." )
    }
    else if (res_tbl$value >= 0.03 & res_tbl$value <= 0.5) {
      print("A moderate uphill (positive) relationship between" & num_var_1 & "and" & num_var_2 & "." )
    }
    else if (res_tbl$value >= 0.7) {
      text(" A strong uphill (positive) linear relationship between" & num_var_1 & "and" & num_var_2 & ".")
    }
    else if (res_tbl$value == 1) {
      print("A perfect uphill (positive) linear relationship between" & num_var_1 & "and" & num_var_2 & ".")
    }
  }
  return(res_tbl)
}

###########################
############ UI ###########
###########################

ui <- fluidPage(navbarPage(title = "My first app", 
                   theme = shinytheme("cerulean"),
                   
                   tabPanel("Data",
                            sidebarLayout(
                              sidebarPanel(
                                fileInput("file1", "Choose CSV File",
                                          multiple = FALSE,
                                          accept = c("text/csv","text/comma-separated-values,text/plain","csv")),
                                tabPanel("file1", DT::dataTableOutput("mytable1")),
                                checkboxGroupInput("show_vars", "Columns to show:",
                                                   names("file1"), selected = names("file1")),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Checkbox if file has header ----
                                checkboxInput("header", "Header", TRUE),
                                
                                checkboxGroupInput("show_vars", "Columns in dataset to show:",
                                                   names("file1"), selected = names("file1")),
                                
                                # Input: Select separator ----
                                radioButtons("sep", "Separator",
                                             choices = c(Comma = ",",
                                                         Semicolon = ";",
                                                         Tab = "\t"),
                                             selected = ","),
                                
                                # Horizontal line ----
                                tags$hr(),
                                
                                # Input: Select number of rows to display ----
                                radioButtons("disp", "Display",
                                             choices = c(Head = "head",
                                                         All = "all"),
                                             selected = "head")
                                
                              ),
                              
                              # Main panel for displaying outputs ----
                              mainPanel(
                                
                                # Output: Data file ----
                                tableOutput("contents")
                                
                              )
                              
                            )
                   ),
                   tabPanel("Statistic",
                            sidebarLayout(
                                  sidebarPanel(
                                      # title = "File slected",
                                      # fileInput("file1", "Select CSV File to Import", accept = ".csv"),
                                      selectInput("num_var_1", "Numerical Variable 1", choices = c(not_sel)),
                                      selectInput("num_var_2", "Numerical Variable 2", choices = c(not_sel)),
                                      selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
                                      br(),
                                      actionButton("run_button", "Run Analysis", icon = icon("play")),
                                      radioButtons(inputId = "plot_1","Select format to download the plot",choices = list("png","pdf"))),
                              
                              mainPanel(
                                tabsetPanel(
                                  tabPanel(
                                    title = "Plot",
                                    plotOutput("plot_1"),
                                    downloadButton("downloadPlot","Download")
                                    
                                  ),
                                  tabPanel(
                                    title = "Statistics",
                                    fluidRow(
                                      column(width = 4, strong(textOutput("num_var_1_title"))),
                                      column(width = 4, strong(textOutput("num_var_2_title"))),
                                      column(width = 4, strong(textOutput("fact_var_title")))
                                    ),
                                    fluidRow(
                                      column(width = 4, tableOutput("num_var_1_summary_table")),
                                      column(width = 4, tableOutput("num_var_2_summary_table")),
                                      column(width = 4, tableOutput("fact_var_summary_table"))
                                    ),
                                    fluidRow(
                                      column(width = 12, strong("Combined Statistics"))
                                    ),
                                    fluidRow(
                                      column(width = 12, tableOutput("combined_summary_table"))
                                    )
                              )
                            )
                   ))),
                   ## Use navbarmenu to get the tab with menu capabilities
                   navbarMenu("Menu Options",
                              tabPanel("Menu item A - Summary stats", verbatimTextOutput("summ")),
                              tabPanel("Menu item B - Link to code",
                                       h4(HTML(paste("Thank you for using my app", a(href="", "link"), "."))),
                                       h4(HTML(paste("In case you have questions", a(href="linhday2301@gmail.com", "Email me"), ".")))
                                       
                              )),
                   tabPanel("About", 
                            h4("Hi my name is Linh, I'm a student from IUT2's STID department in University Grenoble Alpes.",
                               br(),
                               "This is my first app for statistical analysis")
                   )))

##########################
######## SERVER ##########
##########################

server <- function(input, output ,session){
  data_input <- reactive({
    req(input$file1)
    fread(input$file1$datapath)
  })
  
######### plot ##########
  plot_1 <- eventReactive(input$run_button,{
    draw_plot_1(data_input(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$plot_1 <- renderPlot(plot_1())
  
######### List of variable #########
  observeEvent(data_input(),{
    choices <- c(not_sel,names(data_input()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
  })
  
  num_var_1 <- eventReactive(input$run_button,input$num_var_1)
  num_var_2 <- eventReactive(input$run_button,input$num_var_2)
  fact_var <- eventReactive(input$run_button,input$fact_var)
  
######### Summary tables #########
  
  output$num_var_1_title <- renderText(paste("Num Var 1:",num_var_1()))
  
  num_var_1_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var_1())
  })
  
  output$num_var_1_summary_table <- renderTable(num_var_1_summary_table(),colnames = FALSE)
  
  output$num_var_2_title <- renderText(paste("Num Var 2:",num_var_2()))
  
  num_var_2_summary_table <- eventReactive(input$run_button,{
    create_num_var_table(data_input(), num_var_2())
  })
  
  output$num_var_2_summary_table <- renderTable(num_var_2_summary_table(),colnames = FALSE)
  
  output$fact_var_summary_table <- renderTable(fact_var_summary_table(),colnames = FALSE)
  
  output$fact_var_title <- renderText(paste("Factor Var:",fact_var()))
  
  fact_var_summary_table <- eventReactive(input$run_button,{
    create_fact_var_table(data_input(), fact_var())
  })
  
  output$fact_var_summary_table <- renderTable(fact_var_summary_table(),colnames = FALSE)
  
######## Display column to show ###########
  # table_1 = df[nrow(df), ]
  # output$mytable1 <- DT::renderDataTable({
  #   DT::datatable(table_1[, input$show_vars, drop = FALSE])
  # })
  
####### Display summary table for dataset ##########
  output$summ <- renderPrint({
    summary()
  })
  
######## Display data ############
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  
  })
###### multi-d summary table
  
  combined_summary_table <- eventReactive(input$run_button,{
    create_combined_table(data_input(), num_var_1(), num_var_2(), fact_var())
  })
  
  output$combined_summary_table <- renderTable(combined_summary_table())
  
}

shinyApp(ui = ui, server = server)