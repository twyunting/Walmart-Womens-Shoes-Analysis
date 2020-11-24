# The analysis of Walmart Women’s Shoes 
## 0. libraries
library(shiny)
library(tidyverse)
library(broom)
library(lubridate)

## 1. download and clean data
ws1 <- read.csv(file = "../data/Datafiniti_Womens_Shoes_Jun19.csv")
# read other data
ws2 <- read.csv(file = "../data/Datafiniti_Womens_Shoes.csv")
ws3 <- read.csv(file = "../data/7003_1.csv")  
ws1 %>%
    select(dateAdded, brand, categories, colors, manufacturer,
           prices.amountMax, prices.color, 
           prices.offer, prices.size, sizes,
    ) -> ws1

ws2 %>%
    select(dateAdded, brand, categories, colors, manufacturer,
           prices.amountMax, prices.color, 
           prices.offer, prices.size, sizes,
    ) -> ws2

ws3 %>%
    select(dateAdded, brand, categories, colors, manufacturer,
           prices.amountMax,  prices.color, 
           prices.offer, prices.size, sizes,
    )  %>%
    mutate(prices.amountMax = as.numeric(prices.amountMax)
    ) -> ws3

# combine all data
ws1 %>%
    bind_rows(ws2) %>%
    bind_rows(ws3) -> ws
# uniform `prices.size` to the US size
ws %>%
    # convert Europe sizes to the US sizes
    mutate(sizesUS = str_extract(prices.size, "\\d{1,}")) %>%
    separate(dateAdded, into = c("date", "time"), sep = "T") %>%
    mutate(date = ymd(date),
           time = hms(time),
           sizesUS = recode(sizesUS, "44" = "12",
                            "43" = "11",
                            "42" = "11",
                            "41" = "10", 
                            "40" = "9",
                            "39" = "8",
                            "38" = "7.5",
                            "37" = "6.5",
                            "36" = "6",
                            "35" = "5"),
           sizesUS = as.numeric(sizesUS)
    ) -> ws

# check NAs for the dataset (no any NA)
# summarize(across(everything(), ~sum(is.na(.))))


ui <- fluidPage(
    titlePanel("EDA of Estate Data", windowTitle = "EDA of Estate Data"),
    tabsetPanel(type = "tabs",
                tabPanel("Univariate",
                         sidebarLayout(
                             sidebarPanel(
                                 varSelectInput("var1", "Variable?", 
                                                data = estate, selected = "Price($K)"),
                                 checkboxInput("log1", "Log_Transform?"),
                                 sliderInput("bins",
                                             "Number of Bins?",
                                             value = 40,
                                             min = 1,
                                             max = 100),
                                 numericInput("mu", "Null Value", value = 0),
                                 tableOutput("table1")
                             ),
                             mainPanel(plotOutput("plot1")
                             )#sidebarPanel
                         )# sidebarLayout
                ),# tabPanel
                tabPanel("Bivariate",
                         sidebarLayout(
                             sidebarPanel(
                                 varSelectInput("var2X", "X Variable?",
                                                data = estate, selected = "Price($K)"),
                                 checkboxInput("log2X", "Log_Transform?"),
                                 varSelectInput("var2Y", "Y Variable?",
                                                data = estate, selected = "Price($K)"),
                                 checkboxInput("log2Y", "Log_Transform?"),
                                 checkboxInput("OLS", "Fit OLS?")
                                 
                                 
                             ),
                             mainPanel(plotOutput("plot2")
                             )#sidebarPanel
                         )#sidebarLayout
                ), # tabPanel
                tabPanel("SpreadSheet",
                         dataTableOutput("sheets")
                )# tabPanel
    ),# tabsetPanel
    
    # extra credit - input
    
    fluidRow(title = "Outputs",
             column(4,
                    verbatimTextOutput("lm")
             ),
             column(4,
                    plotOutput("residual")
             ),
             column(4,
                    plotOutput("qq")
             )
             
    ) #fluidRow
    
)# fluidPage


# Output
server <- function(input, output) {
    
    output$plot1 <- renderPlot({
        
        pl1 <- ggplot(estate, aes(x = !!input$var1))
        if(is.numeric(estate[[input$var1]])){
            if(input$log1){ #test
                pl1 <- pl1 + geom_histogram(bins = input$bins) +
                    scale_x_log10() +
                    labs(x = paste("Log(",input$var1,")"))
            }else{
                pl1 <- pl1 + geom_histogram(bins = input$bins)
            }
        }else{
            if(input$log1){
                validate(
                    need(is.double(estate[[input$var1]]),
                         "Hi User! The variable is not numeric.")
                )   
            }
            pl1 <- pl1 + geom_bar()
        }
        pl1
        
    })# renderplot
    
    output$table1 <- renderTable({
        if(is.numeric(estate[[input$var1]])){
            if(input$log1){ #test
                estate %>%
                    select(input$var1) %>% #test
                    log() %>%
                    t.test(alternative = "two.sided", 
                           mu = input$mu , conf.level = 0.95) %>% 
                    broom::tidy() %>%
                    select("P-value" = p.value, "Estimate" = estimate,
                           "95 % Lower" = conf.low, "95 % Upper" = conf.high)
            }else{
                estate %>%
                    select(input$var1) %>% #test
                    t.test(alternative = "two.sided", 
                           mu = input$mu , conf.level = 0.95) %>% 
                    broom::tidy() %>%
                    select("P-value" = p.value, "Estimate" = estimate,
                           "95 % Lower" = conf.low, "95 % Upper" = conf.high)
            }
        }else{
            print("Variable is not numeric")
        }
    })# renderTable (End of the first tab)
    
    output$plot2 <- renderPlot({
        pl2 <- ggplot(estate, aes(x = !!input$var2X, y = !!input$var2Y))
        if(is.numeric(estate[[input$var2X]]) & is.numeric(estate[[input$var2Y]])){
            if(input$OLS){
                if(input$log2X & input$log2Y){
                    pl2 <- pl2 + 
                        geom_point() +
                        scale_x_log10() +
                        scale_y_log10() +
                        geom_smooth(method = lm, se = FALSE) +
                        labs(x = paste("Log(",input$var2X,")")) +
                        labs(y = paste("Log(",input$var2Y,")"))
                }else if(input$log2X){
                    pl2 <- pl2 + 
                        geom_point() +
                        scale_x_log10() +
                        geom_smooth(method = lm, se = FALSE) +
                        labs(x = paste("Log(",input$var2X,")")) 
                }else if(input$log2Y){
                    pl2 <- pl2 + 
                        geom_point() +
                        scale_y_log10() +
                        geom_smooth(method = lm, se = FALSE) +
                        labs(y = paste("Log(",input$var2Y,")"))
                }else{
                    pl2 <- pl2 + 
                        geom_point() +
                        geom_smooth(method = lm, se = FALSE)
                }
            }else{
                if(input$log2X & input$log2Y){
                    pl2 <- pl2 + 
                        geom_point() +
                        scale_x_log10() +
                        scale_y_log10() +
                        labs(x = paste("Log(",input$var2X,")")) +
                        labs(y = paste("Log(",input$var2Y,")"))
                }else if(input$log2X){
                    pl2 <- pl2 + 
                        geom_point() +
                        scale_x_log10() +
                        labs(x = paste("Log(",input$var2X,")"))
                }else if(input$log2Y){
                    pl2 <- pl2 + 
                        geom_point() +
                        scale_y_log10() +
                        labs(y = paste("Log(",input$var2Y,")"))
                }else{
                    pl2 <- pl2 + geom_point()
                }
            }# OLS
        }else if(is.numeric(estate[[input$var2X]]) & !is.numeric(estate[[input$var2Y]])){
            if(input$log2X & input$log2Y){
                validate(
                    need(is.double(estate[[input$var2X]]) & is.double(estate[[input$var2Y]]),
                         "Hi User! The Y variable is not numeric.")
                )
            }else if(input$log2Y){
                validate(
                    need(is.double(estate[[input$var2Y]]),
                         "Hi User! The Y variable is not numeric.")
                ) 
            }else if(input$log2X){
                pl2 <- pl2 + 
                    geom_boxplot() +
                    scale_x_log10() +
                    labs(x = paste("Log(",input$var2X,")"))
            }
            else{
                pl2 <- pl2 + 
                    geom_boxplot()  
            }
            
            
        }else if(!is.numeric(estate[[input$var2X]]) & is.numeric(estate[[input$var2Y]])){
            if(input$log2X & input$log2Y){
                pl2 <- pl2 +
                    validate(
                        need(is.double(estate[[input$var2X]]) & is.double(estate[[input$var2Y]]),
                             "Hi User! The X variable is not numeric.")
                    ) 
            }else if(input$log2X){
                validate(
                    need(is.double(estate[[input$var2X]]),
                         "Hi User! The X variable is not numeric.")
                ) 
            }else if(input$log2Y){
                pl2 <- pl2 + 
                    geom_boxplot() +
                    scale_y_log10() +
                    labs(y = paste("Log(",input$var2Y,")"))
                
            }
            else{
                pl2 <- pl2 + 
                    geom_boxplot()
            }
            
        }else{
            if(input$log2X & input$log2Y){
                validate(
                    need(is.double(estate[[input$var2X]]&estate[[input$var2Y]]),
                         "Hi User! The variable is not numeric.")
                )  
            }else if(input$log2X){
                validate(
                    need(is.double(estate[[input$var2X]]),
                         "Hi User! The variable is not numeric.")
                )
            }else if(input$log2Y){
                validate(
                    need(is.double(estate[[input$var2Y]]),
                         "Hi User! The variable is not numeric.")
                )
            }else{
                pl2 <- pl2 + geom_jitter()
            }
        }
        pl2
    })# renderPlot (End of second table)
    
    output$sheets <- renderDataTable({
        keep(estate, ~ typeof(.) == "double")
        
    })# renderDataTable
    
    # extra credit - output
    
    output$lm <- renderPrint({
        if(is.numeric(estate[[input$var2X]]) & 
           is.numeric(estate[[input$var2Y]]) &
           input$OLS){
            summary(lm(log(estate[[input$var2Y]]) ~ log(estate[[input$var2X]])))
        }
    })#renderPrint
    
    output$residual <- renderPlot({
        if(is.numeric(estate[[input$var2X]]) & 
           is.numeric(estate[[input$var2Y]]) &
           input$OLS){
            model <- augment(lm(log(estate[[input$var2Y]]) ~ log(estate[[input$var2X]])))
            model %>%
                ggplot(aes(x = .fitted, y = .resid)) + 
                geom_point() +
                labs(title = "Residuals vs Fitted",
                     x = "x",
                     y = "y")
        }
    })#renderPlot
    
    output$qq <- renderPlot({
        if(is.numeric(estate[[input$var2X]]) & 
           is.numeric(estate[[input$var2Y]]) &
           input$OLS){
            model <- augment(lm(log(estate[[input$var2Y]]) ~ log(estate[[input$var2X]])))
            model %>%
                ggplot() + 
                ggtitle("QQ Plot") +
                geom_qq(aes(sample = .resid)) +
                geom_qq_line(aes(sample = .resid))
        }
    })# renderPlot
    
}# server 

# Run the application 
shinyApp(ui = ui, server = server)
