# The analysis of Walmart Women’s Shoes 
# installing libraries
library(shiny)
library(tidyverse)
library(broom)
library(lubridate)
library(shinythemes)

# download data
ws1 <- read_csv(file = "../data/Datafiniti_Womens_Shoes_Jun19.csv")
ws2 <- read_csv(file = "../data/Datafiniti_Womens_Shoes.csv")
ws3 <- read_csv(file = "../data/7003_1.csv")  

# clean and tidy
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

# combined all data
ws1 %>%
    bind_rows(ws2) %>%
    bind_rows(ws3) -> ws

ws %>%
    # convert Europe sizes to the US sizes
    mutate(sizesUS = str_extract(prices.size, "\\d{1,}"),
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
           sizesUS = as.numeric(sizesUS)) %>%
    # time 
    separate(dateAdded, into = c("date", "time"), sep = " ") %>%
    mutate(date = ymd(date),
           time = hms(time),
           # add discount price (% off) and calculate after discounted prices
           discount = (str_extract(prices.offer, "\\d{1,}")),
           discount = case_when(is.na(discount) ~ 100,
                                TRUE ~ as.numeric(discount)),
           prices.discounted =  prices.amountMax*(discount/100)
    ) %>%
    select(-sizes, -prices.size) -> ws

ws %>%
    select(date, prices.amountMax, prices.discounted) %>%
    rename("origPrices" = "prices.amountMax",
           "discPrices" = "prices.discounted") %>%
    mutate(Year = str_extract(date, "\\d\\d\\d\\d")) %>%
    filter(!is.na(origPrices)) -> wsRegression

#filter out the year
# 2014
ws %>%
    filter(date < parse_date("2015", format = "%Y")) %>%
    select(prices.amountMax, prices.discounted) %>%
    rename("14origPrices" = "prices.amountMax",
           "14discPrices" = "prices.discounted") -> ws2014

# 2015
ws %>%
    filter(date >= parse_date("2015", format = "%Y") & date < parse_date("2016", format = "%Y")) %>%
    select(prices.amountMax, prices.discounted) %>%
    rename("origPrices15" = "prices.amountMax",
           "discPrices15" = "prices.discounted") %>%
    arrange(desc(origPrices15)) %>%
    head(500) -> ws2015

# 2016
ws %>%
    filter(date >= parse_date("2016", format = "%Y") & date < parse_date("2017", format = "%Y")) %>%
    select(prices.amountMax, prices.discounted) %>%
    rename("origPrices16" = "prices.amountMax",
           "discPrices16" = "prices.discounted") %>%
    arrange(desc(origPrices16)) %>%
    head(500) -> ws2016

# 2017
ws %>%
    filter(date >= parse_date("2017", format = "%Y") & date < parse_date("2018", format = "%Y")) %>%
    select(prices.amountMax, prices.discounted) %>%
    rename("origPrices17" = "prices.amountMax",
           "discPrices17" = "prices.discounted") %>%
    arrange(desc(origPrices17)) %>%
    head(500) -> ws2017

# 2018
ws %>%
    filter(date >= parse_date("2018", format = "%Y") & date < parse_date("2019", format = "%Y")) %>%
    select(prices.amountMax, prices.discounted) %>%
    rename("origPrices18" = "prices.amountMax",
           "discPrices18" = "prices.discounted") %>%
    arrange(desc(origPrices18)) %>%
    head(500) -> ws2018

# 2019
ws %>%
    filter(date >= parse_date("2019", format = "%Y")) %>%
    select(prices.amountMax, prices.discounted) %>%
    rename("origPrices19" = "prices.amountMax",
           "discPrices19" = "prices.discounted") %>%
    arrange(desc(origPrices19)) %>%
    head(500) -> ws2019

ws2015 %>%
    bind_cols(ws2016, ws2017, ws2018, ws2019) -> prices

# input 
ui <- fluidPage(
    navbarPage("The analysis of Walmart Women’s Shoes",
               windowTitle = "The analysis of Walmart Women’s Shoes",
               theme = shinytheme("united")),
    tabsetPanel(type = "tabs",
                tabPanel("Regression",
                         helpText("This tab is suppose to campare the most 500 expensives women shoes's prices in 2015-2019"),
                         sidebarLayout(
                             sidebarPanel(
                                 varSelectInput("var2X", "X year prices",
                                                data = prices, selected = 1),
                                 checkboxInput("log2X", "Log_Transform?"),
                                 varSelectInput("var2Y", "Y year prices?",
                                                data = prices, selected = 2),
                                 checkboxInput("log2Y", "Log_Transform?"),
                                 checkboxInput("OLS", "Fit OLS?"),
                                 tableOutput("table1")
                             ),
                             mainPanel(plotOutput("plot2")
                             )#sidebarPanel
                         )#sidebarLayout
                ), # tabPanel
                tabPanel("SpreadSheet",
                         dataTableOutput("sheets")
                )# tabPanel
    ),# tabsetPanel
    
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
    
)#fluidPage


# SERVER
server <- function(input, output) {
    
   # datasetInput <- reactive({
        #temp <- data.frame(wsLists[[(input$dataset)]])
   # }) 
    output$table1 <- renderTable({
        # specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall = k))
        stopifnot(is.numeric(prices[[input$var2X]]) & is.numeric(prices[[input$var2Y]]))
            if(input$log2X & input$log2Y){
                prices %>%
                    select(input$var2X, input$var2Y) %>% 
                    log() %>%
                    t.test(alternative = "two.sided", 
                           mu = 0 , conf.level = 0.95) %>% 
                    broom::tidy() %>%
                    select("P-value" = p.value, "Estimate" = estimate,
                           "95 % Lower" = conf.low, "95 % Upper" = conf.high)
            }else if(!input$log2X & !input$log2Y){
                prices %>%
                    select(input$var2X, input$var2Y) %>% 
                    t.test(alternative = "two.sided", 
                           mu = 0 , conf.level = 0.95) %>% 
                    broom::tidy() %>%
                    select("P-value" = p.value, "Estimate" = estimate,
                           "95 % Lower" = conf.low, "95 % Upper" = conf.high)
                }else{
                    print("Both variables should be transferred together")
                    }
    })# renderTable 
    
    output$plot2 <- renderPlot({
       # mydata <- datasetInput()
        pl2 <- ggplot(prices, aes(x = !!input$var2X, y = !!input$var2Y))
        if(is.numeric(prices[[input$var2X]]) & is.numeric(prices[[input$var2Y]])){
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
        }else if(is.numeric(wsRegression[[input$var2X]]) & !is.numeric(wsRegression[[input$var2Y]])){
            if(input$log2X & input$log2Y){
                validate(
                    need(is.double(wsRegression[[input$var2X]]) & is.double(wsRegression[[input$var2Y]]),
                         "Hi User! The Y variable is not numeric.")
                )
            }else if(input$log2Y){
                validate(
                    need(is.double(wsRegression[[input$var2Y]]),
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
            
            
        }else if(!is.numeric(wsRegression[[input$var2X]]) & is.numeric(wsRegression[[input$var2Y]])){
            if(input$log2X & input$log2Y){
                pl2 <- pl2 +
                    validate(
                        need(is.double(wsRegression[[input$var2X]]) & is.double(wsRegression[[input$var2Y]]),
                             "Hi User! The X variable is not numeric.")
                    ) 
            }else if(input$log2X){
                validate(
                    need(is.double(wsRegression[[input$var2X]]),
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
                    need(is.double(wsRegression[[input$var2X]]&wsRegression[[input$var2Y]]),
                         "Hi User! The variable is not numeric.")
                )  
            }else if(input$log2X){
                validate(
                    need(is.double(wsRegression[[input$var2X]]),
                         "Hi User! The variable is not numeric.")
                )
            }else if(input$log2Y){
                validate(
                    need(is.double(wsRegression[[input$var2Y]]),
                         "Hi User! The variable is not numeric.")
                )
            }else{
                pl2 <- pl2 + geom_jitter()
            }
        }
        pl2
    })# renderPlot (End of second table)
    
    output$sheets <- renderDataTable({
        keep(ws, ~ typeof(.) == "double")
        
    })# renderDataTable
    
    # extra credit - output
    
    output$lm <- renderPrint({
        if(is.numeric(prices[[input$var2X]]) & 
           is.numeric(prices[[input$var2Y]]) &
           input$OLS){
            summary(lm(log(prices[[input$var2Y]]) ~ log(prices[[input$var2X]])))
        }
    })#renderPrint
    
    output$residual <- renderPlot({
        if(is.numeric(prices[[prices$var2X]]) & 
           is.numeric(prices[[prices$var2Y]]) &
           input$OLS){
            model <- augment(lm(log(prices[[input$var2Y]]) ~ log(prices[[input$var2X]])))
            model %>%
                ggplot(aes(x = .fitted, y = .resid)) + 
                geom_point() +
                labs(title = "Residuals vs Fitted",
                     x = "x",
                     y = "y")
        }
    })#renderPlot
    
    output$qq <- renderPlot({
        if(is.numeric(prices[[input$var2X]]) & 
           is.numeric(prices[[input$var2Y]]) &
           input$OLS){
            model <- augment(lm(log(prices[[input$var2Y]]) ~ log(prices[[input$var2X]])))
            model %>%
                ggplot() + 
                ggtitle("Normal QQ Plot") +
                geom_qq(aes(sample = .resid)) +
                geom_qq_line(aes(sample = .resid))
        }
    })# renderPlot
    
}# server 

# Run the application 
shinyApp(ui = ui, server = server)