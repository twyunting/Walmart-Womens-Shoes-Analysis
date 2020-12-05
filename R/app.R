# The analysis of Walmart Women’s Shoes 
# installing libraries
library(shiny)
library(tidyverse)
library(broom)
library(lubridate)
library(shinythemes)
library(mosaic)

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
################################## end clean and tidy dataframe ###################################
ws$colors <- as.character(ws$colors) 
################################## end tidy Xubo ########################################

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

################################## end tidy Yunting ########################################

# input 
ui <- fluidPage(
    navbarPage("The analysis of Walmart Women’s Shoes",
               windowTitle = "The analysis of Walmart Women’s Shoes",
               theme = shinytheme("cerulean")),
    tabsetPanel(type = "tabs",
                tabPanel("Vignette",
                         includeMarkdown("../vignettes/vignette.rmd")
                         ),
                tabPanel("Descriptive Analysis by Date",
                         sidebarLayout(
                           sidebarPanel(
                             dateRangeInput(inputId = "daterange",
                                            label = "Select the date range",
                                            start = min(ws$date),
                                            end = max(ws$date),
                                            min = min(ws$date),
                                            max = max(ws$date),
                                            format = "yyyy/mm/dd",
                                            separator = "-"
                             )#dateRangeInput
                           ),#sidebarPanel
                           mainPanel(
                             plotOutput("dist"),
                             plotOutput("rank"),
                             wordcloud2Output("wordcloud")
                           )#mainPanel
                         )#sidebarLayout
                ),#tabPanel
                tabPanel("Statistical Models",
                         helpText("1. This tab panel is showing up top the 500 women's shoes ranked by prices descending order in 2015~2019."),
                         helpText("2. origPricesXX is means original prices for women's shoes in this year; disPricesXX is means discounted prices for women's shoes in this year. e.g. discPrices16 indicates discounted prices in 2016."),
                         sidebarLayout(
                             sidebarPanel(
                                 varSelectInput("var2X", "X - prices of this year",
                                                data = prices, selected = "origPrices19"),
                                 varSelectInput("var2Y", "Y - prices of this year",
                                                data = prices, selected = "discPrices19"),
                                 checkboxInput("log", "Log Transformation"),
                                 checkboxInput("t", "T-Procedures"),
                                 checkboxInput("slr", "Simple Linear Regression"),
                                 includeMarkdown("../vignettes/eda.rmd"),
                                 tableOutput("eda"),
                                 includeMarkdown("../vignettes/assumptions.rmd"),
                                 width = 12
                             ),
                             mainPanel(
                                 tableOutput("ttest"),
                                 width = 12,
                                 fluidRow(title = "Outputs",
                                          column(3,
                                                 plotOutput("hisX")
                                          ),
                                          column(3,
                                                 plotOutput("hisY")
                                          ),
                                          column(3,
                                                 plotOutput("qqX")
                                          ),
                                          column(3,
                                                 plotOutput("qqY")
                                          ),
                                          column(12,
                                                 plotOutput("SLRplot")
                                          ),
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
                             )#sidebarPanel
                         )#sidebarLayout
                ), # tabPanel
                tabPanel("Dataset",
                         dataTableOutput("sheets")
                )# tabPanel
    ),# tabsetPanel
    
  
    
)#fluidPage

################################## end INPUT ########################################
# SERVER
server <- function(input, output) {
    
    output$dist <- renderPlot({
    s = subset(ws, ws$date >= input$daterange[1] & ws$date <= input$daterange[2])
    ggplot(data = s, aes(x = s$prices.amountMax)) + geom_histogram()
   })
  
    output$rank <- renderPlot({
    s2 = subset(ws, ws$date >= input$daterange[1] & ws$date <= input$daterange[2])
    as.data.frame(table(s2$brand)) %>%
      arrange(-Freq) %>%
      head(10) -> brd
    ggplot(data = brd) +
      geom_point(mapping = aes(x = fct_reorder(Var1, Freq), y = Freq))+
      coord_flip()
  })
  
    output$wordcloud <- renderWordcloud2({
    s1 = subset(ws, ws$date >= input$daterange[1] & ws$date <= input$daterange[2])
    s1 %>%
      unnest_tokens(word, colors) %>%
      count(word, sort = T) -> wscloud
    wordcloud2(wscloud)
  })  

################################## end output Xubo ########################################
    
    output$eda <- renderTable({
       stopifnot(is.numeric(prices[[input$var2X]]) & is.numeric(prices[[input$var2Y]]))
        if(input$log){
            prices %>%
                select(input$var2X, input$var2Y) %>% 
                log() %>%
                tidy(favstats())
        }else if(!input$log){
            prices %>%
                select(input$var2X, input$var2Y) %>% 
                tidy(favstats())

        }else{
            print("Both variables should be transferred together")
        }
    })# renderTable 
    
    output$ttest <- renderTable({
        stopifnot(is.numeric(prices[[input$var2X]]) & is.numeric(prices[[input$var2Y]]))
       # specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
       if(input$t){
            if(input$log){
                prices %>%
                   select(input$var2X, input$var2Y) %>% 
                    log() %>%
                    t.test(alternative = "two.sided", 
                           mu = 0 , conf.level = 0.95) %>% 
                    broom::tidy() %>%
                    select("T-statistic" = statistic, "DF" = parameter,
                           "P-value" = p.value, "Estimate" = estimate,
                           "95 % Lower" = conf.low, "95 % Upper" = conf.high)
            }else if(!input$log){
                prices %>%
                   select(input$var2X, input$var2Y) %>% 
                   t.test(alternative = "two.sided", 
                           mu = 0 , conf.level = 0.95) %>% 
                    broom::tidy() %>%
                    select("T-statistic" = statistic, "DF" = parameter,
                           "P-value" = p.value, "Estimate" = estimate,
                           "95 % Lower" = conf.low, "95 % Upper" = conf.high)
                }
       }
    })# renderTable 
    
    output$hisX <- renderPlot({
      stopifnot(is.numeric(prices[[input$var2X]]) & is.numeric(prices[[input$var2Y]]))
      if(input$t){
        if(input$log){
          ggplot(data = prices ,aes(x = !!input$var2X)) +
            geom_histogram() +
            scale_x_log10() +
            theme_bw() +
            ggtitle("Find the Normality of a Histogram")
        }else if(!input$log){
          ggplot(data = prices ,aes(x = !!input$var2X)) +
            geom_histogram() +
            theme_bw() +
            ggtitle("Find the Normality of a Histogram")
        }
      }
    })# renderTable 
    
    output$hisY <- renderPlot({
      stopifnot(is.numeric(prices[[input$var2X]]) & is.numeric(prices[[input$var2Y]]))
      if(input$t){
        if(input$log){
          ggplot(data = prices ,aes(x = !!input$var2Y)) +
            geom_histogram() +
            scale_x_log10() +
            theme_bw() +
            ggtitle("Find the Normality of a Histogram")
        }else if(!input$log){
          ggplot(data = prices ,aes(x = !!input$var2Y)) +
            geom_histogram() +
            theme_bw() +
            ggtitle("Find the Normality of a Histogram")
        }
      }
    })# renderTable 
    
    output$qqX <- renderPlot({
      stopifnot(is.numeric(prices[[input$var2X]]) & is.numeric(prices[[input$var2Y]]))
      if(input$t){
        if(input$log){
          prices %>%
          ggplot() + 
            ggtitle("Checking Normality for X-variable with Log") +
            geom_qq(aes(sample = !!input$var2X)) +
            geom_qq_line(aes(sample = !!input$var2X)) +
            scale_x_log10() +
            theme_bw()
        }else if(!input$log){
          prices %>%
          ggplot() + 
          ggtitle("Checking Normality for X-variable") +
            geom_qq(aes(sample = !!input$var2X)) +
            geom_qq_line(aes(sample = !!input$var2X)) +
            theme_bw()
        }
      }
    })# renderTable 
    
    output$qqY <- renderPlot({
      stopifnot(is.numeric(prices[[input$var2X]]) & is.numeric(prices[[input$var2Y]]))
      if(input$t){
        if(input$log){
          prices %>%
          ggplot() + 
            ggtitle("Checking Normality for Y-variable with Log") +
            geom_qq(aes(sample = !!input$var2Y)) +
            geom_qq_line(aes(sample = !!input$var2Y)) +
            scale_x_log10() +
            theme_bw()
        }else if(!input$log){
          prices %>%
          ggplot() + 
          ggtitle("Checking Normality for Y-variable") +
            geom_qq(aes(sample = !!input$var2Y)) +
            geom_qq_line(aes(sample = !!input$var2Y)) +
            theme_bw() 
        }
      }
    })# renderTable 

########################### simple Linear regression ################################################  
    
    output$SLRplot <- renderPlot({
        stopifnot(is.numeric(prices[[input$var2X]]) & is.numeric(prices[[input$var2Y]]))
            if(input$slr & input$log){
                ggplot(prices, aes(x = !!input$var2X, y = !!input$var2Y)) +
                        geom_point() +
                        scale_x_log10() +
                        scale_y_log10() +
                        geom_smooth(method = lm, se = FALSE) +
                        labs(x = paste("Log(",input$var2X,")")) +
                        labs(y = paste("Log(",input$var2Y,")"),
                             title = "Scatter Plot")
                }else if(input$slr){
                ggplot(prices, aes(x = !!input$var2X, y = !!input$var2Y)) + 
                        geom_point() +
                        geom_smooth(method = lm, se = FALSE) +
                        ggtitle("Scatter Plot")
                }
            })# renderPlot 
      
    output$sheets <- renderDataTable({
      ws
       # keep(ws, ~ typeof(.) == "double")
        
    })# renderDataTable
    
# separate - output
    
    output$lm <- renderPrint({
        if(is.numeric(prices[[input$var2X]]) & 
           is.numeric(prices[[input$var2Y]]) &
           input$slr &
           input$log){
            summary(lm(log(prices[[input$var2Y]]) ~ log(prices[[input$var2X]])))
        }else if(is.numeric(prices[[input$var2X]]) & 
                 is.numeric(prices[[input$var2Y]]) &
           input$slr){
            summary(lm(prices[[input$var2Y]] ~ prices[[input$var2X]]))
        }
    })#renderPrint
    
    output$residual <- renderPlot({
        if(is.numeric(prices[[input$var2X]]) & 
           is.numeric(prices[[input$var2Y]]) &
           input$slr &
           input$log){
            model <- augment(lm(log(prices[[input$var2Y]]) ~ log(prices[[input$var2X]])))
            model %>%
                ggplot() + 
                geom_point(aes(x = .fitted, y = .resid)) +
                labs(title = "Residuals vs Fitted",
                     x = "Fitted Values",
                     y = "Residuals")
            
        }else if(is.numeric(prices[[input$var2X]]) & 
                 is.numeric(prices[[input$var2Y]]) &
                 input$slr &
                 !input$log){
            model <- augment(lm(prices[[input$var2Y]] ~ prices[[input$var2X]]))
            model %>%
                ggplot() + 
                geom_point(aes(x = .fitted, y = .resid)) +
                labs(title = "Residuals vs Fitted",
                     x = "Fitted Values",
                     y = "Residuals")
        }
    })#renderPlot
    
    output$qq <- renderPlot({
        if(is.numeric(prices[[input$var2X]]) & 
           is.numeric(prices[[input$var2Y]]) &
           input$slr &
           input$log){
            model <- augment(lm(log(prices[[input$var2Y]]) ~ log(prices[[input$var2X]])))
            model %>%
                ggplot() + 
                ggtitle("Normal QQ Plot of Residuals") +
                geom_qq(aes(sample = .resid)) +
                geom_qq_line(aes(sample = .resid))
        }else if(is.numeric(prices[[input$var2X]]) & 
                 is.numeric(prices[[input$var2Y]]) &
                 input$slr){
            model <- augment(lm(prices[[input$var2Y]] ~ prices[[input$var2X]]))
            model %>%
                ggplot() + 
                ggtitle("Normal QQ Plot of Residuals") +
                geom_qq(aes(sample = .resid)) +
                geom_qq_line(aes(sample = .resid))
        }
    })# renderPlot
    
}# server 
################################## end output Yunting ########################################

# Run the application 
shinyApp(ui = ui, server = server)