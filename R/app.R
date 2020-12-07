# Shiny App for STAT-613 final project at American University
# Date: Dec 05, 2020
# Analysis of Walmart Women’s Shoes 

# Installing libraries
library(shiny)
library(tidyverse)
library(broom)
library(lubridate)
library(shinythemes)
library(mosaic)
library(wordcloud2)
library(tidytext)

# Download data
ws1 <- read_csv(file = "../data/Datafiniti_Womens_Shoes_Jun19.csv")
ws2 <- read_csv(file = "../data/Datafiniti_Womens_Shoes.csv")
ws3 <- read_csv(file = "../data/7003_1.csv")  

# Clean and tidy dataset
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

# Combined all data
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
           prices.discounted =  prices.amountMax*(discount/100)) %>%
    select(-sizes, -prices.size) %>%
    rename("prices" = "prices.amountMax") -> ws

################################## end clean and tidy dataframe ###################################
ws$colors <- as.character(ws$colors) 
################################## end tidy Xubo and Jiarong ########################################
ws %>%
  mutate(month = month(date)) %>%
  select(c(month,sizesUS,discount)) -> month
################################## end tidy Shan ########################################
# Filter out the year
# 2015
ws %>%
    filter(date >= parse_date("2015", format = "%Y") & date < parse_date("2016", format = "%Y")) %>%
    select(prices, prices.discounted) %>%
    rename("Original_Prices_2015" = "prices",
           "Discounted_Prices_2015" = "prices.discounted") %>%
    arrange(desc(Original_Prices_2015)) %>%
    head(500) -> ws2015

# 2016
ws %>%
    filter(date >= parse_date("2016", format = "%Y") & date < parse_date("2017", format = "%Y")) %>%
    select(prices, prices.discounted) %>%
    rename("Original_Prices_2016" = "prices",
           "Discounted_Prices_2016" = "prices.discounted") %>%
    arrange(desc(Original_Prices_2016)) %>%
    head(500) -> ws2016

# 2017
ws %>%
    filter(date >= parse_date("2017", format = "%Y") & date < parse_date("2018", format = "%Y")) %>%
    select(prices, prices.discounted) %>%
    rename("Original_Prices_2017" = "prices",
           "Discounted_Prices_2017" = "prices.discounted") %>%
    arrange(desc(Original_Prices_2017)) %>%
    head(500) -> ws2017

# 2018
ws %>%
    filter(date >= parse_date("2018", format = "%Y") & date < parse_date("2019", format = "%Y")) %>%
    select(prices, prices.discounted) %>%
    rename("Original_Prices_2018" = "prices",
           "Discounted_Prices_2018" = "prices.discounted") %>%
    arrange(desc(Original_Prices_2018)) %>%
    head(500) -> ws2018

# 2019
ws %>%
    filter(date >= parse_date("2019", format = "%Y")) %>%
    select(prices, prices.discounted) %>%
    rename("Original_Prices_2019" = "prices",
           "Discounted_Prices_2019" = "prices.discounted") %>%
    arrange(desc(Original_Prices_2019)) %>%
    head(500) -> ws2019

ws2015 %>%
    bind_cols(ws2016, ws2017, ws2018, ws2019) -> prices

################################## end tidy Yunting ########################################

# input 
ui <- fluidPage(
    navbarPage("Walmart Women's Shoes Analysis",
               windowTitle = "Walmart Women's Shoes Analysis",
               theme = shinytheme("cerulean")),
    tabsetPanel(type = "tabs",
                # ----------------------------------
                # tab panel 1 - Vignette
                tabPanel("Vignette",
                         includeMarkdown("../vignettes/vignette.rmd")
                         ),
                # ----------------------------------
                # tab panel 2 - Descriptive Analysis by Date (Xubo and Jiarong)
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
                # ----------------------------------
                # tab panel 3 - (Shan)
                tabPanel("Scatterplots",
                         varSelectInput("varx","X variable",data = month , selected = "sizesUS"),
                         varSelectInput("vary","Y variable",data = month, selected = "discount"),
                         plotOutput("plot")),
                # ----------------------------------
                # tab panel 4 - (Shan)
                tabPanel("Month Table",
                         fluidRow(column(12,
                                         dataTableOutput("shoes")))
                ),#tabPanel
                # ----------------------------------
                # tab panel 5 - Statistical Models (Yunting)
                tabPanel("Statistical Models",
                         helpText("This tab panel is showing up the top 500 women's shoes ranked by prices descending order in 2015~2019."),
                         sidebarLayout(
                             sidebarPanel(
                                 varSelectInput("var2X", "X - prices of this year",
                                                data = prices, selected = "Original_Prices_2019"),
                                 varSelectInput("var2Y", "Y - prices of this year",
                                                data = prices, selected = "Discounted_Prices_2019"),
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
                                          column(6,
                                                 plotOutput("SLRplot")
                                          ),
                                          column(6,
                                                 verbatimTextOutput("lm")
                                          ),
                                          column(6,
                                                 plotOutput("residual")
                                          ),
                                          column(6,
                                                 plotOutput("qq")
                                          )
                                 ) #fluidRow
                             )#sidebarPanel
                         )#sidebarLayout
                ), # tabPanel
                # ----------------------------------
                # tab panel 6 - Dataset
                tabPanel("Data",
                         dataTableOutput("sheets")
                )# tabPanel
    ),# tabsetPanel
    
)#fluidPage

################################## end INPUT ######################################################

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

################################## end output Xubo and Jiarong ########################################
    output$plot <- renderPlot({
      
      ggplot(month,aes(x=!!input$varx, y = !!input$vary))+
        geom_point()+
        scale_x_log10()+
        scale_y_log10()+
        theme_bw()
    })
    
    output$shoes<-renderDataTable({
      colnames(month[map_lgl(month,is.double)])->column_name
      month%>%
        select(column_name)
    },options = list(pagelength=5))
################################## end output Shan ######################################## 
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
    
# t.test   
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

# Simple linear regression 
    
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