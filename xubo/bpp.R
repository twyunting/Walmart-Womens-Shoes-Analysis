# The analysis of Walmart Women’s Shoes 
# installing libraries
library(shiny)
library(tidyverse)
library(broom)
library(lubridate)
library(shinythemes)
library(wordcloud2)
library(tidytext)

# download data
ws1 <- read.csv(file = "../data/Datafiniti_Womens_Shoes_Jun19.csv")
# read other data
ws2 <- read.csv(file = "../data/Datafiniti_Womens_Shoes.csv")
ws3 <- read.csv(file = "../data/7003_1.csv")  

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
    separate(dateAdded, into = c("date", "time"), sep = "T") %>%
    mutate(date = ymd(date),
           time = hms(time),
           # add discount price (% off) and calculate after discounted prices
           discount = (str_extract(prices.offer, "\\d{1,}")),
           discount = case_when(is.na(discount) ~ 100,
                                TRUE ~ as.numeric(discount)),
           prices.discounted =  prices.amountMax*(discount/100)
    ) %>%
    select(-sizes, -prices.size) -> ws

ws$colors <- as.character(ws$colors)


ui <- fluidPage(
    navbarPage("The analysis of Walmart Women’s Shoes",
               windowTitle = "The analysis of Walmart Women’s Shoes",
               theme = shinytheme("united")),
    tabsetPanel(type = "tabs",
                # The first panel
                tabPanel("Descriptive analysis by date",
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
                tabPanel("Regression",
                         sidebarLayout(
                             sidebarPanel(
                                 varSelectInput("var2X", "X Variable?",
                                                data = ws, selected = "discount"),
                                 checkboxInput("log2X", "Log_Transform?"),
                                 varSelectInput("var2Y", "Y Variable?",
                                                data = ws, selected = "prices.discounted"),
                                 checkboxInput("log2Y", "Log_Transform?"),
                                 checkboxInput("OLS", "Fit OLS?")
                             ),
                             mainPanel(
                                 plotOutput("plot2")
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
    
    output$lm <- renderPrint({
        if(is.numeric(ws[[input$var2X]]) & 
           is.numeric(ws[[input$var2Y]]) &
           input$OLS){
            summary(lm(log(ws[[input$var2Y]]) ~ log(ws[[input$var2X]])))
        }
    })#renderPrint
    
    output$residual <- renderPlot({
        if(is.numeric(ws[[input$var2X]]) & 
           is.numeric(ws[[input$var2Y]]) &
           input$OLS){
            model <- augment(lm(log(ws[[input$var2Y]]) ~ log(ws[[input$var2X]])))
            model %>%
                ggplot(aes(x = .fitted, y = .resid)) + 
                geom_point() +
                labs(title = "Residuals vs Fitted",
                     x = "x",
                     y = "y")
        }
    })#renderPlot
    
    output$qq <- renderPlot({
        if(is.numeric(ws[[input$var2X]]) & 
           is.numeric(ws[[input$var2Y]]) &
           input$OLS){
            model <- augment(lm(log(ws[[input$var2Y]]) ~ log(ws[[input$var2X]])))
            model %>%
                ggplot() + 
                ggtitle("QQ Plot") +
                geom_qq(aes(sample = .resid)) +
                geom_qq_line(aes(sample = .resid))
        }
    })# renderPlot
    
}# server 


shinyApp(ui = ui, server = server)
