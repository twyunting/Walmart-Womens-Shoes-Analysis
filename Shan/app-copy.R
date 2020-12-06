#Shan Lin

library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)





ws1 <- read_csv(file = "../data/Datafiniti_Womens_Shoes_Jun19.csv")
# read other data
ws2 <- read_csv(file = "../data/Datafiniti_Womens_Shoes.csv")
ws3 <- read_csv(file = "../data/7003_1.csv")  




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
mutate(month = month(date))%>%
    select(c(month,sizesUS,discount))-> month






ui <- fluidPage(
    titlePanel("Women shoes Data"),
    tabsetPanel(
    tabPanel("Scatterplot",
             varSelectInput("varx","X variable",data = month , selected = "sizesUS"),
             varSelectInput("vary","Y variable",data = month, selected = "discount"),
             plotOutput("plot")),
    tabPanel("Spreadsheet",
             fluidRow(column(12,
                             dataTableOutput("shoes")))
    )#tabPanel
    )#tabsetPanel
)#fluidPage
    

server <- function(input, output, session) {
    output$plot<- renderPlot({
        
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
}

shinyApp(ui, server)









