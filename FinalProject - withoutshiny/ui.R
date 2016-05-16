library(shiny)
library(rvest)
library(XML)
library(RMySQL)
library(stringr)
library(lubridate)
library(RCurl)
library(tidyr)
library(ggplot2)
library(rCharts)
library(plyr)
library(reshape2)
library(rjson)
library(knitr)

shinyUI(navbarPage("IPO Performance Forecasting",
                   tabPanel("About",
                        h4("Introduction"),
                        p("The Indian equity market is currently in good stead and is gaining steam. It is poised to give a return of 10- 20 percent in 2016 depending on which brokerage firm you speak to. Given this background there has been a spurt in primary market activities in the first half of 2016. There have been some hits and some misses. Is there a way to predict the hits and misses? The goal of this project is to study a limited pre-IPO data that is available for a company and determine if the IPO is going to be a success or a failure", br(),strong("How do I determine success or failure?"),br(),"There are different opinions on what qualifies as a successful IPO. For the purpose of this project, I would term an IPO as successful if I make 7% or more on listing day.",span("And why 7%?",style = "color:blue"),"This value is closer to the median listing day gains for the IPOs in the training set."),
                        br(),
                        br(),
                        h4("Data that is used in the Analysis"),
                        p("The data that is used is as below:", br(),"1. BSE Index data - This data will give me the trend of the market on the day the IPO listed. I reason that IPOs may be successful if the market is on a upward swing during the listing. I intend to use the DMA (Daily Moving Average) to determine the trend. I consider the trend as:",br(), "Positive - if the day previous to the listing day closed higher than previous day DMA for the index",br(),"Negative - if the day previous to the listing day closed Lower or equal to previous day DMA for the index",br(),"2. Pre-IPO Data - This is data about the IPO issue. Some of this is IPO rating, IPO Reviews, IPO Over/Under Subscribed, etc"),
                        br(),
                        br(),
                        h4("Training and Testing"),
                        p("In this project, I would like to study IPOs from the 2007 to 2014. This becomes my training set. I then logistic regression to this training set and use the model to forecast the success for IPOs in 2015 and 2016 that are held in the testing / validation.",br(), br(),"I finally conclude by providing an analysis on the performance of the model.")
                   ),

                   tabPanel("Data Sourcing",
                        titlePanel("Data Sources"),
                        sidebarLayout(
                            sidebarPanel(
                                helpText("This tab will display the data and the code that was used to source the data."),
                                selectInput("ds", 
                                            label = "Select the data sources you want to see",
                                            choices = c("IPO_Issue_Details","Company_Sectors","IPO_ListingDates",
                                                        "IPO_OverSub_ListingGains","IPO_Rating_Details",
                                                        "BSEData","LeadManager_IPO_List", "AllData"),
                                            selected = "IPO_Issue_Details")
                            ),
                            mainPanel(
                                textOutput("text1"),
                                tableOutput("table1")
                            )
                        )
                   ), 

                   tabPanel("Transformations",
                            titlePanel("Transformation"),
                            sidebarLayout(
                                sidebarPanel(
                                    helpText("There are a few transformations that can change based on what is selected in the options in this tab."),
                                    sliderInput("Profit_Cutoff", label = h3("Cutoff to determine IPO Success / Failure"),
                                                min = 1, max = 40, value = 7)
                                ),
                                mainPanel(
                                    h2("Data Transformations"), 
                                    br(),
                                    h4("1. Date Related transformations"), 
                                    br(), 
                                    "Generate a WeekDay", 
                                    br(),
                                    "Generate Year, Month, Day", 
                                    br(),
                                    br(),
                                    h4("2. Calculation of Trend"),
                                    br(),
                                    "The trend is calculated as the comparision of the previous day DMA to the previous day closing price.",
                                    br(),
                                    "Calculate Trend based on 7 DMA and store as another column", 
                                    br(),
                                    "Note: Trend for TODAY is Previous day Close > previous day DMA",
                                    br(),
                                    br(),
                                    h4("3. Successful or Unsuccessful IPO flag"),
                                    br(),
                                    "Based on a listing gains cut-off, we calculate the success or failure of the IPO. Below is the table of success / failure based on the current cutoff:",
                                    br(),
                                    tableOutput("table2"),
                                    br(),
                                    br(),
                                    h4("4. SubscriptionRate"), 
                                    br(),
                                    "Based on the subscription percentages by QIB, NII, RII and EMP I create a total subscription rate for the IPO. The assumption is that higher the subscription, higher the listing gains.",
                                    br(),
                                    br(),
                                    h4("5. ReviewScore"),
                                    br(),
                                    "For each IPO there are counts for the advice given by brokerages. The advice is either subscribe, avoid or neutral. I would like to generate a review score based on these columns. This will give me a value with which I can compare the reviews.",
                                    br(),
                                    "We can now remove the original columns.",
                                    br(),
                                    "Below is the result after the transformation",
                                    tableOutput("table3")
                                )
                            )
                   ), 
                   
                   tabPanel("Exploratory Analysis",
                        titlePanel("Select Variables"),
                        sidebarLayout(
                            sidebarPanel(
                                helpText("This tab will do some basic exploratory analysis. Select a variable to see the charts appropriate to the selected variable"),
#                                 selectInput("exploreVar", 
#                                             label = "Select a variable:",
#                                             choices = c('IssuePrice', 'IssueType', 'IssueSize', 'IssuerSector', 'ListingMonth', 'IPORating', 'ListingWeekDay', 'Trend', 'SubscriptionRate', 'ReviewScore'),
#                                             selected = "IssuePrice")
#                             ),
                            selectInput("exploreVar", 
                                        label = "Select a variable:",
                                        choices = c('IssuePrice', 'IssueSize', 'SubscriptionRate', 'ReviewScore'),
                                        selected = "IssuePrice")
                        ),
                        
                                mainPanel(
                                textOutput("text2"),
                                plotOutput("plot1")
                            )
                        )                   
                   ), 
                   
                   tabPanel("Model Training / Evaluation",
                            titlePanel("Model Training"),
                            sidebarLayout(
                                sidebarPanel(
                                ),
                                mainPanel(
                                    textOutput("text3"),
                                    textOutput("text4"),
                                    tableOutput("table4")
                                    
                                )
                            )                   
                   ) 
                   
))