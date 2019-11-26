###  install packages
library(highcharter)
library(shiny)
library(shinyBS)
library(readxl)
# library(xlsx)

shinyUI(tagList(shinyjs::useShinyjs(),
  tags$head(tags$script(HTML("Shiny.addCustomMessageHandler('closeWindow', function(m) {window.close();});"))),
  navbarPage("Backtest System", inverse = TRUE, id = "navbar", 
             tabPanel("Strategy", 
                      sidebarLayout(
                        sidebarPanel(
                          id = "stratPanel", width = 3, style = "overflow-y:scroll; max-height: 550px; position:relative;",
                          fileInput("stratUpload", "Choose XLSX file", accept = c('.xls', '.xlsx')),
                          h3("Indicator"),
                          helpText("You should first define your indicators here before you set up signals. 
                                   It is necessary to give labels to every indicator"),
                          wellPanel(
                            selectizeInput("indicator", "Choose indicator", 
                                           choices = c("SMA" = "SMA", 
                                                       "RSI" = "RSI",
                                                       "lagATR" = "lagATR",
                                                       "MACD" = "MACD")),
                            numericInput("n", "n", value = 10),
                            textInput("label", "Label", value = "quickMA"),
                            checkboxInput("delete", "Remove specific row", value = FALSE),
                            conditionalPanel(
                              condition = "input.delete == true",
                              numericInput("deleteRowN", "Delete Row", value = 1)
                            ),
                            actionButton("submitInd", "Add", class="btn-primary"),
                            actionButton("revInd", "Remove", class="btn-success"),
                            actionButton("resetInd", "Reset", class="btn-info")
                          ),
                          h3("Signal"),
                          helpText("Labels are obtained from indicators you give above"),
                          wellPanel(
                            uiOutput("signal"),
                            selectizeInput("comparison", "Logic", 
                                           choices = c(">" = ">",
                                                       ">=" = ">=",
                                                       "<" = "<",
                                                       "<=" = "<=",
                                                       "&&" = "&&")),
                            selectizeInput("status", "Status", 
                                           choices = c("Compare Indicators" = "sigCompare",
                                                       "Combine Signals" = "sigAND",
                                                       "Threshold" = "sigThreshold")),
                            conditionalPanel(
                              condition = "input.status == 'sigCompare'",
                              checkboxInput("cross", "Crossover", TRUE),
                              uiOutput("signal2")
                            ),
                            conditionalPanel(
                              condition = "input.status == 'sigAND'",
                              uiOutput("signal3")
                            ),
                            conditionalPanel(
                              condition = "input.status == 'sigThreshold'",
                              numericInput("threshold", "Threshold", value = 5)
                            ),
                            textInput("label2", "Label", value = "longEntry"),
                            checkboxInput("delete2", "Remove specific row", value = FALSE),
                            conditionalPanel(
                              condition = "input.delete2 == true",
                              numericInput("deleteRowN2", "Delete Row", value = 1)
                            ),
                            actionButton("submitsig", "Add", class="btn-primary"),
                            actionButton("revsig", "Remove", class="btn-success"),
                            actionButton("resetsig", "Reset", class="btn-info")
                          ),
                          downloadButton("stratDown", "Export")
                          ),
                        mainPanel(
                          h1("Indicators Info"),
                          tableOutput("table"),
                          br(),
                          br(),
                          br(),
                          h1("Signals Info"),
                          tableOutput("table2")
                        )
             )
  ),
  tabPanel("Backtest", 
           sidebarLayout(
             sidebarPanel(
               width = 3,
               bsTooltip("stocks", title = "Please enter stock code from yahoo finance.", placement = "bottom", trigger = "hover", options = NULL),
               textInput("stocks", label = "Stock Code", value = "2330.TW"),
               dateRangeInput("dateRange", label = "Strategy Period", start = "2014-01-01", end = Sys.Date()),
               conditionalPanel("input.conditionedPanels == 'Backtest Result'",
                                actionButton("zoom", "Zoom"),
                                br(),
                                br()
               ), 
               bsTooltip("tradeSize", title = "This is your initial capitals.", placement = "bottom", trigger = "hover", options = NULL),
               numericInput("tradeSize", "Initial Equity", 1000000),
               selectizeInput("choice", "Download data", 
                              choices = c("Daily Price" = "stocks",
                                          "Trade Details" = "tradeDetails",
                                          "Order Book" = "orderBook")),
               actionButton("start", "Start", class="btn-primary btn-lg"),
               # bsModal("verbose", "Matched Orders", "start", wellPanel(
               #   includeScript("sendBacktest.js"), 
               #   div(
               #     style = "overflow-y:scroll; max-height:250px; position:relative;",
               #     id = "text")
               # )),
               downloadButton("downloadData", label = "Download", class="btn-warning btn-lg"),
               br(),
               br(),
               helpText("Demonstration"),
               HTML('<iframe width="270" height="150" src="//www.youtube.com/embed/Yl7qzBkTo5k" frameborder="0" allowfullscreen></iframe>')
             ), 
             mainPanel(
               tabsetPanel(id = "conditionedPanels", 
                           tabPanel("Live Chart", 
                                    conditionalPanel(condition="input.start > 0 && $('html').hasClass('shiny-busy')",
                                                     tags$div(style="float:left; margin-left:300px; margin-top:150px;",
                                                              tags$img(src="loading2.gif",height=200,width=200))
                                    ),
                                    highchartOutput("highchart", width = "112%", height = "590px")),
                           tabPanel("Data", dataTableOutput("dataTable")),
                           tabPanel("Trade Details", dataTableOutput("tradeTable")),
                           tabPanel("Order Book", dataTableOutput("orderBook")),
                           tabPanel("Backtest Result", 
                                    conditionalPanel(condition="input.start > 0 && $('html').hasClass('shiny-busy')",
                                                     tags$div(style="float:left; margin-left:300px; margin-top:150px;",
                                                              tags$img(src="loading2.gif",height=200,width=200))
                                    ),
                                    uiOutput("backtest")),
                           tabPanel("Backtest Statistics", 
                                    bootstrapPage(
                                      selectizeInput("pReturn", "Return Period", 
                                                     choices = c("Monthly" = "mReturn",
                                                                 "Yearly" = "yReturn")),
                                      uiOutput("stats"))
                           )
               )
             )
           )
  ),
  tabPanel("WFA",
           sidebarLayout(
             sidebarPanel(
               id = "WFAPanel", width = 3, style = "overflow-y:scroll; max-height: 550px; position:relative;",
               helpText(h4("Strategy Settings")),
               wellPanel(
                 bsTooltip("stocks", title = "Please enter stock code from yahoo finance.", placement = "bottom", trigger = "hover", options = NULL),
                 textInput("stocks", label = "Stock Code", value = "2330.TW"),
                 dateRangeInput("WFAdateRange", label = "Strategy Period", start = "2006-01-01", end = Sys.Date()),
                 conditionalPanel("input.conditionedPanels == 'Backtest Result'",
                                  actionButton("zoom", "Zoom"),
                                  br(),
                                  br()
                 ), 
                 bsTooltip("tradeSize", title = "This is your initial capitals.", placement = "bottom", trigger = "hover", options = NULL),
                 numericInput("tradeSize", "Initial Equity", 1000000)
               ),
               checkboxInput("params", "Do you want to change default set of parameters?", FALSE),
               conditionalPanel(
                 condition = "input.params == true",
                 helpText(h4("Technical Indecators Settings")),
                 wellPanel(
                   sliderInput("rsi", "RSI range", min = 2, max = 100, value = c(2,15)),
                   sliderInput("quickMA", "Quick MA range", min = 2, max = 60, value = c(5,15)),
                   sliderInput("filterMA", "Slow MA range", min = 120, max = 300, value = c(220,260)),
                   sliderInput("stoploss", "Stop Loss Percent", min = 0.01, max = 0.5, value = c(0.05, 0.3))
                 )
               ),
               checkboxInput("WFAchoice", "Do you want to change default settings of WFA?", FALSE),
               conditionalPanel(
                 condition = "input.WFAchoice == true", 
                 helpText(h4("Walk Forward Analysis Settings")),
                 helpText("It is recommended to use a testing period of approximately 25% of the length of the optimization period"),
                 wellPanel(
                   bsTooltip("WFAperiod", title = "Choose a time unit to apply WFA.", placement = "bottom", trigger = "hover", options = NULL),
                   selectizeInput("WFAperiod", "WFA Period",
                                  choices = c("Years" = "years",
                                              "Months" = "months",
                                              "Days" = "days"
                                  )),
                   bsTooltip("WFAtrain", title = "It is an in-sample data(IS) period based on WFA Period. <br>By default it is 4 years.", placement = "bottom", trigger = "hover", options = NULL),
                   numericInput("WFAtrain", "Training Period", value = 4),
                   bsTooltip("WFAtest", title = "It is an out-of-sample data(OOS) period based on WFA Period. <br>By default it is 2 years", placement = "bottom", trigger = "hover", options = NULL),
                   numericInput("WFAtest", "Testing Period", value = 2),
                   checkboxInput("WFAcheck", "Do you want to draw samples from the set of parameters?", FALSE),
                   conditionalPanel(
                     condition = "input.WFAcheck == true",
                     numericInput("WFAnumeric", "Combos", value = 100)
                   ),
                   bsTooltip("WFAanchor", title = "If time window is anchored, you will train all historical data from where you anchored.", placement = "top", trigger = "hover", options = NULL),
                   checkboxInput("WFAanchor", "Do you want to anchor the training window?", FALSE)
                 )
               ),
               actionButton("WFAstart", "Start", class="btn-primary btn-lg"),
               bsModal("modal", "Other services will be paused during the comptutation. Still want to continue?", "WFAstart", wellPanel(
                 includeScript("sendBacktest.js"),
                 div(
                   style = "overflow-y:scroll; max-height: 250px; position:relative;",
                   id = "WFAtext"
                 )
               ),
               actionButton("yes_button", "Yes"),
               actionButton("no_button", "No")
               )
             ),
             mainPanel(
               tabsetPanel(id = "WFA",
                           tabPanel("Live Chart", highchartOutput("WFAdemo")), 
                           tabPanel("Training Sample", 
                                    bootstrapPage(
                                      numericInput("trainset", "Show train results", value = 1),
                                      dataTableOutput("IS")
                                    ),
                                    conditionalPanel(condition="input.yes_button > 0 && $('html').hasClass('shiny-busy')",
                                                     tags$div(style="float:left; margin-left:270px; margin-top:110px;",
                                                              tags$img(src="loading2.gif",height=200,width=200))
                                    )
                           ),
                           tabPanel("Testing Sample", 
                                    conditionalPanel(condition="input.yes_button > 0 && $('html').hasClass('shiny-busy')",
                                                     tags$div(style="float:left; margin-left:300px; margin-top:150px;",
                                                              tags$img(src="loading2.gif",height=200,width=200))
                                    ),
                                    tableOutput("OOS")),
                           tabPanel("WFA Result", uiOutput("WFAresult")),
                           tabPanel("Statistics", uiOutput("WFAstats"))
               )
             )
           )),
  tabPanel("Optimization", 
           titlePanel("Still in development...")
  ),
  tabPanel("Chat Room", 
           bootstrapPage(
             # We'll add some custom CSS styling -- totally optional
             includeCSS("shinychat.css"),
             
             # And custom JavaScript -- just to send a message when a user hits "enter"
             # and automatically scroll the chat window for us. Totally optional.
             includeScript("sendOnEnter.js"),
             
             div(
               # Setup custom Bootstrap elements here to define a new layout
               class = "container-fluid", 
               div(class = "row-fluid",
                   # Set the page title
                   tags$head(tags$title("ShinyChat")),
                   
                   # Create the header
                   div(class="span6", style="padding: 10px 0px;",
                       h4("This is a pre-alpha version, so don't take it seriously."),
                       h4("Your opinion is most welcome!")
                   )
               ),
               # The main panel
               div(
                 class = "row-fluid", 
                 mainPanel(
                   # Create a spot for a dynamic UI containing the chat contents.
                   uiOutput("chat"),
                   
                   # Create the bottom bar to allow users to chat.
                   fluidRow(
                     div(class="span10",
                         textInput("entry", "")
                     ),
                     div(class="span2 center",
                         actionButton("send", "Send")
                     )
                   )
                 ),
                 # The right sidebar
                 sidebarPanel(
                   # Let the user define his/her own ID
                   textInput("user", "Your User ID:", value=""),
                   tags$hr(),
                   h5("Connected Users"),
                   # Create a spot for a dynamic UI containing the list of users.
                   uiOutput("userList"),
                   tags$hr(),
                   helpText(HTML("<p>Built using R & <a href = \"http://rstudio.com/shiny/\">Shiny</a>.<p>Source code available <a href =\"https://github.com/trestletech/ShinyChat\">on GitHub by trestletech</a>"))
                 )
               )
             )
           )
  ),
  tabPanel("News", 
           sidebarLayout(
             sidebarPanel(
               width = 3,
               bsTooltip("stocks", title = "Please enter stock code from yahoo finance.", placement = "bottom", trigger = "hover", options = NULL),
               textInput("stocks", label = "Stock Code", value = "2330.TW"),
               bsTooltip("NEWsdateRange", title = "Select period of news.", placement = "bottom", trigger = "hover", options = NULL),
               dateRangeInput("NEWsdateRange", label = "Choose Period", start = "2015-01-01", end = Sys.Date()),
               actionButton("getNews", "Start", class="btn-primary btn-lg")
             ),
             mainPanel(
               bootstrapPage(
                 h3("Source: PTT Stock"),
                 conditionalPanel(condition="input.getNews > 0 && $('html').hasClass('shiny-busy')",
                                  tags$div(style="float:left; margin-left:270px; margin-top:130px;",
                                           tags$img(src="loading2.gif",height=200,width=200))
                 ),
                 dataTableOutput("News"))
             )
           )),
  tabPanel(title = "", value = "Stop", icon = icon("power-off"))
  # tags$head(tags$style('
  #                      nav .container:first-child {
  #                          margin-left:10px; width: 100%;
  #                      }'))
)))
