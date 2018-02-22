library(shiny)
library(shinydashboard)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, 
                     repos='http://cran.us.r-project.org')
  sapply(pkg, require, character.only = TRUE)
} 

packages<-c("quantmod","dplyr","Rblpapi","RMySQL","shiny","foreign","ggplot2","forecast",
            "shinyjs","shinydashboard","ggExtra","gridExtra","tidyverse","lubridate",
            "reshape2","broom","magrittr","TTR","Hmisc","plotly","rhandsontable","dygraphs")

ipak(packages)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  
  dashboardHeader(title="FX Volatility"),
  # Application title
  dashboardSidebar(
    # Side Bar Menus ----------------------------
    sidebarMenu(
      id = "tabs",
      menuItem("Macro Analysis", tabname="MacroAnalysis",
               menuSubItem("Volatilities", tabName = "Volatilities"),
               icon = icon("gbp",lib="glyphicon")),
      menuItem("Single Currency", tabName = "SingleCurrency", 
               menuSubItem("General Analysis", tabName = "GeneralAnalysis"),
               menuSubItem("Realized Volatility Analysis", tabName = "RealizedVolatilityAnalysis"),
               menuSubItem("ATM Analysis", tabName = "ATMAnalysis"),
               menuSubItem("Risk Reversal Analysis", tabName = "RiskReversalAnalysis"),
               menuSubItem("Butterfly Analysis", tabName = "ButterflyAnalysis"),
               icon = icon("usd",lib="glyphicon")),
      menuItem("Multi Currency", tabname="MultiCurrency",
               menuSubItem("Correlation Analysis", tabName = "CorrelationAnalysis"),
               icon = icon("eur",lib="glyphicon")),
      menuItem("Broker Quotes", tabname="Brokerquotes",
               menuSubItem("Intraday Quotes", tabName = "Intraquotes"),
               icon = icon("cog",lib="glyphicon")),
      menuItem("Update Database", tabname="UpdateDatabase",
               menuSubItem("All Databases", tabName = "AllDatabases"),
               menuSubItem("Single Database", tabName = "SingleDatabase"),
               icon = icon("cog",lib="glyphicon"))
    )
    # --------------
  ),
  
  # GENERA EL DASHBOARD
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "Volatilities"
        
        
        
        
      ),
      

      # TAB ALL DATABASES ---------------------------------------------------------
      tabItem(
        tabName = "Intraquotes",
        box(
          uiOutput("bases7"),
          uiOutput("instrumentos_broker"),
          uiOutput("tenors")
        ),
        box(
          solidHeader = TRUE,
          collapsible = TRUE,
          title = "Plot",
          status = "primary",plotlyOutput("Broker_data_plot")
        )
      ),
      
      tabItem(
        tabName = "AllDatabases",
        actionButton("button", "Update All Databases")
      ),
      # TAB SINGLE DATABASES ---------------------------------------------------------------------------
      tabItem(
        tabName = "SingleDatabase",
        actionButton("button2", "Update Database")
      ),
      # TAB REALIZED VOLATILITY ANALYISIS --------------------------------------------------------------
      tabItem(
        tabName = "RealizedVolatilityAnalysis",
        box(
          uiOutput("bases5"),
          dateRangeInput("daterange5", "Date range:",
                         start = '2012-11-13',
                         end = Sys.Date()),
          box(solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary",
              title = "Compare same methodology, diff Tenor",
          box(
            checkboxInput("bool1w", "1w", value = TRUE, width = NULL),
            checkboxInput("bool2w", "2w", value = FALSE, width = NULL),
            checkboxInput("bool1m", "1m", value = FALSE, width = NULL),
            checkboxInput("bool2m", "2m", value = FALSE, width = NULL),
            checkboxInput("bool3m", "3m", value = FALSE, width = NULL),
            checkboxInput("bool6m", "6m", value = FALSE, width = NULL)
          ),
          box(
            radioButtons("voltype", "Vol:",
                         c("C 2 C" = "c2c",
                           "G Kl" = "gk",
                           "ParK"="park",
                           "G-K Y-Z"="nomames"))
          )
          ),
          box(solidHeader = TRUE,
              collapsible = TRUE,
              status = "primary",
              title = "Compare same Tenor, diff methodologies",
          box
          (
            radioButtons("tenortype", "Tenor:",
                         c("1w" = "w1",
                           "2w" = "w2",
                           "1m"="m1",
                           "2m"="m2",
                           "3m"="m3",
                           "6m"="m6"))
          ),
          box(
            checkboxInput("boolc2c", "C 2 C", value = TRUE, width = NULL),
            checkboxInput("boolgk", "G K", value = FALSE, width = NULL),
            checkboxInput("boolpark", "Park", value = FALSE, width = NULL),
            checkboxInput("boolnomames", "G-K Y-Z", value = FALSE, width = NULL)
          )
          )
        ),
        fluidRow(
        box(
          solidHeader = TRUE,
          collapsible = TRUE,
          title = "Plot Same methodology, different tenor",
          status = "primary",dygraphOutput("distPlotrealvol")
        ),
        box(
          solidHeader = TRUE,
          collapsible = TRUE,
          title = "Plot Same Tenor, different methodology",
          status = "primary",dygraphOutput("distPlotrealvol2")
        )
        )
      ),
      # TAB GENERAL ANALYSIS ----------------------------------------------------------------------------
      tabItem(
        
        tabName="GeneralAnalysis",
        box(
          title = "Inputs", solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary",
          uiOutput("bases"),
          uiOutput("instrumentos"),
          dateRangeInput("daterange", "Date range:",
                         start = '2012-11-13',
                         end = Sys.Date()),
          box(
            checkboxInput("bool", "Trend Line", value = FALSE, width = NULL),
            checkboxInput("bool2", "Smooth", value = FALSE, width = NULL),
            checkboxInput("bool3", "Returns", value = FALSE, width = NULL),
            checkboxInput("bool4", "Realized Volatility", value = FALSE, width = NULL),
            checkboxInput("bool5", "Standarized Risk Reversal", value = FALSE, width = NULL)
          ),
          box(
            sliderInput("varwindow","Volatility Window",min=10,max=360,value=10)
          )
          
        ),
        box(
          solidHeader = TRUE,
          collapsible = TRUE,
          title = "Plot",
          status = "primary",
          plotlyOutput("distPlot"),
          fluidRow(
            column(align="center",
                   tableOutput("summarytable"), width = 12)
          )
        ),
        fluidRow(
        box(
          solidHeader = TRUE,
          collapsible = TRUE,
          title = "Histogram",
          status = "primary",
          plotOutput("HistPlot")
          ),
        box(
          solidHeader = TRUE,
          collapsible = TRUE,
          title = "QQ Plot",
          status = "primary",
          plotOutput("QQPlot")
        )
        
        )
        
      ),
      # TAB CORRELATION ANALYSIS ------------------------------------------------------------------
      tabItem(
        tabName = "CorrelationAnalysis",
        box
        (
          solidHeader = TRUE,
          collapsible = TRUE,
          dateRangeInput("daterange2", "Date range:",
                         start = '2012-11-13',
                         end = Sys.Date()),
          box(
            uiOutput("bases2"),
            uiOutput("instrumentos2"),
            sliderInput("lagg","Lag",min=0,max=100,value=0),
            checkboxInput("boolvar1", "Realized Volatility", value = FALSE, width = NULL),
            checkboxInput("booldiff1", "Differences", value = FALSE, width = NULL),
            checkboxInput("boollogdiff1", "Log Returns", value = FALSE, width = NULL),
            
            sliderInput("varwindow1","Volatility Window",min=10,max=360,value=10)
          ),
          box
          (
            uiOutput("bases3"),
            uiOutput("instrumentos3"),
            #textInput("string_lag2", "Lag", value = 0, width = NULL, placeholder = NULL),
            sliderInput("lagg2","Lag",min=0,max=100,value=0),
            
            checkboxInput("boolvar2", "Realized Volatility", value = FALSE, width = NULL),
            checkboxInput("booldiff2", "Differences", value = FALSE, width = NULL),
            checkboxInput("boollogdiff2", "Log Returns", value = FALSE, width = NULL),
            sliderInput("varwindow2","Volatility Window",min=10,max=360,value=10),
            checkboxInput("boolbis", "Trend Line", value = FALSE, width = NULL),
            checkboxInput("bool2bis", "Smooth", value = FALSE, width = NULL)
          )
        ),
        
        fluidRow(box(plotlyOutput("distPlot2")),
                 box(dygraphOutput("Multitimeseries"))
                 )
      ),
      
      # ATM Analysis ------------------------
      
      tabItem(
        tabName = "ATMAnalysis",
        box(
          uiOutput("bases6"),
          dateRangeInput("daterange6", "Date range:",
                         start = '2012-11-13',
                         end = Sys.Date()),
          box(
            checkboxInput("bool5w", "1w", value = TRUE, width = NULL),
            checkboxInput("bool6w", "2m", value = FALSE, width = NULL),
            checkboxInput("bool7m", "1m", value = FALSE, width = NULL)
          )
        )
      )
      # ------------------------
      
    )
  )
)