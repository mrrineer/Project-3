library(ggplot2)
library(shiny)
library(plotly)
library(tidyverse)

#bring in data
data<-read_csv("Leading Causes of Death.csv") 
#filter data
deathData<- data %>% select(Year, `Cause Name`, State, Deaths, `Age-adjusted Death Rate`)

#create training and test sets
set.seed(7)
train <- sample(1:nrow(deathData), size = nrow(deathData)*0.8)
test <- dplyr::setdiff(1:nrow(deathData), train)
deathDataTrain <- deathData[train, ]
deathDataTest <- deathData[test, ]

#set up UI file
shinyUI(fluidPage(navbarPage(
    "Exploration of the 10 Leading Causes of Death in The United States",
    #set up infromation tab
    tabPanel("Information",
        sidebarLayout(
            sidebarPanel(
                h4("Link to Data:"),
                a("NCHS-Leading Cause of Death: United States Data", href="https://catalog.data.gov/dataset/age-adjusted-death-rates-for-the-top-10-leading-causes-of-death-united-states-2013",
                  target="_blank")),
             mainPanel(
                 h1("The Data:"),
                 textOutput("theData"),
                 h1("The App:"),
                 textOutput("appDataExplore"),
                 br(),
                 textOutput("clustering"),
                 br(),
                 textOutput("datamodeling"),
                 br(),
                 textOutput("datatable"))
             )),
    #set up data exploration tab
    tabPanel("Data Exploration",
        sidebarLayout(
             sidebarPanel(
                h3(HTML("User Specifications for <em>Graphical Summaries</em>:")),
                h5("Select State:"),
                selectizeInput("State", "State", choices=levels(as.factor(deathData$State))),
                h3(HTML("User Specifications for <em>Numeric Summaries</em>:")),
                h5("Select Cause of Death Name for Five Number Summary of Deaths:"),
                selectizeInput("Cause Name","Cause of Death Name", levels(as.factor(deathData$`Cause Name`))),
                h5("Select State for Five Number Summary of Deaths:"),
                selectizeInput("statesum", "State", levels(as.factor(deathData$State))),
                checkboxInput("checkbox","Choose Year for Proportional Data?"),
                conditionalPanel(condition="input.checkbox",
                    radioButtons("Year", "Year", levels(as.factor(deathData$Year))))
                ),
             mainPanel(
                 fluidRow(
                     column(12,align="center",
                        h1("Graphical Summaries:"))),
                 fluidRow(
                     column(6,
                        plotOutput("deathPlot", hover= "plot_hover"), 
                        verbatimTextOutput("deathInfo"),
                        downloadButton("deathDownload", "Download Death Plot")),
                    column(6,
                        plotOutput("ratePlot", hover="plot_hover2"), 
                        verbatimTextOutput("rateInfo"),
                        downloadButton("ageAdjDownload", "Download Age-adjusted Death Rate Plot"))),
                 br(),
                 br(),
                 fluidRow(
                     column(12,align="center",
                        h1("Numeric Summaries:"))),
                 br(),
                 fluidRow(
                     column(12,align="center",
                        h4("Five Number Summary:"),
                        verbatimTextOutput("fivesum"),
                        br(),
                        br(),
                        h4(uiOutput("text")),
                        withMathJax(),
                        helpText('The formula: $$\\frac{\\text{ deaths for specified cause name} }{\\text{deaths for all causes}}$$'),
                        verbatimTextOutput("summary"))))
        )
        ),
    #set up clustering tab
    tabPanel("Clustering",
        sidebarLayout(
            sidebarPanel(
                h4("Select X Variable:"),
                selectInput('xcol', 'X Variable', c("Deaths", "Year", "Age-adjusted Death Rate")),
                h4("Select Y Variable:"),
                selectInput('ycol', 'Y Variable', c("Deaths", "Year", "Age-adjusted Death Rate"), selected="Age-adjusted Death Rate"),
                h4("Select Number of Clusters (max 10):"),
                numericInput('clusters','Cluster Count',4, min=1, max=10),
                h4("Select Number of Algoirth Iterations (max 20):"),
                numericInput('iteration', 'Number of Iterations of Algorithm', value=1, min=1, max=20)
            ),
            mainPanel(
                fluidRow(
                    column(12,align="center",
                           h1("K Means Clustering"))
                ),
                plotOutput("clusterplot")
            )
            )
        ),
    #set up data modeling tab
    tabPanel("Data Modeling",
             sidebarLayout(
                sidebarPanel(
                    style="position:fixed;width:inherit;",
                    fluidRow(
                        h4("Select State:"),
                        selectizeInput("place", "State", choices=levels(as.factor(deathData$State))),
                        h4("Select Cause of Death Name:"),
                        selectizeInput("disease","Cause of Death Name", levels(as.factor(deathData$`Cause Name`)))
                         )),
                mainPanel(
                    fluidRow(
                        column(12,align="center",
                            h1("Simple Linear Regression"))),
                    br(),
                    fluidRow(
                        column(6,
                            plotOutput("simpLinRegPlot1"),
                            downloadButton("simpDownload1","Download Deaths vs Year Plot"),
                            br(),
                            verbatimTextOutput("regsummary1"),
                            numericInput("inputYearPredictDeaths", label="Input Year to Predict Number of Deaths:", value=2016, min=2016, max=2050),
                            verbatimTextOutput("predictDeaths")),
                        column(6,
                            plotOutput("simpLinRegPlot2"),
                            downloadButton("simpDownload2","Download Age-adjusted Death Rate vs Year"),
                            br(),
                            verbatimTextOutput("regsummary2"),
                            numericInput("inputYearPredictRate", label="Input Year to Predict Age-adjusted Death Rate:", value=2016, min=2016, max=2050),
                            verbatimTextOutput("predictRate"))),
                    br(),
                    fluidRow(
                        column(12,align="center",
                            h1("K Nearest Neighbors"))),
                    br(),
                    fluidRow(
                        column(6,
                            numericInput("inputAgeRate",label="Input Age-adjusted Death Rate to Predict Cause of Death Name:", value=1, min=0, max=1500)),
                        column(6,
                            numericInput("inputDeaths", label="Input Number of Deaths to Predict Cause of Death Name:",value=1, min=0, max=3000000))),
                    fluidRow(
                        column(12, align="center",
                            verbatimTextOutput("knn")))
                     )
             )),
    #set up data table tab
    tabPanel("Data Table",
        sidebarLayout(
            sidebarPanel(
                h4("Download Data as CSV File Below"),
                downloadButton("downloadData", "Download Data Table")
                 ),
            mainPanel(
                fluidRow(
                    column(12, align="center",
                        h1("Ten Leading Causes of Deaths in The United States from 1999-2016")
                     )),
                DT::dataTableOutput("table"))
        )
    )
)))
