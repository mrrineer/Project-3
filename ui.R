library(ggplot2)
library(shiny)
library(plotly)
library(tidyverse)

#bring in data
data<-read_csv("Leading Causes of Death.csv") 
#filter data
deathData<- data %>% select(Year, `Cause Name`, State, Deaths, `Age-adjusted Death Rate`)

shinyUI(fluidPage(navbarPage(
    "Exploring the 10 Leading Cause of Death in The United States",
    tabPanel("Information",
        sidebarLayout(
            sidebarPanel(
                h4("Link to Data:"),
                a("NCHS-Leading Cause of Death: United States Data", href="https://catalog.data.gov/dataset/age-adjusted-death-rates-for-the-top-10-leading-causes-of-death-united-states-2013")),
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
    tabPanel("Data Exploration",
        sidebarLayout(
             sidebarPanel(
                h3(HTML("User Specifications for <em>Graphical Summaries</em>:")),
                h5("Choose State:"),
                selectizeInput("State", "State", choices=levels(as.factor(deathData$State))),
                h3(HTML("User Specifications for <em>Numeric Summaries</em>:")),
                h5("Choose Cause Name for Five Number Summary of Deaths:"),
                selectizeInput("Cause Name","Cause of Death Name", levels(as.factor(deathData$`Cause Name`))),
                h5("Choose State for Five Number Summary of Deaths:"),
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
                        verbatimTextOutput("summary"))))
        )
        ),
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
    tabPanel("Data Modeling",
             sidebarLayout(
                 sidebarPanel(
                     style="position:fixed;width:inherit;",
                     fluidRow(
                            h4("Choose State"),
                            selectizeInput("place", "State", choices=levels(as.factor(deathData$State))),
                            h4("Choose Cause Name"),
                            selectizeInput("disease","Cause of Death Name", levels(as.factor(deathData$`Cause Name`)))
                         )),
                     mainPanel(
                         fluidRow(
                             column(12,align="center",
                                    h1("Simple Linear Regression"))),
                         fluidRow(
                            column(6,
                         plotOutput("simpLinRegPlot1"),
                         downloadButton("simpDownload1","Download Deaths vs Year Plot"),
                         verbatimTextOutput("regsummary1"),
                         numericInput("inputYearPredictDeaths", label="Input Year to Predict Number of Deaths:", value=2016, min=2016, max=2050),
                         verbatimTextOutput("predictDeaths")),
                         column(6,
                                plotOutput("simpLinRegPlot2"),
                                downloadButton("simpDownload2","Download Age-adjusted Death Rate vs Year"),
                                verbatimTextOutput("regsummary2"),
                                numericInput("inputYearPredictRate", label="Input Year to Predict Age-adjusted Death Rate:", value=2016, min=2016, max=2050),
                         verbatimTextOutput("predictRate"))),
                                fluidRow(
                                    column(12,align="center",
                                           h1("K Nearest Neighbors"))),
                         numericInput("inputAgeRate",label="Input Age-adjusted Death Rate to Predict Cause Name", value=1, min=0, max=1500),
                         numericInput("inputDeaths", label="Input Number of Deaths to Predict Cause Name",value=1, min=0, max=3000000),
                         verbatimTextOutput("knn")
                     )
             )),
    tabPanel("Data Table",
        sidebarLayout(
             sidebarPanel(
                 h4("Download Data as CSV File Below"),
                 downloadButton("downloadData", "Download Data Table")
                 ),
                 mainPanel(
                     fluidRow(
                         column(12, align="center",
                                h1("Ten Leading Causes of Deaths in The United States from 1999-2016 Data")
                     )),
                     DT::dataTableOutput("table"))
        )
    )
)))
