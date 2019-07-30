library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(DT)
library(caret)


#bring in data
data<-read_csv("Leading Causes of Death.csv") 
#filter data
deathData<- data %>% select(Year, `Cause Name`, State, Deaths, `Age-adjusted Death Rate`)

train <- sample(1:nrow(deathData), size = nrow(deathData)*0.8)
test <- dplyr::setdiff(1:nrow(deathData), train)
deathDataTest <- deathData[test, ]
deathDataTrain <- deathData[train, ]

knnTest <- train(`Cause Name` ~ `Age-adjusted Death Rate`+Deaths, data = deathDataTrain, method = "knn",
                 trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
                 preProcess = c("center", "scale"))

#set up server file
shinyServer(function(input, output, session) {

  #create text for information tab formatted to break into paragraphs
  output$theData<- renderText(
  "The data utilized for this app contains information on the 10 leading causes of death in the United States. The   variables present are: Year, State, Cause Name, Deaths, and Age-adjusted Death Rate. The data was collected beginning in 1999 and up through 2016.The data was gleaned from all resident death certificates filed in all 50 states as well as D.C. Age-adjusted death rates (per 100,000 population) were based upon the 2000 U.S. standard population and populations used for computing death rates after 2010 are estimates beased on the 2010 census.")
  
  output$appDataExplore<-renderText(
    "The app begins with Graphical Summaries of the data collected on the 10 leading causes of death in the United States from 1999-2016. Under the 'Data Exploration' tab the user begins by selecting a state of interest and then they can view a scatterplot depicting deaths per year that is color-coordinated by the cause of death name. The user can also view a scatterplot of the age-adjusted death rate per year that is again color-coordinated by the cause of death name. These two graphical summaries allow the user to view how the number of deaths and age-adjusted death rate for each cause of death varies year to year as well as from state to state. The user is also able to generate numeric summaries on the 'Data Exploration' tab as well. The user can specify the cause of death and state of interest in order to generate the five number summary for the death variable. In addition, the user can also choose to specify a year of interest in order to see a print out of the proportion of all deaths accounted for by the 10 leading causes of death.")
  
  output$clustering<-renderText(
    "Within the 'Clustering' tab the user is able to see a visual of K Means Clustering. The user is able to specify the x variable, y variable, cluster count, and the number of iterations of the algorithm and the visual updates appropriately. This allows the user to visual in real time how the cluster assignments change based on cluster count as well as iterations of the algorithm. The user is also able to visual for themselves where the data seems to fall into natural clusters.")
  
  output$datamodeling<-renderText(
    "Within the 'Data Modeling' tab the user is able to choose a state and cause of death of interest and a simple linear regression model of deaths vs year, as well as, a simple linear regression model of age-adjusted death rate vs year are generated. The summary statistics for each model fit is also provided for the user to view. Within the summary statistic ouput the adjusted R square value is provided which gives the user a measure of how well the model is doing. For each simple linear regression fit the user is also able to input a year of interest and predict both the deaths and the age-adjusted death rate for the specified state and cause of death.  In addition, a K Nearest Neighbors model is fit with the data. The user is able to input age-adjusted death rate and deaths in order to predict the cause of death with the K Nearest Neighbors model.")
  
  output$datatable<-renderText(
    "Within the 'Data Table' tab the user is able to scroll through the entire data set that was used throughout the app. The data has a search function and is able to be sorted via the arrows next to each column name so as to allow the user to view data of interest more quickly.  In addition, the user is able to download the data table to a csv file if interested.")

  #begin code for data exploration tab
  #graphical summaries
  #make data reactive
  getData<-reactive({
    newData<-deathData %>% filter(State==input$State)
  })
  
  #create deaths and year plot 
  output$deathPlot <- renderPlot({
    #get reactive data
    newData<-getData()
    #create plot
    g <- ggplot(newData, aes (x = Year, y = Deaths))
    g+geom_point(size=3, aes(col=`Cause Name`))+ggtitle("Deaths Per Year in User Specified State")+labs(col="Cause of Death Name")+theme(plot.title=element_text(hjust=.5))
  })
  #create code to allow user to hover over plot and output the x and y coordinates
  output$deathInfo<-renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    paste0("hover: ", xy_str(input$plot_hover))
  })

  #create death plot download button
  plotInput=function(){
    newData<-getData()
    g <- ggplot(newData, aes (x = Year, y = Deaths))
    g+geom_point(size=3, aes(col=`Cause Name`))+ggtitle("Deaths Per Year in User Specified State")+labs(col="Cause of Death Name")+theme(plot.title=element_text(hjust=.5))}
  
  output$deathDownload<-downloadHandler(
    filename='deathPlot.png',
    content=function(file){
      device<-function(...,width, height){
        grDevices::png(...,width=width,height=height,res=300,units="in")
      }
      ggsave(file,plot=plotInput(),device=device)
    })

  #create age-adjusted death rate and year plot
  output$ratePlot<-renderPlot({
    newData<-getData()
    g1<-ggplot(newData, aes(x=Year, y=`Age-adjusted Death Rate`))
    g1+geom_point(size=3, aes(col=`Cause Name`))+ggtitle("Age-adjusted Death Rate Per Year \n in User Specified State")+labs(col="Cause of Death Name")+ylab("Age-adjusted Death Rate")+theme(plot.title=element_text(hjust=.5))
  })
  
  #hover capabilities 
  output$rateInfo<-renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    paste0("hover: ", xy_str(input$plot_hover2))
  })
  
  #age-adjusted death rate plot download button
  ageplotInput=function(){
    newData<-getData()
    g1<-ggplot(newData, aes(x=Year, y=`Age-adjusted Death Rate`))+ggtitle("Age-adjusted Death Rate Per Year \n in User Specified State")+labs(col="Cause of Death Name")+ylab("Age-adjusted Death Rate")
    g1+geom_point(size=3, aes(col=`Cause Name`))+theme(plot.title=element_text(hjust=.5))
  }
  
  output$ageAdjDownload<-downloadHandler(
    filename='ageAdjustedDeathRatePlot.png',
    content=function(file){
      device<-function(...,width, height){
        grDevices::png(...,width=width,height=height,res=300,units="in")
      }
      ggsave(file,plot=ageplotInput(),device=device)
    })
  
  #numeric summaries
  #create proportion of all deaths accounted for by each of the 10 leading causes of death output
  output$summary<-renderPrint({
    deathDatastat<-deathData %>% filter(State==input$State) %>% filter(Year==input$Year)
    deathDatastat[1:10,4]/sum(deathDatastat[11,4])
    as.data.frame(deathDatastat[1:10,4]/sum(deathDatastat[11,4]), row.names =c("Kidney Disease", "Suicide","Alzheimer's Disease","Influenze and Pneumonia", "Diabetes", "Unintential Injuries","Chronic Lower Respiratory Disease", "Stroke","Cancer","Heart Disease"))
  })
  
  #make title reactive to update with the year selected by the user
  output$text<-renderUI({
    if(input$Year == 1999){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 1999:"
    }else if (input$Year == 2000){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2000:"
    }else if (input$Year == 2001){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2001:"
    }else if (input$Year == 2002){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2002:"
    }else if (input$Year == 2003){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2003:"
    }else if (input$Year == 2004){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2004:"
    }else if (input$Year == 2005){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2005:"
    }else if (input$Year == 2006){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2006:"
    }else if (input$Year == 2007){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2007:"
    }else if (input$Year == 2008){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2008:"
    }else if (input$Year == 2009){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2009:"
    }else if (input$Year == 2010){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2010:"
    }else if (input$Year == 2011){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2011:"
    }else if (input$Year == 2012){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2012:"
    }else if (input$Year == 2013){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2013:"
    }else if (input$Year == 2014){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2014:"
    }else if (input$Year == 2015){
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2015:"
    }else{
      "Proportion of All Deaths Accounted for by 10 Leading Causes of Death in 2016:"
    }
    })
  
  #five number summary
  output$fivesum<-renderPrint({
    dataFiveSum<-deathData %>% filter(State==input$statesum) %>% filter(`Cause Name` ==input$`Cause Name`)
    summary(dataFiveSum$Deaths)
  })
  
  #begin code for clustering tab
  #k means cluster plot 
  selectedData<-reactive({
    deathData[,c(input$xcol, input$ycol)]
  })
  clusters<-reactive({
    kmeans(selectedData(),input$clusters, iter.max=input$iteration, algorithm="MacQueen")
  })
  
  output$clusterplot<-renderPlot({
    plot(selectedData(),
         col=clusters()$cluster,
         pch=20, cex=3)
    points(clusters()$centers, pch=4, cex=4, lwd=4)
  })
  
  
  
  #begin code for data modeling tab
  #simple linear regression
  #deaths vs year plot
  getRegData<-reactive({
    regData<-deathData %>% filter(State==input$place) %>% filter(`Cause Name` ==input$disease)
  })
  output$simpLinRegPlot1<-renderPlot({
    regData<-getRegData()
    ggplot(regData, aes(x=Year, y=Deaths)) + geom_point() + stat_smooth(method="lm", col="blue")+ggtitle("Deaths vs Year for User Specified \n State and Cause of Death Name")+theme(plot.title=element_text(hjust=.5))
  })
  
  #download button for deaths vs year plot
  deathRegPlotInput=function(){
    regData<-getRegData()
    
    ggplot(regData, aes(x=Year, y=Deaths)) + geom_point() + stat_smooth(method="lm", col="blue")+ggtitle("Deaths vs Year for User Specified \n State and Cause of Death Name")+theme(plot.title=element_text(hjust=.5))
    
  }
  output$simpDownload1<-downloadHandler(
    filename='DeathsvsYearSimpleLinearRegression.png',
    content=function(file){
      device<-function(...,width, height){
        grDevices::png(...,width=width,height=height,res=300,units="in")
      }
      ggsave(file,plot=deathRegPlotInput(),device=device)
    })
  #fit summary stats for death vs year 
  output$regsummary1<-renderPrint({
    regData<-getRegData()
    fit<-lm(Deaths~ Year, regData)
    summary(fit)
  })
  
  #predict number of deaths via year with model
  output$predictDeaths<-renderPrint({
    regData<-getRegData()
    invisible(cat(predict(lm(Deaths~ Year, regData),newdata=data.frame(Year=input$inputYearPredictDeaths))))
  })
  
  #age-adjusted death rate vs year plot
  getRegData<-reactive({
    regData<-deathData %>% filter(State==input$place) %>% filter(`Cause Name` ==input$disease)
  })
  output$simpLinRegPlot2 <- renderPlot({
    regData<-getRegData()
    ggplot(regData, aes(x=Year, y=`Age-adjusted Death Rate`))+geom_point()+stat_smooth(method="lm", col="red")+ylab("Age-adjusted Death Rate")+ggtitle("Age-adjusted Death Rate vs Year for User Specified \n State and Cause of Death Name ") + theme(plot.title=element_text(hjust=.5))
  })
  
  #download button for age-adjusted death rate vs year plot
  ageAdjRegPlotInput=function(){
    regData<-getRegData()
    
    ggplot(regData, aes(x=Year, y=`Age-adjusted Death Rate`))+geom_point()+stat_smooth(method="lm", col="red")+ylab("Age-adjusted Death Rate")+ggtitle("Age-adjusted Death Rate vs Year for User Specified \n State and Cause of Death Name ") + theme(plot.title=element_text(hjust=.5))
    
  }
  output$simpDownload2<-downloadHandler(
    filename='AgeAdjDeathRatevsYearSimpleLinearRegression.png',
    content=function(file){
      device<-function(...,width, height){
        grDevices::png(...,width=width,height=height,res=300,units="in")
      }
      ggsave(file,plot=ageAdjRegPlotInput(),device=device)
    })
  
  #summary stats for age-adjusted death rate vs year model fit
  output$regsummary2<-renderPrint({
    regData<-getRegData()
    fit<-lm(`Age-adjusted Death Rate`~ Year, regData)
    summary(fit)
  })
  
  #predict age-adjusted death rate via year with model
  output$predictRate<-renderPrint({
    regData<-getRegData()
    invisible(cat(predict(lm(`Age-adjusted Death Rate`~ Year, regData),newdata=data.frame(Year=input$inputYearPredictRate))))
  })
  
  #k nearest neighbors model
    
    #predict cause of death name via death rate and number of deaths
    output$knn<-renderPrint({
    predict(train(`Cause Name` ~ `Age-adjusted Death Rate`+Deaths, data = deathData, method = "knn",
                  trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
                  preProcess = c("center", "scale")), 
            newdata=data.frame(`Age-adjusted Death Rate`=input$ageRate,Deaths=input$numDeaths))
    
  })
  
  #begin code for data table tab
  #create data table
  output$table<-DT::renderDataTable({
    DT::datatable(deathData, rownames=F)
  })
  
  #download data table to csv file button
  output$downloadData<-downloadHandler(
    filename="deathDataTable.csv",
    content=function(file){
      write.csv(deathData,file)
    }
  )
  
})
