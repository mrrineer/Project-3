library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(DT)
library(caret)


shinyServer(function(input, output, session) {
 
   #bring in data
  data<-read_csv("Leading Causes of Death.csv") 
  #filter data
  deathData<- data %>% select(Year, `Cause Name`, State, Deaths, `Age-adjusted Death Rate`)
  
  #make data reactive
  getData<-reactive({
    newData<-deathData %>% filter(State==input$State)
  })
  
  #create information text
  output$informationText<- renderText(
  "insert text later")
  
  
  #create deaths and year plot 
  output$deathPlot <- renderPlot({
    #get data
    newData<-getData()
    
    g <- ggplot(newData, aes (x = Year, y = Deaths))
    g+geom_point(size=3, aes(col=`Cause Name`))+ggtitle("Deaths Per Year in User Specified State")+labs(col="Cause of Death Name")
  })
  output$deathInfo<-renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    paste0("hover: ", xy_str(input$plot_hover))
  })

  #death plot download button
  plotInput=function(){
    newData<-getData()
    
    g <- ggplot(newData, aes (x = Year, y = Deaths))
    g+geom_point(size=3, aes(col=`Cause Name`))+ggtitle("Deaths Per Year in User Specified State")+labs(col="Cause of Death Name")
    
  }
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
    g1+geom_point(size=3, aes(col=`Cause Name`))+ggtitle("Age-adjusted Death Rate Per Year in User Specified State")+labs(col="Cause of Death Name")+ylab("Age-adjusted Death Rate")
  })
  
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
    g1<-ggplot(newData, aes(x=Year, y=`Age-adjusted Death Rate`))+ggtitle("Age-adjusted Death Rate Per Year in User Specified State")+labs(col="Cause of Death Name")+ylab("Age-adjusted Death Rate")
    g1+geom_point(size=3, aes(col=`Cause Name`))
    
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

  output$summary<-renderPrint({
    deathDatastat<-deathData %>% filter(State==input$State) %>% filter(Year==input$Year)
    
    deathDatastat[1:10,4]/sum(deathDatastat[11,4])
    as.data.frame(deathDatastat[1:10,4]/sum(deathDatastat[11,4]), row.names =c("Kidney Disease", "Suicide","Alzheimer's Disease","Influenze and Pneumonia", "Diabetes", "Unintential Injuries","Chronic Lower Respiratory Disease", "Stroke","Cancer","Heart Disease"))
  })
  
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
  
  #k means cluster
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
  
  #data modeling
  #simple linear regression
  
  getRegData<-reactive({
    regData<-deathData %>% filter(State==input$place) %>% filter(`Cause Name` ==input$disease)
  })
  output$simpLinRegPlot1<-renderPlot({
    regData<-getRegData()
    ggplot(regData, aes(x=Year, y=Deaths)) + geom_point() + stat_smooth(method="lm", col="blue")+ggtitle("Deaths vs Year for User Specified State and Cause of Death Name")
  })
  
  #download button
  deathRegPlotInput=function(){
    regData<-getRegData()
    
    ggplot(regData, aes(x=Year, y=Deaths)) + geom_point() + stat_smooth(method="lm", col="blue")+ggtitle("Deaths vs Year for User Specified State and Cause of Death Name")
    
  }
  output$simpDownload1<-downloadHandler(
    filename='DeathsvsYearSimpleLinearRegression.png',
    content=function(file){
      device<-function(...,width, height){
        grDevices::png(...,width=width,height=height,res=300,units="in")
      }
      ggsave(file,plot=deathRegPlotInput(),device=device)
    })
  
  
  
  output$regsummary1<-renderPrint({
    regData<-getRegData()
    fit<-lm(Deaths~ Year, regData)
    summary(fit)
  })
  
  getRegData<-reactive({
    regData<-deathData %>% filter(State==input$place) %>% filter(`Cause Name` ==input$disease)
  })
  output$simpLinRegPlot2 <- renderPlot({
    #get data
    regData<-getRegData()
    ggplot(regData, aes(x=Year, y=`Age-adjusted Death Rate`))+geom_point()+stat_smooth(method="lm", col="red")+ylab("Age-adjusted Death Rate")+ggtitle("Age-adjusted Death Rate vs Year for User Specified State and Cause of Death Name ")
  })
  
  #download button
  ageAdjRegPlotInput=function(){
    regData<-getRegData()
    
    ggplot(regData, aes(x=Year, y=`Age-adjusted Death Rate`))+geom_point()+stat_smooth(method="lm", col="red")+ylab("Age-adjusted Death Rate")+ggtitle("Age-adjusted Death Rate vs Year for User Specified State and Cause of Death Name ")
    
  }
  output$simpDownload2<-downloadHandler(
    filename='AgeAdjDeathRatevsYearSimpleLinearRegression.png',
    content=function(file){
      device<-function(...,width, height){
        grDevices::png(...,width=width,height=height,res=300,units="in")
      }
      ggsave(file,plot=ageAdjRegPlotInput(),device=device)
    })
  
  output$regsummary2<-renderPrint({
    regData<-getRegData()
    fit<-lm(`Age-adjusted Death Rate`~ Year, regData)
    summary(fit)
  })
  output$predictRate<-renderPrint({
    regData<-getRegData()
    invisible(cat(predict(lm(`Age-adjusted Death Rate`~ Year, regData),newdata=data.frame(Year=input$inputYearPredictRate))))
  })
  
  
  output$predictDeaths<-renderPrint({
    regData<-getRegData()
    invisible(cat(predict(lm(Deaths~ Year, regData),newdata=data.frame(Year=input$inputYearPredictDeaths))))
  })
  
  #k nearest neighbors
  output$knn<-renderPrint({
    #create training and test sets
    set.seed(7)
    train <- sample(1:nrow(deathData), size = nrow(filtered)*0.8)
    test <- dplyr::setdiff(1:nrow(deathData), train)
    deathDataTrain <- deathData[train, ]
    deathDataTest <- deathData[test, ]
    #train model
    knnFit <- train(
                     `Cause Name` ~ `Age-adjusted Death Rate`+ Deaths, 
                     data = deathDataTrain, 
                     method = "knn", 
                     trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5), 
                     preProcess = c("center", "scale"))
    predict(knnFit, newdata=data.frame(`Age-adjusted Death Rate`=input$inputAgeRate,Deaths=input$inputDeaths))
    
  })
  
  #data table
  output$table<-DT::renderDataTable({
    DT::datatable(deathData, rownames=F)
  })
  #download button
  output$downloadData<-downloadHandler(
    filename="deathDataTable.csv",
    content=function(file){
      write.csv(deathData,file)
    }
  )
  
})
