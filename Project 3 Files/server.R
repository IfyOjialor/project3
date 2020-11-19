
library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(caret)
library(plotly)
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
    
    #data collected was split into two datasets, one without the target variable 
    #The train dataset was used for visualizations
    
    train <- read.csv("loanData/train.csv")
    test <- read.csv("loanData/test.csv")
    test$Loan_Status <- as.factor("NA")
    
    #Data downloaded from kaggle came in two sets(train and test). So I combined both of them.
    #Combine training and test set
    loan1 <- rbind(train[,2:13], test[,2:13])
    loan1 <- filter(loan1, Gender != '')
    loan1 <- filter(loan1, Married!= '')
    loan1 <- filter(loan1, Dependents != '')
    loan1 <- filter(loan1, Self_Employed != '')
    loan1 <- filter(loan1, Loan_Status != 'NA')
    
    loan1$Credit_History <- as.factor(loan1$Credit_History)
    loan1$Gender <- as.factor(loan1$Gender)
    loan1$Married <- as.factor(loan1$Married)
    loan1$Dependents <- as.factor(loan1$Dependents)
    loan1$Education <- as.factor(loan1$Education)
    loan1$Self_Employed <- as.factor(loan1$Self_Employed)
    loan1$Property_Area <- as.factor(loan1$Property_Area)
    loan1$Loan_Status <- as.factor(loan1$Loan_Status)
    
    
    loan <- na.omit(loan1)
    
    
    #Split data into train and test sets for later use.
    set.seed(25)
    loanIndex <- createDataPartition(loan$Loan_Status, p=0.1, list=FALSE)
    loanTrain <- loan[loanIndex, ]
    loanTest <- loan[-loanIndex, ]
    
    
    getData <- reactive({
      if(input$Married == "Married"){
        data <- filter(loan, Married == "Yes")
      }else if (input$Married == "Single"){
        data <- filter(loan, Married == "No")
      }else if(input$Gender == "Female"){
        data <- filter(loan, Gender == "Female")
      }else if (input$Gender == "Male"){
        data <- filter(loan, Gender == "Male")
      }
        #gender filter for cluster tab
        else if(input$gclust <- "Female"){
          data <- filter(loan, Gender == "Female")
        }else if(input$gclust == "Male"){
          data <- filter(loan, Gender == "Male")
        }else if(input$mclust== "Married"){
          data <- filter(loan, Married == "Yes")
        }else if (input$mclust == "Single"){
          data <- filter(loan, Married == "No")
        }
      })
       
    
    #update select input for clustering
    observe({
      updateSelectInput(session, "cols",
                        selected =c('ApplicantIncome', 'CoapplicantIncome'))
    })
    
    #create reactive title panel
    output$text<- renderUI({
      info <- paste0('Modelling for ', input$predz , ' variables is shown below')
      h4(info)
    })
    
    #Univariate analysis for numeric variables
    output$histPlot <- renderPlotly({
        plotInputNum()
    })
    
    #For Categorical variables
    plotInputCat <- reactive({
    if (input$cat == 'Gender'){
        ggplot(loan %>% filter(Gender != "") %>% group_by(Gender) %>% summarise(Count = n())) + geom_bar(aes(Gender, Count), 
        stat = "identity", fill = "#f88379") + geom_label(aes(Gender, Count, label = Count), vjust = 0.5) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$cat == 'Married'){
        ggplot(loan %>% filter(Married != "") %>% group_by(Married) %>% summarise(Count = n())) + geom_bar(aes(Married, Count), 
        stat = "identity", fill = "#f88379") + geom_label(aes(Married, Count, label = Count), vjust = 0.5) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
      
    } else if (input$cat == 'Dependents'){
        ggplot(loan %>% filter(Dependents != "") %>% group_by(Dependents) %>% summarise(Count = n())) + geom_bar(aes(Dependents, Count), 
        stat = "identity", fill = "#f88379") + geom_label(aes(Dependents, Count, label = Count), vjust = 0.5) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$cat == 'Education'){
        ggplot(loan %>% filter(Education != "") %>% group_by(Education) %>% summarise(Count = n())) + geom_bar(aes(Education, Count), 
        stat = "identity", fill = "#f88379") + geom_label(aes(Education, Count, label = Count), vjust = 0.5) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
      
    } else if (input$cat == 'Self Employed'){
        ggplot(loan %>% filter(Self_Employed != "") %>% group_by(Self_Employed) %>% summarise(Count = n())) + geom_bar(aes(Self_Employed, Count), 
        stat = "identity", fill = "#f88379") + geom_label(aes(Self_Employed, Count, label = Count), vjust = 0.5) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$cat == 'Credit History'){
        ggplot(loan %>% filter(Credit_History != "") %>% group_by(Credit_History) %>% summarise(Count = n())) + geom_bar(aes(Credit_History, Count), 
        stat = "identity", fill = "#f88379") + geom_label(aes(Credit_History, Count, label = Count), vjust = 0.5) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$cat == 'Property Area'){
        ggplot(loan %>% filter(Property_Area != "") %>% group_by(Property_Area) %>% summarise(Count = n())) + geom_bar(aes(Property_Area, Count), 
        stat = "identity", fill = "#f88379")+ geom_label(aes(Property_Area, Count, label = Count), vjust = 0.5) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$cat == 'Loan_Status'){
        ggplot(loan %>% filter(Loan_Status != "") %>% group_by(Loan_Status) %>% summarise(Count = n())) + geom_bar(aes(Loan_Status, Count), 
        stat = "identity", fill = "#f88379") + geom_label(aes(Loan_Status, Count, label = Count), vjust = 0.5) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
      
    }
    })
    
    #Univariate analysis for categorical variables
    output$freqPlot <- renderPlot({
        plotInputCat()
        
    })
    
    #visualizations for explore tab
    output$barPlot <- renderPlotly({
       data <- getData()
        if (input$var == "Applicant Income"){
          y <- ggplot(data, aes(ApplicantIncome)) + geom_histogram(bins = input$bins)+xlab("Applicant Income")
          ggplotly(y)
          #g + geom_bar(aes(ApplicantIncome, fill = Loan_Status))
        }else if(input$var == "Coapplicant Income"){
          x <- ggplot(data, aes(CoapplicantIncome)) + geom_histogram(bins = input$bins) +xlab("Coapplicant Income")
          ggplotly(x)
        }
      }
    )
    
    #Create numerical summaries for variables
    output$summaryD <- renderPrint({
      data <- getData()
      summary(data)
    })
    
    #Create plot to download plot of numeric values
    plotInputNum <- reactive({
      data <- getData()
      if (input$num == 'Applicant Income'){
        x <- ggplot(data = loan) + geom_histogram(aes(ApplicantIncome), stat = "bin", fill = 'green')+
          theme(axis.text = element_text(size = 12))
        ggplotly(x)
      }
      else if (input$num == 'Coapplicant Income'){
        y <- ggplot(data=loan) + geom_histogram(aes(CoapplicantIncome), stat = "bin", fill = 'green')+
          theme(axis.text = element_text(size = 12))
        ggplotly(y)
        
      }
      
      else if (input$num == 'Loan Amount'){
        z <- ggplot(data=loan) + geom_histogram(aes(LoanAmount), stat = "bin", fill = 'green')+
          theme(axis.text = element_text(size = 12))
        ggplotly(z)
      }
      else if (input$num == 'Loan Amount Term'){
        m <- ggplot(data=loan) + geom_histogram(aes(Loan_Amount_Term), stat = "bin", fill = 'green')+
          theme(axis.text = element_text(size = 12))
        ggplotly(m)
      }
        
      })

    
    output$download_plot2 <- downloadHandler(
      filename = function() {
        'EDAcat.png'
      },
      content = function(file) {
        ggsave(file, plot = plotInputCat(), device = 'png')}
    )
    
    
    plotInputDash <- reactive({
      data <- getData()
      if (input$var == "Applicant Income"){
        y <- ggplot(data, aes(ApplicantIncome)) + geom_histogram(bins = input$bins)+xlab("Applicant Income")
        ggplotly(y)
      }else if(input$var == "Coapplicant Income"){
        x <- ggplot(data, aes(CoapplicantIncome)) + geom_histogram(bins = input$bins) +xlab("Coapplicant Income")
        ggplotly(x)
      }
    }
    )
    
    
    output$table <- renderTable({
      Data <- getData()
      summary(Data)
    })
    
  
    
    #get PCA summary
    output$summary <- renderPrint({
      da <- getData()
      x <- length(input$cols)
      if(x < 2 ){
        stop("Please select at least two variables")
      }
      else{
      summary(prcomp(select(da, input$cols)))
      }
      }
      
    )
    
    #generate biplot for PCA
    output$biplot <- renderPlot({
      data <- getData()
      p <- prcomp(select(data, input$cols), scale = TRUE)
      biplot(p, xlabs = rep(".", nrow(data)), cex = 1.2)
    })
    
    #reactive data for prediction
    p <- reactive({
      if (input$predz == 'ApplicantIncome'){
        data_p <- select(loanTrain, Loan_Status, ApplicantIncome)
      } 
      else if (input$predz == 'CoapplicantIncome'){
        data_p <- select(loanTrain, Loan_Status, CoapplicantIncome)
      }
      else if (input$predz == 'LoanAmount'){
        data_p <- select(loanTrain, Loan_Status, LoanAmount)
      }
      else if (input$predz == 'Loan_Amount_Term'){
        data_p <- select(loanTrain, Loan_Status, Loan_Amount_Term)
      }
    })
    
    #reactive data for predicting test set
    testPr <- reactive({
      if (input$predz == 'ApplicantIncome'){
        data_test <- select(loanTest, Loan_Status, ApplicantIncome)
      } 
      else if (input$predz == 'CoapplicantIncome'){
        data_test <- select(loanTest, Loan_Status, CoapplicantIncome)
      }
      else if (input$predz == 'LoanAmount'){
        data_test <- select(loanTest, Loan_Status, LoanAmount)
      }
      else if (input$predz == 'Loan_Amount_Term'){
        data_test <- select(loanTest, Loan_Status, Loan_Amount_Term)
      }
    })
    
    #train model
    output$modelling <- renderPrint({
      withProgress(message = 'Model is Training',
                   detail = 'This may take a while...', value = 1.0, {
      data_p <- p()
      
      if(input$model == 'Logistic Regression'){
        if (input$predz == 'ALL'){
          logFit <- train(Loan_Status ~ ., data = loanTrain,
                          method = "glm",
                          family = "binomial",
                          preProcess = c("center", "scale"),
                          trControl = trainControl(method = "cv", number = 10))
          logFit
        }
        else{
          logFit1 <- train(Loan_Status ~ ., data = data_p,
                           method = "glm",
                           family = "binomial",
                           preProcess = c("center", "scale"),
                           trControl = trainControl(method = "cv", number = 10))
          logFit1
        }
      }
      else if(input$model == 'XGBoost'){
        if(input$predz == 'ALL'){
          fit <- train(Loan_Status ~ ., data=loanTrain, method="rpart",
                       preProcess=c("center","scale"),
                       trControl=trainControl(method="cv",number=10),
                       tuneGrid=NULL)
          fit
        }
        else if(input$predz != 'ALL'){
          fit2 <- train(Loan_Status ~ ., data=data_p, method="rpart",
                        preProcess=c("center","scale"),
                        trControl=trainControl(method="cv",number=10),
                        tuneGrid=NULL)
          fit2

        }}
                   }
   
)})
      #predicting on test set
      output$predTest <- renderPrint({ 
      withProgress(message = 'Model is Training',
                   detail = 'This may take a while...', value = 1.0, {
                     data_p <- p()
                     data_test <- testPr()
                     if(input$model == 'Logistic Regression'){
                       if (input$predz == 'ALL'){
                         logFit <- train(Loan_Status ~ ., data = loanTrain,
                                         method = "glm",
                                         family = "binomial",
                                         preProcess = c("center", "scale"),
                                         trControl = trainControl(method = "cv", number = 10))
                        predict(logFit, newData =loanTest)
                         
                       }
                       else{
                         logFit1 <- train(Loan_Status ~ ., data = data_p,
                                          method = "glm",
                                          family = "binomial",
                                          preProcess = c("center", "scale"),
                                          trControl = trainControl(method = "cv", number = 10))
                         predict(logFit1, newData = data_test)
                         
                       }
                     }
                     else if(input$model == 'XGBoost'){
                       if(input$predz == 'ALL'){
                         fit <- train(Loan_Status ~ ., data=loanTrain, method="rpart",
                                      preProcess=c("center","scale"),
                                      trControl=trainControl(method="cv",number=10),
                                      tuneGrid=NULL)
                         predict(fit, newData = loanTest)
                         
                       }
                       else if(input$predz != 'ALL'){
                         fit2 <- train(Loan_Status ~ ., data=data_p, method="rpart",
                                       preProcess=c("center","scale"),
                                       trControl=trainControl(method="cv",number=10),
                                       tuneGrid=NULL)
                          predict(fit2, newData = data_test)
                         
                         
                       }}
                   }
                   
      )
      
      })
   
    output$tabled <- renderTable({
      if(input$Marr == "Married"){
        data <- filter(loan, Married == "Yes")
      }else if (input$Marr == "Single"){
        data <- filter(loan, Married == "No")
      }else if(input$Gend == "Female"){
        data <- filter(loan, Gender == "Female")
      }else if (input$Gend == "Male"){
        data <- filter(loan, Gender == "Male")
      }
      
      #save data to a csv file
      if(input$save){
        write.csv(data,"./loan_file.csv", row.names = FALSE)
        }
      view(data)
    })
} )  
    
    
    
  


