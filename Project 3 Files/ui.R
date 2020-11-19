

library(shiny)
library(shinydashboard)
library(readr)
library(tidyverse)
library(plotly)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("journal"),

    dashboardPage(
        dashboardHeader(title = 'LOAD PREDICTION EXPLORER', 
                        titleWidth = 290),
        dashboardSidebar(width = 290,
                         sidebarMenu(
                                     menuItem("Info", tabName = "About",
                                              icon = icon("users")),
                                     menuItem("Plots", tabName = "plots", icon = icon('bar-chart-o')),
                                     menuItem("Explore", tabName = "dash", 
                                              icon = icon('compass')),
                                     menuItem("PCA", tabName = "pca", icon = icon('poll')),
                                     menuItem("Predictions", tabName = "Predi",
                                              icon = icon("list-alt")),
                                     menuItem("Data", tabName = "data",
                                              icon = icon("table")))),
        dashboardBody(tags$head(tags$style(HTML('.main-header .logo {font-weight: bold;}'))),
                      tabItems(
                        tabItem('About',
                                
                                fluidRow(
                                  
                                  #Add latex functionality
                                  withMathJax(),
                                  
                                  #App audience and description of dataset
                                  box(
                                    title = 'App Overview',
                                    status = 'danger',
                                    p('This application is intended for users who wish to analyze, explore, and predict variables within a dataset.'),
                                    p(
                                      strong("Data Description")
                                    ),
                                      
                                   p(
                                    'The dataset used in this app was taken from Kaggle. You can dowload the dataset', 
                                    a(href = 'https://www.kaggle.com/altruistdelhite04/loan-prediction-problem-dataset', 'here', target = '_blank'),
                                    ". This dataset is from a dream house finance company who deals in all home loans. Customers first apply for a loan after that the company validates the customer eligibility for the loan.",
                                    "Companies use details such as; Gender, Marital Status, Education, Number of dependents, Income, Loan Amount, Credit history and other variables to automate the loan eligibility process."),
                                  p(
                                    "This is a classification problem in which we have to predict whether a loan would be approved or not based on the customer segments. ")
                                ),
                                
                                box(
                                  title = "How to use this app",
                                  status = 'danger',
                                  p("This app has six tabs/pages. The plots tab is used for visualizing an individual variable either numeric or categorical."),
                                  p(strong("Explore Tab"),
                                    "The Explore tab is used for visualizing multiple variables"),
                                  p(strong("PCA tab"),
                                    "The PCA tab is used for principal components analysis where the user can specify aspects of the method."),
                                  p(strong("Predictions tab"), 
                                    "The modelling is used for making predictions about the data, the user can specify the kind of model."),
                                  p(strong("Data tab"),
                                    "The data tab allows the user to scroll through the data, subset, and download it.")
                                )
                                )
                        ),
                          #Plots tab content for univariate analysis
                          tabItem('plots', 
                                  #Histogram filter
                                  box(status = 'success', title = 'Filter for the histogram plot', 
                                      selectInput('num', "Numerical variables:", c('Applicant Income', 
                                                  'Coapplicant Income', 'Loan Amount', 'Loan Amount Term')),
                                      footer = 'Histogram plot for numerical variables'),
                                  box(downloadButton('download_plot1', 'Download Plot')),
                                  box(plotlyOutput('histPlot')),
                                  
                                  #Frequency plot filter
                                  box(status = 'success', title = 'Filter for the frequency plot',
                                      selectInput('cat', 'Categorical variables:', c('Loan_Status','Gender', 'Married',
                                                 'Dependents', 'Education', 'Self Employed', 'Property Area', 'Credit History')),
                                      footer = 'Frequency plot for categorical variables'),
                                  #Boxes to display the plots
                                  box(downloadButton('download_plot2', 'Download Plot')),
                                  box(plotOutput('freqPlot'))
                                  
                                  
                                  ),
                        
                          tabItem('dash',
                                  #Dashboard filters
                                  box(title = 'Filters', status = 'primary', width = 12,
                                                  radioButtons('Gender', 'Subset by Gender?', c('Female', 'Male')),
                                                  radioButtons('Married', 'Subset by Marriage Status?', c('Yes', 'No')),
                                                  selectInput('var', 'Select variables to plot', c('Applicant Income', 'Coapplicant Income')),
                                                  sliderInput('bins', 'Number of bins:', min = 1, max = 100, value = 30),
                                                  h4("The plot will not be displayed when the slider is less than 10")
                                                  ),
                                  box(conditionalPanel("input.bins >= 10",
                                                       plotlyOutput('barPlot'))),
                                  box(downloadButton('download_plot3', 'Download Plot')),
                                  box(verbatimTextOutput('summaryD'))),
                                  
                          tabItem('pca',
                                  box(title = "Principal Component Analysis", status = 'success',
                                      width = 12),
                                  
                                  selectInput('gclust', 'Subset by Gender', c("Female", "Male")),
                                  selectInput('mclust', 'Subset by Married Status', c("Married", "Single")),
                                             
                                  h4("Choose the columns of your data to include in the PCA. Only columns
                                     containing numeric data are showed here because PCA does not work with
                                     non-numeric data."),
              
                                  selectizeInput('cols', 'Choose variables for PCA', c('ApplicantIncome', 
                                                'CoapplicantIncome', 'LoanAmount', 'Loan_Amount_Term'), multiple = TRUE),
                                  box(verbatimTextOutput('summary')),
                                  box(plotOutput('biplot'))),
                        
                          tabItem('Predi',
                                  
                                  box(title = 'Prediction result', status = 'primary',
                                  p('This is a classification problem since we have to predict a binary outcome(Loan_Status) given a set of independent variables',
                                    'Logistic Regression is an estimation of the Logit function. The Logit function is simply a log of odds in favor of an event'),
                                  withMathJax(
                                  p('$$logit(p) = log(\\frac{p}{1-p}) = \\beta_0+\\beta_1X_1+\\beta_2X_2+...++\\beta_kX_k$$'))),
                                  selectInput('model', 'Choose a model type', c('Logistic Regression',
                                              'XGBoost')),
                                  selectInput('predz', 'Choose the predictor variables for modelling',
                                              c('ALL', 'ApplicantIncome', 
                                                'CoapplicantIncome', 'LoanAmount', 'Loan_Amount_Term')),
                                  uiOutput("text"),
                                  box(verbatimTextOutput('modelling', placeholder = TRUE)),
                                  box(verbatimTextOutput('predTest', placeholder = TRUE)),
                                 
                                  ),
                                 
                         
                          tabItem('data',
                                  box(title = 'Data', status = 'primary', width = 12, 
                                  radioButtons('Gend', 'Subset by Gender?', c('Female', 'Male')),
                                  radioButtons('Marr', 'Subset by Marriage Status?', c('Yes', 'No'))),
                                  actionButton('save', 'Save File'),
                                  tableOutput('tabled'))
                                  
                          )))))

