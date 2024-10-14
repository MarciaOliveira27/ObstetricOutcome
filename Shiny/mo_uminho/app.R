library(shiny)
library(shinydashboard)
library(shinyBS)
library(readxl)
library(dplyr)
library(nnet)
library(caret)
library(MASS)
library(glmnet)
library(readr)

addResourcePath("prefix", "assets")


style_css <- "

.box {
  box: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
  padding: 20px;
  background-color: white;
  border-radius: 5px;
  margin-bottom: 20px; 
}
.box h2 {
  font-weight: bold;  
font-size: 22px;    
}

.box p {
  font-size: 16px;    
}
.skin-blue .main-header .navbar {
  background-color: #D87093; 
  }
.skin-blue .main-header .navbar:hover {
  background-color: #D87093; 
}
.skin-blue .main-header .logo {
  background-color: white; 
    color: #FFFFFF; 
}
.skin-blue .main-header .logo:hover {
  background-color: white; 
}
.skin-blue .main-header .navbar .sidebar-toggle:hover {
    background-color: #D87093;
}

  .skin-blue .main-sidebar {
    background-color: #FADADD; 
  }

  .skin-blue .main-sidebar .sidebar .sidebar-menu a {
    color: #D87093;
  }

  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
    background-color: #F5B7B1; 
      color: #FFFFFF; 
  }
  .skin-blue .content-wrapper {
    background-color: white; 
  }

.skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
    background: #F5B7B1;
    border-left-color: #D87093;
}
  
"


#Define UI 
ui <- fluidPage(
  
  dashboardPage(
    dashboardHeader(title = tags$div(
      style = "text-align: left;",  
      tags$a(
        icon("baby"), 
        "ObstetricOutcome", 
        target = "_blank", 
        style = "font-size: 20px; color: #D87093; font-weight: bold; font-family: Georgia"
      )
    ), titleWidth = 800),
    
    dashboardSidebar(
      sidebarMenu(id = "sidebar",
        menuItem("Home Page",
                 tabName = "home_page",
                 icon = icon("home")),
        menuItem("About",
                 tabName = "about",
                 icon = icon("circle-info")),
        menuItem("Form",
                 tabName = "form",
                 icon = icon("clipboard-list")),
        menuItem("Result",
                 tabName = "result",
                 icon = icon("bar-chart"))
    )),
    
    dashboardBody(tags$head(
      tags$script(HTML("
        document.title = 'ObstetricOutcome';  
      ")),
      
        tags$style(HTML(style_css))  
      
      ),
      
      tags$style(
        HTML("
        .custom-box {
          border: 2px solid #FADADD; 
          border-radius: 5px; 
          background-color: white; 
        }
      ")
      ),
      
      tabItems(
          tabItem(tabName = "home_page",
                  div(style = "text-align: center;",
                      tags$img(src = "prefix/logo.png", width = 1150, height = 800)
                  ),
                  tags$footer(
                    tags$p("© 2024 All rights reserved.",
                      style = "text-align: center; color: black; margin: 20px 0; font-size: 20px;"
                    ),
                    style = "position: absolute; bottom: 0;width: 100%;background-color: white; padding: 20px;text-align: center;"
                  )
          ),
        
          
          tabItem(tabName = "about", 
                  
                  div(class = "box",
                      h2("Purpose of the application"),
                      p("Obstetrics plays a crucial role in maternal and fetal health, with a significant impact on the pregnancy outcome. The ability to accurately predict obstetric outcome is of paramount importance in providing proper and personalized medical care during gestation."),
                      p("The ObstetricOutcome application is designed to enable healthcare professionals to obtain an estimate of the obstetric outcome based on the value of a specific indicator."),
                  ),
                    
                  div(class = "box",
                      h2("Information about the application"),  
                      p("This application is user-friendly, open-source software developed using the R programming language, the RStudio environment, and the Shiny package."),
                      br(),
                      splitLayout(cellWidths = c("13%", "25%", "14%"), cellArgs = list(style = "padding: 0px"), style = "border: 0px; padding: 0px;",
                      tags$img(src = "prefix/R.jpeg", width=100, height=100),
                      tags$img(src = "prefix/Rstudio.png", width=200, height=100),
                      tags$img(src = "prefix/shiny.jpeg", width=100, height=100)),
                      br(),
                      p("The prediction model implemented in the application is based on a multinomial logistic regression algorithm. This predictive model incorporates the", strong("embedded Elastic Net method"), "with an alpha parameter set at 0.1."),
                  
                  ),
                  
                  div(class = "box",
                      h2("Author"),
                      p("The application was developed by",strong("Márcia Oliveira")," ,under the supervision of",strong("Professor Ana Cristina Braga"),"and co-supervision of",strong("Professor Rosete Nogueira"),"."), 
                      p("This project is a vital component of the Master's dissertation 
                         in Bioinformatics, 'Prediction of obstetric outcome based on the fetal weight/placental weight ratio indicator', 
                         at the University of Minho."),
                      tags$img(src = "prefix/logo_um.png", width=150, height=100),
                  ),
                  
                  div(class = "box",
                      h2("Contacts"),
                      p("If you have any questions, please do not hesitate to contact the person responsible for the development of the application via email at ", strong("marcia27999@gmail.com"),".")
                  )
          ),
          
          tabItem(tabName= "form", tags$h2("Form", style = "font-size: 22px; font-weight: bold;"), 
                  tags$span("Please fill out all the fields below in order to proceed with the obstetric outcome prediction.", 
                            style = "font-size: 16px; margin-bottom: 20px; display: block;"),

                  fluidRow(
                    column(width = 6,
                           box(title = "Maternal and Gestational Information", width = NULL, class = "custom-box", 
                               
                               column(width = 6, numericInput(inputId = "MaternalAge", 
                                                              label = tags$span(
                                                                  "Maternal age:",
                                                                   tags$i(
                                                                     class = "fa fa-question-circle custom-icon", 
                                                                     title = "Enter the maternal age. It must be an valid value between 15.1 and 48.2.")), 
                                                              value = NULL, min = 15.1, max = 48.2)),
                                                                 
                               column(width = 6,
                                      numericInput(
                                        inputId = "GA",
                                        label = tags$span(
                                          "Gestational age (weeks):",
                                          tags$i(
                                            class = "fa fa-question-circle custom-icon", 
                                            title = "Enter the gestational age in weeks. It must be an integer between 22 and 41 weeks."
                                          )
                                        ),
                                        value = NULL,                   
                                        min = 22,                     
                                        max = 41
                                      )
                               ),
                               
                               column(width = 6,   
                                      radioButtons("Fetalgender", "Fetal gender:", list("female", "male", "unknown"))
                               )
                           )
                    ),
                    
                    column(width = 6,
                           box(title = "Placental Measurements", width = NULL, class = "custom-box",
                               column(width = 6,
                                      numericInput(
                                        inputId = "Diameter1",
                                        label = tags$span(
                                          "Diameter 1 (cm):",
                                          tags$i(
                                            class = "fa fa-question-circle custom-icon", 
                                            title = "Enter the value of the largest diameter of the placenta in centimeters (length). It must be a valid value between 5.5 and 32."
                                          )
                                        ),
                                        value = NULL,
                                        min = 5.5,
                                        max = 32
                                      )
                               ),
                               
                               column(width = 6,
                                      numericInput(
                                        inputId = "Diameter2",
                                        label = tags$span(
                                          "Diameter 2 (cm):",
                                          tags$i(
                                            class = "fa fa-question-circle custom-icon", 
                                            title = "Enter the value of the smallest diameter of the placenta in centimeters (width). It must be a valid value between 5 and 30."
                                          )
                                        ),
                                        value = NULL,
                                        min = 5,
                                        max = 30
                                      )
                               ),
                               
                               column(width = 6,
                                      numericInput(
                                        inputId = "Placentalthickness",
                                        label = tags$span(
                                          "Placental thickness (cm):",
                                          tags$i(
                                            class = "fa fa-question-circle custom-icon", 
                                            title = "Enter the value of the placental thickness in centimeters. It must be a valid value between 0.8 and 13.5."
                                          )
                                        ),
                                        value = NULL,
                                        min = 0.8,
                                        max = 13.5
                                      )
                               )
                           )
                    )
                  ),
                  
                  fluidRow(
                    column(width = 6, 
                           box(title = "Weights and FPW Ratio", width = NULL, class = "custom-box",
                               column(width = 6, 
                                      numericInput(
                                        inputId = "FetalWeight",
                                        label = tags$span(
                                          "Fetal Weight (g):",
                                          tags$i(
                                            class = "fa fa-question-circle custom-icon", 
                                            title = "Enter the weight of the fetus in grams. It must be a valid value between 135 and 4880."
                                          )
                                        ),
                                        value = NULL,
                                        min = 135,
                                        max = 4880
                                      )
                               ),
                               
                               column(width = 6,
                                      numericInput(
                                        inputId = "PlacentalWeight",
                                        label = tags$span(
                                          "Placental Weight (g):",
                                          tags$i(
                                            class = "fa fa-question-circle custom-icon", 
                                            title = "Enter the weight of the placenta in grams. It must be a valid value between 52 and 995."
                                          )
                                        ),
                                        value = NULL,
                                        min = 52,
                                        max = 995
                                      )
                               ),
                               
                               column(width = 6,
                                      strong(textOutput("FPWRatio"))
                                      )
                  ))),
                  
                  #Confirmation button 
                  fluidRow(
                    column(width = 12, 
                           tags$span("Please confirm all the data before proceeding with the estimation.", 
                                     style = "font-size: 16px; margin-bottom: 20px; display: block;"),
                           box(width = NULL,
                               actionButton(inputId = "button_confirm_data", label = "Confirm", icon = icon("check"))
                           )
                    )
                  )),
                  
          
        tabItem(tabName="result", 
          #Button to perform regression
          actionButton(inputId = "button_run_regression", label = "Estimate", icon = icon("arrows-rotate")),
          
          tags$br(),  
          tags$br(),
      
          uiOutput(outputId = "result_ui"))
  
        ))))

#Define server logic required
    server <- function(input, output, session) {
      
      calculateFPWRatio <- reactive({
        if (!is.null(input$FetalWeight) && !is.null(input$PlacentalWeight) && !is.na(input$FetalWeight) && !is.na(input$PlacentalWeight) && input$PlacentalWeight != 0) {
          ratio <- input$FetalWeight / input$PlacentalWeight
          return(round(ratio, 2))
        } else {
          return(NULL)
        }
      })
      
      output$FPWRatio <- renderText({
        ratio <- calculateFPWRatio()
        
        if (!is.null(ratio)) {
          paste("FPW Ratio:", ratio)
        } else {
          ""
        }
      })
      
      dataset_train <- read_xlsx("d_train_smote.xlsx")
      x_dataset_train <- dataset_train[ , -which(names(dataset_train) == "outcome")]
      y_dataset_train <- dataset_train$outcome 
      y_dataset_train <- factor(y_dataset_train)
      y_dataset_train  <- relevel(y_dataset_train , ref = "newborn")
      x_dataset_train$Fetalgender <- as.factor(x_dataset_train$Fetalgender)
      cgl_cols <- which(sapply(x_dataset_train, is.factor))
      x_dataset_train[, cgl_cols] <- lapply(x_dataset_train[, cgl_cols], as.factor)
      x_dataset_matrix <- model.matrix(~ . - 1, data = x_dataset_train)
      
      cat("x_dataset_matrix:\n")
      print(head(x_dataset_matrix))
      
      output$inputTable <- renderTable({ data.frame(Maternal_age = input$MaternalAge, Gestational_age = input$GA, Fetal_gender = input$Fetalgender, Diameter1 = input$Diameter1, Diameter2 = input$Diameter2, 
                                                    Placental_thickness = input$Placentalthickness, Fetal_weight = input$FetalWeight, Placental_weight = input$PlacentalWeight, ratio_FPW = calculateFPWRatio())}, options = list(scrollX = TRUE) ) #})
      
      
      observeEvent(input$button_confirm_data, {
        showModal(modalDialog(
          title = "Entered data",
          tags$style(HTML(".modal-body {overflow-x: auto;
            }")),
          tableOutput("inputTable"),  
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_estimate", "Confirm")
          )
        ))
      })
      
      observeEvent(input$confirm_estimate, {
        removeModal()
        #Switch to results
        updateTabItems(session, "sidebar", selected = "result")
      })
      
      observeEvent(input$button_run_regression, {
        
        ratio <- calculateFPWRatio()
        
        formula <- as.formula("outcome ~ MaternalAge + GA + Fetalgender + Diameter1 + Diameter2 + 
                                                    Placentalthickness + ratio")
        
        entered_data <- data.frame(
          MaternalAge = input$MaternalAge,
          GA = input$GA,
          Fetalgender = input$Fetalgender,
          Diameter1 = input$Diameter1,
          Diameter2 = input$Diameter2,
          Placentalthickness = input$Placentalthickness,
          ratio = ifelse(!is.null(ratio), ratio, NA)
        )
        
        pre_train <- readRDS("pre_train.rds")
        entered_data_scaled <- predict(pre_train, newdata = entered_data[, c("MaternalAge", "GA", "Diameter1", "Diameter2", "Placentalthickness", "ratio")])
        entered_data_scaled$Fetalgender <- entered_data$Fetalgender
        entered_data_scaled$Fetalgender <- factor(entered_data_scaled$Fetalgender, levels = levels(x_dataset_train$Fetalgender))
        entered_data_matrix <- model.matrix(~ . - 1, data = entered_data_scaled)
        
        tctl <- trainControl(method = "cv", number = 10)
        
        hyperparameter_grid <- expand.grid(alpha = 0.1,  
                                           lambda = 0.15)
        
        elasticFit <- train(x_dataset_matrix, y_dataset_train, method = "glmnet", 
                            trControl = tctl, tuneGrid = hyperparameter_grid)
        
        predictions <- predict(elasticFit, newdata = entered_data_matrix, type = "prob")
        predicted_classes <- colnames(predictions)[apply(predictions, 1, which.max)]
        predicted_classes_factor <- factor(predicted_classes, levels = levels(y_dataset_train))
        print(predictions)
        
        #Presentation of Results
        output$result_ui <- renderUI({
          
            predictions_df <- data.frame(
              Outcome = c("Newborn", "Intrauterine fetal death", "Neonatal death"),
              Probability = as.numeric(predictions)
            )
            
            circle_colors <- c("green", "red", "orange")
            
            predictions_table <- tags$table(
              class = "table table-bordered",
              style = "max-width: 600px; font-size: 16px;",
              tags$thead(
                tags$tr(
                  tags$th(style = "width: 15%;", "Outcome"),
                  tags$th(style = "width: 5%;", "Probability"),
                  tags$th(style = "width: 5%;", "Status")
                )
              ),
              tags$tbody(
                lapply(1:nrow(predictions_df), function(i) {
                  tags$tr(
                    tags$td(predictions_df$Outcome[i]),
                    tags$td(round(predictions_df$Probability[i], 2)), 
                    tags$td(
                      tags$div(
                        style = paste("width: 20px; height: 20px; border-radius: 50%; background-color:", circle_colors[i], ";")
                      )
                    )
                  )
                })
              )
            )
                
            
            tagList(
              tags$h3("Predicted probabilities for obstetric outcome:", 
                      style = "color: black; font-weight: bold; font-size: 22px;"),
              
              tags$br(),
              
              predictions_table,
              
              tags$br(), 
              tags$br(),
              
              if (predicted_classes_factor == "newborn") {
                tagList(
                  tags$h3("Prediction result:", style = "color: black; font-weight: bold; font-size: 22px;"),
                  tags$p("The model predicts that the individual will be a newborn with a normal development.", style = "color: green; font-size: 24px;")
                )
              } else if (predicted_classes_factor == "intra_fetal_death") {
                tagList(
                  tags$h3("Prediction result:", style = "color: black; font-weight: bold; font-size: 22px;"),
                  tags$p("The model predicts that the individual is at high risk of intrauterine fetal death.", style = "color: red; font-size: 24px;")
                )
              } else {
                tagList(
                  tags$h3("Prediction result:", style = "color: black; font-weight: bold; font-size: 22px;"),
                  tags$p("The model predicts that the individual is at high risk of neonatal death.", style = "color: orange; font-size: 24px;")
                )
              }
            )
          })
      })
    }
    
    
#Run the application 
shinyApp(ui = ui, server = server)


