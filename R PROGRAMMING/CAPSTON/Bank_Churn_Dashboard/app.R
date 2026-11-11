# =====================================================
# BANK CUSTOMER CHURN ANALYSIS AND RISK ASSESSMENT
# Modern Banking Analytics Dashboard
# Developed using R Shiny
# =====================================================

# ------------------ LIBRARIES ------------------------

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)

# ------------------ LOAD DATA ------------------------

customer_data <- read.csv(
  "data/cleaned_bank_churn.csv"
)

risk_data <- read.csv(
  "data/customer_risk_analysis.csv"
)

log_model <- readRDS(
  "models/logistic_model.rds"
)

rf_model <- readRDS(
  "models/random_forest_model.rds"
)

# ------------------ DATA PREPARATION -----------------

customer_data$Exited <- as.integer(
  customer_data$Exited
)

# Customer Statistics

total_customers <- nrow(customer_data)

churned <- sum(customer_data$Exited == 1)

active <- sum(customer_data$Exited == 0)

churn_rate <- round(
  (churned / total_customers) * 100, 2
)

# ------------------ CUSTOM CSS -----------------------

custom_css <- "
body, .content-wrapper {
  background-color: #f4f7fb !important;
  font-family: Arial, sans-serif;
}

.main-header .logo {
  background-color: #102b50 !important;
  color: white !important;
  font-weight: bold;
}

.main-header .navbar {
  background-color: #102b50 !important;
}

.main-sidebar {
  background-color: #102b50 !important;
}

.sidebar-menu > li > a {
  color: #dce6f2 !important;
  padding: 15px;
}

.sidebar-menu > li.active > a {
  background-color: #2463eb !important;
  border-left: 4px solid white;
}

.sidebar-menu > li:hover > a {
  background-color: #1b416e !important;
}

.box {
  border-radius: 12px !important;
  border-top: 0 !important;
  box-shadow: 0 3px 15px rgba(0,0,0,0.06) !important;
}

.box-title {
  font-weight: bold !important;
  color: #102b50 !important;
}

.small-box {
  border-radius: 12px !important;
  box-shadow: 0 3px 15px rgba(0,0,0,0.06);
}

.content {
  padding: 20px;
}

h2 {
  color: #102b50;
  font-weight: bold;
}

.btn-primary {
  background-color: #2463eb !important;
  border: none;
  border-radius: 6px;
  padding: 10px 20px;
}

"

# =====================================================
# USER INTERFACE
# =====================================================

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "BANK ANALYTICS"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard Overview",
        tabName = "home",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Customer Analytics",
        tabName = "analysis",
        icon = icon("users")
      ),
      
      menuItem(
        "Churn Analysis",
        tabName = "churn",
        icon = icon("chart-bar")
      ),
      
      menuItem(
        "Churn Prediction",
        tabName = "prediction",
        icon = icon("brain")
      ),
      
      menuItem(
        "Risk Assessment",
        tabName = "risk",
        icon = icon("exclamation-triangle")
      ),
      
      menuItem(
        "Model Performance",
        tabName = "performance",
        icon = icon("chart-line")
      )
      
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML(custom_css))
    ),
    
    tabItems(
      
      # =================================================
      # PAGE 1: DASHBOARD OVERVIEW
      # =================================================
      
      tabItem(
        
        tabName = "home",
        
        h2("Bank Customer Analytics"),
        
        p("Overview of customer behaviour and churn performance."),
        
        fluidRow(
          
          valueBox(
            total_customers,
            "Total Customers",
            icon = icon("users"),
            color = "blue"
          ),
          
          valueBox(
            active,
            "Active Customers",
            icon = icon("user-check"),
            color = "green"
          ),
          
          valueBox(
            churned,
            "Churned Customers",
            icon = icon("user-times"),
            color = "red"
          ),
          
          valueBox(
            paste0(churn_rate, "%"),
            "Customer Churn Rate",
            icon = icon("chart-line"),
            color = "yellow"
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Customer Churn Distribution",
            width = 6,
            plotlyOutput("home_churn")
          ),
          
          box(
            title = "Geography Distribution",
            width = 6,
            plotlyOutput("home_geo")
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Customer Age Distribution",
            width = 12,
            plotlyOutput("home_age")
          )
          
        )
        
      ),
      
      # =================================================
      # PAGE 2: CUSTOMER ANALYTICS
      # =================================================
      
      tabItem(
        
        tabName = "analysis",
        
        h2("Customer Analytics"),
        
        fluidRow(
          
          box(
            
            title = "Customer Filters",
            
            width = 12,
            
            selectInput(
              "filter_geo",
              "Select Geography",
              
              choices = c(
                "All",
                sort(unique(customer_data$Geography))
              )
            ),
            
            selectInput(
              "filter_gender",
              "Select Gender",
              
              choices = c(
                "All",
                sort(unique(customer_data$Gender))
              )
            ),
            
            sliderInput(
              "filter_age",
              "Select Age Range",
              
              min = min(customer_data$Age),
              max = max(customer_data$Age),
              
              value = range(customer_data$Age)
            ),
            
            actionButton(
              "reset_filters",
              "Reset Filters",
              class = "btn-primary"
            )
            
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Age Distribution",
            width = 6,
            plotlyOutput("age_chart")
          ),
          
          box(
            title = "Gender Distribution",
            width = 6,
            plotlyOutput("gender_chart")
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Account Balance Analysis",
            width = 12,
            plotlyOutput("balance_chart")
          )
          
        ),
        
        box(
          
          title = "Customer Records",
          
          width = 12,
          
          DTOutput("customer_table"),
          
          br(),
          
          downloadButton(
            "download_customers",
            "Download Customer CSV",
            class = "btn-primary"
          )
          
        )
        
      ),
      
      # =================================================
      # PAGE 3: CHURN ANALYSIS
      # =================================================
      
      tabItem(
        
        tabName = "churn",
        
        h2("Customer Churn Analysis"),
        
        fluidRow(
          
          box(
            title = "Geography-wise Churn",
            width = 6,
            plotlyOutput("geo_churn")
          ),
          
          box(
            title = "Age-wise Churn",
            width = 6,
            plotlyOutput("age_churn")
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Churn by Gender",
            width = 12,
            plotlyOutput("gender_churn")
          )
          
        )
        
      ),
      
      # =================================================
      # PAGE 4: CHURN PREDICTION
      # =================================================
      
      tabItem(
        
        tabName = "prediction",
        
        h2("Customer Churn Prediction"),
        
        box(
          
          title = "Enter Customer Information",
          
          width = 12,
          
          fluidRow(
            
            column(
              4,
              
              numericInput(
                "credit",
                "Credit Score",
                650,
                min = 300,
                max = 900
              )
              
            ),
            
            column(
              4,
              
              selectInput(
                "geography",
                "Geography",
                
                choices = sort(unique(customer_data$Geography))
              )
              
            ),
            
            column(
              4,
              
              selectInput(
                "gender",
                "Gender",
                
                choices = sort(unique(customer_data$Gender))
              )
              
            )
            
          ),
          
          fluidRow(
            
            column(
              4,
              
              numericInput(
                "age",
                "Age",
                35,
                min = 18
              )
              
            ),
            
            column(
              4,
              
              numericInput(
                "tenure",
                "Tenure",
                5,
                min = 0,
                max = 10
              )
              
            ),
            
            column(
              4,
              
              numericInput(
                "balance",
                "Account Balance",
                50000,
                min = 0
              )
              
            )
            
          ),
          
          fluidRow(
            
            column(
              4,
              
              numericInput(
                "products",
                "Number of Products",
                1,
                min = 1,
                max = 4
              )
              
            ),
            
            column(
              4,
              
              checkboxInput(
                "creditcard",
                "Has Credit Card",
                TRUE
              )
              
            ),
            
            column(
              4,
              
              checkboxInput(
                "member",
                "Active Member",
                TRUE
              )
              
            )
            
          ),
          
          numericInput(
            "salary",
            "Estimated Salary",
            50000,
            min = 0
          ),
          
          actionButton(
            "predict",
            "Predict Customer Churn",
            icon = icon("brain"),
            class = "btn-primary"
          ),
          
          br(),
          br(),
          
          uiOutput("prediction_output")
          
        )
        
      ),
      
      # =================================================
      # PAGE 5: RISK ASSESSMENT
      # =================================================
      
      tabItem(
        
        tabName = "risk",
        
        h2("Customer Risk Assessment"),
        
        fluidRow(
          
          box(
            title = "Customer Risk Distribution",
            width = 6,
            plotlyOutput("risk_chart")
          ),
          
          box(
            title = "High Risk Customers",
            width = 6,
            DTOutput("high_risk")
          )
          
        ),
        
        fluidRow(
          
          box(
            
            title = "Download Risk Report",
            
            width = 12,
            
            downloadButton(
              "download_risk",
              "Download High-Risk Customer Report",
              class = "btn-primary"
            )
            
          )
          
        )
        
      ),
      
      # =================================================
      # PAGE 6: MODEL PERFORMANCE
      # =================================================
      
      tabItem(
        
        tabName = "performance",
        
        h2("Machine Learning Performance"),
        
        fluidRow(
          
          box(
            title = "Model Comparison",
            width = 12,
            
            tableOutput("model_comparison")
            
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Model Accuracy Comparison",
            width = 12,
            
            plotlyOutput("accuracy_chart")
            
          )
          
        )
        
      )
      
    )
    
  )
  
)

# =====================================================
# SERVER
# =====================================================

server <- function(input, output, session) {
  
  # ---------------------------------------------------
  # CUSTOMER FILTERS
  # ---------------------------------------------------
  
  observeEvent(input$reset_filters, {
    
    updateSelectInput(
      session,
      "filter_geo",
      selected = "All"
    )
    
    updateSelectInput(
      session,
      "filter_gender",
      selected = "All"
    )
    
    updateSliderInput(
      session,
      "filter_age",
      value = range(customer_data$Age)
    )
    
  })
  
  filtered_customers <- reactive({
    
    result <- customer_data
    
    if (!is.null(input$filter_geo) &&
        input$filter_geo != "All") {
      
      result <- result %>%
        filter(Geography == input$filter_geo)
      
    }
    
    if (!is.null(input$filter_gender) &&
        input$filter_gender != "All") {
      
      result <- result %>%
        filter(Gender == input$filter_gender)
      
    }
    
    if (!is.null(input$filter_age)) {
      
      result <- result %>%
        filter(
          Age >= input$filter_age[1],
          Age <= input$filter_age[2]
        )
      
    }
    
    result
    
  })
  
  # ===================================================
  # HOME PAGE CHARTS
  # ===================================================
  
  output$home_churn <- renderPlotly({
    
    p <- ggplot(
      
      customer_data,
      
      aes(
        x = factor(Exited),
        fill = factor(Exited)
      )
      
    ) +
      
      geom_bar() +
      
      labs(
        x = "Churn Status (0 = Stayed, 1 = Churned)",
        y = "Customer Count"
      ) +
      
      scale_fill_manual(
        values = c("#2463eb", "#ef4444")
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  output$home_geo <- renderPlotly({
    
    p <- ggplot(
      
      customer_data,
      
      aes(
        x = Geography,
        fill = Geography
      )
      
    ) +
      
      geom_bar() +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  output$home_age <- renderPlotly({
    
    p <- ggplot(
      customer_data,
      aes(x = Age)
    ) +
      
      geom_histogram(
        bins = 20,
        fill = "#2463eb",
        color = "white"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ===================================================
  # CUSTOMER ANALYSIS CHARTS
  # ===================================================
  
  output$age_chart <- renderPlotly({
    
    p <- ggplot(
      filtered_customers(),
      aes(x = Age)
    ) +
      
      geom_histogram(
        bins = 20,
        fill = "#2463eb",
        color = "white"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  output$gender_chart <- renderPlotly({
    
    p <- ggplot(
      filtered_customers(),
      aes(x = Gender, fill = Gender)
    ) +
      
      geom_bar() +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  output$balance_chart <- renderPlotly({
    
    p <- ggplot(
      filtered_customers(),
      aes(
        x = Geography,
        y = Balance,
        fill = Geography
      )
    ) +
      
      geom_boxplot() +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  output$customer_table <- renderDT({
    
    datatable(
      filtered_customers(),
      
      filter = "top",
      
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
      
    )
    
  })
  
  # ===================================================
  # CUSTOMER CSV DOWNLOAD
  # ===================================================
  
  output$download_customers <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "customer_report_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      write.csv(
        filtered_customers(),
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
  # ===================================================
  # CHURN ANALYSIS
  # ===================================================
  
  output$geo_churn <- renderPlotly({
    
    p <- ggplot(
      
      customer_data,
      
      aes(
        x = Geography,
        fill = factor(Exited)
      )
      
    ) +
      
      geom_bar(position = "dodge") +
      
      labs(
        x = "Geography",
        y = "Customer Count",
        fill = "Churn Status"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  output$age_churn <- renderPlotly({
    
    p <- ggplot(
      
      customer_data,
      
      aes(
        x = Age,
        fill = factor(Exited)
      )
      
    ) +
      
      geom_histogram(
        position = "identity",
        alpha = 0.6,
        bins = 20
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  output$gender_churn <- renderPlotly({
    
    p <- ggplot(
      
      customer_data,
      
      aes(
        x = Gender,
        fill = factor(Exited)
      )
      
    ) +
      
      geom_bar(position = "dodge") +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # ===================================================
  # CHURN PREDICTION
  # ===================================================
  
  prediction_result <- eventReactive(
    input$predict,
    {
      
      validate(
        
        need(input$credit >= 300 &&
               input$credit <= 900,
             "Credit score must be between 300 and 900."),
        
        need(input$age >= 18,
             "Age must be at least 18."),
        
        need(input$tenure >= 0 &&
               input$tenure <= 10,
             "Tenure must be between 0 and 10."),
        
        need(input$balance >= 0,
             "Balance cannot be negative."),
        
        need(input$products >= 1 &&
               input$products <= 4,
             "Number of products must be between 1 and 4."),
        
        need(input$salary >= 0,
             "Salary cannot be negative.")
        
      )
      
      new_customer <- data.frame(
        
        CreditScore = input$credit,
        
        Geography = factor(
          input$geography,
          levels = sort(unique(customer_data$Geography))
        ),
        
        Gender = factor(
          input$gender,
          levels = sort(unique(customer_data$Gender))
        ),
        
        Age = input$age,
        
        Tenure = input$tenure,
        
        Balance = input$balance,
        
        NumOfProducts = input$products,
        
        HasCrCard = as.integer(input$creditcard),
        
        IsActiveMember = as.integer(input$member),
        
        EstimatedSalary = input$salary
        
      )
      
      probability <- predict(
        
        rf_model,
        
        newdata = new_customer,
        
        type = "prob"
        
      )
      
      risk <- as.numeric(
        probability$Churned[1] * 100
      )
      
      level <- if (risk >= 60) {
        
        "High Risk"
        
      } else if (risk >= 30) {
        
        "Medium Risk"
        
      } else {
        
        "Low Risk"
        
      }
      
      list(
        probability = risk,
        level = level
      )
      
    }
    
  )
  
  output$prediction_output <- renderUI({
    
    result <- prediction_result()
    
    risk_color <- if (
      result$level == "High Risk"
    ) {
      
      "#dc2626"
      
    } else if (
      result$level == "Medium Risk"
    ) {
      
      "#d97706"
      
    } else {
      
      "#16a34a"
      
    }
    
    div(
      
      style = paste0(
        "background:white;",
        "padding:25px;",
        "border-radius:12px;",
        "border-left:6px solid ",
        risk_color,
        ";"
      ),
      
      h3("Customer Prediction Result"),
      
      h1(
        
        paste0(
          round(result$probability, 2),
          "%"
        ),
        
        style = paste0(
          "color:",
          risk_color,
          ";font-weight:bold;"
        )
        
      ),
      
      h3(
        
        paste(
          "Risk Level:",
          result$level
        ),
        
        style = paste0(
          "color:",
          risk_color,
          ";"
        )
        
      ),
      
      p(
        "Prediction generated using the Random Forest model."
      )
      
    )
    
  })
  
  # ===================================================
  # RISK ASSESSMENT
  # ===================================================
  
  output$risk_chart <- renderPlotly({
    
    p <- ggplot(
      
      risk_data,
      
      aes(
        x = Risk_Level,
        fill = Risk_Level
      )
      
    ) +
      
      geom_bar() +
      
      labs(
        x = "Risk Category",
        y = "Number of Customers"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  output$high_risk <- renderDT({
    
    high <- risk_data %>%
      
      filter(
        Risk_Level == "High Risk"
      ) %>%
      
      arrange(
        desc(Churn_Probability)
      )
    
    datatable(
      
      head(high, 20),
      
      options = list(
        pageLength = 5,
        scrollX = TRUE
      )
      
    )
    
  })
  
  # ===================================================
  # HIGH RISK CSV DOWNLOAD
  # ===================================================
  
  output$download_risk <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "high_risk_customers_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      high <- risk_data %>%
        
        filter(
          Risk_Level == "High Risk"
        )
      
      write.csv(
        high,
        file,
        row.names = FALSE
      )
      
    }
    
  )
  
  # ===================================================
  # MODEL PERFORMANCE
  # ===================================================
  
  model_comparison_data <- data.frame(
    
    Model = c(
      "Logistic Regression",
      "Random Forest"
    ),
    
    Accuracy = c(
      max(log_model$results$Accuracy),
      max(rf_model$results$Accuracy)
    ),
    
    Kappa = c(
      max(log_model$results$Kappa),
      max(rf_model$results$Kappa)
    )
    
  )
  
  output$model_comparison <- renderTable({
    
    model_comparison_data
    
  })
  
  output$accuracy_chart <- renderPlotly({
    
    p <- ggplot(
      
      model_comparison_data,
      
      aes(
        x = Model,
        y = Accuracy,
        fill = Model
      )
      
    ) +
      
      geom_col(width = 0.5) +
      
      labs(
        x = "Machine Learning Model",
        y = "Cross-Validation Accuracy"
      ) +
      
      scale_y_continuous(
        labels = scales::percent
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  
}

# =====================================================
# RUN APPLICATION
# =====================================================

shinyApp(
  ui = ui,
  server = server
)