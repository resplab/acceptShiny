library(shiny)
library(shinyjs)
library(shinyhelper)
library(shinythemes)
library(plotly)
library(ggplot2)
library(ggthemes)
library(stringr)
library(data.table)
library(rmarkdown) #for markdown file
library(knitr) #for markdown file
library(extrafont)
library(htmltools)
library(dplyr)
library(accept)
#options(shiny.error = browser) #debug, amin

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


appCSS <-
  ".mandatory_star { color: red; }"

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page



button_width <- 160

# FEV1_lmer_function_output_summary <- NULL
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jsResetCode, functions = c("reset")),                      # Add the js code to the page
  shinyjs::inlineCSS(appCSS),
  theme = shinytheme("united"),
  tags$head(tags$script(src = "message-handler.js")),
  tags$head(tags$style(".shiny-notification {position: fixed; top: 25% ;left: 50%")),

  titlePanel("Acute COPD Exacerbation Prediction Tool (ACCEPT)"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("male", labelMandatory("Gender"),list('','female', 'male'), selected = "female"),
      numericInput("age", labelMandatory("Age (year)"), value = 70, min = 20, max = 100, step = 1),
      selectInput("smoker", labelMandatory("Is the patient currently a smoker?"),list('','yes', 'no'), selected = 'yes') %>% 
        helper(icon = "question-circle",
               colour = "black",
               type = "markdown",
               content = "smoker"),
      numericInput('FEV1', labelMandatory('Post-bronchodilator FEV1 (% predicted)'), value = 40, min = 0, max = 100, step = 1),
      numericInput('SGRQ', 'St. Georges Respiratory Questionnaire Score (SGRQ)', value = 30, min = 0, max = 100, step = 1) %>% 
            helper(icon = "question-circle",
                   colour = "black",
                   type = "markdown",
                   content = "SGRQ"),
      numericInput('CAT', 'If SGRQ is not available, please enter COPD Assessment Test (CAT) Score', value =NA , min = 0, max = 40, step = 1) %>% 
        helper(icon = "question-circle",
               colour = "black",
               type = "markdown",
               content = "CAT"), 
      numericInput("BMI", labelMandatory("Body mass index (BMI)"), value = 25, min = 5, max = 50, step = 0.1) %>% 
        helper(icon = "question-circle",
               colour = "black",
               type = "markdown",
               content = "BMI"),
      selectInput("oxygen", labelMandatory("Has the patient received oxygen therapy within the last year?"),list('','yes', 'no'), selected = "yes"),
      selectInput("statin", labelMandatory("Is the patient currently on statins?"),list('','yes', 'no'), selected = "no"),
      selectInput("LAMA", labelMandatory("Is the patient currently on LAMAs?"),list('','yes', 'no'), selected = "yes"),
      selectInput("LABA", labelMandatory("Is the patient currently on LABAs?"),list('','yes', 'no'), selected = "yes"),
      selectInput("ICS", labelMandatory("Is the patient currently on inhaled corticosteroids?"),list('','yes', 'no'), selected = "no"),
      numericInput("LastYrExacCount", labelMandatory("Number of All Exacerbations within the last 12 months"), value = 4, min = 0, max = 20,  step = 1) %>% 
        helper(icon = "question-circle",
               colour = "black",
               type = "markdown",
               content = "LastYrExacCount"),
      numericInput("LastYrSevExacCount", labelMandatory("Number of Severe Exacerbations within the last 12 months"), value = 2, min = 0, max = 20,  step = 1) %>% 
        helper(icon = "question-circle",
               colour = "black",
               type = "markdown",
               content = "severeExacerbations"),
      
      
      
      # br(), br(), icon("floppy-o"),"  ",
      # a(id = "toggleSaveLoad", "Save/Load Inputs", href = "#"),
      # shinyjs::hidden(
      #   div(id = "SaveLoad",
      #       downloadButton("save_inputs_button", "Save Inputs"),
      #       fileInput("load_inputs_button","Choose CSV File to Load", accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), buttonLabel = "Load Inputs")
      #   )                 
      # ),
      
      uiOutput('inputParam'),
      
      # br(),
      # br(),

      shinyjs::hidden(
        div(id = "FEV1_range",
            HTML(paste(tags$span(style="color:red", "FEV1 % predicted must be between 0% and 100%")))
        )
      ),
      shinyjs::hidden(
        div(id = "SGRQ_range",
            HTML(paste(tags$span(style="color:red", "St. George's Respiratory Questionnaire Score (SGRQ) must be between 0 and 100")))
        )
      ),
      
      shinyjs::hidden(
        div(id = "CAT_range",
            HTML(paste(tags$span(style="color:red", "COPD Assessment Test (CAT) score must be between 0 and 40")))
        )
      ),
      
      shinyjs::hidden(
        div(id = "CAT_SGRQ",
            HTML(paste(tags$span(style="color:red", "Either SGRQ or CAT score must be entered.")))
        )
      ),
      
      shinyjs::hidden(
        div(id = "age_range",
            HTML(paste(tags$span(style="color:red", "age must be between 20 and 100")))
        )
      ),
      shinyjs::hidden(
        div(id = "BMI_range",
            HTML(paste(tags$span(style="color:red", "BMI out of range")))
        )
      ),
      
      shinyjs::hidden(
        div(id = "exac_range",
            HTML(paste(tags$span(style="color:red", "Number of severe exacerbation cannot be larger than all exacerbations")))
        )
      ),
      
      shinyjs::hidden(
        div(id = "exac_minimum",
            HTML(paste(tags$span(style="color:red", "The model is only valid for patients with at least one exacerbation within the past 12 months")))
        )
      ),
      

      checkboxInput("termsCheck",HTML(paste("I agree to ", tags$span(style="color:tomato", tags$a(href="./disclaimer.html", target="_blank", "terms")), sep = "")), FALSE),
      actionButton("submit", "Run the prediction model"),
      actionButton("reset_button", "Start over")
    ),
    
    
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Exacerbation Risk",
                           br(),
                           div(id = "background", includeMarkdown("./background.rmd")),
                           shinyjs::hidden(radioButtons("error_risk", inline = T,  "Uncertainty:", choices = list ("Hide" = 0, "95% Prediction Interval - For Individual Patient" = 1, 
                                                                                                                                      "95% Confidence Interval - For Population Mean" = 2), selected = 0)),
                           splitLayout(cellWidths = c("50%", "50%"), plotOutput("exac_risk"), plotOutput("severe_exac_risk")),
                           br(),
                           htmlOutput("text_risk"),
                           br()
                           #tableOutput("table_exac_risk")
                           
                  ),
                  
                  tabPanel("Exacerbation Rate",
                           br(),
                           shinyjs::hidden(radioButtons("error_rate", inline = T, "Uncertainty:", choices = list ("Hide" = 0, "95% Prediction Interval - For Individual Patient" = 1, 
                             "95% Confidence Interval - For Population Mean" = 2), selected = 0)),
                           splitLayout(cellWidths = c("50%", "50%"), plotOutput("exac_rate"), plotOutput("severe_exac_rate")),
                           br(),
                           htmlOutput("text_rate"),
                           br()
                           #tableOutput("table_exac_rate")
                           
                  ),
                  tabPanel("Likely Scenarios",
                           br(),
                           htmlOutput("text_heatmap"),
                           br(),
                           plotlyOutput("heatMap")),
                  
                  tabPanel("Probability Distribution",  
                           br(),
                           htmlOutput("text_surface"),
                           br(),
                           plotlyOutput("surfacePlot")),

                  
                  tabPanel("Terms",  includeMarkdown("./disclaimer.rmd")),
                  tabPanel("About",  includeMarkdown("./about.rmd")#, 
                           #imageOutput("logos")
                           )
      )
    )
  )
)

server <- function(input, output, session) {
  

  observe_helpers(help_dir = "helpfiles")
  
  # Output Function Constants-------------------------------------------------------------------------------------------------
  coverageInterval <- "95% coverage interval"
  xlab="Time (years)"
  ylab="FEV1 (% Predicted)"
  errorLineColor <- "darkcyan"
  errorLineColorSmoker <- "salmon"
  errorLineColorNonSmoker <- "darkcyan"
  lineColorSmoker <- "red"
  lineColorNonSmoker <- "dodgerblue4"
  
  buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
  
  
  # Shinyjs-----------------------------------------------------------------------------------------------------------
  
  
  #shinyjs::onclick("toggleSaveLoad",
   #                shinyjs::toggle(id = "SaveLoad", anim = TRUE)) 
  
  # observe({
  # 
  #   if (!input$termsCheck || is.na(input$FEV1) || (input$FEV1 == "") || is.na(input$SGRQ) || (input$SGRQ == "") || is.na (input$age) || (input$age == "") || is.null (input$gender) || (input$gender == "")) {
  #     shinyjs::disable("submit")
  #   }else {
  #     shinyjs::enable("submit")
  #   }
  #     
  # })  

  observe({
    if (!is.na(input$FEV1) && (input$FEV1!="")) {
      if ((input$FEV1 < 0)  || (input$FEV1 > 100))  {
        shinyjs::show (id = "FEV1_range", anim = TRUE)}
      else shinyjs::hide (id = "FEV1_range", anim = TRUE)
    }
  })    
  
  
  observe({
    if ((!is.na(input$LastYrSevExacCount) && (input$LastYrSevExacCount!="")  && (!is.na(input$LastYrExacCount)) && (!is.na(input$LastYrExacCount)))) {
      if ((input$LastYrSevExacCount) > (input$LastYrExacCount))  {
        shinyjs::show (id = "exac_range", anim = TRUE)}
      else shinyjs::hide (id = "exac_range", anim = TRUE)
    }
  })    
  
  observe({
    if ((!is.na(input$LastYrSevExacCount) && (input$LastYrSevExacCount!="")  && (!is.na(input$LastYrExacCount)) && (!is.na(input$LastYrExacCount)))) {
      if (input$LastYrExacCount < 1)  {
        shinyjs::show (id = "exac_minimum", anim = TRUE)}
      else shinyjs::hide (id = "exac_minimum", anim = TRUE)
    }
  })
  
  observe({
      if (!is.na(input$SGRQ) && (input$SGRQ!="")) {
        if ((input$SGRQ< 0)  || (input$SGRQ > 100))  {
          shinyjs::show (id = "SGRQ_range", anim = TRUE)}
        else {
          shinyjs::hide (id = "SGRQ_range", anim = TRUE)
        }
      }
    })
  
  observe({
    if (!is.na(input$CAT) && (input$CAT!="")) {
      if ((input$CAT< 0)  || (input$CAT > 40))  {
        shinyjs::show (id = "CAT_range", anim = TRUE)}
      else {
        shinyjs::hide (id = "CAT_range", anim = TRUE)
      }
    }
  })
    
  observe({
    sympChecker <- as.numeric (is.na(input$SGRQ) + is.na(input$CAT))
      if (sympChecker  == 0 || sympChecker == 2)  {
        shinyjs::show (id = "CAT_SGRQ", anim = TRUE)}
      else {
        shinyjs::hide (id = "CAT_SGRQ", anim = TRUE)
      }
    
    
  })
  
  observe({
    if (!is.na(input$age) && (input$age!="")) {
      if ((input$age < 40)  || (input$age > 100))  {
        shinyjs::show (id = "age_range", anim = TRUE)}
      else shinyjs::hide (id = "age_range", anim = TRUE)
    }
  })  
  
 
  observe({
    if (!is.na(input$BMI) && (input$BMI!="")) {
      if ((input$BMI < 5)  || (input$BMI > 50))  {
        shinyjs::show (id = "BMI_range", anim = TRUE)}
      else shinyjs::hide (id = "BMI_range", anim = TRUE)
    }
  })  
  
  observe({
    sympChecker <- as.numeric (is.na(input$SGRQ) + is.na(input$CAT))
    if (!input$termsCheck || is.na(input$FEV1) || (input$FEV1 == "")  || (sympChecker == 2) || (sympChecker == 0) || 
        is.na (input$age) || (input$age == "") || is.null (input$male) || (input$male == "") || 
        is.na (input$BMI) || input$BMI == "" || is.na(input$LastYrSevExacCount) || input$LastYrSevExacCount == "" ||
        is.na (input$statin) || input$statin == "" || is.na(input$LastYrExacCount) || input$LastYrExacCount == "" ||
        is.na (input$LAMA) || input$LAMA == "" || is.na(input$LABA) || input$LABA == "" ||
        is.na (input$ICS) || input$ICS == "" || is.na(input$oxygen) || input$oxygen == "" ||is.na(input$smoker) || input$smoker == "" ||
        ((input$LastYrSevExacCount) > (input$LastYrExacCount)) || ((input$BMI < 5)  || (input$BMI > 50))  || 
        ((input$age < 40)  || (input$age > 100)) || (input$LastYrExacCount < 1) 
        
    )
        {
      shinyjs::disable("submit")
    }else {
      shinyjs::enable("submit")
    }
  })
  
  # Output Functions-----------------------------------------------------------------------------------------------------------

  output$logos <- renderImage({
    width  <- session$clientData$output_logos_width
    height <- session$clientData$output_logos_height
    # Return a list containing the filename
    list(src = "./logos2.png",
         contentType = 'image/png',
         width = width,
         alt = "This is alternate text")
  }, deleteFile = FALSE)

  
  observeEvent(input$prev_input_cat2, {
    updateTabsetPanel(session, "category", selected = "panel1")
  })
  
  observeEvent(input$prev_input_cat3, {
    updateTabsetPanel(session, "category", selected = "panel2")
  })  
  
  observeEvent(input$prev_input_cat4, {
    updateTabsetPanel(session, "category", selected = "panel3")
  })
  
  observeEvent(input$next_input_cat1, {
    updateTabsetPanel(session, "category", selected = "panel2")
  })
  
  observeEvent(input$next_input_cat2, {
    updateTabsetPanel(session, "category", selected = "panel3")
  })
  
  observeEvent(input$next_input_cat3, {
    updateTabsetPanel(session, "category", selected = "panel4")
  })
  
  #Browse button - prompts user to select input values file and loads it into GUI
  observeEvent(input$load_inputs_button,{
    inFile <- input$load_inputs_button
    if (is.null(inFile))
      return(NULL)
    
    #load the data frame from the csv file
    loadedInputs <- read.csv(inFile$datapath)
    
  })
  
  #'Clear Inputs' button - set all inputs to NULL
  observeEvent(input$reset_button, {
    shinyjs::js$reset()
  })
  
  
  # #Save Inputs button - prompts user to save inputs to a csv file
  # output$save_inputs_button <- downloadHandler(
  #   filename = function() {
  #     paste("ACCEPT-input-", Sys.Date(), ".csv", sep = "")
  #   },
  #   
  #   content = function(file) {
  #     # browser()
  #     #labels - 1st column in the data frame
  #     #write.csv(FEV_data_frame, file)
  #   }
  # )
  
  
  observeEvent(input$submit, {
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    #disabling inputs
    shinyjs::disable("male")  
    shinyjs::disable("smoker")  
    shinyjs::disable("LastYrExacCount")  
    shinyjs::disable("LastYrSevExacCount")  
    shinyjs::disable("FEV1") 
    shinyjs::disable("SGRQ") 
    shinyjs::disable("age") 
    shinyjs::disable("BMI")
    shinyjs::disable("oxygen")
    shinyjs::disable("statin")
    shinyjs::disable("LAMA")
    shinyjs::disable("LABA")
    shinyjs::disable("ICS")
    shinyjs::disable("submit") 

    progress$set(message = "Collecting the data...", value = 0.10)
    
    patientData <- samplePatients[1,]
    
    patientData$male <- input$male
    patientData$age     <- input$age
    patientData$smoker  <- input$smoker
    patientData$oxygen  <- input$oxygen
    patientData$statin  <- input$statin
    patientData$LAMA   <- input$LAMA
    patientData$LABA   <- input$LABA
    patientData$ICS    <- input$ICS
    patientData$FEV1 <- input$FEV1
    patientData$BMI   <- input$BMI
    if (!is.na(input$SGRQ)) {patientData$SGRQ   <- input$SGRQ}
    if (is.na(input$SGRQ)) {patientData$SGRQ   <- 18.87 + 1.53*input$CAT} #based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4940016/
    patientData$LastYrSevExacCount <- input$LastYrSevExacCount
    patientData$LastYrExacCount <- input$LastYrExacCount
    
    progress$set(message = "processing the data...", value = 0.20)
    
    patientData$randomized_azithromycin  <- 0
    patientData$randomized_statin <- 0
    patientData$randomized_LAMA <- 0
    patientData$randomized_LABA <- 0
    patientData$randomized_ICS <- 0
  
    patientData <- patientData  %>% mutate (male = recode (male, male = 1, female = 0))
    patientData <- patientData  %>% mutate (smoker = recode (smoker, yes = 1, no = 0))
    patientData <- patientData  %>% mutate (oxygen = recode (oxygen, yes = 1, no = 0))
    patientData <- patientData  %>% mutate (statin = recode (statin, yes = 1, no = 0))
    patientData <- patientData  %>% mutate (LAMA  = recode (LAMA , yes = 1, no = 0))
    patientData <- patientData  %>% mutate (LABA   = recode (LABA  , yes = 1, no = 0))
    patientData <- patientData  %>% mutate (ICS    = recode (ICS   , yes = 1, no = 0))
    
    progress$set(message = "running the model...", value = 0.25)
    
    results <- predictACCEPT(patientData = patientData, random_sampling_N = 5000, random_distribution_iteration = 2e4)
    results <- results %>% select(-c(male, age, smoker, oxygen, statin, LAMA, LABA, ICS, FEV1, BMI, SGRQ, LastYrExacCount, 
                                     LastYrSevExacCount, randomized_azithromycin,	randomized_statin,	randomized_LAMA,	
                                     randomized_LABA,	randomized_ICS))
    
    
   azithroResults <- results %>% select (ID, contains("azithro")) %>% mutate (Treatment = "With Azithromycin") %>%
                                 rename_all(list(~str_replace(., "azithromycin_", "")))
   noAzithroResults <- results %>% select (-contains("azithro")) %>% mutate (Treatment = "No Azithromycin")
   
   progress$set(message = "plotting...", value = 0.90)    
   shinyjs::hide("background")
   
   plotData <- rbind(azithroResults, noAzithroResults)
   
    probabilities <- plotData %>% select (Treatment, contains("probability"))
    rates <- plotData %>% select (Treatment, contains("rate"))
    

    
    output$exac_risk <- renderPlot({
      tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  

      plotProb <- ggplot(probabilities , aes (x = Treatment)) + 
                   geom_col(aes(y=100*predicted_exac_probability, fill=Treatment), show.legend = T, width = 0.7) + 
                   geom_text(
                    aes(label = paste0(round (100*predicted_exac_probability, 1), "%"), y = 100*predicted_exac_probability),
                    nudge_x = -0.25, nudge_y = 2)  +                  
                   theme_tufte(base_family = tuftefont, base_size = 14) + labs (title="Risk of Exacerbation", x="", y="Probability (%)" ) + ylim(c(0, 100)) +
                  theme(axis.title.x=element_blank(),
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank()) 
      if (input$error_risk==1) {
        plotProb <- plotProb + geom_errorbar(aes(ymin = 100*predicted_exac_probability_lower_PI, ymax = 100*predicted_exac_probability_upper_PI), width = 0.1)
      }
      
      if (input$error_risk==2) {
        plotProb <- plotProb + geom_errorbar(aes(ymin = 100*predicted_exac_probability_lower_CI, ymax = 100*predicted_exac_probability_upper_CI), width = 0.1)
      }
      plotProb
    })
    
    output$severe_exac_risk <- renderPlot({
      tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
      
      plotProb <- ggplot(probabilities , aes (x = Treatment)) + 
        geom_col(aes(y=100*predicted_severe_exac_probability, fill=Treatment), width = 0.7) + 
        geom_text(
          aes(label = paste0(round (100*predicted_severe_exac_probability, 1), "%"), y = 100*predicted_severe_exac_probability),
          nudge_x = -0.25, nudge_y = 2)  +
        theme_tufte(base_family = tuftefont, base_size = 14 ) + labs (title="Risk of Severe Exacerbation", x="", y="Probability (%)" ) + ylim(c(0, 100)) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) 
      
      if (input$error_risk==1) {
        plotProb <- plotProb + geom_errorbar(aes(ymin = 100*predicted_severe_exac_probability_lower_PI, ymax = 100*predicted_severe_exac_probability_upper_PI), width = 0.1) 
      }
      
      if (input$error_risk==2) {
        plotProb <- plotProb + geom_errorbar(aes(ymin = 100*predicted_severe_exac_probability_lower_CI, ymax = 100*predicted_severe_exac_probability_upper_CI), width = 0.1) 
      }
      
      plotProb
    })   
    
    output$exac_rate <- renderPlot({
      tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
      upperInterval <- max (rates$predicted_exac_rate_upper_PI)
      plotProb <- ggplot(rates, aes (x = Treatment)) + 
        geom_col(aes(y=predicted_exac_rate, fill=Treatment), show.legend = T,  width = 0.7) + ylim(0, upperInterval) +
        geom_text(
          aes(label = round (predicted_exac_rate, 1), y = predicted_exac_rate),
          nudge_x = -0.3, nudge_y = 2*upperInterval/100)  + 
        theme_tufte(base_family = tuftefont, base_size = 14) + labs (title="Predicted Exacerbation Rate", x="", y="Exacerbations per year" ) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) 
      
      if (input$error_rate==1) {
        plotProb <- plotProb + geom_errorbar(aes(ymin = predicted_exac_rate_lower_PI, ymax = predicted_exac_rate_upper_PI), width = 0.1) 
      }
      
      if (input$error_rate==2) {
        plotProb <- plotProb + geom_errorbar(aes(ymin = predicted_exac_rate_lower_CI, ymax = predicted_exac_rate_upper_CI), width = 0.1) 
      }
      plotProb
    })
    
      
    output$severe_exac_rate <- renderPlot({
      tuftefont <- choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif"), quiet = FALSE)  
      upperInterval <- max (rates$predicted_severe_exac_rate_upper_PI)
      plotProb <- ggplot(rates, aes (x = Treatment)) + 
        geom_col(aes(y=predicted_severe_exac_rate, fill=Treatment),  width = 0.7) + ylim(0, upperInterval) +
        geom_text(
          aes(label = round (predicted_severe_exac_rate, 1), y = predicted_severe_exac_rate),
          nudge_x = -0.3, nudge_y = 2*upperInterval/100)  +
        theme_tufte(base_size = 14, base_family = tuftefont) + labs (title="Predicted Severe Exacerbation Rate", x="", y="Severe Exacerbations per year" ) +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) 
      
      
      if (input$error_rate==1) {
        plotProb <- plotProb +  geom_errorbar(aes(ymin = predicted_severe_exac_rate_lower_PI, ymax = predicted_severe_exac_rate_upper_PI), width = 0.1) 
      }
      
      if (input$error_rate==2) {
        plotProb <- plotProb +  geom_errorbar(aes(ymin = predicted_severe_exac_rate_lower_CI, ymax = predicted_severe_exac_rate_upper_CI), width = 0.1) 
      }
      plotProb
    })
    
    shinyjs::show("error_risk")
    shinyjs::show("error_rate")
    
    output$table_exac_risk <- renderTable({
      return (probabilities %>% mutate("1 Yr Exacerbation Risk (%)" = predicted_exac_probability*100, "1 Yr Severe Exacerbation Risk (%)" = predicted_severe_exac_probability*100) %>% select (Treatment, contains("Exacerbation")))
    },
    digits = 1,
    include.rownames=T,
    caption="1 Year Exacerbation Risk Prediction",
    caption.placement = getOption("xtable.caption.placement", "top"))
    
    output$table_exac_rate <- renderTable({
      return (rates %>% mutate("Exacerbations Rate (per year)" = predicted_exac_rate, "Severe Exacerbation Rate (per year)" = predicted_severe_exac_rate) %>% select (Treatment, contains("Exacerbation")))
    },
    digits = 1,
    include.rownames=T,
    caption="Predicted Average Number of Exacerbations per Year",
    caption.placement = getOption("xtable.caption.placement", "top"))

    output$text_risk <- renderUI({
      azithro_risk_diff <- round((noAzithroResults$predicted_exac_probability - azithroResults$predicted_exac_probability)*100, 1)
      azithro_severe_risk_diff <- round((noAzithroResults$predicted_severe_exac_probability - azithroResults$predicted_severe_exac_probability)*100, 1)
      text <- paste0("Based on the MACRO trial, Azithromycin (250mg/day) will reduce the absolute exacerbation risk by ", azithro_risk_diff, "% for all exacerbations, and ", 
                     azithro_severe_risk_diff , "% for severe exacerbations.")
      sevRisk <- noAzithroResults$predicted_severe_exac_probability*100
      
      # roflumilastBenefitProb is calculated based on digitization of the plot in Yu T, Fain K, Boyd CM, et al. Benefits and harms of roflumilast in moderate to severe COPD. Thorax 2014; 69: 616–22.
      if (sevRisk <= 15) {roflumilastBenefitProb <- 0}
      else if (sevRisk <= 35) {
        roflumilastBenefitProb <- round(454.4914758494446 -92.11798616195082*sevRisk + 
                                      6.363577462586879*(sevRisk^2) - 0.1734143958325213*(sevRisk^3) + 
                                      0.001668176*(sevRisk^4),0)
        }
      else {roflumilastBenefitProb = 95}
      
      if (roflumilastBenefitProb == 0) {text_roflumilast <- paste0("Based on the harm-benefit analysis by Yu et al., the probability that roflumilast (500 µg/day) will provide a net benefit to this patient is almost zero.")}
      else {
        # reducing accuracy to account for digitization as well as small variations between genders
        roflumilastBenefitProbFloor <- floor(roflumilastBenefitProb/10)*10
        roflumilastBenefitProbCeiling <- ceiling(roflumilastBenefitProb/10)*10
        text_roflumilast <- paste0("Based on the harm-benefit analysis by Yu et al., the probability that roflumilast (500 µg/day) will provide a net benefit to this patient is between ",
                                   roflumilastBenefitProbFloor, "% to ", roflumilastBenefitProbCeiling, "%.")
      }

      treatmentTitle <- HTML(paste(tags$span(style="color:tomato", "Treatment Effect:")))
      HTML(paste(tags$strong(treatmentTitle), tags$strong(text), 
                 tags$a(href="https://www.nejm.org/doi/full/10.1056/NEJMoa1104623", target="_blank", "Reference: Albert RK, Connett J, Bailey WC, et al. Azithromycin for prevention of exacerbations of COPD. N Engl J Med 2011; 365: 689–98."), 
                 tags$a(href="https://academic.oup.com/aje/article/184/9/681/2332840", target="_blank", "Reference: Sadatsafavi M, Sin DD, Zafari Z, et al. The Association Between Rate and Severity of Exacerbations in Chronic Obstructive Pulmonary Disease: An Application of a Joint Frailty-Logistic Model. Am J Epidemiol 2016; 184: 681–9."), 
                 tags$strong(text_roflumilast),
                 tags$a(href="https://thorax.bmj.com/content/69/7/616", target="_blank", "Reference: Yu T, Fain K, Boyd CM, et al. Benefits and harms of roflumilast in moderate to severe COPD. Thorax 2014; 69: 616–22."), 
                 sep = '<br/>'))
      
      })
        
    output$text_rate <- renderUI({
      #azithro_rate_diff <- rates["No Azithromycin", "predicted_exac_rate"] - rates["With Azithromycin", "predicted_exac_rate"] 
      azithro_rate_diff <- round(100 * (noAzithroResults$predicted_exac_rate - azithroResults$predicted_exac_rate), 0)
      azithro_severe_rate_diff <- round(100 * (noAzithroResults$predicted_severe_exac_rate - azithroResults$predicted_severe_exac_rate), 0)
      text <- paste0("Based on the MACRO trial, for every 100 people treated with Azithromycin (250mg/day) an average of ", azithro_rate_diff, " exacerbations, and ", 
                      azithro_severe_rate_diff , " severe exacerbations will be prevented every year.")
      treatmentTitle <- HTML(paste(tags$span(style="color:tomato", "Treatment Effect:")))
      HTML(paste(tags$strong(treatmentTitle), tags$strong(text), 
                 tags$a(href="https://www.nejm.org/doi/full/10.1056/NEJMoa1104623", target="_blank", "Reference: Albert et al., Azithromycin for prevention of exacerbations of COPD, New England Journal of Medicine 365.8 (2011): 689-698"),
                 tags$a(href="https://academic.oup.com/aje/article/184/9/681/2332840", target="_blank", "Reference: Sadatsafavi M, Sin DD, Zafari Z, et al. The Association Between Rate and Severity of Exacerbations in Chronic Obstructive Pulmonary Disease: An Application of a Joint Frailty-Logistic Model. Am J Epidemiol 2016; 184: 681–9."),
                 sep = '<br/>'))
      
    })
    
    output$surfacePlot <- renderPlotly({
      
      Probability <- predictCountProb(noAzithroResults, n=10, shortened = F) * 100

      plot_ly(z = ~Probability, width = 800, height = 800)  %>% add_surface()  %>%
        layout(
          title = "Probability Distribution",
          scene = list(
            xaxis = list(title = "No. of Severe Exacerbations"),
            yaxis = list(title = "No. of All Exacerbations"),
            zaxis = list(title = "Probability (%)",  nticks = 10),
            autosize = T
          )) %>% config(displaylogo=F)
    })
    
    output$heatMap <- renderPlotly({
      
      probs <- predictCountProb(noAzithroResults, n=10) * 100
      probs <- round(probs, 1)
      heatPlotly <- t(probs)

      buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
      
      plot_ly(x = c("none", "one", "two", "3 or more"),
              y = c("none", "one", "two", "3 or more"),
              z = heatPlotly, type = "heatmap", hoverinfo = 'text', colors = colorRamp(c("steelblue4", "tomato")))  %>%
        layout(
          title = "Likely Scenarios",
          yaxis = list(title = "Number of Severe Exacerbations"),
          xaxis = list(title = "Number of All Exacerbations")
        ) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, scrollZoom=F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) 
      
    })
    
    
    # output$azithroHeatMap <- renderPlotly({
    #   
    #   probs <- predictCountProb(azithroResults, n=10) * 100
    #   probs <- round(probs, 1)
    #   heatPlotly <- t(probs)
    #   
    #   buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
    #   
    #   plot_ly(x = c("none", "one", "two", "3 or more"),
    #           y = c("none", "one", "two", "3 or more"),
    #           z = heatPlotly, type = "heatmap", hoverinfo = 'text', colors = colorRamp(c("steelblue4", "tomato")))  %>%
    #     layout(
    #       title = "Likely Scenarios - with Azithromycin",
    #       yaxis = list(title = "Number of Severe Exacerbations"),
    #       xaxis = list(title = "Number of All Exacerbations")
    #     ) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, scrollZoom=F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) 
    #   
    # })
    
    output$text_heatmap <- renderUI({
      text <- paste0("The heatmap shows the probablity of all possible numbers of exacerbation and severe exacerbations with 
                     the next year.")
      
      plotTitle <- HTML(paste(tags$span("interpretation Guide:")))
      HTML(paste(tags$strong(plotTitle), (text),  sep = '<br/>'))
    })
    
    output$text_surface <- renderUI({
      text <- paste0("The 3D plot shows the probablity of all possible numbers of exacerbation and severe exacerbations with 
                     the next year.")
      
      plotTitle <- HTML(paste(tags$span("Interpreation Guide:")))
      HTML(paste(tags$strong(plotTitle), (text),  sep = '<br/>'))
    })
    progress$set(message = "Done!", value = 1)
    
  }) 
  
  
} #end of server <- function


#Run the application
shinyApp(ui = ui, server = server)
