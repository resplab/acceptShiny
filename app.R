library(shiny)
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


source('./FEV_functions.R')



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
      selectInput("smoker", labelMandatory("Is the patient currently a smoker?"),list('','yes', 'no'), selected = 'yes'),
      numericInput('FEV1', labelMandatory('FEV1 (L)'), value = 3.2, min = 1, max = 5, step = 0.25),
      numericInput('SGRQ', labelMandatory('St. Geroges Respiratory Questionnaire Score (SGRQ)'), value = 30, min = 1, max = 100, step = 1),
      numericInput("BMI", labelMandatory("Body mass index (BMI)"), value = 25, min = 5, max = 50, step = 0.1),
      selectInput("oxygen", labelMandatory("Has the patient received oxygen therapy within the last year?"),list('','yes', 'no'), selected = "yes"),
      selectInput("statin", labelMandatory("Is the patient on statins?"),list('','yes', 'no'), selected = "no"),
      selectInput("LAMA", labelMandatory("Is the patient on LAMAs?"),list('','yes', 'no'), selected = "yes"),
      selectInput("LABA", labelMandatory("Is the patient on LABAs?"),list('','yes', 'no'), selected = "yes"),
      selectInput("ICS", labelMandatory("Is the patient on inhaled coricosteroids?"),list('','yes', 'no'), selected = "no"),
      numericInput("LastYrExacCount", labelMandatory("Number of All Exacerbations within the last 12 months"), value = 5, min = 0, max = 20,  step = 1),
      numericInput("LastYrSevExacCount", labelMandatory("Number of Severe Exacerbations within the last 12 months"), value = 3, min = 0, max = 20,  step = 1),
      
      
      
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
            HTML(paste(tags$span(style="color:red", "FEV1 must be between 1L and 5L")))
        )
      ),
      shinyjs::hidden(
        div(id = "SGRQ_range",
            HTML(paste(tags$span(style="color:red", "St. George's Respiratory Questionnaire Score (SGRQ) must be between 0 and 100")))
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
      

      checkboxInput("termsCheck",HTML(paste("I agree to ", tags$span(style="color:tomato", tags$a(href="./disclaimer.html", target="_blank", "terms")), sep = "")), FALSE),
      actionButton("submit", "Run the prediction model"),
      actionButton("reset_button", "Start over")
    ),
    
    
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Exacerbation Risk",
                           div(id = "background", includeMarkdown("./background.rmd")),
                           shinyjs::hidden(radioButtons("error_risk", inline = T,  "Uncertainty:", choices = list ( "95% Prediction Interval - For Individual Patient" = 1, 
                                                                                                                                      "95% Confidence Interval - For Population Mean" = 2), selected = 1)),
                           splitLayout(cellWidths = c("50%", "50%"), plotOutput("exac_risk"), plotOutput("severe_exac_risk")),
                           br(),
                           htmlOutput("text_risk"),
                           br(),
                           tableOutput("table_exac_risk")
                           
                  ),
                  
                  tabPanel("Exacerbation Rate",
                           shinyjs::hidden(radioButtons("error_rate", inline = T, "Uncertainty:", choices = list ( "95% Prediction Interval - For Individual Patient" = 1, 
                             "95% Confidence Interval - For Population Mean" = 2), selected = 1)),
                           splitLayout(cellWidths = c("50%", "50%"), plotOutput("exac_rate"), plotOutput("severe_exac_rate")),
                           br(),
                           htmlOutput("text_rate"),
                           br(),
                           tableOutput("table_exac_rate")
                           
                  ),
                  tabPanel("3D Plot",  
                           br(),
                           br(),
                           plotlyOutput("surfacePlot")),
                  
                  tabPanel("Terms",  includeMarkdown("./disclaimer.rmd")),
                  tabPanel("About",  includeMarkdown("./about.rmd"), 
                           imageOutput("logos"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  # Output Function Constants-------------------------------------------------------------------------------------------------
  
  coverageInterval <- "95% coverage interval"
  xlab="Time (years)"
  ylab="FEV1 (L)"
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
      if ((input$FEV1 < 1)  || (input$FEV1 > 5))  {
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
      if (!is.na(input$SGRQ) && (input$SGRQ!="")) {
        if ((input$SGRQ< 0)  || (input$SGRQ > 100))  {
          shinyjs::show (id = "SGRQ_range", anim = TRUE)}
        else {
          shinyjs::hide (id = "SGRQ_range", anim = TRUE)
        }
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
    if (!input$termsCheck || is.na(input$FEV1) || (input$FEV1 == "") || is.na(input$SGRQ) || (input$SGRQ == "") || 
        is.na (input$age) || (input$age == "") || is.null (input$male) || (input$male == "") || 
        is.na (input$BMI) || input$BMI == "" || is.na(input$LastYrSevExacCount) || input$LastYrSevExacCount == "" ||
        is.na (input$statin) || input$statin == "" || is.na(input$LastYrExacCount) || input$LastYrExacCount == "" ||
        is.na (input$LAMA) || input$LAMA == "" || is.na(input$LABA) || input$LABA == "" ||
        is.na (input$ICS) || input$ICS == "" || is.na(input$oxygen) || input$oxygen == "" ||is.na(input$smoker) || input$smoker == "" ||
        ((input$LastYrSevExacCount) > (input$LastYrExacCount)) || ((input$BMI < 5)  || (input$BMI > 50))  || 
        ((input$age < 40)  || (input$age > 100)) || ((input$SGRQ< 0)  || (input$SGRQ > 100))
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
    patientData$SGRQ   <- input$SGRQ
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
    
    results <- predictACCEPT(patientData = patientData)
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
                   theme_tufte(base_family = tuftefont, base_size = 14) + labs (title="1 yr Probablity of Exacerbation", x="", y="Probability (%)" ) + ylim(c(0, 100)) 
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
        theme_tufte(base_family = tuftefont, base_size = 14 ) + labs (title="1 yr Probablity of Severe Exacerbation", x="", y="Probability (%)" ) + ylim(c(0, 100)) 
      
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
        theme_tufte(base_family = tuftefont, base_size = 14) + labs (title="Predicted Exacerbation Rate", x="", y="Exacerbations per year" ) 
      
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
        theme_tufte(base_size = 14, base_family = tuftefont) + labs (title="Predicted Severe Exacerbation Rate", x="", y="Severe Exacerbations per year" ) 
      
      
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
      return (probabilities %>% mutate("Predicted Exacerbation Risk" = predicted_exac_probability, "Predicted Severe Exacerbation Risk" = predicted_severe_exac_probability) %>% select (Treatment, contains("Exacerbation")))
    },
    include.rownames=T,
    caption="Exacerbation Prediction",
    caption.placement = getOption("xtable.caption.placement", "top"))
    
    output$table_exac_rate <- renderTable({
      return (rates %>% mutate("Predicted Exacerbation Rate" = predicted_exac_rate, "Predicted Severe Exacerbation Risk" = predicted_severe_exac_rate) %>% select (Treatment, contains("Exacerbation")))
    },
    include.rownames=T,
    caption="Exacerbation Prediction",
    caption.placement = getOption("xtable.caption.placement", "top"))

    output$text_risk <- renderUI({
      azithro_risk_diff <- round((noAzithroResults$predicted_exac_probability - azithroResults$predicted_exac_probability)*100, 2)
      azithro_severe_risk_diff <- round((noAzithroResults$predicted_severe_exac_probability - azithroResults$predicted_severe_exac_probability)*100, 2)
      text <- paste0("Azithromycin (250mg, daily) will reduce the absolute exacerbation risk by ", azithro_risk_diff, "% for all exacerbations, and ", 
                     azithro_severe_risk_diff , "% for severe exacerbations.")
      treatmentTitle <- HTML(paste(tags$span(style="color:tomato", "Treatment Effect:")))
      HTML(paste(tags$strong(treatmentTitle), tags$strong(text),  sep = '<br/>'))
      })
        
    output$text_rate <- renderUI({
      #azithro_rate_diff <- rates["No Azithromycin", "predicted_exac_rate"] - rates["With Azithromycin", "predicted_exac_rate"] 
      azithro_rate_diff <- round(noAzithroResults$predicted_exac_rate - azithroResults$predicted_exac_rate, 2)
      azithro_severe_rate_diff <- round(noAzithroResults$predicted_severe_exac_rate - azithroResults$predicted_severe_exac_rate, 2)
      text <- paste0( "Azithromycin (250mg, daily) will prevent an average of ", azithro_rate_diff, " exacerbations, and ", 
                      azithro_severe_rate_diff , " severe exacerbations per year.")
      treatmentTitle <- HTML(paste(tags$span(style="color:tomato", "Treatment Effect:")))
      HTML(paste(tags$strong(treatmentTitle), tags$strong(text),  sep = '<br/>'))
      
    })
    
    output$surfacePlot <- renderPlotly({
      
      probs <- predictCountProb(noAzithroResults, n=10) * 100
      

      plot_ly(z = ~probs, width = 800, height = 800)  %>% add_surface()  %>%
        layout(
          title = "Predicted Probability of Experiencing Certain Number of Exacerbations",
          scene = list(
            xaxis = list(title = "No. Severe Exacerbations"),
            yaxis = list(title = "No. All Exacerbations"),
            zaxis = list(title = "Probability (%)",  nticks = 10),
            autosize = T
          ))
    })
    progress$set(message = "Done!", value = 1)
    
  }) 
  
  
} #end of server <- function


#Run the application
shinyApp(ui = ui, server = server)
