library(shiny)
library(dplyr)
source("MakeStim_func.R") # script contains functions to create timing files

###########################################################################
# LOAD DATA ---------------------------------------------------------------
###########################################################################

scan1 <- read.csv("/Shared/lstrathearn/functional/Mother_Infant/sourcedata/allEDAT_scan1.csv", skip = 1)
scan2 <- read.csv("/Shared/lstrathearn/functional/Mother_Infant/sourcedata/allEDAT_scan2.csv", skip = 1)
cry_duration_excel <- read.csv("/Shared/lstrathearn/functional/Mother_Infant/sourcedata/Cry_Duration.csv")
ids <- read.csv("/Shared/lstrathearn/functional/Mother_Infant/code/timing_files/MotherInfantStudy_mastersheetinfo.csv")


###########################################################################
# GET SUBJECT AND SESSION INFO --------------------------------------------
###########################################################################

scan1$Subject <- scan1$Subject + 100000
scan2$Subject <- scan2$Subject + 100000

subjects <- ids$subjid
subjects_1 <- subjects[c(TRUE, FALSE)]
subjects_2 <- subjects[c(FALSE, TRUE)]
sessions <- ids$mrqid
sessions_1 <- sessions[c(TRUE, FALSE)]
sessions_2 <- sessions[c(FALSE, TRUE)]
runs <- c("run-1", "run-2", "run-3", "run-4")
runnumb <- c(1, 2, 3, 4)

# remove subjects who did not have a session id
subjects_1_subset <- subjects_1[!is.na(sessions_1)]
sessions_1_subset <- sessions_1[!is.na(sessions_1)]

subjects_2_subset <- subjects_2[!is.na(sessions_2)]
sessions_2_subset <- sessions_2[!is.na(sessions_2)]

cryo <- c("cryo1", "cryo2", "cryo3", "cryo4", "cryo5", "cryo6", "cryo7", "cryo8", "cryo9", "cryo10")
cryu <- c("cryu1", "cryu2", "cryu3", "cryu4", "cryu5", "cryu6", "cryu7", "cryu8", "cryu9", "cryu10")
cry_stims <- c(cryo, cryu)
ho <- c("ho1", "ho2", "ho3", "ho4", "ho5", "ho6", "ho7", "ho8", "ho9", "ho10")
hu <- c("hu1", "hu2", "hu3", "hu4", "hu5", "hu6", "hu7", "hu8", "hu9", "hu10")
so <- c("so1", "so2", "so3", "so4", "so5", "so6", "so7", "so8", "so9", "so10")
su <- c("su1", "su2", "su3", "su4", "su5", "su6", "su7", "su8", "su9", "su10")
bell <- c("bell", "bellbell")

###########################################################################
# CREATE A SHINY APP  -----------------------------------------------------
###########################################################################

# The app creates task fMRI timing files for the subject_session specified by user

ui <- fluidPage(
  titlePanel("makeStim"),
  
  selectInput("session", 
              label = "Choose session 1 or session 2.",
              choices = c("scan1", 
                          "scan2")),
  conditionalPanel(
    condition = "input.session == 'scan1'",
    selectInput("subject", 
                label = "Choose a subject.",
                choices = c(subjects_1_subset))),
  conditionalPanel(
    condition = "input.session == 'scan2'",
    selectInput("subject", 
                label = "Choose a subject.",
                choices = c(subjects_2_subset))),
  actionButton("goButton", "Generate Timing Files", icon("refresh", verify_fa = FALSE)),
  helpText("After clicking the button above, check the BIDS subject folder to make sure that the timing",
           "files are generated"),
  
  mainPanel(
    textOutput("selected_session"),
    textOutput("selected_subject")
  )
)

server <- function(input, output) {
  observeEvent(input$goButton, ignoreInit=TRUE, {
    output$selected_session <- renderText({ 
      paste("You have selected session: ", input$session)
    })
    output$selected_subject <- renderText({ 
      paste("You have selected subject: ", input$subject)
    })
    if (input$session == "scan1") {
      makeStim(emerge = scan1, subid = input$subject, 
               sessid = sessions_1_subset[which(subjects_1_subset == input$subject)])
    } else {
      makeStim(emerge = scan2, subid = input$subject, 
               sessid = sessions_2_subset[which(subjects_1_subset == input$subject)])
    }
  })
}

shinyApp(ui, server)
