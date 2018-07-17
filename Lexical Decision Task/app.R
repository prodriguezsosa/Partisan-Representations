# --------------------------------
#
#
# SECTION A: PRELIMS    ----
#
#
# --------------------------------

# --------------------------
# SECTION A1: load libraries ----
# --------------------------
library(shiny)
library(rdrop2)
library(dplyr)
library(shiny.semantic)
library(purrr)
library(magrittr)
library(data.table)

# --------------------------
# SECTION A2: load data and functions----
# --------------------------
topN <- readRDS("data/topN.rds")
screening_data <- readRDS("data/screening_data.rds")
trial_data <- readRDS("data/trial_data.rds")
source("LexicalDecisionTask.R")

# data in easier format
source1 <- "republican"
source2 <- "democrat"
source1_topN <- data.frame(topN[source1][2:nrow(topN),], stringsAsFactors = FALSE)
source2_topN <- data.frame(topN[source2][2:nrow(topN),], stringsAsFactors = FALSE)
colnames(source1_topN) <- colnames(source2_topN) <- tolower(topN[source1][1,])

# set timer precision
options(digits.secs = 6)

# --------------------------
# SECTION A3: data saving    -----
# --------------------------
saveDataLocation <- "dropbox"  # either dropbox, email, or local
outputDir <- "GitHub/Partisan-Representations/Lexical Decision Task/Output/Congress"  # directory to save data

# Dropbox
droptoken <- readRDS("droptoken.rds")   # reads in authentication for dropbox (must be store in same folder as this code)

# --------------------------
# SECTION A4: java scripts   -----
# --------------------------

# js to register when keyboard is used
# Shiny.onInputChange only reacts when the js object it references changes
# to solve this we add a random number generator
# see: https://stackoverflow.com/questions/35831811/register-repeated-keyboard-presses-in-shiny
keyboard <- ' $(document).on("keydown", function (e) {
Shiny.onInputChange("lastkeypresscode", [e.which, Math.random()]); 
});
'

# --------------------------
# SECTION A6: ldt data.table      -----
# --------------------------

N <- 4  # number of candidates to sample for each cue
#source1_candidate <- apply(source1_topN, 2, function(x) sample(x, N, replace = TRUE))  # sample from human embeddings (leaner BUT does not avoid matches between both sources)
#source2_candidate <- apply(source2_topN, 2, function(x) sample(x, N, replace = TRUE))  # sample from machine embeddings (leaner BUT does not avoid matches between both sources)

# for each cue sample 10 from each source making sure sources do not match on candidate context word
source1_candidate <- c()
source2_candidate <- c()
for(w in 1:ncol(source1_topN)){
  samp_source1 <- samp_source2 <- "start"
  while(any(samp_source1 == samp_source2)){
    # sample human
    if(N > 1){
    samp_source1 <- rep_len(source1_topN[,w], length.out = N) # by using rep, we avoid too many repeats
    samp_source1 <- samp_source1[sample(seq(1,length(samp_source1),1), replace = FALSE)]
    # sample machine
    samp_source2 <- rep_len(source2_topN[,w], length.out = N)
    samp_source2 <- samp_source2[sample(seq(1,length(samp_source2),1), replace = FALSE)]
    }else{
      samp_source1 <- sample(source1_topN[,w], 1)
      samp_source2 <- sample(source2_topN[,w], 1)
    }
  }
  source1_candidate <- data.frame(cbind(source1_candidate, samp_source1))
  source2_candidate <- data.frame(cbind(source2_candidate, samp_source2)) 
}

source1_candidate <- data.frame(lapply(source1_candidate, as.character), stringsAsFactors=FALSE) # convert to character
source2_candidate <- data.frame(lapply(source2_candidate, as.character), stringsAsFactors=FALSE) # convert to character
colnames(source1_candidate) <- colnames(source1_topN)
colnames(source2_candidate) <- colnames(source1_topN)
# randomize cue order
cues <- sample(colnames(source1_topN), replace = FALSE)
if(length(cues) > 1){
  source1_candidate <- source1_candidate[,cues]
  source2_candidate <- source2_candidate[,cues]
}else{
  source1_candidate <- source1_candidate[cues]
  source2_candidate <- source2_candidate[cues] 
}
# sample 10 comparisons for each cue, randomizing left/right order
ldt_data <- data.table()
for(j in 1:length(cues)){
  for(i in 1:N){
    source_order <- sample(c("R","D"), 2, replace = FALSE)
    if(N>1){
    candidates <- if(source_order[1] == "R"){c(unname(source1_candidate[i,j]), unname(source2_candidate[i,j]))}else{c(unname(source2_candidate[i,j]),unname(source1_candidate[i,j]))}
    }else{candidates <- if(source_order[1] == "R"){unname(unlist(c(source1_candidate[j], source2_candidate[j])))}else{unname(unlist(c(source2_candidate[j],unname(source1_candidate[j]))))}}
    ldt_data <- rbind(ldt_data, data.table("workerid" = as.character(NA), cue = cues[j], "left.source" = source_order[1], "right.source" = source_order[2], "left.word" = candidates[1], "right.word" = candidates[2],
                                           "screener" = FALSE, "left.correct" = as.character(NA), "right.correct" = as.character(NA)))
  }
}
#ldt_data <- ldt_data[1:5,]
# add screening data
for(s in 1:length(screening_data)){
ldt_data <- rbind(ldt_data,
                  data.table("workerid" = as.character(NA), 
                             "cue" = screening_data[[s]][1], 
                             "left.source" = NA,  
                             "right.source" = NA, 
                             "left.word" = screening_data[[s]][2], 
                             "right.word" = screening_data[[s]][3], 
                             "screener" = TRUE,
                             "left.correct" = screening_data[[s]][4],
                             "right.correct" = screening_data[[s]][5]))

}
# shuffle order
ldt_data <- ldt_data[sample(seq(1,nrow(ldt_data),1), nrow(ldt_data), replace = FALSE),]
# create page names
ldt_data$variable <- c(paste0(rep("lexical", nrow(ldt_data)), seq(1,nrow(ldt_data),1)))
# add nextInputID (defines next page)
ldt_data$nextInputID <- c(ldt_data$variable[2:nrow(ldt_data)], "survey")
num_ldt <- nrow(ldt_data)
# --------------------------
# SECTION A6: trial words      -----
# --------------------------
ldt_trial <- data.table()
for(i in 1:length(trial_data)){
  ldt_trial <- rbind(ldt_trial, data.table("cue" = trial_data[[i]][1], "left.word" = trial_data[[i]][2], "right.word" = trial_data[[i]][3], "left.correct" = trial_data[[i]][4], "right.correct" = trial_data[[i]][5]))
}
# shuffle order
ldt_trial <- ldt_trial[sample(seq(1,nrow(ldt_trial),1), nrow(ldt_trial), replace = FALSE),]
# create page names
ldt_trial$variable <- c(paste0(rep("trial", nrow(ldt_trial)), seq(1,nrow(ldt_trial),1)))
# add nextInputID (defines next page)
if(length(trial_data) == 1){ldt_trial$nextInputID <- "instructions3"}else{ldt_trial$nextInputID <- c(ldt_trial$variable[2:nrow(ldt_trial)], "instructions3")}
num_trials <- nrow(ldt_trial)
total_tasks <- num_trials + num_ldt

# --------------------------------
#
#
# SECTION B: USER INTERFACE    ----
#
#
# --------------------------------
ui <- fluidPage(
  theme = "cosmo.css",   # css theme dowloaded from bootswatch.com (https://bootswatch.com/3/)
  tags$script(keyboard),     # load java scripts
  title = "Semantica",       # define title
  uiOutput("MainAction"),    # render function for dynamic ui
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;}")   # prevents gray screen during Sys.sleep()
  #tags$style("input[type=checkbox] {
  #                  transform: scale(2.5);
  #           }") # checkbox size # added it directly to css
)

# --------------------------------
#
#
# SECTION C: SERVER FUNCTION     ----
#
#
# --------------------------------
server <- function(input, output, session) {
  
  # --------------------------------
  #   SECTION C1: Define Reactive Values ----
  #   These store the main values in the game
  # --------------------------------
  
  # CurrentValues stores scalars representing the latest game outcomes and values
  
  CurrentValues <- reactiveValues(page = "welcome",
                                  errors = "none",
                                  trial_index = 1,
                                  index = 1)

  LexicalData <- reactiveValues()
  
  # --------------------------------
  # SECTION C2: Page Layouts         ----
  # --------------------------------
  
  # Send dynamic UI to ui - DON'T CHANGE!
  output$MainAction <- renderUI( {
    PageLayouts()
  })
  
  PageLayouts <- reactive({
    # --------------------------------
    # (1) WELCOME PAGE                ---
    # --------------------------------
    if (CurrentValues$page == "welcome") {   # conditionl determining whether page is displayed
      
      # conditional: if ID not entered, ouput inputLabel text in red to remind user that he must enter an ID
      if (CurrentValues$errors == "Blank_Name") {
        inputLabel <- p(style = "color:Red", "Please enter your MTurk ID!")   
      } else {
        inputLabel <- p("Please enter your MTurk ID")
      }
      
      # page content
      return(
        list(
          br(),
          h1(span(strong("Context Words"), style="color:#2780e3")),   # title
          br(),
            mainPanel(    
          p(span(strong("Purpose:"), style="color:#2780e3"),"evaluate context words."),
          p(span(strong("Confidentiality:"), style="color:#2780e3"), "responses are anonymous, we have no way of linking the data to individual identities."),
          p(span(strong("Length:"), style="color:#2780e3"), "task takes on average less than 5 minutes to complete."),
          p(span(strong("Compensation:"), style="color:#2780e3"), "$0.50"),
          br(),
          p("If you consent to participate in this study, please enter your MTurk ID and press ''Start''.")),
          # main panel contents
          mainPanel(
            # text input control
          textInput(inputId = "workerid",   # control ID
                    label = inputLabel,     # label to appear on top of control (color conditioned above)
                    placeholder = "enter MTurk ID here"), # text to appear as placeholder inside control (an example of a unique ID)
          # action button to be pressed by user to continue
          actionButton(inputId = "consent",   # button ID
                       label = "Start",   # button label (text displayed to the user)
                       class = "btn btn-primary")   # css class (defines button color as specified in the css file)
          )
        )
      )}
    
    # --------------------------------
    # (4) INSTRUCTIONS - 1     ---
    # --------------------------------
    if (CurrentValues$page == "instructions1") {   # conditionl determining whether page is displayed
      
      # content
      return(
        list(
          br(),
          span(h2(strong("Context Words")), style="color:#2780e3"),   # page title (smaller h2)
          p("A famous maxim in the study of linguistics states that:"),
          p(strong(em("You shall know a word by the company it keeps.")), "(Firth, 1957)"),
          p("This task is designed to help us understand the nature of the ''company'' that words ''keep'': that is, their CONTEXT."),
          br(),
          p("Specifically, for a CUE WORD, its CONTEXT WORDS include words that:"),
          column(12,
                 wellPanel(
                   tags$ul(
                     tags$li("Tend to occur in the vicinity of the CUE WORD. That is, they are words that appear close to the CUE WORD in written or spoken language.")),
                   p("AND/OR", align = "center"),
                   tags$ul(
                     tags$li("Tend to occur in similar situations to the CUE WORD in spoken and written language. That is, they are words that regularly appear with other words that are closely related to the CUE WORD.")))),
          br(),
          p("For example, CONTEXT WORDS for the cue word COFFEE include:"),
          tags$ol(
            tags$li(em("cup"), "(tends to occur in the vicinity of COFFEE)."), 
            tags$li(em("tea"), "(tends to occur in similar situations to COFFEE, for example when discussing drinks).")
          ),
          br(),
          p("Click ''Next'' to continue"),
          # action button to be pressed by user to continue
          actionButton(inputId = "goto.instructions2",   # button ID 
                       label = "Next",   # button label (text displayed to the user) 
                       class = "btn btn-primary"),   # css class (defines button color as specified in the css file)
          br()
        )
      )}
    
    # --------------------------------
    # (4) INSTRUCTIONS - 1     ---
    # --------------------------------
    if (CurrentValues$page == "instructions2") {   # conditionl determining whether page is displayed
      
      # content
      return(
        list(
          br(),
          span(h2(strong("Task Description")), style="color:#2780e3"),   # page title (smaller h2)
          br(),
          p("For each iteration of the task (", total_tasks, " in total including trial and screener tasks):"),
          br(),
          tags$ol(
            tags$li("You will be given a cue word (top center of the screen) and two candidate context words (on either side of the cue word)."),
            br(),
            tags$li("Please select the candidate context word that you find best meets the definition of a context word."),
            br(),
            tags$li("If both are reasonable context words, please select whichever you find most intuitive."),
            br(),
            tags$li("You must select", strong("one and only one"), "of the two candidate context words.")),
            br(),
          p("Keep in mind, some iterations are for screening purposes. These are tasks for which there is clearly a correct answer."),
          br(),
          p("Wrong answers in these screening tasks will automatically end your participation so", strong("be sure to read carefully.")),
          br(),
          p("The trial task that follows is meant for you to practice. Like screening tasks, the trial task has a correct answer."),
          br(),
          p("Click ''Next'' to continue to the trial runs"),
          # action button to be pressed by user to continue
          actionButton(inputId = "goto.trial1",   # button ID 
                       label = "Next",   # button label (text displayed to the user) 
                       class = "btn btn-primary"),   # css class (defines button color as specified in the css file)
          br()
        )
      )}
    
    # --------------------------------
    # (5) TEST ROUNDS - LDT TASK ---
    # --------------------------------

    # --------------------------------
    # (4) trial - 2     ---
    # --------------------------------
    
    if (CurrentValues$page %in% ldt_trial$variable){   # conditionl determining whether page is displayed
      CurrentValues$trial_index <- which(ldt_trial$variable == CurrentValues$page)
      return(
        LexicalDecisionTask(title = paste0("Trial ", CurrentValues$trial_index, " of ", num_trials), variable = ldt_trial$variable[CurrentValues$trial_index], 
                            cue = ldt_trial$cue[CurrentValues$trial_index], context = c(ldt_trial$left.word[CurrentValues$trial_index], ldt_trial$right.word[CurrentValues$trial_index]), 
                            nextInputID = ldt_trial$nextInputID[CurrentValues$trial_index], CurrentValues = CurrentValues)
      )}
    
    # --------------------------------
    # (7) INSTRUCTIONS - 4     ---
    # --------------------------------
    if (CurrentValues$page == "instructions3") {   # conditionl determining whether page is displayed
      
      # content
      return(
        list(
          br(),
          span(h2(strong("Ready to Start?")), style="color:#2780e3"),   # page title (smaller h2)
          br(),
          p("Thank you for completing the trial runs, if you feel ready to begin the real tasks, click ''Continue''. The first cue will immediately appear on the screen."),
          br(),
          p("If you want to return to the instructions click on ''Back to instructions''."), 
          br(),
          # action button to be pressed by user to continue
          
          actionButton(inputId = "goto.lexical1",   # button ID 
                       label = "Continue",   # button label (text displayed to the user) 
                       class = "btn btn-primary"),   # css class (defines button color as specified in the css file)
          HTML("<br><br>"),
          actionButton(inputId = "goto.instructions2",   # button ID 
                       label = "Back to instructions",   # button label (text displayed to the user) 
                       class = "btn btn-primary"))   # css class (defines button color as specified in the css file)
      )}
    
    
    # --------------------------------
    # (4) LDT-1    ---
    # --------------------------------
    
    if (CurrentValues$page %in% ldt_data$variable){   # conditionl determining whether page is displayed
    CurrentValues$index <- which(ldt_data$variable == CurrentValues$page)
      return(
        LexicalDecisionTask(title = paste0("Task ", CurrentValues$index, " of ", num_ldt), variable = ldt_data$variable[CurrentValues$index], 
                            cue = ldt_data$cue[CurrentValues$index], context = c(ldt_data$left.word[CurrentValues$index], ldt_data$right.word[CurrentValues$index]), 
                            nextInputID = ldt_data$nextInputID[CurrentValues$index], CurrentValues = CurrentValues)
      )}
    
    # --------------------------------
    # (8) SURVEY        ---
    # --------------------------------
    
    if (CurrentValues$page == "survey") {
      
      # Throw an error if not all question have been answered.
      if (CurrentValues$errors == "answerQuestions") {
        answerQuestions <- p(style = "color:Red", "Please answer all required questions!")
      } else {
        answerQuestions <- ""
      }
      
      return(list(
        
        span(h2(strong("Survey (1 of 1)")), style="color:#2780e3"),
        
        br(),
        
        p("To conclude please fill out this short survey."),
        
        br(),
        
        radioButtons(inputId = "party", 
                     label = "Generally speaking, do you usually think of yourself as a Democrat, a Republican, an Independent, or what?",
                     choices = list("Strong Democrat" =  1,
                                 "Weak Democrat" = 2,
                                 "Independent Democrat" = 3,
                                 "Independent Independent" = 4,
                                 "Independent Republican" = 5,
                                 "Weak Republican" = 6,
                                 "Strong Republican" = 7,
                                 "Other party" = 8,
                                 "No preference" = 9
                     ), selected = 99, width = "100%"),
        
        br(),
        
        radioButtons(inputId = "ideology", 
                     label = "We hear a lot of talk these days about liberals and conservatives. 
                     Here is a seven-point scale on which the political views that people might hold are arranged from extremely liberal to extremely conservative. 
                     Where would you place yourself on this scale?",
                     choices = list("Extremely liberal" =  1,
                                 "Liberal" = 2,
                                 "Slightly liberal" = 3,
                                 "Moderate; middle of the road" = 4,
                                 "Slightly conservative" = 5,
                                 "Conservative" = 6,
                                 "Extremely conservative" = 7,
                                 "Haven't thought much about this" = 8
                     ), selected = 99, width = "100%"),
        
        br(),
        
        radioButtons(inputId = "sex",
                     label = "What is your sex?",
                     choices = list("Male" = 1, "Female" = 2, "Other" = 3),
                     selected = 99, width = "100%"),
        
        #HTML("<br>"),
        
        #radioButtons("interesting", 
        #             label = "How engaging did you find the HIT?",
        #             choices = c("1 - Not at all engaging" =  1,
        #                         "2" = 2,
        #                         "3" = 3,
        #                         "4" = 4,
        #                         "5 - Very engaging" = 5
        #             ), selected = 99, width = "100%"),
        
        #textAreaInput("comments",
        #              label = "If you have any additional comments, please enter them below",
        #              resize = "both"),
        
        HTML("<br>"),
        
        p(answerQuestions),
        
        actionButton(inputId = "goto.savedata",
                     label = "Next", 
                     class = "btn btn-primary"),
        
        HTML("<br><br><br>"))
      )
    }
    
    # --------------------------------
    # (4) SAVE DATA    ---
    # --------------------------------
    
    if (CurrentValues$page == "savedata") {   # conditionl determining whether page is displayed
      
      # content
      return(
        list(
          br(),
          span(h2(strong("Save Data")), style="color:#2780e3"),   # page title (smaller h2)
          br(),
          p("You have completed all the required tasks. To save your data and get your HIT completion code press ''Save my data''."),
          br(),
          p("If for some reason you do not want to have your data saved, simply close this window (NOTE: you will not receive compensation)."), 
          br(),
          # action button to be pressed by user to continue
          
          actionButton(inputId = "goto.goodbye",   # button ID 
                       label = "Save my data",   # button label (text displayed to the user) 
                       class = "btn btn-primary"))   # css class (defines button color as specified in the css file)
      )}
    

    # --------------------------------
    # (18) GOODBYE    ---
    # --------------------------------
    if (CurrentValues$page == "goodbye") {
      
      # CALCULATE COMPLETION CODE  
      completion.code <- paste0("LDT-", sample(100:999, size = 1), "-", sample(100:999, size = 1), "-", sample(100:999, size = 1))
      
      return(list(
        br(),
        h3("Thank you for your participation!"),
        br(),
        p("Here is your randomly generated study completion code. Please enter this code to submit your HIT."),
        h3(completion.code),
        br(),
        h3("What was this survey about?"),
        br(),
        p("This survey is part of an academic study exploring context words."),
        br(),
        p("You may proceed to close this window.")
      ))
    }
    
    # --------------------------------
    # (18) BOOTED   ---
    # --------------------------------
    if (CurrentValues$page == "booted1") {
      
      return(list(
        br(),
        h3("SORRY!"),
        br(),
        p("You failed a trial task. You can either:"),
        br(),
        tags$ul(
          tags$li("Restart the HIT by refreshing your browser (you will be taken back to the consent form)."),
          br(),
          p("OR"),
          br(),
          tags$li("Close this window and not complete the HIT (NOTE: you will not receive compensation).")),
        br()
        #br(),
        #p("You may proceed to close this window.")
      ))
    }
    
    if (CurrentValues$page == "booted2") {
      
      return(list(
        br(),
        h3("SORRY!"),
        br(),
        p("You failed a screener task. You can either:"),
        br(),
        tags$ul(
          tags$li("Restart the HIT by refreshing your browser (you will be taken back to the consent form)."),
          br(),
          p("OR"),
          br(),
          tags$li("Close this window and not complete the HIT (NOTE: you will not receive compensation).")),
        br()
        #br(),
        #p("You may proceed to close this window.")
      ))
    }
    
    
  })
  
  # --------------------------------
  # SECTION C3: PAGE NAVIGATION        ----
  # --------------------------------
  
  # consent page
  observeEvent(input$consent, {
    if (input$workerid == ""){
      CurrentValues$errors <- "Blank_Name"
    } else {
      CurrentValues$page <- "instructions1"
    }
  })
  
  # LDTs trial
  observeEvent(input[[ldt_trial$nextInputID[CurrentValues$trial_index]]], {
    if (input[[paste0(ldt_trial$variable[CurrentValues$trial_index],".left")]] == input[[paste0(ldt_trial$variable[CurrentValues$trial_index],".right")]]) {
      CurrentValues$errors <- paste0(ldt_trial$variable[CurrentValues$trial_index], ".error")
    } else {
      if(ldt_trial$left.correct[CurrentValues$trial_index] != input[[paste0(ldt_trial$variable[CurrentValues$trial_index],".left")]]){
        CurrentValues$page <- "booted1"
      }else{
      
      CurrentValues$page <- ldt_trial$nextInputID[CurrentValues$trial_index]
      }
    }
  })
  
  # LDTs
  observeEvent(input[[ldt_data$nextInputID[CurrentValues$index]]], {
    if (input[[paste0(ldt_data$variable[CurrentValues$index],".left")]] == input[[paste0(ldt_data$variable[CurrentValues$index],".right")]]) {
      CurrentValues$errors <- paste0(ldt_data$variable[CurrentValues$index], ".error")
    } else {
      if(ldt_data$screener[CurrentValues$index] & (ldt_data$left.correct[CurrentValues$index] != input[[paste0(ldt_data$variable[CurrentValues$index],".left")]])){
        CurrentValues$page <- "booted2"
      }else{
      
      LexicalData[[ldt_data$variable[CurrentValues$index]]] <- data.table(input[[paste0(ldt_data$variable[CurrentValues$index],".left")]],
                                                                          input[[paste0(ldt_data$variable[CurrentValues$index],".right")]])
      
      CurrentValues$page <- ldt_data$nextInputID[CurrentValues$index]
      }
    }
  })
  
  # survey
  observeEvent(input$goto.savedata, {
    # check wether all questions have been answered:
    if(any(is.null(input[["sex"]]), is.null(input[["party"]]), is.null(input[["ideology"]]) )){
      CurrentValues$errors <- "answerQuestions"
      }else{
      CurrentValues$page <- "savedata"
    }})
  
  # other
  observeEvent(input$goto.instructions2, CurrentValues$page <- "instructions2")
  observeEvent(input$goto.trial1, CurrentValues$page <- "trial1")
  observeEvent(input$goto.lexical1, CurrentValues$page <- "lexical1")

  # --------------------------------
  # SECTION C5: SAVE DATA        ----
  # --------------------------------
  observeEvent(input[["goto.goodbye"]], {
   
    # Create progress message
    withProgress(message = "Saving data...",
                 value = 0, {
                   
                   incProgress(.25)
                   
                   # Write associations data
                   LexicalData.i <- lapply(ldt_data$variable, function(x) LexicalData[[x]])
                   LexicalData.i <- do.call(rbind, LexicalData.i) %>% set_colnames(., c("left.choice", "right.choice"))
                   ldt_data$workerid <- input$workerid
                   LexicalData.i <- cbind(ldt_data, LexicalData.i)
                   LexicalData.i <- LexicalData.i[screener == FALSE,] # keep only the non-screeners
                   LexicalData.i <- LexicalData.i[, c("workerid", "cue", "left.source", "right.source", "left.word", "right.word", "left.choice", "right.choice")]
                   rownames(LexicalData.i) <- NULL
                   
                   incProgress(.5)
                   
                   # Write survey data to a datatable
                   
                   SurveyData.i <- data.table("workerid" = input$workerid,
                                              "sex" = input$sex,
                                              "party" = input$party,
                                              "ideology" = input$ideology
                                              #"interesting" = input$interesting
                   )
                   
                   
                   incProgress(.5)
                   
                   LexicalDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(LexicalData.i), "_ldt.csv")
                   SurveyDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(SurveyData.i), "_survey.csv")
       
                   # Create The Filepath and save the data depending on the method chosen:
                   
                   LexicalDatafilePath <- file.path(tempdir(), LexicalDatafileName)
                   write.csv(LexicalData.i, LexicalDatafilePath, row.names = TRUE, quote = TRUE)
                   rdrop2::drop_upload(LexicalDatafilePath, path = outputDir, dtoken = droptoken)
                   
                   SurveyDatafilePath <- file.path(tempdir(), SurveyDatafileName)
                   write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
                   rdrop2::drop_upload(SurveyDatafilePath, path = outputDir, dtoken = droptoken)
                   
                   # report progress (of data saving) to the user
                   incProgress(.40)
                   
                   # go to goodbye page
                   CurrentValues$page <- "goodbye"
                   Sys.sleep(.25)
                   incProgress(1)
                 })
  })
  
  # --------------------------------
  # SECTION C6: WHAT TO DO WHEN A KEY IS PRESSED ----
  # http://www.javascripter.net/faq/keycodes.htm
  # --------------------------------
  # upon observing a key event
  observeEvent(input$lastkeypresscode, {
    # isolate keyboard event
    n <- input$lastkeypresscode[1]
    # if we are on the welcome page
    if (CurrentValues$page == "welcome") {
      if (n == 13) {   # if the enter key is pressed
        CurrentValues$page <- "instructions1"   # go to the literature page
      }
    }
  })
  
}

# Create app!
shinyApp(ui = ui, server = server)