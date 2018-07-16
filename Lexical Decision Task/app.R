# ------------------------------
#  SEMANTICA
#  Authors: Pedro L Rodriguez
#  Last modified: 06-06-2018
#
#   CODE SECTIONS
#
#   SECTION A: PRELIMS
#   SECTION B: USER INTERFACE
#   SECTION C: SERVER
#
# ------------------

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

N <- 1  # number of candidates to sample for each cue
#human_candidate1 <- apply(human_top5, 2, function(x) sample(x, N, replace = TRUE))  # sample from human embeddings (leaner BUT does not avoid matches between both sources)
#machine_candidate1 <- apply(glove_top5, 2, function(x) sample(x, N, replace = TRUE))  # sample from machine embeddings (leaner BUT does not avoid matches between both sources)

# for each cue sample 10 from each source making sure sources do not match on candidate context word
human_candidate <- c()
machine_candidate <- c()
for(w in 1:ncol(human_top5)){
  samp_human <- samp_machine <- "start"
  while(any(samp_human == samp_machine)){
    # sample human
    if(N > 1){
    samp_human <- rep_len(human_top5[,w], length.out = N) # by using rep, we avoid too many repeats
    samp_human <- samp_human[sample(seq(1,length(samp_human),1), replace = FALSE)]
    # sample machine
    samp_machine <- rep_len(glove_top5[,w], length.out = N)
    samp_machine <- samp_machine[sample(seq(1,length(samp_machine),1), replace = FALSE)]
    }else{
      samp_human <- sample(human_top5[,w], 1)
      samp_machine <- sample(glove_top5[,w], 1)
    }
  }
  human_candidate <- data.frame(cbind(human_candidate, samp_human))
  machine_candidate <- data.frame(cbind(machine_candidate, samp_machine)) 
}

human_candidate <- data.frame(lapply(human_candidate, as.character), stringsAsFactors=FALSE) # convert to character
machine_candidate <- data.frame(lapply(machine_candidate, as.character), stringsAsFactors=FALSE) # convert to character
colnames(human_candidate) <- colnames(human_top5)
colnames(machine_candidate) <- colnames(human_top5)
# randomize cue order
cues <- sample(colnames(human_top5), replace = FALSE)
human_candidate <- human_candidate[,cues]
machine_candidate <- machine_candidate[,cues]
# sample 10 comparisons for each cue, randomizing left/right order
ldt_data <- data.table()
for(j in 1:length(cues)){
  for(i in 1:N){
    source_order <- sample(c("H","M"), 2, replace = FALSE)
    if(N>1){
    candidates <- if(source_order[1] == "H"){c(unname(human_candidate[i,j]), unname(machine_candidate[i,j]))}else{c(unname(machine_candidate[i,j]),unname(human_candidate[i,j]))}
    }else{candidates <- if(source_order[1] == "H"){unname(unlist(c(human_candidate[j], machine_candidate[j])))}else{unname(unlist(c(machine_candidate[j],unname(human_candidate[j]))))}}
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
ldt_data$nextInputID <- c(ldt_data$variable[2:nrow(ldt_data)], "savedata")
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
          p(span(strong("Compensation:"), style="color:#2780e3"), "$0.60"),
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
                   
                   incProgress(.5)
                   
                   LexicalDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(LexicalData.i), "_ldt.csv")
       
                   # Create The Filepath and save the data depending on the method chosen:
                   
                   LexicalDatafilePath <- file.path(tempdir(), LexicalDatafileName)
                   write.csv(LexicalData.i, LexicalDatafilePath, row.names = TRUE, quote = TRUE)
                   rdrop2::drop_upload(LexicalDatafilePath, path = outputDir, dtoken = droptoken)
                   
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