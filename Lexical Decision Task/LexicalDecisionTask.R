LexicalDecisionTask <- function(title = NULL, variable = NULL, cue = NULL, context = NULL, nextInputID = NULL, CurrentValues = CurrentValues){

  if (CurrentValues$errors == paste0(variable, ".error")){
    instructions <- p(style = "color:Red", "Select the best candidate context word for the cue word provided by clicking on the respective checkbox below the word.")   
  } else {
    instructions <- p("Select the best candidate context word for the cue word provided by clicking on the respective checkbox below the word.")
  }
  
  # content
  return(
    list(
      br(),
      span(h3(strong(title)), style="color:#2780e3"),
      br(),
      fluidPage(
        br(),
        fluidRow(
          column(12,
                 h3(strong(toupper(cue))),
                 br(),
                 column(4,
                        wellPanel(
                          p(style = "font-size:120%;", context[1])), 
                        checkboxInput(inputId = paste0(variable, ".left"), ""),
                        offset = 1, align = "center"),
                 
                 column(4,
                        wellPanel(
                          p(style = "font-size:120%;", context[2])), 
                        checkboxInput(inputId = paste0(variable, ".right"), ""),
                        offset = 2, align = "center"), 
                 align = "center")
        ),
        br(),
        column(12,
        instructions, align = "center"),
        # action button to be pressed by user to continue
        br(),
        br(),
        
        column(12,
               #column(4,   
        p("Click ''Next'' to continue"),
        actionButton(inputId = nextInputID,   # button ID 
                     label = "Next",   # button label (text displayed to the user) 
                     class = "btn btn-primary"), align = "center"),   # css class (defines button color as specified in the css file)
        br(),
        br()
      )
    )
  )}