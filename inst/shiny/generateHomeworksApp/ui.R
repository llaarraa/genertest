library(shiny)
library(shinyIncubator)
library(genertest)

### shinyApp to use the generateHomeworks function from the genertest package

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("shiny for generateHomeworks"),
  
  sidebarPanel(
    
    #input of csv file
    
    #if (interactive() == TRUE) {
    #  actionButton('upload', 'Upload')
    #} else {
    #  fileInput('file1')
    #}, 
    
    
    #submitButton("Upload"),
    
    
    #read how many tests
    #numericInput("num.tests", "Number of tests to generate:", 2, min=1),
    
    
    fileInput('file1', 'Choose the Tab delimited file containing the assignments (questions)',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')
    ), #end fileInput
    #      tags$hr(),
    
    
    fileInput('file2', 'Choose the Tab delimited file containing information about the groups',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')
    ), #end fileInput
    #      tags$hr(),
    
    
    
    #options for fileInput
    #      checkboxInput('header', 'Header', TRUE),
    #      radioButtons('sep', 'Separator',
    #                   c(Comma=',',
    #                     Semicolon=';',
    #                     Tab='\t'),
    #                   'Tab'),
    #      radioButtons('quote', 'Quote',
    #                   c(None='',
    #                     'Double Quote'='"',
    #                     'Single Quote'="'"),
    #                   'Double Quote'),
    
    
    ############# options for the genertest function
    
    
    conditionalPanel(
      condition = "output.contents && output.contents2",
      

      helpText(strong("Specify how would you like to generate the assignments"),
               strong("using the questions data base as an input")),
      
      
  
  #    numericInput("repeat.each.test", "Number of times that each test needs to be permuted:", 1, min=0),
      
      
      
       
          
      # numericInput("tot.points", "Total number of points to include in each test:", 50, min=0) ,
      
      #matrix with the IDs of the questions to select
   #   uiOutput("QID"),
      
      
      numericInput("my.seed", "Seed used to inizialize the random number generator:", 1999),
      
      
      #numericInput("min.distance", "Minimum distance between two consecutive tests that contain the same questions", 0, min=0),
      
      
      textInput("my.final.sentence", "Sentence to write at the end of each assignment", value = "Good luck!"),
      
      
      textInput("my.language", "Language used in the assignments", value = "english"),
      
      
      
      helpText(strong("Specify how the header should look like")),
      
  
      
       
      textInput("my.title", "Name of the assignment", value = "Assignment"),
      
      textInput("my.date", "Date of the assignment", value = "31.07.2013"),
      
    #  textInput("my.prefix", "String with which the names of the files with the assignments begin", value = "exam"),
      
      helpText(strong("Specify how these words should appear in the header")),
      
      
      textInput("source.text", "Sources", value = "Sources"),
      
      textInput("template.text", "Template", value = "Template"),
      
      textInput("date.text", "Due date", value = "Due date"),
      
      textInput("group.member.text", "Group members", value = "Group members"),
  
      textInput("text.groupdata", "Data about the group", value = "Data about the group"),
      
      textInput("text.assignment", "Assignment", value = "Assignment"),
      
      
      
      
      
      
      
      
      
      #radioButtons("dist", "Distribution type:",
      #             list("Normal" = "norm",
      #                  "Uniform" = "unif",
      #                  "Log-normal" = "lnorm",
      #                  "Exponential" = "exp")),
      
      
      
      
      
      checkboxInput(inputId = "generate.solutions",
                    label = strong("Generate also the solutions"),
                    value = FALSE),
      
      
      checkboxInput(inputId = "use.Sweave",
                    label = strong("Use Sweave code"),
                    value = TRUE),
      
      checkboxInput(inputId = "compile.pdf",
                    label = strong("Compile to PDF files"),
                    value = TRUE),#,
      
      
      textInput("my.outdir", "Existing directory where you would like to store the results. (If left empty the results will be stored in a subdirectory of the shiny app)", value = ""),
      
      
      fileInput('files.to.move', 'Additional files needed to generate the outputs (images, txt, ...)', multiple=TRUE#, 
                #accept=""
                #accept=c('text/csv', 'text/comma-separated-values,text/plain')
      ) #end fileInput
      #      tags$hr(),
      
      
     # checkboxInput(inputId = "merge.pdf",
     #               label = strong("Merge the PDF files"),
    #              value = TRUE)#,
    #  
      
      
      #submitButton("Generate the assignments")
      
      #checkboxInput(inputId = "start",
      #              label = strong("Start generating the files"),
      #              value = FALSE)#,#,
      
      #, submitButton("Generate the assignments")
  #     actionButton("action_button", "Generate the assignments")
      
   # )#end conditionPanel1 
      
    )#end conditionPanel2     
      
    
  ),#end sidebarPanel
  
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Assignment Data", tableOutput("contents")), 
      tabPanel("Group Data", tableOutput("contents2")), 
      
      tabPanel("Summary for assignments", tableOutput("dataSummary")), 
      tabPanel("Summary for groups", tableOutput("dataSummary2")), 
      tabPanel("Results", tableOutput("topics")),
      tabPanel("Debug", textOutput("genertest"))
    )
  )
  
  #  mainPanel(tableOutput('contents'),
  #            uiOutput("Tot.points"),
  #            # uiOutput("QID"),
  #            #uiOutput("genertest"),
  #            uiOutput("dat")
  #  )
  #,
  #tableOutput('my.output'))
  
  #end mainPanel
  
  
) #end sidebarPanel
        
)#end shinyUI 
