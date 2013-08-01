library(shiny)
library(shinyIncubator)
library(genertest)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("shiny for genertest"),
  
  sidebarPanel(
    
    #input of csv file
    
    #if (interactive() == TRUE) {
    #  actionButton('upload', 'Upload')
    #} else {
    #  fileInput('file1')
    #}, 
    
    
    #submitButton("Upload"),
    
    
    fileInput('file1', 'Choose the Tab delimited file containing the questions',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')
    ), 
    
    
    #upload a sample data set
    ##actionButton("file1sample", "Upload a sample dataset"),
    
    #end fileInput
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
      condition = "output.contents",
      
      
      
      helpText(strong("Specify how would you like to generate the tests"),
               strong("using the questions data base as an input")),
      
      
      
      
      numericInput("num.tests", "Number of tests to generate:", 1, min=1),
      
      numericInput("repeat.each.test", "Number of times that each test needs to be permuted:", 1, min=0),
      
      numericInput("tot.points", "Total number of points to include in each test:", 50, min=0) ,
      
      uiOutput("topics.points"),
      
      
      numericInput("my.seed", "Seed used to inizialize the random number generator:", 1999),
      
      
      numericInput("min.distance", "Minimum distance between two consecutive tests that contain the same questions", 0, min=0),
      
      
      textInput("my.final.sentence", "Sentence to write at the end of each test", value = "Good luck!"),
      
      
      helpText(strong("Specify how the header should look like")),
      
      
      
      
      textInput("my.title", "Name of the exam", value = "Exam"),
      
      textInput("my.date", "Date of the exam", value = "31.07.2013"),
      
      textInput("my.prefix", "String with which the names of the files with the tests begin", value = "exam"),
      
      textInput("head.name", "Name of the student", value = "Name"),
      
      textInput("head.id", "Identification number of the student", value = "ID number"),
      
      textInput("head.points", "Number of points", value = "Number of points"),
      
      textInput("head.prefix", "Prefix used to generate and alpha-numerical ID for each test", value = "STAT"),
   
      
      textInput("my.language", "Language used in the tests", value = "english"),
      
      
      
      textInput("my.outdir", "Existing directory where you would like to store the results. (If left empty the results will be stored in a subdirectory of the shiny app)", value = ""),
      
      
      fileInput('files.to.move', 'Additional files needed to generate the outputs (images, txt, ...)', multiple=TRUE#, 
                #accept=""
                #accept=c('text/csv', 'text/comma-separated-values,text/plain')
      ),
      
      
      
      
      
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
                    value = TRUE),
      
      checkboxInput(inputId = "merge.pdf",
                    label = strong("Merge the PDF files"),
                    value = TRUE)#,
      
      
      
      #submitButton("Generate the Tests")
      
      #checkboxInput(inputId = "start",
      #              label = strong("Start generating the files"),
      #              value = FALSE)#,#,
      
      #, submitButton("Generate the tests")
  #     actionButton("action_button", "Generate the tests")
      
    )#end conditionPanel
    
  ),#end sidebarPanel
  
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Data", tableOutput("contents")), 
      tabPanel("Summary", tableOutput("dataSummary")), 
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
